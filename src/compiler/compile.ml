open Ast.Circuit
open Ast.Combinator
open Optimize
open FirstPhase
open Layout
open Utils
open Ast.Bexp
open Ast.Expression
open Composition
open Directive
open Pattern
open Ctxt
open Config

let compile_bexp_to_circuit (o_sig : symbol) (b : bexp) : circuit =
  let circuit = circuit_of_bexp o_sig b in
  if get_optimize () then
    let circuit_opt = primitive_optimization circuit in
    circuit_opt
  else circuit

let interpret_type exp : var_type * expression =
  ( begin
      match exp with
      | Int _ -> TInt
      | Stamp _ -> TStamp
      | Condition _ -> TCondition
      | Signal _ -> TSignal
      | Circuit _ | For _ | Call _ -> TCircuit
      | Pattern _ -> TPattern
      | Var s ->
          let t, _ = Ctxt.lookup s in
          t
    end,
    exp )

let interpret_types (exps : expression list) : (var_type * expression) list =
  List.map interpret_type exps

let rec bottom_out_var e : expression =
  begin
    match e with
    | Var v ->
        let _, v = Ctxt.lookup v in
        bottom_out_var v
    | _ -> e
  end

let expression_of_delayed (exp : delayed_expression) : expression =
  match exp with
  | Immediate exp -> exp
  | Delayed bexp -> (
      let bexp = bind_vars_of_bexp bexp in
      match expression_of_bexp bexp with
      | Immediate exp -> exp
      | Delayed _ -> Circuit (Inline (bexp, "check", None)))

let rec evaluate_expression (expression : expression) : expression =
  let rec inter exp =
    begin
      match exp with
      | Call (p, args) ->
          let args = List.map expression_of_delayed args in
          Circuit (Compiled (evaluate_call p args))
      | Condition b ->
          let b = bind_vars_of_bexp b in
          if valid_condition b then Condition b
          else begin
            match interpret_bexp b with
            | Some i -> Int i
            | None -> Circuit (Inline (b, "signal-check", None))
          end
      | Var v -> inter (bottom_out_var exp)
      | For (concat, id, l_exp, u_exp, count_down, block) ->
          let l_exp = evaluate_expression (expression_of_delayed l_exp) in
          let u_exp = evaluate_expression (expression_of_delayed u_exp) in
          let s exp =
            begin
              match exp with
              | Int i -> i
              | _ ->
                  let ty, _ = interpret_type exp in
                  prerr_endline
                  @@ Printf.sprintf "Loop bound does not evaluate to \"int\", evaluates to \"%s\""
                       (string_of_type ty);
                  exit 1
            end
          in
          let l = s l_exp in
          let u = s u_exp in
          let n_out = num_outputs block in
          if n_out = 0 then (
            prerr_endline "For expression missing output!";
            exit 1)
          else if n_out > 1 then (
            prerr_endline "For expression has multiple outputs, must have exactly 1";
            exit 1);
          Ctxt.add id (TInt, Int l);
          let c = evaluate_for concat id u count_down block in
          Circuit (Compiled c)
      | Signal _ | Int _ | Stamp _ | Circuit _ | Pattern _ -> exp
    end
  in
  inter expression

and evaluate_call (pattern : string) (args : expression list) : compiled_circuit =
  let p_args = Ctxt.lookup_pattern pattern in
  let args =
    List.map
      (fun a ->
        let a = evaluate_expression a in
        interpret_type a)
      args
  in
  let lp = List.length p_args in
  let l = List.length args in
  if lp <> l then (
    prerr_endline @@ Printf.sprintf "Pattern \"%s\" expects %d arguments, got %d" pattern lp l;
    exit 1);
  let zipped = List.combine p_args args in
  let old_bindings = Ctxt.get () in
  List.iter
    (fun (p, a) ->
      let pty, pname = p in
      let aty, exp = a in
      if pty <> aty then (
        prerr_endline
          (Printf.sprintf
             "Pattern \"%s\" expects argument \"%s\" to have type \"%s\", but an argument of type \
              \"%s\" was provided"
             pattern pname (string_of_type pty) (string_of_type aty));
        exit 1)
      else Ctxt.add pname (pty, exp))
    zipped;
  let circuit = evaluate_pattern pattern args in
  Ctxt.set old_bindings;
  circuit

and compile_ctree_to_circuit (ctree : ctree) : compiled_circuit =
  let w o_sig bexp =
    let ast = if get_optimize_b () then optimize_bexp bexp else bexp in
    begin
      match bexp with
      | Var i ->
          let e = bottom_out_var (Var i) in
          begin
            match e with
            | Signal _ | Condition _ | Stamp _ | Int _ ->
                Abstract (compile_bexp_to_circuit o_sig ast)
            | Circuit c -> compile_ctree_to_circuit c
            | _ ->
                prerr_endline "Cannot bind variable to circuit. This error should never occur";
                exit 1
          end
      | _ -> Abstract (compile_bexp_to_circuit o_sig ast)
    end
  in
  let rec r ctree =
    let comp f c1 c2 loc =
      let c1 = r c1 in
      let c2 = r c2 in
      let o =
        begin
          match loc with
          | Some l -> l
          | None -> get_origin ()
        end
      in
      let x c1 c2 rev =
        let delta = 2 in
        let c2, c2_layout = c2 in
        let p2, s2, pl2 = c2_layout in
        let o2 = offset o p2 in
        let p2, s2, pl2 = move_layout c2_layout o2 in
        let l1 = layout c1 in
        let p1, s1, _ = l1 in
        let mv =
          if rev then (float_of_int (fst s2 + delta), 0.) else (float_of_int (-fst s1 - delta), 0.)
        in
        let o1 = offset (offset o p1) mv in
        let p1, s1, pl1 = move_layout l1 o1 in
        let c = if rev then f c2 c1 else f c1 c2 in
        let o = if rev then p2 else p1 in
        Concrete
          (c, (o, (fst s1 + fst s2 + delta, snd s1 + snd s2), if rev then pl2 @ pl1 else pl1 @ pl2))
      in
      begin
        match (c1, c2) with
        | Abstract c1, Abstract c2 ->
            let c = f c1 c2 in
            begin
              match loc with
              | Some pos -> Concrete (c, layout ~pos c)
              | None -> Abstract c
            end
        | Abstract c1, Concrete c2 -> x c1 c2 false
        | Concrete c1, Abstract c2 -> x c2 c1 true
        | Concrete c1, Concrete c2 ->
            let c1, c1_layout = c1 in
            let c2, c2_layout = c2 in
            let p1, _, pl1 = c1_layout in
            let p2, _, pl2 = c2_layout in
            let o1 = offset o p1 in
            let o2 = offset o p2 in
            let p1, s1, pl1 = move_layout c1_layout o1 in
            let p2, s2, pl2 = move_layout c2_layout o2 in
            let c = f c1 c2 in
            let l = (o, (fst s1 + fst s2, max (snd s1) (snd s2)), pl1 @ pl2) in
            Concrete (c, l)
      end
    in
    begin
      match ctree with
      | Compiled c -> c
      | Inline (b, o_sig, loc) ->
          let c = w o_sig b in
          begin
            match loc with
            | Some loc -> compile_ctree_to_circuit (bind_loc (Compiled c) loc)
            | None -> c
          end
      | Union (b1, b2, loc) -> comp circuit_union b1 b2 loc
      | Concat (b1, b2, loc) -> comp circuit_concat b1 b2 loc
      | Expression (e, loc) -> evaluate_expression_to_ctree (expression_of_delayed e) loc
    end
  in
  let circuit = r ctree in
  circuit

and evaluate_expression_to_ctree exp loc : compiled_circuit =
  let exp = evaluate_expression exp in
  let rec inter exp =
    begin
      match exp with
      | Signal s -> Inline (Signal s, s, loc)
      | Int i -> Inline (Lit i, "signal-check", loc)
      | Condition b | Stamp b -> Inline (b, "signal-check", loc)
      | Circuit c -> c
      | Pattern _ ->
          prerr_endline "Can't evaluate pattern to circuit. Did you mean to provide arguments?";
          exit 1
      | _ -> failwith "Expression not reduced! Impossible error"
    end
  in
  let ctree = inter exp in
  begin
    match loc with
    | Some loc -> compile_ctree_to_circuit (bind_loc ctree loc)
    | None -> compile_ctree_to_circuit ctree
  end

and evaluate_for (concat : bool) (id : string) (bound : int32) (count_down : bool) (block : block) :
    compiled_circuit =
  let _, i = Ctxt.lookup id in
  let i =
    begin
      match i with
      | Int i -> Int32.to_int i
      | _ -> failwith "Invalid for ctr type, shouldn't happen"
    end
  in
  let bound = Int32.to_int bound in
  let finished i = (i < bound && count_down) || (i > bound && not count_down) in
  if finished i then (
    prerr_endline "For loop bounds result in 0 executions of loop, no output circuit";
    exit 1);
  let cmp () =
    let old = Ctxt.get () in
    let c = Compiled (List.nth (compile_block_to_circuits block) 0) in
    Ctxt.set old;
    c
  in
  let update_ctr i =
    Ctxt.remove id;
    let i = if count_down then i - 1 else i + 1 in
    Ctxt.add id (TInt, Int (Int32.of_int i));
    i
  in
  let rec eval prev i =
    if finished i then prev
    else
      let c = cmp () in
      let c = if concat then Concat (prev, c, None) else Union (prev, c, None) in
      eval c (update_ctr i)
  in
  let first = cmp () in
  let c = eval first (update_ctr i) in
  compile_ctree_to_circuit c

and compile_block_to_circuits (commands : block) : compiled_circuit list =
  let register ident b o_sig concrete =
    Ctxt.add ident
      (TCircuit, Circuit (Inline (b, o_sig, if concrete then Some (get_origin ()) else None)))
  in
  let compile command =
    begin
      match command with
      | CircuitBind (ident, b, o_sig_exp, concrete) ->
          let ty, exp = interpret_type @@ evaluate_expression (expression_of_delayed o_sig_exp) in
          let o_sig =
            begin
              match exp with
              | Signal s -> s
              | _ ->
                  prerr_endline
                  @@ Printf.sprintf "Cannot bind expression of type \"%s\" to circuit output signal"
                       (string_of_type ty);
                  exit 1
            end
          in
          register ident b o_sig concrete;
          None
      | Assign (ident, ty, exp) ->
          let exp = expression_of_delayed exp in
          let i_ty, _ = interpret_type exp in
          if i_ty <> ty then (
            prerr_endline
            @@ Printf.sprintf "Cannot assign expression of type \"%s\" to variable of type \"%s\""
                 (string_of_type i_ty) (string_of_type ty);
            exit 1);
          Ctxt.add_no_dup ident (ty, exp);
          None
      | Output e ->
          let e = expression_of_delayed e in
          Some (evaluate_expression_to_ctree e None)
      | OutputAt (e, (e1, e2)) ->
          let e = expression_of_delayed e in
          let e1 = bind_vars_of_bexp e1 in
          let e2 = bind_vars_of_bexp e2 in
          begin
            match (e1, e2) with
            | Lit v1, Lit v2 ->
                let loc = (Int32.to_float v1, Int32.to_float v2) in
                Some (evaluate_expression_to_ctree e (Some loc))
            | _ ->
                prerr_endline @@ "Output location doesn't map to tuple of ints";
                exit 1
          end
    end
  in
  let circuit_opts = List.map compile commands in
  deopt_list circuit_opts

let compile (commands : block) : compiled_circuit list =
  register_builtins ();
  compile_block_to_circuits commands
