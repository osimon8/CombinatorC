open Ast.Expression

type pattern = (var_type * string) list


module Ctxt = struct

  type t = (string * (var_type * expression)) list
  let empty = []
  let bindings = ref []

  let clear () = 
    bindings := empty

  let get () = 
    !bindings
  let set new_binds = 
    bindings := new_binds

  let add (id:string) (bnd:var_type * expression) : unit = 
    bindings := (id,bnd) :: !bindings 

  let add_no_dup (id:string) (bnd:var_type * expression) : unit = 
    begin match List.assoc_opt id !bindings with 
    | Some _ -> prerr_endline @@ "Duplicate identifier: \"" ^ id ^ "\""; exit 1
    | None ->  add id bnd 
  end 

  let remove (id:string) : unit = 
    bindings := List.remove_assoc id !bindings

  let lookup (id:string) : var_type * expression =
    begin match List.assoc_opt id !bindings with 
    | Some s -> s
    | None -> prerr_endline @@ "Unknown identifier: \"" ^ id ^ "\""; exit 1 
  end 

  let lookup_pattern (id:string) : pattern =
    match List.assoc_opt id !bindings with
    | Some (TPattern, Pattern args) -> args
    | _ -> prerr_endline @@ "\"" ^ id ^ "\" is not a known pattern"; exit 1

end