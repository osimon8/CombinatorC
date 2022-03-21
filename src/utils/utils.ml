type json = Yojson.Safe.t

let create_ctr ?i:(i=1) () = 
  let s = ref (i - 1)  in
  fun () -> 
    incr s;
    (* print_endline ("INCED, RETURNING " ^ (string_of_int !s)); *)
    !s

let deopt_list l = 
    List.concat @@ List.map (function | None -> [] | Some x -> [x]) l