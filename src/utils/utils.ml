type json = Yojson.Safe.t

let create_ctr ?i:(i=1) () = 
  let s = ref (i - 1)  in
  fun () -> 
    incr s;
    (* print_endline ("INCED, RETURNING " ^ (string_of_int !s)); *)
    !s