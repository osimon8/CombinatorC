type json = Yojson.Basic.t

let create_ctr () = 
  let s = ref (0)  in
  fun () -> 
    incr s;
    !s