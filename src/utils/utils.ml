let create_ctr () = 
  let s = ref (-1)  in
  fun () -> 
    incr s;
    !s