type json = Yojson.Basic.t

let create_ctr ?i:(i=1) () = 
  let s = ref (i - 1)  in
  fun () -> 
    incr s;
    !s