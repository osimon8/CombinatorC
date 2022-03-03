open Bigarray

let json_output s =
  `Assoc
    [ ("blueprint", `Assoc [("item", `String "blueprint"); ("label", `String s)])
    ]

let to_json_string (s : string) = Yojson.Basic.to_string (json_output s)

let bigstring_of_string (src: string): Zlib.bigstring =
  let len = String.length src in
  let dims = Array.of_list [len] in 
  let dst = Genarray.create char c_layout dims in
  let src = String.to_bytes src in
  Memcpy.(memcpy_from_bytes (bigarray Ctypes.genarray dims char)) ~src ~dst ~dst_off:0;
  array1_of_genarray dst

let string_of_bigstring (src: Zlib.bigstring) (len: int) : string =
  let dst :bytes = Bytes.init len (fun i -> Array1.unsafe_get src i) in
  String.of_bytes dst

let encode (s : string) = 
  let json = to_json_string s in 

  let in_buf = bigstring_of_string json in   
  let in_len = Array1.dim in_buf in

  (* output_string stdout ((string_of_bigstring in_buf in_len) ^ "\n"); *)

  let deflate = Zlib.create_deflate ~level:(-1) ~strategy:Default_strategy ~window_bits:(15) ~memory:8  () in

  deflate.data_type <- 1; (* text *)
  deflate.in_buf <- in_buf;

  (* output_string stdout ("INLEN: " ^ (Int.to_string deflate.out_ofs) ^ "\n"); *)

  let bound = Zlib.deflate_bound deflate.state in_len in

  (* output_string stdout ("BOUND: " ^ (Int.to_string bound) ^ "\n"); *)

  deflate.out_buf <- Array1.create char c_layout bound;

    (* output_string stdout ("OUT_DATA: " ^ (Int.to_string deflate.out_ofs) ^ "\n"); *)


  begin match Zlib.flate deflate Zlib.Finish with
    | Stream_end -> ()
    | Ok -> failwith "Ok"
    | Need_dict -> failwith "Need_dict"
    | Buf_error -> failwith "Buf_error"
    | Data_error s -> failwith ("Data_error: " ^ s)
  end;

  (* output_string stdout ("OUTLEN: " ^ (Int.to_string deflate.out_total ) ^ "\n"); *)

  let deflated = string_of_bigstring deflate.out_buf deflate.out_total in
  let enced = Base64.encode_exn deflated in  
   "0" ^ enced
