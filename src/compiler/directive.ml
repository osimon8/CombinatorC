type layout_type = 
  | Identity
  | Naive 

type directive = 
  | Layout of layout_type

let parse_directive d s : directive = 
  let d = String.lowercase_ascii d in
  let s = String.lowercase_ascii s in
  begin match d with 
  | "layout" -> 
    begin match s with 
    | "identity" -> Layout Identity 
    | "naive" -> Layout Naive
    | _ -> failwith ("Unsupported layout type: " ^ s)
    end
  | _ -> failwith ("Unsupported directive: " ^ d)
  end


