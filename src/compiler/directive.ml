open Circuit

type layout_type = 
  | Identity
  | Naive 

type directive = 
  | Layout of layout_type
  | Primary of wire_color

exception DirectiveError of string

let parse_directive d s : directive = 
  let d = String.lowercase_ascii d in
  let s = String.lowercase_ascii s in
  begin match d with 
  | "layout" -> 
    begin match s with 
    | "identity" -> Layout Identity 
    | "naive" -> Layout Naive
    | _ -> raise @@ DirectiveError ("Unsupported layout type: " ^ s)
    end
  | "primary" -> 
    begin match s with 
    | "red" -> Primary Red 
    | "green" -> Primary Green 
    | _ -> raise @@ DirectiveError ("Unsupported primary wire color: " ^ s)
    end
  | _ -> raise @@ DirectiveError ("Unsupported directive: " ^ d)
  end


