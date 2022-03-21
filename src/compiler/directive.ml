open Circuit

type layout_type = 
  | Identity
  | Naive 

type directive = 
  | Layout of layout_type
  | Primary of wire_color

exception DirectiveError of string

let parse_directive d s : directive = 
  let dl = String.lowercase_ascii d in
  let sl = String.lowercase_ascii s in
  begin match dl with 
  | "layout" -> 
    begin match sl with 
    | "identity" -> Layout Identity 
    | "naive" -> Layout Naive
    | _ -> raise @@ DirectiveError ("Unsupported layout type: \"" ^ s  ^ "\"")
    end
  | "primary" -> 
    begin match sl with 
    | "red" -> Primary Red 
    | "green" -> Primary Green 
    | _ -> raise @@ DirectiveError ("Unsupported primary wire color: \"" ^ s  ^ "\"")
    end
  | _ -> raise @@ DirectiveError ("Unsupported directive: \"" ^ d ^ "\"")
  end


