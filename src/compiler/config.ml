open Directive
open Circuit

type compiler_config = { layout:layout_type; primary_color:wire_color }

let default_layout = Identity
let default_primary = Red

let default_config = { layout=default_layout; primary_color=default_primary } 

let config = ref default_config  

let set_config cfg = 
  config := cfg

let get_config () = !config

let get_layout () = 
  let { layout=l } = !config in 
  l

let get_primary () = 
  let { primary_color=c } = !config in 
  c

let get_secondary () = 
  let { primary_color=c } = !config in 
  begin match c with 
  | Red -> Green 
  | Green -> Red 
  end 

let config_of_directives directives : compiler_config = 
  let layout = ref default_layout in 
  let color = ref default_primary in 

  let internal d = 
    begin match d with 
    | Layout l -> layout := l
    | Primary c -> color := c
  end
  in
  List.iter internal directives;
  { layout=(!layout); primary_color=Red }