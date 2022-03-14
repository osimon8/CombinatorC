open Directive

type compiler_config = { layout:layout_type }

let default_layout = Identity

let default_config = { layout=default_layout } 

let config = ref default_config  

let set_config cfg = 
  config := cfg

let get_config () = !config

let config_of_directives directives : compiler_config = 
  let layout = ref default_layout in 

  let internal d = 
    begin match d with 
    | Layout l -> layout := l
  end
  in
  List.iter internal directives;
  { layout=(!layout) }