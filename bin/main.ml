type range = {
  start   : int;
  finish  : int;
}
[@@deriving show]

type location = {
  column    : range;
  line      : range;
  filename  : string;
}
[@@deriving show]

type module_node = {
  location : location;
  uniquename : string
}
[@@deriving show]

type entry = {
  uid : int;
  module_node : module_node;
}
[@@deriving show]

type module_node_ref = {
  uid : int
}
[@@deriving show]

type context = {
  entry : entry list
}
[@@deriving show]

type modules = {
  root_module: string;
  context: context;
  module_node_ref : module_node_ref list;
  module_node : module_node list;
}
[@@deriving show]

type tree =
  | E of Xmlm.tag * tree list
  | D of string
[@@deriving show]

let xml_to_tree xml =
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data xml
  
let () =
  let file = "Test.xml" |> open_in in
  let xml = `Channel file |>  Xmlm.make_input ~strip:true ~enc:(Some `UTF_8) in
  let (_, tree) = xml_to_tree xml in
  print_endline (show_tree tree);
  close_in file
