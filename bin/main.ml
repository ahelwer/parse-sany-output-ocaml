type tree =
  | Node of Xmlm.tag * tree list
  | Value of string
[@@deriving show]

let is_tag (tag_name : string) (node : tree) =
  match node with
  | Node (((_, name), _), _) -> String.equal name tag_name
  | _ -> false

let show_tree_list (xs : tree list) =
  Printf.sprintf "[%s]" (xs |> List.map show_tree |> String.concat "; ")

let find_tag (tag_name : string) (children : tree list) =
  match List.find_opt (is_tag tag_name) children with
  | Some v -> v
  | None -> Invalid_argument (Printf.sprintf "Unable to find tag %s in children %s" tag_name (show_tree_list children)) |> raise

let xml_to_tagged_string (tag_name : string) (children : tree list) =
  match find_tag tag_name children with
  | (Node (_, [Value d])) -> d
  | xml -> Invalid_argument (Printf.sprintf "Cannot convert %s to tagged string %s" (show_tree xml) tag_name) |> raise

let xml_to_tagged_int (tag_name : string) (children : tree list) =
  match find_tag tag_name children with
  | (Node (_, [Value d])) -> int_of_string d
  | xml -> Invalid_argument (Printf.sprintf "Cannot convert %s to tagged int %s" (show_tree xml) tag_name) |> raise

type range = {
  start   : int;
  finish  : int;
}
[@@deriving show]

let xml_to_range xml =
  match xml with
  | Node (((_, _), _), children) -> {
      start = children |> xml_to_tagged_int "begin";
      finish = children |> xml_to_tagged_int "end";
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to range type" (show_tree xml)) |> raise

type location = {
  column    : range;
  line      : range;
  filename  : string;
}
[@@deriving show]

let xml_to_location xml =
  match xml with
  | Node (((_, "location"), _), children) -> {
      column = children |> find_tag "column" |> xml_to_range;
      line = children |> find_tag "line" |> xml_to_range;
      filename = children |> xml_to_tagged_string "filename";
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to location type" (show_tree xml)) |> raise

type module_node = {
  location : location;
  uniquename : string
}
[@@deriving show]

let xml_to_module_node xml =
  match xml with
  | Node (((_, "ModuleNode"), _), children) -> {
      uniquename = children |> xml_to_tagged_string "uniquename";
      location = children |> find_tag "location" |> xml_to_location;
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to module_node type" (show_tree xml)) |> raise

type module_node_ref = {
  uid : int
}
[@@deriving show]

let xml_to_module_node_ref xml =
  match xml with
  | Node (((_, "ModuleNodeRef"), _), children) -> {
      uid = children |> xml_to_tagged_int "UID";
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to module_node_ref type" (show_tree xml)) |> raise

type entry = {
  uid : int;
  module_node : module_node;
}
[@@deriving show]

let xml_to_entry xml =
  match xml with
  | Node (((_, "entry"), _), children) -> {
      uid = children |> xml_to_tagged_int "UID";
      module_node = children |> find_tag "ModuleNode" |> xml_to_module_node;
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to entry type" (show_tree xml)) |> raise

type context = {
  entry : entry list
}
[@@deriving show]

let xml_to_context xml =
  match xml with
  | Node (((_, "context"), _), children) -> {
      entry = children |> List.find_all (is_tag "entry") |> List.map xml_to_entry;
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to context type" (show_tree xml)) |> raise

type modules = {
  root_module: string;
  context: context;
  module_node_ref : module_node_ref list;
  module_node : module_node list;
}
[@@deriving show]

let xml_to_modules xml =
  match xml with
  | Node (((_, "modules"), _), children) -> {
      root_module = xml_to_tagged_string "RootModule" children;
      context = children |> find_tag "context" |> xml_to_context;
      module_node_ref = children |> List.find_all (is_tag "ModuleNodeRef") |> List.map xml_to_module_node_ref;
      module_node = children |> List.find_all (is_tag "ModuleNode") |> List.map xml_to_module_node;
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to modules type" (show_tree xml)) |> raise

let xml_to_tree xml =
  let el tag childs = Node (tag, childs) in
  let data d = Value d in
  Xmlm.input_doc_tree ~el ~data xml

let () =
  let file = "Test3.xml" |> open_in in
  let xml = `Channel file |>  Xmlm.make_input ~strip:true ~enc:(Some `UTF_8) in
  let (_, tree) = xml_to_tree xml in
  print_endline (tree |> xml_to_modules |> show_modules);
  close_in file
