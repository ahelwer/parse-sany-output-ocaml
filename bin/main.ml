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

type op_decl_node = {
  uniquename : string
}
[@@deriving show]

let xml_to_op_decl_node (xml : tree) : op_decl_node =
  match xml with
  | Node (((_, "OpDeclNode"), _), children) -> ({
      uniquename = children |> xml_to_tagged_string "uniquename";
    } : op_decl_node)
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to op_decl_node type" (show_tree xml)) |> raise


type user_defined_op_kind = {
  uniquename : string
}
[@@deriving show]

let xml_to_user_defined_op_kind xml : user_defined_op_kind =
  match xml with
  | Node (((_, "UserDefinedOpKind"), _), children) -> {
      uniquename = children |> xml_to_tagged_string "uniquename";
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to user_defined_op_kind type" (show_tree xml)) |> raise

type built_in_kind = {
  uniquename : string
}
[@@deriving show]

let xml_to_built_in_kind xml : built_in_kind =
  match xml with
  | Node (((_, "BuiltInKind"), _), children) -> {
      uniquename = children |> xml_to_tagged_string "uniquename";
    }
  | _ -> Invalid_argument (Printf.sprintf "Cannot translate value %s to built_in_kind type" (show_tree xml)) |> raise

type entry_kind =
  | ModuleNode of module_node
  | OpDeclNode of op_decl_node
  | UserDefinedOpKind of user_defined_op_kind
  | BuiltInKind of built_in_kind
[@@deriving show]

let xml_to_entry_kind (children : tree list) =
  let rec find_variant (candidates : tree list) =
    match candidates with
    | x :: xs -> (
      match x with
      | Node (((_, "ModuleNode"), _), _) -> ModuleNode (xml_to_module_node x)
      | Node (((_, "OpDeclNode"), _), _) -> OpDeclNode (xml_to_op_decl_node x)
      | Node (((_, "UserDefinedOpKind"), _), _) -> UserDefinedOpKind (xml_to_user_defined_op_kind x)
      | Node (((_, "BuiltInKind"), _), _) -> BuiltInKind (xml_to_built_in_kind x)
      | _ -> find_variant xs
    )
    | [] -> Invalid_argument (Printf.sprintf "Unable to find entry_kind variant in children %s" (show_tree_list children)) |> raise
  in find_variant children

type entry = {
  uid : int;
  kind : entry_kind;
}
[@@deriving show]

let xml_to_entry xml =
  match xml with
  | Node (((_, "entry"), _), children) -> {
      uid = children |> xml_to_tagged_int "UID";
      kind = xml_to_entry_kind children;
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
