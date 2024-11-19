type tree =
  | Node of Xmlm.tag * tree list
  | Value of string
[@@deriving show]

let conversion_failure fn_name xml =
  let err_msg = Printf.sprintf "%s conversion failure on %s" fn_name (show_tree xml) in
  Invalid_argument err_msg |> raise

let is_tag (tag_name : string) (node : tree) =
  match node with
  | Node (((_, name), _), _) -> String.equal name tag_name
  | _ -> false

let children_of (xml : tree) =
  match xml with
  | Node (_, children) -> children
  | Value _ -> Invalid_argument (Printf.sprintf "Cannot get children of node %s" (show_tree xml)) |> raise

let child_of (xml : tree) =
  match xml with
  | Node (_, [child]) -> child
  | Node (_, _) -> Invalid_argument (Printf.sprintf "Require single child of node %s" (show_tree xml)) |> raise
  | Value _ -> Invalid_argument (Printf.sprintf "Cannot get children of node %s" (show_tree xml)) |> raise

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
  | _ -> conversion_failure __FUNCTION__ xml

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
  | _ -> conversion_failure __FUNCTION__ xml

type node = {
  location  : location;
  level     : int;
}
[@@deriving show]

let xml_to_inline_node (children : tree list) = {
  location  = children |> find_tag "location" |> xml_to_location;
  level     = children |> xml_to_tagged_int "level"
}

type numeral_node = {
  node  : node;
  value : int;
}
[@@deriving show]

let xml_to_numeral_node (xml : tree) =
  match xml with
  | Node (((_, "NumeralNode"), _), children) -> {
      node  = children |> xml_to_inline_node;
      value = children |> xml_to_tagged_int "IntValue"
    }
  | _ -> conversion_failure __FUNCTION__ xml

type formal_param_node_ref = {
  uid : int
}
[@@deriving show]

let xml_to_formal_param_node_ref xml =
  match xml with
  | Node (((_, "FormalParamNodeRef"), _), children) -> {
    uid = children |> xml_to_tagged_int "UID";
  }
  | _ -> conversion_failure __FUNCTION__ xml

type unbound_symbol = {
  formal_param_node_ref : formal_param_node_ref;
  is_tuple : bool;
}
[@@deriving show]

let xml_to_unbound_symbol xml =
  match xml with
  | Node (((_, "unbound"), _), children) -> {
    formal_param_node_ref = children |> find_tag "FormalParamNodeRef" |> xml_to_formal_param_node_ref;
    is_tuple = children |> List.exists (is_tag "tuple")
  }
  | _ -> conversion_failure __FUNCTION__ xml

type symbols =
  | Unbound of unbound_symbol
(*| Bound of bound_symbol*)
[@@deriving show]

let xml_to_symbols xml =
  match xml with
  | Node (((_, "unbound"), _), _) -> Unbound (xml_to_unbound_symbol xml)
(*| Node (((_, "bound"), _), _) -> *)
  | _ -> conversion_failure __FUNCTION__ xml

type op_appl_node = {
  node      : node;
  operands  : expr_or_op_arg list;
  bound_symbols : symbols list;
}

and expression =
(*| AtNode of at_node*)
(*| DecimalNode of decimal_node*)
(*| LabelNode of label_node*)
(*| LetInNode of let_in_node*)
  | NumeralNode of numeral_node
  | OpApplNode of op_appl_node
(*| StringNode of string_node*)
(*| SubstInNode of subst_in_node*)
(*| TheoremDefRef of theorem_def_ref*)
(*| AssumeDefRef of assume_def_ref*)

and expr_or_op_arg =
  | Expression of expression
(*| OpArg of operator_arg*)
[@@deriving show]

let rec xml_to_expr_or_op_arg xml =
  try Expression (xml_to_expression xml)
with Invalid_argument _ -> conversion_failure __FUNCTION__ xml

and xml_to_op_appl_node xml =
  match xml with
  | Node (((_, "OpApplNode"), _), children) -> {
    node    = children |> xml_to_inline_node;
    operands = children |> find_tag "operands" |> children_of |> List.map xml_to_expr_or_op_arg;
    bound_symbols = children |> List.find_opt (is_tag "boundSymbols") |> Option.map children_of |> Option.value ~default:[] |> List.map xml_to_symbols;
  }
  | _ -> conversion_failure __FUNCTION__ xml

and xml_to_expression xml =
  match xml with
  | Node (((_, "NumeralNode"), _), _) -> NumeralNode (xml_to_numeral_node xml)
  | Node (((_, "OpApplNode"), _), _) -> OpApplNode (xml_to_op_appl_node xml)
  | _ -> conversion_failure __FUNCTION__ xml

type module_node_ref = {
  uid : int
}
[@@deriving show]

let xml_to_module_node_ref xml =
  match xml with
  | Node (((_, "ModuleNodeRef"), _), children) -> {
      uid = children |> xml_to_tagged_int "UID";
    }
  | _ -> conversion_failure __FUNCTION__ xml

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
  | _ -> conversion_failure __FUNCTION__ xml

type op_decl_node = {
  uniquename : string
}
[@@deriving show]

let xml_to_op_decl_node (xml : tree) : op_decl_node =
  match xml with
  | Node (((_, "OpDeclNode"), _), children) -> ({
      uniquename = children |> xml_to_tagged_string "uniquename";
    } : op_decl_node)
  | _ -> conversion_failure __FUNCTION__ xml


type user_defined_op_kind = {
  node        : node;
  uniquename  : string;
  arity       : int;
  body        : expression;
}
[@@deriving show]

let xml_to_user_defined_op_kind xml : user_defined_op_kind =
  match xml with
  | Node (((_, "UserDefinedOpKind"), _), children) -> {
      node        = children |> xml_to_inline_node;
      uniquename  = children |> xml_to_tagged_string  "uniquename";
      arity       = children |> xml_to_tagged_int     "arity";
      body        = children |> find_tag "body" |> child_of |> xml_to_expression;
    }
  | _ -> conversion_failure __FUNCTION__ xml

type built_in_kind = {
  uniquename : string
}
[@@deriving show]

let xml_to_built_in_kind xml : built_in_kind =
  match xml with
  | Node (((_, "BuiltInKind"), _), children) -> {
      uniquename = children |> xml_to_tagged_string "uniquename";
    }
  | _ -> conversion_failure __FUNCTION__ xml

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
  | _ -> conversion_failure __FUNCTION__ xml

type context = {
  entry : entry list
}
[@@deriving show]

let xml_to_context xml =
  match xml with
  | Node (((_, "context"), _), children) -> {
      entry = children |> List.find_all (is_tag "entry") |> List.map xml_to_entry;
    }
  | _ -> conversion_failure __FUNCTION__ xml

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
  | _ -> conversion_failure __FUNCTION__ xml

let xml_to_tree xml =
  let el tag childs = Node (tag, childs) in
  let data d = Value d in
  Xmlm.input_doc_tree ~el ~data xml

let () =
  let file = "AddTwo.xml" |> open_in in
  let xml = `Channel file |>  Xmlm.make_input ~strip:true ~enc:(Some `UTF_8) in
  let (_, tree) = xml_to_tree xml in
  Printexc.record_backtrace true;
  try
    print_endline (tree |> xml_to_modules |> show_modules)
  with
    Invalid_argument e ->
      Printexc.print_backtrace stderr;
      print_endline e;
  close_in file
