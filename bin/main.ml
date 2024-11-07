open Protocol_conv_xmlm

type range = {
  start   : int; [@key "begin"]
  finish  : int; [@key "end"]
} [@@deriving protocol ~driver:(module Xmlm)]

type location = {
  column    : range; [@key "column"]
  line      : range; [@key "line"]
  filename  : string; [@key "filename"]
} [@@deriving protocol ~driver:(module Xmlm)]

type module_node = {
  location : location; [@key "location"]
  uniquename : string
} [@@deriving protocol ~driver:(module Xmlm)]

type entry = {
  uid : int; [@key "UID"]
  module_node : module_node; [@key "ModuleNode"]
} [@@deriving protocol ~driver:(module Xmlm)]

type module_node_ref = {
  uid : int [@key "UID"]
} [@@deriving protocol ~driver:(module Xmlm)]

type context = {
  entry : entry list [@key "entry"]
} [@@deriving protocol ~driver:(module Xmlm)]

type modules = {
  root_module: string; [@key "RootModule"]
  context: context; [@key "context"]
  module_node_ref : module_node_ref list; [@key "ModuleNodeRef"]
  module_node : module_node list; [@key "ModuleNode"]
} [@@deriving protocol ~driver:(module Xmlm)]

let () =
  let xml = "Test.xml" |> open_in |> In_channel.input_all |> Xmlm.of_string in
  match modules_of_xmlm xml with
  | Ok s -> s |> modules_to_xmlm |> Xmlm.to_string |> Printf.printf "t serialized: %s\n";
  | Error e -> print_endline (Xmlm.error_to_string_hum e)
