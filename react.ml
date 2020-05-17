open Js_of_ocaml

let react_module = Js.Unsafe.js_expr {|require("react")|}
let react_dom_module = Js.Unsafe.js_expr {|require("react-dom")|}
let createElement = Js.Unsafe.get react_module "createElement"
let render = Js.Unsafe.get react_dom_module "render"

type element
type fragment_component
type class_component

module type FC = sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val make: t -> element -> element
end

type 'a component =
  | HtmlTagComponent of string
  | FragmentComponent of fragment_component
  | ClassComponent of class_component
  | FunctionalComponent of ('a -> element -> element)

let create_element ~component ~props ~(children: element list) ~props_of_js : element =
  let comp = match component with
    | HtmlTagComponent s -> Js.Unsafe.inject @@ Js.string s
    | FragmentComponent frc -> Js.Unsafe.inject frc
    | ClassComponent cc -> Js.Unsafe.inject cc
    | FunctionalComponent fn ->
      let props_of_js = Option.get props_of_js in
      Js.Unsafe.inject @@ Ojs.fun_to_js 1 (fun p ->
        let c = Js.Unsafe.get p "children" in
        let p = props_of_js p in
        fn p c)
  in
  Js.Unsafe.fun_call createElement [|comp;
    Js.Unsafe.inject props;
    children |> Array.of_list |> Js.array |> Js.Unsafe.inject;
  |]

module Attr = struct
    type mouse_event = <
      nativeEvent: Dom_html.mouseEvent Js.t Js.readonly_prop;
      preventDefault: unit -> unit Js.meth
    > Js.t
    type keyboard_event = <
      nativeEvent: Dom_html.keyboardEvent Js.t Js.readonly_prop;
      preventDefault: unit Js.meth
    > Js.t

    type 'a attr =
      | OnClick of (mouse_event -> unit)
      | OnContextMenu of (mouse_event -> unit)
      | OnInput of (keyboard_event -> unit)
      | OnKeyDown of (keyboard_event -> unit)
      | OnKeyUp of (keyboard_event -> unit)
      | Key of string
      (* | Ref of 'a Js.Opt.t ref_object *)
      | Value of string
      | Style of Dom_html.cssStyleDeclaration Js.t
      | ClassName of string

    let props_to_attr = function
      | OnClick fn -> ("onClick", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnContextMenu fn -> ("onContextMenu", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnInput fn -> ("onChange", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyDown fn -> ("onKeyDown", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | OnKeyUp fn -> ("onKeyUp", Js.Unsafe.inject @@ Js.Unsafe.callback fn)
      | Key v -> ("key", Js.Unsafe.inject @@ v)
      (* | Ref v -> ("ref", Js.Unsafe.inject @@ v) *)
      | Style v -> ("style", Js.Unsafe.inject @@ v)
      | Value v -> ("value", Js.Unsafe.inject @@ v)
      | ClassName v -> ("className", Js.Unsafe.inject @@ v)

    let create_js_obj props_to_attr_fn attrs = List.map props_to_attr_fn attrs
      |> Array.of_list
      |> Js.Unsafe.obj

    let create_reg_attr attr = create_js_obj props_to_attr attr
end

let html_factory tag at children = create_element
  ~component:(HtmlTagComponent tag)
  ~props:(Attr.create_reg_attr at)
  ~children
  ~props_of_js:None
let div (at: Dom_html.divElement Attr.attr list) ch = html_factory "div" at ch

let fc
  (type a)
  (module ReactComponent: FC with type t = a)
  (props: a) children
= create_element
  ~component:(FunctionalComponent ReactComponent.make)
  ~props:(ReactComponent.t_to_js props) ~children
  ~props_of_js:(Some ReactComponent.t_of_js)

let string str: element = Ojs.string_to_js str |> Obj.magic
let null: element = Js.Unsafe.pure_js_expr "null"

module Dom = struct
  let render (element: element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
    Js.Unsafe.fun_call render
    [|Js.Unsafe.inject element;
      Js.Unsafe.inject node|]
end