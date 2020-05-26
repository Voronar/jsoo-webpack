open Js_of_ocaml
module NullOpt = Js.Opt
module UndefOpt = Js.Optdef

let react_module = Js.Unsafe.js_expr {|require("react")|}
let react_dom_module = Js.Unsafe.js_expr {|require("react-dom")|}
let r_createElement = Js.Unsafe.get react_module "createElement"
let r_memo = Js.Unsafe.get react_module "memo"
let r_lazy = Js.Unsafe.get react_module "lazy"
let r_useRef = Js.Unsafe.get react_module "useRef"
let r_useEffect = Js.Unsafe.get react_module "useEffect"
let r_useState = Js.Unsafe.get react_module "useState"
let r_useCallback = Js.Unsafe.get react_module "useCallback"
let r_useReducer = Js.Unsafe.get react_module "useReducer"
let r_useContext = Js.Unsafe.get react_module "useContext"
let r_createContext = Js.Unsafe.get react_module "createContext"
let r_fragment = Js.Unsafe.get react_module "Fragment"
let r_suspense = Js.Unsafe.get react_module "Suspense"
let rd_render = Js.Unsafe.get react_dom_module "render"

type element
external element_to_js : element -> Ojs.t = "%identity"
external element_of_js : Ojs.t -> element = "%identity"

type fragment_component
type other_component
type 'a context_js
type 'a ctx_provider

module type PROVIDER = sig
  type v
  type t = {
    value: v;
  }
  val provider: v ctx_provider
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val v_to_js: v -> Ojs.t
  val v_of_js: Ojs.t -> v
end

type memo_fc_type
type 'a fc_type =
  | Normal of ('a -> element)
  | Memo of memo_fc_type

let norm_fc fc = Normal fc
let memo_fc fc = Memo fc

let memo (fn: 'a -> element) ~t_of_js =
  let el: memo_fc_type = Js.Unsafe.(fun_call r_memo [|
    inject @@ Ojs.fun_to_js 1 (fun p -> let p = t_of_js p in fn p)
  |]) in el

module type FC = sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val make: t fc_type
end

module type NORM_FC = sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val make: t -> element
end

module NormFc (NormComponent: NORM_FC): FC with type t = NormComponent.t = struct
  include NormComponent
  let make = norm_fc NormComponent.make
end

module MemoFc (NormComponent: NORM_FC): FC with type t = NormComponent.t = struct
  include NormComponent
  let make = memo_fc (memo ~t_of_js NormComponent.make)
end

module type EXTERNALC = sig
  type t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val component: other_component
end

type ('a, 'b) component =
  | HtmlTagComponent of string
  | ProviderComponent of 'b ctx_provider
  | OtherComponent of other_component
  | FunctionalComponent of 'a fc_type

let parse_children = let open Js.Unsafe in function
  | [] -> inject Js.undefined
  | [ch] -> inject ch
  | ch -> ch |> Array.of_list |> Js.array |> inject

let create_element ~component ~props ~(children: element list) ~props_of_js : element = let open Js.Unsafe in
  let comp = match component with
    | HtmlTagComponent s -> inject @@ Js.string s
    | OtherComponent cc -> inject cc
    | ProviderComponent fn -> inject fn
    | FunctionalComponent (Memo fn) -> inject fn
    | FunctionalComponent (Normal fn) ->
      let props_of_js = Option.get props_of_js in
      inject @@ Ojs.fun_to_js 1 (fun p -> p |> props_of_js |> fn)
  in
  fun_call r_createElement [|comp;
    inject props;
    children |> parse_children;
  |]

(* Context *)
let create_context (dft_value: 'a): 'a context_js = Js.Unsafe.(fun_call r_createContext [|inject dft_value|])
let mk_ctx_value v = object%js val value = v end
let get_provider (ctx_js: 'a context_js): 'a ctx_provider = Js.Unsafe.get ctx_js "Provider"
let use_context (ctx: 'a context_js): 'a = Js.Unsafe.(fun_call r_useContext [|inject ctx|])

(* Muttable refs *)
type 'a mut_ref_object = < current: 'a Js.prop > Js.t
let ref_current = "current"
let (!) (ref: 'a mut_ref_object): 'a = Js.Unsafe.get ref ref_current
let (:=) (ref: 'a mut_ref_object) (v: 'a): unit = Js.Unsafe.set ref ref_current v

let use_ref (value: 'a): 'a mut_ref_object =
  Js.Unsafe.(fun_call r_useRef [|inject value|])

module Html = struct
  module InlineStyle = struct
    type style =
      | Display of string
      | Width of string
      | Height of string
      | BackgroundColor of string
      | Color of string
      | TextDecoration of string

    let props_to_attr = function
      | Display s -> ("display", Js.Unsafe.inject s)
      | Width s -> ("width", Js.Unsafe.inject s)
      | Height s -> ("height", Js.Unsafe.inject s)
      | BackgroundColor s -> ("backgroundColor", Js.Unsafe.inject s)
      | Color s -> ("color", Js.Unsafe.inject s)
      | TextDecoration s -> ("textDecoration", Js.Unsafe.inject s)
    let create_js_obj attrs = List.map props_to_attr attrs
      |> Array.of_list
      |> Js.Unsafe.obj
    let create attrs: Dom_html.cssStyleDeclaration Js.t = create_js_obj attrs
  end

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
        | Ref of 'a Js.Opt.t mut_ref_object
        | Value of string
        | Style of Dom_html.cssStyleDeclaration Js.t
        | ClassName of string

      let props_to_attr = let open Js.Unsafe in function
        | OnClick fn -> ("onClick", inject @@ callback fn)
        | OnContextMenu fn -> ("onContextMenu", inject @@ callback fn)
        | OnInput fn -> ("onChange", inject @@ callback fn)
        | OnKeyDown fn -> ("onKeyDown", inject @@ callback fn)
        | OnKeyUp fn -> ("onKeyUp", inject @@ callback fn)
        | Key v -> ("key", inject @@ v)
        | Ref v -> ("ref", inject @@ v)
        | Style v -> ("style", inject @@ v)
        | Value v -> ("value", inject @@ v)
        | ClassName v -> ("className", inject @@ v)

      let create_js_obj props_to_attr_fn attrs = List.map props_to_attr_fn attrs
        |> Array.of_list
        |> Js.Unsafe.obj

      let create_reg_attr attr = create_js_obj props_to_attr attr
  end
end

let html_factory tag at children = create_element ~children
  ~component:(HtmlTagComponent tag)
  ~props:(Html.Attr.create_reg_attr at)
  ~props_of_js:None

let div (at: Dom_html.divElement Html.Attr.attr list) ch = html_factory "div" at ch
let span (at: Dom_html.htmlElement Html.Attr.attr list) ch = html_factory "span" at ch
let p (at: Dom_html.paragraphElement Html.Attr.attr list) ch = html_factory "p" at ch

let fc (type a)
  (module ReactComponent: FC with type t = a)
  (props: a)
= let props = (ReactComponent.t_to_js props) in
  let children = match props with
    | v when Ojs.is_null v -> []
    | v -> [Ojs.get v "children" |> Obj.magic]
  in
  create_element ~props ~children
  ~component:(FunctionalComponent (ReactComponent.make))
  ~props_of_js:(Some ReactComponent.t_of_js)

let extnc (type a)
  (module ReactComponent: EXTERNALC with type t = a)
  (props: a) children
= create_element ~children
  ~component:(OtherComponent ReactComponent.component)
  ~props:(ReactComponent.t_to_js props)
  ~props_of_js:None

let fragment ?key children =
  let open Js.Unsafe in
  let props = match key with
    | Some s -> inject @@ object%js val key = Js.string s end
    | None -> inject Js.undefined
  in
  create_element ~props ~children
    ~component:(OtherComponent r_fragment)
    ~props_of_js:None

let suspense ~fallback children =
  let open Js.Unsafe in
  let props = inject @@ object%js
    val fallback: element = fallback
  end in
  create_element ~props ~children
    ~component:(OtherComponent r_suspense)
    ~props_of_js:None

let provider (type a)
  (module Provider: PROVIDER with type v = a)
  value children =
  let props = Provider.{value} |> Provider.t_to_js in
  create_element ~props ~children
    ~component:(ProviderComponent Provider.provider)
    ~props_of_js:None

let string str: element = Ojs.string_to_js str |> Obj.magic
let int int: element = Ojs.int_to_js int |> Obj.magic
let null: element = Ojs.null |> Obj.magic

(* Hooks *)
let use_effect (fn: unit -> (unit -> unit) Js.optdef): unit = let open Js.Unsafe in
  fun_call r_useEffect [|inject @@ callback fn|]

let use_effect0 (fn: unit -> (unit -> unit) Js.optdef): unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback fn; inject @@ Js.array [||]|]

let use_effect1 (fn: unit -> (unit -> unit) Js.optdef) d1 : unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback (fn); inject @@ Js.array [|d1|]|]

let use_effect2 (fn: unit -> (unit -> unit) Js.optdef) ((d1,d2): ('a * 'b)) : unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback fn; inject @@ Js.array ([|d1;d2|] |> Array.map inject)|]

let use_effect3 (fn: unit -> (unit -> unit) Js.optdef) ((d1,d2,d3): ('a * 'b * 'c)) : unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback fn; inject @@ Js.array ([|d1;d2;d3|] |> Array.map inject)|]

let use_effect4 (fn: unit -> (unit -> unit) Js.optdef) ((d1,d2,d3,d4): ('a * 'b * 'c * 'd)) : unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback fn; inject @@ Js.array ([|d1;d2;d3;d4|] |> Array.map inject)|]

let use_effect5 (fn: unit -> (unit -> unit) Js.optdef) ((d1,d2,d3,d4,d5): ('a *'b *'c *'d *'e)) : unit = let open Js.Unsafe in
  fun_call r_useEffect
  [|inject @@ callback fn; inject @@ Js.array ([|d1;d2;d3;d4;d5|] |> Array.map inject)|]

let use_state (init_value: unit -> 'a): ('a * (('a -> 'a) -> unit)) =
  let open Js.Unsafe in
  let state_tuple = fun_call r_useState [|inject @@ callback init_value|] in
  (get state_tuple "0", get state_tuple "1")

let use_reducer
  ?init:(init = fun (x: 'a) -> x)
  (reducer: 'a -> 'b -> 'a)
  (init_value: 'a)
:('a * ('b -> unit)) =
  let open Js.Unsafe in
  let state_tuple = fun_call r_useReducer
    [|inject @@ callback reducer;
      inject @@ inject init_value;
      inject @@ callback init|] in
  (get state_tuple "0", get state_tuple "1")

let use_callback0 (fn: 'a -> 'b): 'a -> 'b = let open Js.Unsafe in
  fun_call r_useCallback [|inject @@ callback fn; inject @@ Js.array [||]|]

let use_callback1 (fn: 'a -> 'b) (d1: 'c): 'a -> 'b = let open Js.Unsafe in
  fun_call r_useCallback [|inject @@ callback fn; inject @@ Js.array ([|d1|] |> Array.map inject)|]

let use_callback2 (fn: 'a -> 'b) ((d1, d2): ('c * 'd)): 'a -> 'b = let open Js.Unsafe in
  fun_call r_useCallback [|inject @@ callback fn; inject @@ Js.array ([|d1;d2|] |> Array.map inject)|]

let use_callback3 (fn: 'a -> 'b) ((d1, d2, d3): ('c * 'd * 'e)): 'a -> 'b = let open Js.Unsafe in
  fun_call r_useCallback [|inject @@ callback fn; inject @@ Js.array ([|d1;d2;d3|] |> Array.map inject)|]

let use_callback4 (fn: 'a -> 'b) ((d1,d2,d3,d4): ('c * 'd * 'e * 'f)): 'a -> 'b = let open Js.Unsafe in
  fun_call r_useCallback [|inject @@ callback fn; inject @@ Js.array ([|d1; d2; d3;d4|] |> Array.map inject)|]

(* Others *)

let lazyc (fn: unit -> element): 'a =
  Js.Unsafe.(fun_call r_lazy [|inject @@ callback fn|])

module Dom = struct
  let render (element: element) (node: Dom_html.element Js.t ): Dom_html.element Js.t =
    let open Js.Unsafe in
    fun_call rd_render [|inject element; inject node|]
end
