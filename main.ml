open Js_of_ocaml
let _ = Js.Unsafe.js_expr {|require("antd/dist/antd.less")|}
(* let antd = Js.Unsafe.js_expr {|require("antd")|} *)
(* let table = Js.Unsafe.get antd "Table" *)
let lazy_table = React.lazyc (fun () -> Js.Unsafe.js_expr {|require.ensure(["antd/lib/table/Table"], function() {
  return require("antd/lib/table/Table");
})|})
(* let _ = Global.console_log lazy_table *)
let ctx = React.create_context 0

module Table: React.EXTERNALC with type t = unit = struct
  type t = unit
  let t_to_js _ = Ojs.null 
  let t_of_js _ = ()
  let component = lazy_table
end

module NumberProvider: React.PROVIDER with type v = int = struct
  type v = int [@@js]
  type t = {
    value: v;
  }[@@js]
  let provider = React.get_provider ctx
end

module ChildProps = struct
  type t = {
    text1: string;
  }[@@js]
end

module Child = React.MemoFc(struct
  type t = ChildProps.t [@@js]
  open React

  let make = fun ChildProps.{text1} ->
    let c = use_context ctx in
    let _ = Global.console_log (text1) in
    suspense ~fallback:(string "loading") [
      div [] [
        if c = 0 then null else extnc (module Table) () []
      ]
    ];
end)

module EntryPageProps = struct
  type t = {
    text: string;
    children: React.element;
  }[@@js]
end

let text1 = "only one"
module EntryPage = React.NormFc(struct
  type t = EntryPageProps.t [@@js]
  open React

  let make = fun EntryPageProps.{text;_} ->
    let _ = Global.console_log (text) in
    let (value, set_state) = use_state (fun _ -> 0) in
    let onClick () = set_state (fun s -> s + 1) in
    provider (module NumberProvider) value [
      div [Html.Attr.OnClick (fun _ ->
        onClick ()
      )] [string "click me"];
      fc (module Child) {text1}
    ]
end)

let app: <foo: Js.js_string Js.t Js.prop> Js.t  = Js.Unsafe.js_expr {|require("./../../app.js")|}
let t = (app##.foo) |> Js.to_string
let el = React.fc (module EntryPage) {text=t;children=(React.string "comps")}
let _ = React.Dom.render el (Dom_html.getElementById "app")

