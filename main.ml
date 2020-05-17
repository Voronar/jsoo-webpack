open Js_of_ocaml
let _ = Js.Unsafe.js_expr {|require("antd/dist/antd.less")|}
(* let antd = Js.Unsafe.js_expr {|require("antd")|} *)
(* let table = Js.Unsafe.get antd "Table" *)
let lazy_table = React.lazyc (fun () -> Js.Unsafe.js_expr {|require.ensure(["antd/lib/table/Table"], function(require) {
  return require("antd/lib/table/Table");
})|})
let _ = Global.console_log lazy_table
let ctx = React.create_context 1

module Table: React.EXTERNALC with type t = Ojs.t = struct
  type t = Ojs.t
  [@@js]
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
    onClick: unit -> unit
  }[@@js]
end

module Child: React.FC with type t = ChildProps.t = struct
  type t = ChildProps.t [@@js]

  let make ChildProps.{onClick} ch = let open React in
    let c = use_context ctx in
    let _ = Global.console_log c in
    suspense ~fallback:(string "loading") [
      div [] [
        div [Html.Attr.OnClick (fun _ ->
          onClick ()
        )] [ch; int c];
        extnc (module Table) Ojs.null []
      ]
    ];
end

module EntryPageProps = struct
  type t = {
    text: string;
  }[@@js]
end

module EntryPage: React.FC with type t = EntryPageProps.t = struct
  type t = EntryPageProps.t [@@js]

  let make _props ch = let open React in
    let (value, set_state) = use_state (fun _ -> 1) in
    let onClick () = set_state (fun s -> s + 1) in
    provider (module NumberProvider) value [
      fc (module Child) {onClick} [ch]
    ]
end

let app: <foo: Js.js_string Js.t Js.prop> Js.t  = Js.Unsafe.js_expr {|require("./../../app.js")|}
let t = (app##.foo) |> Js.to_string
let el = React.fc (module EntryPage) {text=t} [React.string "comps"]
let _ = React.Dom.render el (Dom_html.getElementById "app")
let _ = Global.console_log el
