open Js_of_ocaml

module EntryPageProps = struct
  type t = {
    text: string;
  }[@@js]
end

module EntryPage: React.FC with type t = EntryPageProps.t = struct
  type t = EntryPageProps.t [@@js]

  let make _props ch =
    React.div [] [ch]
end

let app: <foo: Js.js_string Js.t Js.prop> Js.t  = Js.Unsafe.js_expr {|require("./../../app.js")|}
let t = (app##.foo) |> Js.to_string
let el = React.fc (module EntryPage) {text=t} [React.string "you";React.string " I"]
let _ = React.Dom.render el (Dom_html.getElementById "app")
let _ = Global.console_log app##.foo
