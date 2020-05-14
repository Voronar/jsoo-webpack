open Js_of_ocaml

module Global = struct
  let console_log arg: unit = Js.Unsafe.fun_call (
    Js.Unsafe.js_expr "console.log") [|Js.Unsafe.inject arg|]
end

let react = Js.Unsafe.js_expr {|require("react")|}
let app: <foo: Js.js_string Js.prop> Js.t  = Js.Unsafe.js_expr {|require("./../../app.js")|}
let _ = Global.console_log react
let _ = Global.console_log app##.foo
