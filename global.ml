open Js_of_ocaml

let console_log arg: unit = Js.Unsafe.fun_call (
  Js.Unsafe.js_expr "console.log") [|Js.Unsafe.inject arg|]
