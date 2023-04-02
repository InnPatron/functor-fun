open Lib

module ListStack = ListStack(struct type elt = int end)
let stack = ListStack.mkEmpty()
let stack' = ListStack.push stack 1337
let (_, x) = ListStack.pop stack'
let v = match x with
    | Some v -> v
    | None -> 1337

let () = print_endline (string_of_int v)
