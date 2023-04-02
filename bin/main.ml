open Lib

module IntSer = struct
    type t = int
    let serialize v = string_of_int v
end

module ListStack = ListStackSer1(IntSer)
let stack = ListStack.mkEmpty()
let stack' = ListStack.push stack 1337

let () = print_endline (ListStack.serialize stack')
