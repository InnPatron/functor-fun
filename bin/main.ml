open Lib

module IntSer = struct
    type t = int
    let serialize v = string_of_int v
end

module StackImpl1 = MakeSerStack_v1(ListStack(struct type elt = int end))(IntSer)
module Serializer1 = Serializer(StackImpl1)

let stack : StackImpl1.t = StackImpl1.mkEmpty()
let stack' = StackImpl1.push stack 1337
let stack'' = StackImpl1.push stack' 21

let () = print_endline (Serializer1.serialize stack'')

module StackImpl2 = MakeSerStack_v2(ListStack(struct type elt = int end))(IntSer)
module Serializer2 = Serializer(StackImpl2)
let stack_v2 = StackImpl2.push (StackImpl2.push (StackImpl2.mkEmpty()) 0) 9000


let () = print_endline (Serializer2.serialize stack_v2)
