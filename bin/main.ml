open Lib

module IntSerializer = struct
    type t = int
    let serialize v = string_of_int v
end

module IntStack = ListStack(struct type elt = int end)

(* Serialize with enclosing '[' ']' *)
module StackImpl1 = MakeSerStack_v1(IntStack)(IntSerializer)
module Serializer1 = Serializer(StackImpl1)

let stack : StackImpl1.t = StackImpl1.mkEmpty()
let stack' = StackImpl1.push stack 1337
let stack'' = StackImpl1.push stack' 21

let () = print_endline (Serializer1.serialize stack'')

(* Serialize with enclosing '{' '}' *)
module StackImpl2 = MakeSerStack_v2(IntStack)(IntSerializer)
module Serializer2 = Serializer(StackImpl2)
let stack_v2 = StackImpl2.push (StackImpl2.push (StackImpl2.mkEmpty()) 0) 9000

let () = print_endline (Serializer2.serialize stack_v2)

(*
 * stack of stacks example using alternating serialization schemes
 * '[' and ']' for outer
 * '{' and '}' for inner
 *
 * NOTE: can be polymorphic over the inner Stack type too by making another functor
 *)
module SoS = ListStack(struct type elt = StackImpl2.stack end)
module StackImpl3 = MakeSerStack_v1(SoS)(Serializer2)
module Serializer3 = Serializer(StackImpl3)

let stack_v3 = SoS.mkEmpty()
let stack_v3' = SoS.push (SoS.push stack_v3 stack_v2) (let (s, _) = StackImpl2.pop stack_v2 in s)

let () = print_endline (Serializer3.serialize stack_v3')
