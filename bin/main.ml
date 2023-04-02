open Lib

module IntSerializer = struct
    type t = int
    let serialize v = string_of_int v
end

module IntStack = ListStack(struct type elt = int end)

(* Serialize with enclosing '[' ']' *)
module StackImpl1 = MakeSerStack_v1(IntStack)(IntSerializer)
module Serializer1 = Serializer(StackImpl1)

let stack_v1 : StackImpl1.t = StackImpl1.mkEmpty()
let stack_v1' = StackImpl1.push stack_v1 1337
let stack_v1'' = StackImpl1.push stack_v1' 21

let () = print_endline (String.cat "v1 = " (Serializer1.serialize stack_v1''))

(* Serialize with enclosing '{' '}' *)
module StackImpl2 = MakeSerStack_v2(IntStack)(IntSerializer)
module Serializer2 = Serializer(StackImpl2)
let stack_v2 = StackImpl2.push (StackImpl2.push (StackImpl2.mkEmpty()) 0) 9000

let () = print_endline (String.cat "v2 = " (Serializer2.serialize stack_v2))

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

let () = print_endline (String.cat "v3 = " (Serializer3.serialize stack_v3'))

module SoS_Other = ListStack(struct type elt = StackImpl2.stack end)
let stack_other = SoS_Other.mkEmpty()
(*
 * ERROR: type checker cannot see that SoS_Other.stack = Serializer3.t; how to fix?
 * let () = print_endline (Serializer3.serialize stack_other)
 * let () = print_endline (StackImpl3.serialize stack_other)
 *)

module MakeExecutor(Inner: SerializableStack with type elt = int) = struct
    module InnerSerializer = Serializer(Inner)

    module SoS = ListStack(struct type elt = Inner.stack end)
    module StackImpl = MakeSerStack_v1(SoS)(InnerSerializer)
    module OuterSerializer = Serializer(StackImpl)

    let inner = Inner.push (Inner.push (Inner.mkEmpty()) 0) 9000
    let stack = SoS.mkEmpty()
    let stack' = SoS.push (SoS.push stack inner) (let (s, _) = Inner.pop inner in s)

    let result = OuterSerializer.serialize stack'
end

(* Results like stack_v3', but uses '[' and ']' everywhere *)
module Executor_v4 = MakeExecutor(StackImpl1)
let () =  print_endline (String.cat "v4 = " Executor_v4.result)

(* Results identical to stack_v3 *)
module Executor_v5 = MakeExecutor(StackImpl2)
let () =  print_endline (String.cat "v5 = " Executor_v5.result)
