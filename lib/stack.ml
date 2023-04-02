module type Stack = sig
    type elt
    type stack

    val mkEmpty : unit -> stack
    val push    : stack -> elt -> stack
    val pop     : stack -> stack * elt option
end

module type StackElement = sig
    type elt
end

open Serializable
module type SerializableStack = sig
    include Serializable
    include Stack
end
