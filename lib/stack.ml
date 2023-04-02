module type Stack = sig
    type elt
    type stack

    val mkEmpty : unit -> stack
    val push    : stack -> elt -> stack
    val pop     : stack -> stack * elt option
    val fold    : ('acc -> elt -> 'acc) -> 'acc -> stack -> 'acc
end

module type StackElement = sig
    type elt
end

open Serializable
module type SerializableStack = sig
    include Stack
    (*
     * Cannot get `include Serializable` to work
     * StackImpl.t != StackImpl.stack
     *)
    type t = stack
    val serialize : t -> string
end

module MakeSerStack_v1
    (S: Stack)
    (ES: Serializable with type t = S.elt)
    : (SerializableStack with type elt = S.elt with type stack = S.stack) = struct

    include S

    let helper acc elt = String.cat acc (String.cat (ES.serialize elt) ",")

    type t = S.stack
    let serialize (s: S.stack) : string =
        String.cat (String.cat "[" (S.fold helper "" s)) "]"
end

module MakeSerStack_v2
    (S: Stack)
    (ES: Serializable with type t = S.elt)
    : (SerializableStack with type elt = S.elt with type stack = S.stack) = struct

    include S

    let helper acc elt = String.cat acc (String.cat (ES.serialize elt) ",")

    type t = S.stack
    let serialize (s: S.stack) : string =
        String.cat (String.cat "{" (S.fold helper "" s)) "}"
end
