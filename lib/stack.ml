module type Stack = sig
    type 'a stack

    val mkEmpty : unit -> 'a stack
    val push    : 'a stack -> 'a -> 'a stack
    val pop     : 'a stack -> 'a stack * 'a option
end

module type SerializableStack = sig
    include Serializable.Serializable
end
