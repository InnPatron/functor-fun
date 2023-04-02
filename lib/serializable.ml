module type Serializable = sig
    type t
    val serialize : t -> string
end

module Serializer(S: Serializable) = struct
    let serialize s = S.serialize s
end
