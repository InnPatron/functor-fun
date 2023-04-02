module type Serializable = sig
    type t
    val serialize : t -> string
end
