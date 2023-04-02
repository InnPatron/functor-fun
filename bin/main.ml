open Lib

let stack = ListStack.mkEmpty()
let stack' = ListStack.push stack 1337
let (_, x) = ListStack.pop stack'
let v = match x with
    | Some v -> v
    | None -> 1337

module IntSerializable: Serializable = struct
    type t = int
    let serialize i = string_of_int i
end

module IntStack = SerializableListStack(IntSerializable)

let int_stack = IntStack.mkEmpty()

let () = print_endline (string_of_int v)
