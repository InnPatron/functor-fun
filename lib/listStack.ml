open Stack
open Serializable

module ListStack(E: StackElement) : (Stack with type elt = E.elt) = struct
    type elt = E.elt
    type stack = elt list

    let mkEmpty () = []
    let push stack x = match stack with
        | [] -> [x]
        | _ -> x :: stack
    ;;
    let pop stack = match stack with
        | []            -> ([], None)
        | head :: tail  -> (tail, Some head)
end

module ListStackSer1(ES: Serializable) : (SerializableStack with type elt = ES.t) = struct
    include ListStack(struct
        type elt = ES.t
    end);;

    type t = stack
    (* type t = stack *)
    let rec helper stack = match stack with
        | []            -> ""
        | head :: tail  -> String.cat (ES.serialize head) (helper tail)

    let serialize stack =
        String.cat (String.cat "[" (helper stack)) "]"

end
