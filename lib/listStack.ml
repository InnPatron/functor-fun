open Stack

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
