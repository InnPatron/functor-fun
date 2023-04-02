module ListStack : Stack.Stack = struct
    type 'a stack = 'a list

    let mkEmpty () = []
    let push stack x = match stack with
        | [] -> [x]
        | _ -> x :: stack
    ;;
    let pop stack = match stack with
        | []            -> ([], None)
        | head :: tail  -> (tail, Some head)
end

module
                | [] -> ""
                | h :: t -> String.cat (Elt.serialize h) (serialize t)
            ;;

        end;;

type comparison = Less | Equal | Greater
module type ORDERED_TYPE =
    sig
      type t
      val compare: t -> t -> comparison
    end;;
module Set =
    functor (Elt: ORDERED_TYPE) ->
      struct
        type element = Elt.t
        type set = element list
        let empty = []
        let rec add x s =
          match s with
            [] -> [x]
          | hd::tl ->
             match Elt.compare x hd with
               Equal   -> s         (* x is already in s *)
             | Less    -> x :: s    (* x is smaller than all elements of s *)
             | Greater -> hd :: add x tl
        let rec member x s =
          match s with
            [] -> false
          | hd::tl ->
              match Elt.compare x hd with
                Equal   -> true     (* x belongs to s *)
              | Less    -> false    (* x is smaller than all elements of s *)
              | Greater -> member x tl
      end;;
