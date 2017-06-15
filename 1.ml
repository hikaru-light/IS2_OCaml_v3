module List = struct

exception Empty

type 'a list =
        | Nil
        | Cell of 'a * 'a list

let create =
        Nil
;;

let unshift ele lis =
        Cell (ele, lis)
;;

let shift lis =
        match lis with
          | Nil -> Nil
          | Cell (_, res) -> res
;;

end;;
