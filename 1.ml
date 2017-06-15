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
          | Cell (n, res) -> res
;;

let rec push ele lis =
        match lis with
        | Nil -> Cell (ele, Nil)
        | Cell (n, rest) -> if rest = Nil then
                              Cell (n, Cell (ele, Nil))
                            else
                              Cell (n, push ele rest)
;;

let rec pop lis =
        match lis with
        | Nil -> Nil
        | Cell (n, rest) -> if rest = Nil then
                              Nil
                            else
                              Cell (n, pop rest)

end;;

