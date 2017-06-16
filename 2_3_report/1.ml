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
;;

let rec size lis =
        match lis with
        | Nil -> 0
        | Cell (n, rest) -> if rest = Nil then
                              1
                            else
                              1 + size rest
;;

let rec get num lis =
        match (num, lis) with
        | (1, Cell (l, rest)) -> l
        | (number, Cell (n, rest)) -> get (number-1) rest
;;

let rec set tar num lis =
        match lis with
        | Nil -> Nil
        | Cell (n, rest) -> if n=tar then
                              Cell (num, set tar num rest)
                            else
                              Cell (n, set tar num rest)
;;

let rec remove tar lis =
        match lis with
        | Nil -> Nil
        | Cell (n, rest) -> if n=tar then
                              remove tar rest
                            else
                              Cell (n, remove tar rest)
;;

let rec concat li1 li2 =
        match li1 with
        | Nil -> li2
        | Cell (n, rest) -> Cell (n, concat rest li2)
;;

let rec max lis =
                match lis with
                        | Cell (l, Nil) -> l
                        | Cell (n, Cell (m, rest)) -> if n > m then
                                                        max (Cell (n,  rest))
                                                      else
                                                        max (Cell (m,  rest))
;;

let rec min lis =
        match lis with
        | Cell (l, Nil) -> l
        | Cell (n, Cell (m, rest)) -> if n < m then
                                        min (Cell (n, rest))
                                      else
                                        min (Cell (m, rest))
;;

let rec indexOf num lis =
                match (num, lis) with
                        | (m, Nil) -> failwith "-1"
                        | (number, Cell (n, rest)) -> if n=number then
                                                        0
                                                      else
                                                        1 + (indexOf number rest)
;;

end;;
