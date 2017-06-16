let rec max lis =
        match lis with
        | Nil -> Nil
        | Cell (l, Nil) -> l
        | Cell (n, Cell (m, rest)) -> if n > m then
                                        max (Cell (n,  rest))
                                      else
                                        max (Cell (m,  rest))
;;

let rec indexOf num lis =
        match num with
        | (m, Nil) -> -1
        | (number, Cell (n, rest)) -> if n=number then
                                        0
                                      else
                                        1 + (indexOf number rest)
;;
