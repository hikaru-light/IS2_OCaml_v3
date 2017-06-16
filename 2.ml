module Separate:
sig
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree
val create : 'a tree
exception Empty
val insert : 'a -> 'a tree -> 'a tree

val delete : 'a -> 'a tree -> 'a tree
val search : 'a -> 'a tree -> bool
end
=struct


exception Empty

type 'a tree =
        | Nil
        | Node of 'a * 'a tree * 'a tree

let create = Nil

let rec insert w v = match v with
                           | Nil -> Node(w, Nil, Nil)
                           | Node(x, Nil, Nil) -> if x < w then
                                                    Node(x, Nil, Node(w, Nil, Nil))
                                                  else
                                                    Node(x, Node(w, Nil, Nil), Nil)
                           | Node(x, y, z) -> if x > w then
                                                Node(x, insert w y, z)
                                              else
                                                Node(x, y, insert w z)

let rec search_min = function
                    | Nil -> raise Empty
                    | Node(z, Nil, _) -> z
                    | Node(_, left, _) -> search_min left



let rec delete_min = function
                     | Nil -> raise Empty
                     | Node(_, Nil, right) -> right
                     | Node(z, left, right) -> Node(z, delete_min left, right)






let rec delete c tr = match tr with
                       | Nil -> raise Empty
                       | Node(z, left, right) -> if c = z then
                                                   if left = Nil then
                                                     right
                                                   else
                                                     if right = Nil then
                                                       left
                                                     else let min = serch_min right in Node(min, left, delete_min right)
                                                else
                                                   if c < z then
                                                     Node(z, (delete c left), right)
                                                   else Node(z, left, (delete c right))



let rec search n no = match no with
                | Nil -> false
                | Node(z, left, right) -> if n = z then
                                            true
                                          else
                                            if n < z then
                                              search n left
                                            else
                                              search n right



end;;
