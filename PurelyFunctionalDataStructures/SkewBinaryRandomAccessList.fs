namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/05/skew-binary-random-access-list
module SkewBinaryRandomAccessList =

  type Tree<'a> =
    | Leaf of 'a
    | Node of 'a * Tree<'a> * Tree<'a>

  type t<'a> = list<int * Tree<'a>>

  let empty() = []

  let isEmpty = List.isEmpty

  let cons x = function
    | (w1, t1)::(w2, t2)::ts' as ts ->
        if w1 = w2 then
          (1+w1+w2, Node(x, t1, t2)) :: ts'
        else
          (1, Leaf x) :: ts
    | _ -> failwith "should not get there"

  let singleton x = empty() |> cons x 

  let head = function
    | [] -> raise Empty
    | (1, Leaf x)::_ -> x
    | (_, Node(x, _, _))::_ -> x
    | _ -> failwith "should not get there"

  let tail = function
    | [] -> raise Empty
    | (1, Leaf _)::ts -> ts
    | (w, Node(_, t1, t2))::ts -> (w/2, t1) :: (w/2, t2) :: ts
    | _ -> failwith "should not get there"

  let rec lookupTree = function
    | 1, 0, Leaf x -> x
    | 1, i, Leaf x -> raise Subscript
    | w, 0, Node(x, _, _) -> x
    | w, i, Node(x, t1, t2) ->
        if i < w / 2 then
          lookupTree (w/2, i - 1, t1)
        else
          lookupTree (w/2, i - 1 - w/2, t2)
    | _ -> failwith "should not get there"

  let rec updateTree = function
    | 1, 0, y, Leaf x -> Leaf y
    | 1, i, y, Leaf x -> raise Subscript
    | w, 0, y, Node(x, t1, t2) -> Node(y, t1, t2)
    | w, i, y, Node(x, t1, t2) ->
        if i < w / 2 then
          Node(x, updateTree (w/2, i-1, y, t1) , t2)
        else
          Node(x, t1, updateTree (w/2, i - 1 - w/2, y, t2))
    | _ -> failwith "should not get there"

  let rec lookup i = function
    | [] -> raise Subscript
    | (w, t)::ts ->
        if i < w then lookupTree (w, i, t) else lookup (i - w) ts

  let rec update i y = function
    | []  -> raise Subscript
    | (w, t)::ts ->
        if i < w then
          (w, updateTree(w, i, y, t)) :: ts
        else
          (w, t) :: update (i - w) y ts