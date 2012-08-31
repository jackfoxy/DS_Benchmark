namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/05/binary-random-access-list
module BinaryRandomAccessList =

  type Tree<'a> =
    | Leaf of 'a
    | Node of int * Tree<'a> * Tree<'a>

  type t<'a> = list<option<Tree<'a>>>

  let empty() = []

  let isEmpty = List.isEmpty

  let size = function
    | Leaf _ -> 1
    | Node (w, _, _) -> w

  let link t1 t2 = Node(size t1 + size t2, t1, t2)

  let rec consTree = function
    | t, [] -> [Some t]
    | t, None::ts -> Some t :: ts
    | t1, Some t2 :: ts -> None :: consTree ((link t1 t2) , ts)

  let rec unconsTree = function
    | [] -> raise Empty
    | [Some t] -> t, []
    | Some t :: ts -> t, None::ts
    | None::ts ->
        match unconsTree ts with
        | Node(_, t1, t2), ts' -> t1, Some t2::ts'
        | _ -> failwith "should never get there"

  let cons x ts = consTree ((Leaf x), ts)

  let singleton x = empty() |> cons x 

  let head = function
    | Some (Leaf x) :: _ -> x
    | [] -> raise Empty
    | _ -> failwith "should not get there"

  let tail ts =
    let _, ts' = unconsTree ts
    ts'

  let rec lookupTree = function
    | 0, Leaf x -> x
    | i, Leaf x -> raise Subscript
    | i, Node(w, t1, t2) ->
        if i < w / 2 then lookupTree (i, t1) else lookupTree (i - w/2, t2)

  let rec updateTree = function
    | 0, y, Leaf x -> Leaf y
    | i, y, Leaf x -> raise Subscript
    | i, y, Node(w, t1, t2) ->
        if i < w / 2 then
          Node(w, updateTree (i, y, t1) , t2)
        else
          Node(w, t1, updateTree (i - w/2, y, t2))

  let rec lookup i = function
    | [] -> raise Subscript
    | None::ts -> lookup i ts
    | Some t::ts ->
        if i < size t then lookupTree (i, t) else lookup (i - size t) ts

  let rec update i y = function
    | []  -> raise Subscript
    | None::ts -> None :: update i y ts
    | Some t::ts ->
        if i < size t then
          let a = Some <| updateTree (i, y, t)
          a :: ts
        else
          (Some t) :: update (i - size t) y ts