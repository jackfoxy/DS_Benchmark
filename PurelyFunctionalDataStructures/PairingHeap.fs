namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
module PairingHeap =

  type t<'a> =
    | E
    | T of 'a * list<t<'a>>

  let empty = E

  let isEmpty = function E -> true | _ -> false

  let singleton x = T(x, [])

  let merge t1 t2 =
    match t1, t2 with
    | E, h -> h
    | h, E -> h
    | T(x, xs), T(y, ys) ->
        if x <= y then T(x, t2::xs) else T(y, t1::ys)

  let insert x t =
    merge (singleton x) t

  let rec mergePairs = function
    | [] -> E
    | [x] -> x
    | x::y::tl -> merge (merge x y) (mergePairs tl)

  let rec contains x = function
    | E -> false
    | T (a, ys) ->
        x = a || List.exists (contains x) ys

  let rec findMin = function
    | E -> raise Empty
    | T(x, _) -> x

  let rec deleteMin = function
    | E -> raise Empty
    | T(x, xs) -> mergePairs xs

  let rec remove x = function
    | E -> E
    | T(a, ys) as t ->
        if a = x
        then mergePairs ys
        else T(a, List.map (remove x) ys)