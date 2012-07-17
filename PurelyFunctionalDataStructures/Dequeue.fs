namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/17/double-ended-queue
module Dequeue =

  type t<'a> = list<'a> * list<'a>

  let empty() = [], []

  let isEmpty = function [], [] -> true | _ -> false

  let singleton x =[x], []

  let rec private splitAux n r acc =
    match r with
    | hd::tl when List.length acc < n ->
        splitAux n tl (hd::acc)
    | _ ->
        List.rev r, List.rev acc

  let private split r =
    splitAux (List.length r / 2) r []

  let private checkf = function
    | [], r -> split r
    | deq -> deq

  let private checkr = function
    | f, [] ->
        let a, b = split f
        b, a
    | deq -> deq

  //insert, inspect and remove the front element

  let cons x (f, r) =
    checkr (x::f, r)

  let rec head = function
    | [], [] -> raise Empty
    | hd::tl, _ -> hd
    | [], xs -> List.rev xs |> List.head

  let rec tail = function
    | [], [] -> raise Empty
    | hd::tl, r -> checkf (tl, r)
    | [], r -> split r |> tail

  //insert, inspect and remove the last element  

  let snoc x (f, r) = checkf (f, x::r)

  let rec last = function
    | [], [] -> raise Empty
    | xs, [] -> List.rev xs |> List.head
    | _, hd::tl -> hd

  let rec init = function
    | [], [] -> raise Empty
    | f, hd::tl -> checkr (f, tl)
    | f, [] ->  split f |> init