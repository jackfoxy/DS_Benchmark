namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/21/scheduled-binomial-heap

open FSharpx.Collections

module ScheduledBinomialHeap =

  type Tree<'a> = Node of ('a * list<Tree<'a>>)
  type Digit<'a> = option<Tree<'a>>
  type Schedule<'a> = list<LazyList<Digit<'a>>>
  type t<'a> = LazyList<Digit<'a>> * Schedule<'a>

  let empty() = LazyList.empty, []  

  let singleton x = LazyList.cons (Some x) (LazyList.empty), []

  let isEmpty (x, _) = LazyList.isEmpty x

  let link (Node(x1, c1) as t1) (Node(x2, c2) as t2) =
    if x1 <= x2 then Node (x1, t2::c1) else Node(x2, t1::c2)

  let rec insTree t = function
    | LazyList.Nil -> LazyList.cons (Some t) (LazyList.empty)
    | LazyList.Cons(None, tl) -> LazyList.cons (Some t) tl
    | LazyList.Cons(Some hd, tl) -> LazyList.cons None (insTree (link t hd) tl)

  let rec mrg ds1 ds2 =
    match ds1, ds2 with
    | ds1, LazyList.Nil -> ds1
    | LazyList.Nil, ds2 -> ds2
    | LazyList.Cons(None, tl1), LazyList.Cons(hd, tl2)
    | LazyList.Cons(hd, tl1), LazyList.Cons(None, tl2) -> LazyList.cons hd (mrg tl1 tl2)
    | LazyList.Cons(Some x1, tl1), LazyList.Cons(Some x2, tl2) ->
        LazyList.cons None (insTree (link x1 x2) (mrg tl1 tl2))

  let rec normalize = function
    | LazyList.Nil as ds -> ds
    | LazyList.Cons(hd, tl) as ds ->
        normalize tl |> ignore //evaluate every lazy expression
        ds

  let rec exec = function
    | [] -> []
    | LazyList.Cons(None, job) :: sched -> job::sched
    | _::sched -> sched

  let insert x (ds, sched) =
    let ds' = insTree (Node(x, [])) ds
    ds', exec (exec (ds'::sched))

  let merge (ds1, _) (ds2, _) =
    normalize (mrg ds1 ds2), []

  let rec removeMinTree = function
    | LazyList.Nil -> raise Empty
    | LazyList.Cons(Some t, LazyList.Nil) -> t, LazyList.empty
    | LazyList.Cons(None, ds) ->
        let t', ds' = removeMinTree ds
        t', LazyList.cons None ds'
    | LazyList.Cons(Some (Node (x,_) as t), ds) ->
        let t', ds' = removeMinTree ds
        match t' with
        | Node(x', _) ->
            if x <= x' then
              t, LazyList.cons None ds
            else
              t', LazyList.cons (Some t) ds' 

  let findMin (ds, _) =
    let Node(x, _), _ = removeMinTree ds
    x

  let removeMin (ds, _) =
    let Node(x, c), ds' = removeMinTree ds
    let ds'' = mrg (LazyList.ofList (List.map Some (List.rev c))) ds'
    normalize ds'', []