namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/21/scheduled-bottom-up-merge-sort
module ScheduledBottomUpMergeSort =

  type Schedule<'a> = list<LazyList<'a>>
  type Sortable<'a> = int * list<LazyList<'a> * Schedule<'a>>

  let empty() = (0, [])

  let isEmpty (x, _) = x = 0

  let rec merge xs ys =
    match xs, ys with
    | xs, LazyList.Nil -> xs
    | LazyList.Nil, ys -> ys
    | LazyList.Cons(x, xs'), LazyList.Cons(y, ys') ->
        if x <= y then
          LazyList.cons x (merge xs' ys)
        else
          LazyList.cons y (merge xs ys')

  let rec exec1 = function
    | [] -> []
    | LazyList.Nil :: sched -> exec1 sched
    | LazyList.Cons(x, xs) :: sched -> xs::sched

  let exec2 (xs, sched) = (xs, exec1 (exec1 sched))

  let rec insertSeg xs segs size rsched =
    if size % 2 = 0 then
      (xs, List.rev rsched) :: segs
    else
      match segs with
      | (xs', [])::segs' ->
          let xs'' = merge xs xs'
          insertSeg xs'' segs' (size / 2) (xs''::rsched)
      | _ -> failwith "should not get there"

  let insert x (size, segs) =
    let segs' = insertSeg (LazyList.cons x (LazyList.empty)) segs size []
    size+1, List.map exec2 segs'

  let singleton x = insert x (empty())

  let rec mergeAll = function
    | xs, [] -> xs
    | xs, (xs', _) :: segs -> mergeAll ((merge xs xs'), segs)

  let sort (size, segs) =
    mergeAll (LazyList.empty, segs) |> LazyList.toList