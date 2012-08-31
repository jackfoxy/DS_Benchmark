namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/07/bottom-up-merge-sort
module BottomUpMergeSort =

  type Sortable<'a> = int * Lazy<list<list<'a>>>

  let rec merge xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::tlx, y::tly ->
        if x <= y then
          x :: merge tlx ys
        else
          y :: merge xs tly

  let empty() = (0, lazy [])

  let isEmpty (n, _) = n = 0

  let singleton x = (1, lazy [[x]])

  let rec addSeg seg segs size =
    if size % 2 = 0 then
      seg::segs
    else
      addSeg (merge seg (List.head segs)) (List.tail segs) (size / 2)

  let add x (size, segs) =
    size + 1, lazy addSeg [x] (Lazy.force segs) size

  let rec mergeAll xs ys =
    match xs, ys with
    | xs, [] -> xs
    | xs, seg::segs -> mergeAll (merge xs seg) segs

  let sort (size, segs) =
    mergeAll [] (Lazy.force segs)