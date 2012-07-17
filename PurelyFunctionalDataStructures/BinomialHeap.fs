namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/10/binomial-heap

type BinomialTree<'a> = Node of (int * 'a * list<BinomialTree<'a>>)

module BinomialHeap =

  type t<'a> = list<BinomialTree<'a>>

  let empty = []

  let isEmpty = function [] -> true | _ -> false

  let rank (Node(r, _, _)) = r

  let root (Node(_, x, _)) = x

  let private link (Node(r, x1, xs1) as n1) (Node(_, x2, xs2) as n2) =
    if x1 <= x2 then
      Node(r+1, x1, n2 :: xs1)
    else
      Node(r+1, x2, n1 :: xs2)

  let rec private insertTree t = function
    | [] -> [t]
    | hd::tl as t' ->
        if rank t < rank hd then t::t' else insertTree (link t hd) tl

  let private singletonTree x = Node(0, x, [])

  let singleton x = [singletonTree x] 

  let insert x t =
    insertTree (singletonTree x) t

  let rec merge h1 h2 =
    match h1, h2 with
    | [], x -> x
    | x, [] -> x
    | (hd1::tl1), (hd2::tl2) ->
      if rank hd1 < rank hd2 then hd1 :: merge tl1 h2
      elif rank hd1 > rank hd2 then hd2 :: merge h1 tl2
      else insertTree (link hd1 hd2) (merge ( tl1) ( tl2))

  let rec private removeMinTree = function
    | [] -> raise Empty
    | [t] -> t, []
    | hd::tl ->
        let hd', tl'= removeMinTree tl
        if root hd <= root hd' then hd, tl else hd', hd::tl'

  let findMin h =
    let (t, _) = removeMinTree h
    root t

  let removeMin h =
    let Node(_, x, xs1), xs2 = removeMinTree h
    merge (List.rev xs1) xs2