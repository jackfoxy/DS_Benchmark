namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/31/lazy-binomial-heap

module LazyBinomialHeap =

  type BinomialTree<'a> = Node of (int * 'a * list<BinomialTree<'a>>)  
  type t<'a> = Lazy<list<BinomialTree<'a>>>

  let empty() = lazy []

//  let isEmpty x = Lazy.force x = []
  let isEmpty (x:Lazy<'a list>) = x.Value = []

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

  let singleton x = lazy [singletonTree x]

//  let insert x t =
  let insert x (t:Lazy<BinomialTree<'a> list>) =
//    lazy insertTree (singletonTree x) (Lazy.force t)
    lazy insertTree (singletonTree x) t.Value

  let rec mrg h1 h2 =
    match h1, h2 with
    | [], x | x, [] -> x
    | hd1::tl1, hd2::tl2 ->
        if rank hd1 < rank hd2 then hd1 :: mrg tl1 h2
        elif rank hd1 > rank hd2 then hd2 :: mrg h1 tl2
        else insertTree (link hd1 hd2) (mrg tl1 tl2)

//  let merge h1 h2 =
  let merge (h1:Lazy<BinomialTree<'a> list>) (h2:Lazy<BinomialTree<'a> list>) =
//    lazy mrg (Lazy.force h1) (Lazy.force h2)
    lazy mrg h1.Value h2.Value

  let rec private removeMinTree = function
    | [] -> raise Empty
    | [t] -> t, []
    | hd::tl ->
        let hd', tl'= removeMinTree tl
        if root hd <= root hd' then hd, tl else hd', hd::tl'

  let findMin h =
    let (t, _) = removeMinTree h
    root t

//  let removeMin h =
  let removeMin (h:Lazy<BinomialTree<'a> list>) =
//    let Node(_, x, xs1), xs2 = removeMinTree (Lazy.force h)
    let Node(_, x, xs1), xs2 = removeMinTree h.Value
    lazy mrg (List.rev xs1) xs2