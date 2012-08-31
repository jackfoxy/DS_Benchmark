namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/11/27/custom-stack
module CustomStack =

  type t<'a> =
    | Nil
    | Cons of ('a * t<'a>)

  let empty = Nil

  let isEmpty = function Nil -> true | _ -> false

  let cons x cs = Cons(x, cs)

  let singleton x = cons x empty

  let head = function
    | Nil -> raise Empty
    | Cons (hd, tl) -> hd

  let tail = function
    | Nil -> raise Empty
    | Cons (hd, tl) -> tl

  let rec append x y =
    match x with
    | Nil -> y
    | Cons (hd, tl) -> Cons (hd, append tl y)

  let rec set xs i x =
    match xs, i with
    | Nil, _ -> raise Subscript
    | Cons (hd, tl), 0 -> Cons(x, tl)
    | Cons(hd, tl), n -> Cons(hd, set tl (i-1) x)