namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/11/alternative-binary-random-access-list
module AltBinaryRandomAccessList =

  type t<'a> =
    | Nil
    | Zero of t<'a * 'a>
    | One of 'a * t<'a * 'a>
    //polymorphic recursion cannot be achieved through let-bound functions
    //hence we use static member methods
    static member cons (x : 'a) : t<'a> -> t<'a> = function
      | Nil -> One (x, Nil)
      | Zero ps -> One (x, ps)
      | One(y, ps) ->  Zero(t.cons (x,y) ps)

    static member uncons : t<'a> -> 'a * t<'a> = function
      | Nil -> raise Empty
      | One(x, Nil) -> (x, Nil)
      | One(x, ps) -> (x, Zero ps)
      | Zero ps ->
          let (x,y), ps' = t.uncons ps
          x, (One (y, ps'))

    static member lookup (i:int) : t<'a> -> 'a = function
      | Nil -> raise Subscript
      | One(x, ps) ->
          if i = 0 then x else t.lookup (i-1) (Zero ps)
      | Zero ps ->
          let (x, y) = t.lookup (i/2) ps
          if i % 2 = 0 then x else y

    static member fupdate : ('a -> 'a) * int * t<'a> -> t<'a> = function
      | f, i, Nil -> raise Subscript
      | f, 0, One(x, ps) -> One(f x, ps)
      | f, i, One (x, ps) -> t.cons x (t.fupdate (f, i-1, Zero ps))
      | f, i, Zero ps ->
          let f' (x, y) = if i % 2= 0 then f x, y else x, f y
          Zero(t.fupdate(f', i/2, ps))

  let empty = Nil

  let isEmpty = function Nil -> true | _ -> false

  let cons x xs = t.cons x xs

  let uncons x = t.uncons x

  let head xs =
    let x, _ = uncons xs
    x

  let tail xs =
    let _, xs'  =uncons xs
    xs'

  let rec lookup i = t.lookup

  let update i y xs = t.fupdate ((fun x -> y), i, xs)