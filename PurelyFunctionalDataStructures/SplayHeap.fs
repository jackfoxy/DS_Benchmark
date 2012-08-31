namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/splay-heap
module SplayHeap =

  type t<'a> =
    | E
    | T of t<'a> * 'a * t<'a>

  let empty = E

  let isEmpty = function E -> true | _ -> false

  let singleton x = T(E, x, E)

  let rec partition pivot t =
    match t with
    | E -> (E, E)
    | T (a, x, b) ->
        if x <= pivot then
          match b with
          | E -> (t, E)
          | T(b1, y, b2) ->
              if y <= pivot then
                let small, big = partition pivot b2
                T(T(a, x, b1), y, small), big
              else
                let small, big = partition pivot b1
                T(a, x, small), T(big, y, b2)
        else
          match a with
          | E -> E, t
          | T(a1, y, a2) ->
              if y <= pivot then
                let small, big = partition pivot a2
                T(a1, y, small), T(big, x, b)
              else
                let small, big = partition pivot a1
                small, T(big, y, T(a2, x, b))

  let rec insert x t =
    let small, big = partition x t
    T(small, x, big)

  let rec contains x = function
    | E -> false
    | T (a, y, b) ->
        x = y
        || (x < y && contains x a)
        || contains x b

  let rec remove x = function
    | E -> E
    | T(a, y, b) as t ->
        if x = y then E
        elif x < y then T(remove x a, y, b)
        else T(a, y, remove x b)

  let rec findMin = function
    | E -> raise Empty
    | T(E,x,_) -> x
    | T(a,x,b) -> findMin a

  let rec deleteMin = function
    | E -> raise Empty
    | T(E,x,b) -> b
    | T(T(E,x,b), y, c) -> T(b, y, c)
    | T(T(a,x,b), y, c) -> T(deleteMin a, x, T(b,y,c))

  let rec merge t1 t2=
    match t1, t2 with
    | E, t -> t
    | T(a,x,b), t ->
        let ta, tb = partition x t
        T(merge a ta, x, merge b tb)

  let rec iter f = function
    | E -> ()
    | T(a, x, b) ->
        iter f a
        f x
        iter f b