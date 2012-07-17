namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2010/01/07/lazy-pairing-heap
module LazyPairingHeap =

  type t<'a> =
    | E
    | T of 'a * t<'a> * Lazy<t<'a>>

  let empty = E

  let isEmpty = function E -> true | _ -> false

  let singleton x = T(x, E, lazy E)

  let rec merge t1 t2 =
    match t1, t2 with
    | E, h -> h
    | h, E -> h
    | T(x, _, _), T(y, _, _) ->
        if x <= y then link t1 t2 else link t2 t1

  and link t1 t2 =
    match t1, t2 with
    | T(x, E, m), a -> T(x, a, m)
    | T(x, b, m), a -> T(x, E, lazy merge (merge a b) (Lazy.force m))
    | _ -> failwith "should not get there"

  let insert x a = merge (singleton x) a

  let rec contains x = function
    | E -> false
    | T (y, a, b) ->
        x = y || contains x a || contains x (Lazy.force b)

  let findMin = function
    | E -> raise Empty
    | T(x, _, _) -> x

  let deleteMin = function
    | E -> raise Empty
    | T(x, a, b) -> merge a (Lazy.force b)

  let rec remove x = function
    | E -> E
    | T(y, a, b) as t ->
        if a = x
        then merge a (Lazy.force b)
        else T(y, remove x a, lazy remove x (Lazy.force b))