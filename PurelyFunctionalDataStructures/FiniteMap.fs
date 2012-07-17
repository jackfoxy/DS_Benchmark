namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/03/finite-map
module FiniteMap =

  type t<'a, 'b> =
    | E
    | T of (t<'a, 'b> * ('a * 'b) * t<'a, 'b>)  

  let empty = BinarySearchTree.empty

  let isEmpty = BinarySearchTree.isEmpty

  let singleton k v = BinarySearchTree.singleton (k, v)

  let rec containsKey key = function
    | E -> false
    | T (a, (k, v), b) ->
        key = k
        || (key < k && containsKey key a)
        || (key > k && containsKey key b)

  let rec containsValue x = function
    | E -> false
    | T (a, (k, v), b) ->
        x = v
        || (x < v && containsKey x a)
        || (x > v && containsKey x b)

  let rec tryFind key = function
    | E -> None
    | T (a, (k, v), b) ->
        if key = k then Some v
        elif key < k then tryFind key a
        else tryFind key b

  let rec find key = function
    | E -> raise NotFound
    | T (a, (k, v), b) ->
        if key = k then v
        elif key < k then find key a
        else find key b

  let rec insert k v = function
    | E -> T(E, (k,v), E)
    | T(a, (k', v'), b) as t ->
        if k = k' then t
        elif k < k' then T(insert k v a, (k',v'), b)
        else T(a, (k',v'), insert k v b)

  let rec replace k v = function
    | E -> T(E, (k,v), E)
    | T(a, (k', v'), b) as t ->
        if k = k' then T(a, (k,v), b)
        elif k < k' then T(insert k v a, (k',v'), b)
        else T(a, (k',v'), insert k v b)

  let rec remove key = function
    | E -> E
    | T(a, (k, v), b) as t ->
        if k = key then E
        elif key < k then T(remove key a, (k,v), b)
        else T(a, (k,v), remove key b)