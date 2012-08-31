namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/03/binary-search-tree
module BinarySearchTree =

  type t<'a> =
    | E
    | T of (t<'a> * 'a * t<'a>)  

  let empty = E

  let isEmpty = function E -> true | _ -> false

  let singleton x = T(E, x, E)

  let rec contains x = function
    | E -> false
    | T (a, y, b) ->
        x = y
(*Julien's comment: modified as per the insightful comment. Thanks !
    if we didn't compare x and y, an element smaller than
    the minimum (or larger than the maximum) of the tree
    would be looked for in the entire tree.
*)
        || (x > y && contains x b)
        || (x < y && contains x a)

  let rec insert x = function
    | E -> T(E, x, E)
    | T(a, y, b) as t ->
        if x = y then t
        elif x < y then T(insert x a, y, b)
        else T(a, y, insert x b)

  let rec remove x = function
    | E -> E
    | T(a, y, b) as t ->
        if x = y then E
        elif x < y then T(remove x a, y, b)
        else T(a, y, remove x b)