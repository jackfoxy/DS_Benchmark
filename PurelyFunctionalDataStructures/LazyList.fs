namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/11/24/purely-functional-data-structures-in-f-introduction
// does not play well with release optimize compile
module LazyList =
  let rec private revAux r acc =
    match r with
    | LazyList.Nil -> acc
    | LazyList.Cons(hd, tl) -> revAux tl (LazyList.cons hd acc)

  let rev r =
    revAux r LazyList.empty

  let singleton x =
    LazyList.cons x LazyList.empty

  let rec drop n xs =
    if n < 0 then invalidArg "n" "n was negative"
    elif n > 0 then
      match xs with
      | LazyList.Cons(x, xs') -> drop (n-1) xs'
      | _ -> LazyList.empty
    else
      xs

  let rec private takeAux n xs acc =
    if n < 0 then invalidArg "n" "n was negative"
    elif n > 0 then
      match xs with
      | LazyList.Cons(x, xs') -> takeAux (n-1) xs' (LazyList.cons x acc)
      | _ -> acc
    else
      acc

  let take n xs =
    takeAux n xs LazyList.empty |> rev