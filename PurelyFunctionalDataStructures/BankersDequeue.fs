namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2010/02/11/bankers-double-ended-queue
module BankersDequeue =

  type t<'a> = {
    C : int // c > 1
    FrontLength : int
    Front : LazyList<'a>
    RBackLength : int
    RBack : LazyList<'a>
  }

  let create c lenf f lenr r = {
    C = c
    FrontLength = lenf
    Front = f
    RBackLength = lenr
    RBack = r
  }

  let empty c = create c 0 (LazyList.empty) 0 (LazyList.empty)

  let isEmpty q = q.FrontLength=0 && q.RBackLength=0

  let length q = q.FrontLength + q.RBackLength

  let check q =
    let n = length q
    if q.FrontLength > q.C * q.RBackLength + 1 then
      let i= n / 2
      let j = n - i
      let f' = LazyList.take i q.Front
      let r' = LazyList.drop i q.Front |> LazyList.rev |> LazyList.append q.RBack
      create q.C i f' j r'
    elif q.RBackLength > q.C * q.FrontLength + 1 then
      let j = n / 2
      let i = n - j
      let f' = LazyList.take j q.RBack
      let r' = LazyList.drop j q.RBack |> LazyList.rev |> LazyList.append q.Front
      create q.C i f' j r'
    else
      q

  let cons x q =
    create q.C (q.FrontLength+1) (LazyList.cons x q.Front) q.RBackLength q.RBack
    |> check

  let singleton c x = empty c |> cons x

  let head q =
    match q.Front, q.RBack with
    | LazyList.Nil, LazyList.Nil -> raise Empty
    | LazyList.Nil, LazyList.Cons(x, _) -> x
    | LazyList.Cons(x, _), _ -> x

  let tail q =
    match q.Front, q.RBack with
    | LazyList.Nil, LazyList.Nil -> raise Empty
    | LazyList.Nil, LazyList.Cons(x, _) -> empty q.C
    | LazyList.Cons(x, xs), _ ->
        create q.C (q.FrontLength-1) xs q.RBackLength q.RBack
        |> check 

  let snoc x q =
    create q.C q.FrontLength q.Front (q.RBackLength+1) (LazyList.cons x q.RBack)
    |> check

  let last q =
    match q.Front, q.RBack with
    | LazyList.Nil, LazyList.Nil -> raise Empty
    | _, LazyList.Cons(x, _) ->  x
    | LazyList.Cons(x, _), LazyList.Nil-> x

  let init q =
    match q.Front, q.RBack with
    | LazyList.Nil, LazyList.Nil -> raise Empty
    | _, LazyList.Nil -> empty q.C
    | _, LazyList.Cons(x, xs) ->
        create q.C q.FrontLength q.Front (q.RBackLength-1) xs
        |> check 