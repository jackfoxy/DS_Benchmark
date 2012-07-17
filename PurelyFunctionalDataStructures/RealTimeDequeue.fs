namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2010/02/05/real-time-double-ended-queue
module RealTimeDequeue =

  type t<'a> = {
    C : int //c = 2 or 3
    FrontLength : int
    Front : LazyList<'a>
    StreamFront : LazyList<'a>
    RBackLength : int
    RBack : LazyList<'a>
    StreamRBack : LazyList<'a>
  }

  let create c lenf f sf lenr r sr = {
    C = c
    FrontLength = lenf
    Front = f
    StreamFront = sf
    RBackLength = lenr
    RBack = r
    StreamRBack = sr
  }

  let empty c =
    create c 0 (LazyList.empty) (LazyList.empty) 0 (LazyList.empty) (LazyList.empty) 

  let isEmpty q = q.FrontLength=0 && q.RBackLength=0

  let length q = q.FrontLength + q.RBackLength

  let exec1 = function
    | LazyList.Cons(x, s) -> s
    | s -> s

  let exec2 x = (exec1 >> exec1) x

  let rec rotateRev c = function
    | LazyList.Nil, r, a -> LazyList.append (LazyList.rev r) a
    | LazyList.Cons(x, f), r, a ->
        let a' = LazyList.drop c r
        let b' = LazyList.append (LazyList.take c r) a |> LazyList.rev
        LazyList.cons x (rotateRev c (f, a', b'))

  let rec rotateDrop c f j r =
    if j < c then
      rotateRev c (f, LazyList.drop j r, LazyList.empty)
    else
      match f with
      | LazyList.Cons(x, f') -> LazyList.cons x (rotateDrop c f' (j-c) (LazyList.drop c r))
      | _ -> failwith "should not get there"

  let check q =
    let n = length q
    if q.FrontLength > q.C * q.RBackLength + 1 then
      let i= n / 2
      let j = n - i
      let f' = LazyList.take i q.Front
      let r' = rotateDrop q.C q.RBack i q.Front
      create q.C i f' f' j r' r'
    elif q.RBackLength > q.C * q.FrontLength + 1 then
      let j = n / 2
      let i = n - j
      let f' = LazyList.take j q.RBack
      let r' = rotateDrop q.C q.Front j q.RBack
      create q.C i f' f' j r' r'
    else
      q

  let cons x q =
    create q.C (q.FrontLength+1) (LazyList.cons x q.Front) (exec1 q.StreamFront) q.RBackLength q.RBack (exec1 q.StreamRBack)
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
        create q.C (q.FrontLength-1) xs (exec2 q.StreamFront) q.RBackLength q.RBack (exec2 q.StreamRBack)
        |> check 

  let snoc x q =
    create q.C q.FrontLength q.Front (exec1 q.StreamFront) (q.RBackLength+1) (LazyList.cons x q.RBack) (exec1 q.StreamRBack)
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
        create q.C q.FrontLength (exec2 q.StreamFront) q.Front (q.RBackLength-1) xs (exec1 q.StreamRBack)
        |> check 