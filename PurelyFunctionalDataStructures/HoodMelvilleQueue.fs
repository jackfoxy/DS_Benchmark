namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/21/hood-melville-queue
module HoodMelvilleQueue =

  type RotationState<'a> =
    | Idle
    | Reversing of int * list<'a> * list<'a> * list<'a> * list<'a>
    | Appending of int *list<'a> * list<'a>
    | Done of list<'a>

  type t<'a> = {
    FrontLength : int
    Front : list<'a>
    State : RotationState<'a>
    RBackLength : int
    RBack : list<'a>
  }

  let create lenf f state lenr r = {
    FrontLength = lenf
    Front = f
    State = state
    RBackLength = lenr
    RBack = r
  }

  let exec = function
    | Reversing(ok, x::f, f', y::r, r') -> Reversing(ok+1, f, x::f', r, y::r')
    | Reversing(ok, [], f', [y], r') -> Appending(ok, f', y::r')
    | Appending(0, f', r') -> Done r'
    | Appending(ok, x::f', r') -> Appending(ok-1, f', x::r')
    | state -> state

  let invalidate = function
    | Reversing(ok, f, f', r, r') -> Reversing(ok-1, f, f', r, r')
    | Appending(0, f', x::r') -> Done r'
    | Appending(ok, f', r') -> Appending(ok-1, f', r')
    | state -> state

  let exec2 q =
    match exec (exec q.State) with
    | Done newf -> {q with Front = newf ; State = Idle }
    | newstate -> {q with State = newstate } 

  let check q =
    if q.RBackLength <= q.FrontLength then
      exec2 q
    else
      let newstate = Reversing(0, q.Front, [], q.RBack, [])
      create (q.FrontLength + q.RBackLength) q.Front newstate 0 [] |> exec2

  let empty() = create 0 [] Idle 0 []

  let isEmpty q = q.FrontLength = 0

  let snoc x q =
    check {q with RBackLength = q.RBackLength + 1; RBack = x::q.RBack}

  let singleton x = empty() |> snoc x

  let head q =
    match q.Front with
    | hd::_ -> hd
    | _ -> raise Empty

  let tail q =
    match q.Front with
    | hd::_ ->
        create (q.FrontLength-1) q.Front (invalidate q.State) q.RBackLength q.RBack
        |> check
    | _ -> raise Empty