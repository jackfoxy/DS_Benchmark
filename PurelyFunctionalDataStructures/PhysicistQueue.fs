namespace PurelyFunctionalDataStructures
//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/31/physicist-queue
module PhysicistQueue =

  type t<'a> = {
    Prefix : list<'a>
    FrontLength : int
    Front : Lazy<list<'a>>
    RBackLength : int
    RBack : list<'a>
  }

  let empty() = {
    Prefix = []
    FrontLength = 0
    Front = lazy []
    RBackLength = 0
    RBack = []
  }

  let isEmpty q = q.FrontLength = 0

  let private checkw q =
    match q.Prefix with
//    | [] -> {q with Prefix = Lazy.force q.Front}
    | [] -> {q with Prefix = q.Front.Value}
    | _ -> q

  let private check q =
    if q.RBackLength < q.FrontLength then
      checkw q
    else
//      let Prefix = Lazy.force q.Front
      let Prefix = q.Front.Value
      { Prefix = Prefix
        FrontLength = q.RBackLength + q.FrontLength
        Front = lazy (q.RBack |> List.rev |> List.append q.Prefix)
        RBackLength = 0
        RBack = []
      }

  let snoc x (q:t<_>) =
    { q with
        RBackLength = q.RBackLength + 1
        RBack = x :: q.RBack
    } |> check

  let singleton x = empty() |> snoc x

  let head q =
    match q.Prefix with
    | hd::_ -> List.head q.Prefix
    | _ -> raise Empty

  let tail q =
    match q.Prefix with
    | hd::_ ->
        {q with
          FrontLength = q.FrontLength - 1
//          Front = lazy (Lazy.force q.Front |> List.tail)
          Front = lazy (q.Front.Value |> List.tail)
        } |> check
    | _ -> raise Empty