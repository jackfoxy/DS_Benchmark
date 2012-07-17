namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
module BatchedQueue =

  type t<'a> = {
    Front : list<'a>
    RBack : list<'a>
  }

  let create f r = { Front = f; RBack = r}

  let empty() = create [] []

  let isEmpty q = List.isEmpty q.Front

  let private checkf = function
    | [], r -> create (List.rev r) []
    | f, r -> create f r

  let head q =
    match q.Front with
    | hd::_ -> hd
    | _ -> raise Empty

  let tail q =
    match q.Front with
    | hd::tl -> checkf (tl, q.RBack)
    | _ -> raise Empty

  let snoc x q = checkf (q.Front, x :: q.RBack)

  let singleton x = empty() |> snoc x