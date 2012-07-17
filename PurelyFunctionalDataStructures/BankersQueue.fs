namespace PurelyFunctionalDataStructures
//http://lepensemoi.free.fr/index.php/2009/12/31/bankers-queue
module BankersQueue =

  type t<'a> = {
    FrontLength : int
    Front : LazyList<'a>
    BackLength : int
    Back : LazyList<'a>
  }

  let empty() = {
    FrontLength = 0
    Front = LazyList.empty
    BackLength = 0
    Back = LazyList.empty
  }

  let isEmpty x = x.FrontLength = 0

  let singleton x = {
    FrontLength = 1
    Front = LazyList.singleton x
    BackLength = 0
    Back = LazyList.empty
  }

  let private check x =
    if x.BackLength < x.FrontLength
    then x
    else
      { FrontLength = x.BackLength + x.FrontLength
        Front = LazyList.rev x.Back |> LazyList.append x.Front
        BackLength = 0
        Back = LazyList.empty
      }

  let snoc x q =
    { q with
        BackLength = q.BackLength + 1
        Back = LazyList.cons x q.Back
    } |> check 

  let head = function
    | {FrontLength = 0} -> raise Empty
    | q -> LazyList.head q.Front

  let tail = function
    | {FrontLength = 0} -> raise Empty
    | q ->
      {q with
        FrontLength = q.FrontLength - 1
        Front = LazyList.tail q.Front
      } |> check
 