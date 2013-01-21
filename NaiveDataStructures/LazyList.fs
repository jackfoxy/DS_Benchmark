// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/LazyList.fs

// (c) Microsoft Corporation 2005-2009.  

namespace NaiveDataStructures

open System
open System.Collections.Generic

#nowarn "21" // recursive initialization
#nowarn "40" // recursive initialization

exception UndefinedException

[<NoEquality; NoComparison>]
type LazyList<'T> =
    { mutable status : LazyCellStatus< 'T > }
    
    member x.Value = 
        match x.status with 
        | LazyCellStatus.Value v -> v
        | _ -> 
            lock x (fun () -> 
                match x.status with 
                | LazyCellStatus.Delayed f -> 
                    x.status <- Exception UndefinedException; 
                    try 
                        let res = f () 
                        x.status <- LazyCellStatus.Value res; 
                        res 
                    with e -> 
                        x.status <- LazyCellStatus.Exception(e); 
                        reraise()
                | LazyCellStatus.Value v -> v
                | LazyCellStatus.Exception e -> raise e)
    
    static member consc x (l : LazyList<'T>) = CellCons(x,l)
    static member consDelayed x (l : (unit ->LazyList<'T>)) = LazyList.lzy(fun () -> (LazyList.consc x (LazyList.lzy(fun () ->  (LazyList.force (l()))))))
   // static member consf x l = LazyList.consDelayed x l
    static member force (x: LazyList<'T>) = x.Value
    static member get (x : LazyList<'T>) = match LazyList.force x with CellCons (a,b) -> Some(a,b) | CellEmpty -> None
    static member inline getCell (x : LazyList<'T>) = LazyList.force x
    static member lzy f : LazyList<'T> = { status = Delayed f }

    member this.Append (l : LazyList<'T>) =
        let rec append l1  l2 = LazyList.lzy(fun () ->  (appendc l1 l2))
        and appendc l1 l2 =
          match LazyList.getCell l1 with
          | CellEmpty -> LazyList.force l2
          | CellCons(a,b) -> LazyList.consc a (append b l2)

        append this l

    member this.Cons x = LazyList.lzy(fun () -> (LazyList.consc x this))

    member this.CopyTo (arr: _[]) i =
        let rec copyTo (arr: _[]) s i = 
          match LazyList.getCell s with
          | CellEmpty -> ()
          | CellCons(a,b) -> arr.[i] <- a; copyTo arr b (i+1)

        copyTo arr this i

    member this.Filter p =
        let rec filter p s1 = LazyList.lzy(fun () ->  filterc p s1)
        and filterc p s1 =
            match LazyList.getCell s1 with
            | CellCons(a,b) -> if p a then LazyList.consc a (filter p b) else filterc p b
            | CellEmpty -> CellEmpty

        filter p this

    member this.Find p =
        let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException("An index satisfying the predicate was not found in the collection"))
        match this.TryFind p with
        | Some a -> a
        | None   -> indexNotFound()

    member this.TryFind p = 
        let rec tryFind p s1 =
            match LazyList.getCell s1 with
            | CellCons(a,b) -> if p a then Some a else tryFind p b
            | CellEmpty -> None

        tryFind p this

    member this.Head = 
      match LazyList.getCell this with
      | CellCons(a,_) -> a
      | CellEmpty -> invalidArg "s" "the list is empty"
      
    member this.TryHead = 
      match LazyList.getCell this with
      | CellCons(a,_) -> Some a
      | CellEmpty -> None

    member this.IsEmpty =
      match LazyList.getCell this with
      | CellCons _ -> false
      | CellEmpty -> true

    member this.Iter f =
        let rec iter f s = 
          match LazyList.getCell s with
          | CellEmpty -> ()
          | CellCons(h,t) -> f h; iter f t

        iter f this

    member this.Length() = 
        let rec lengthAux n s = 
          match LazyList.getCell s with
          | CellEmpty -> n
          | CellCons(_,b) -> lengthAux (n+1) b

        lengthAux 0 this

    member this.Map f = 
        let rec map f s = 
          LazyList.lzy(fun () ->  
            match LazyList.getCell s with
            | CellEmpty -> CellEmpty
            | CellCons(a,b) -> LazyList.consc (f a) (map f b))
            
        map f this

    member this.Map2 f (l : LazyList<'T2>) = 
        let rec map2 f s1 s2 =  
          LazyList.lzy(fun () -> 
            match LazyList.getCell s1, LazyList.getCell s2  with
            | CellCons(a1,b1),CellCons(a2,b2) -> LazyList.consc (f a1 a2) (map2 f b1 b2)
            | _ -> CellEmpty)

        map2 f this l

    member this.MapAccum f s =
        let rec loop s l cont =
            match  LazyList.getCell l with
            | CellEmpty -> cont (s, { status = Value CellEmpty })
            | CellCons(x,xs) ->
                let s, y = f s x
                loop s xs (fun (s,ys) -> cont (s, ys.Cons y))
        loop s this id

    member this.Scan f acc =
        let rec scan f acc s1 = 
          LazyList.lzy(fun () -> 
            match LazyList.getCell s1 with
            | CellCons(a,b) -> let acc' = f acc a in LazyList.consc acc (scan f acc' b)
            | CellEmpty -> LazyList.consc acc { status = Value CellEmpty })

        scan f acc this

    member this.Skip n = 

        let rec skipc n s =
          if n = 0 then LazyList.force s 
          else  
            match LazyList.getCell s with
            | CellCons(_,s) -> skipc (n-1) s
            | CellEmpty -> invalidArg "n" "not enough items in the list"

        let rec skip n s = 
          LazyList.lzy(fun () -> 
            if n < 0 then invalidArg "n" "the value must not be negative"
            else skipc n s)

        skip n this

    member this.TrySkip n =

        let rec skipcOpt n s =
          if n = 0 then Some s
          else  
            match LazyList.getCell s with
            | CellCons(_,s) -> match (skipcOpt (n-1) s) with Some x -> Some x | None -> None
            | CellEmpty -> None

        let rec trySkip n s = 
            if n < 0 then None
            else skipcOpt n s

        trySkip n this

    member this.Tail = 
      match LazyList.getCell this with
      | CellCons(_,b) -> b
      | CellEmpty -> invalidArg "s" "the list is empty"

    member this.TryTail = 
      match LazyList.getCell this with
      | CellCons(_,b) -> Some b
      | CellEmpty -> None

    member this.Take n = 
        let rec take n s = 
          LazyList.lzy(fun () -> 
            if n < 0 then invalidArg "n" "the number must not be negative"
            elif n = 0 then CellEmpty 
            else
              match LazyList.getCell s with
              | CellCons(a,s) -> LazyList.consc a (take (n-1) s)
              | CellEmpty -> invalidArg "n" "not enough items in the list" )

        take n this

    member this.TryTake n =
        let rec tryTake n s = 
            if n < 0 then None
            elif n = 0 then Some { status = Value CellEmpty }
            else
                match LazyList.getCell s with
                (* cannot build in naive, try again insandbox *)
                //| CellCons(a,s) -> Some (LazyList.consDelayed a ( fun () -> match (tryTake (n-1) s) with Some x -> x | None -> { status = Value CellEmpty } ) )
                | CellCons(a,s) -> Some (LazyList.lzy(fun () -> (LazyList.consc a (LazyList.lzy(fun () ->  (LazyList.force (( fun () -> match (tryTake (n-1) s) with Some x -> x | None -> { status = Value CellEmpty } )()))))))  )
                | CellEmpty -> None

        tryTake n this

    member this.ToArray() = Array.ofList (this.ToList())

    member this.ToList() = 
      let rec loop s acc = 
          match LazyList.getCell s with
          | CellEmpty -> List.rev acc
          | CellCons(h,t) -> loop t (h::acc)
      loop this []

    member this.ToSeq = (this :> IEnumerable<_>)

    member this.Uncons = 
        match LazyList.force this with 
        | CellCons (a,b) -> a,b
        | CellEmpty -> invalidArg "x" "the list does not contain head and tail"

    member this.TryUncons = match LazyList.force this with CellCons (a,b) -> Some(a,b) | CellEmpty -> None

    member this.Zip (l : LazyList<'T2>) =
        let rec zip s1 s2 = 
          LazyList.lzy(fun () -> 
            match LazyList.getCell s1, LazyList.getCell s2  with
            | CellCons(a1,b1),CellCons(a2,b2) -> LazyList.consc (a1,a2) (zip b1 b2)
            | _ -> CellEmpty)

        zip this l

    member s.GetEnumeratorImpl() = 
        let getCell (x : LazyList<'T>) = x.Value
        let toSeq s = Seq.unfold (fun ll -> match getCell ll with CellEmpty -> None | CellCons(a,b) -> Some(a,b)) s 
        (toSeq s).GetEnumerator()
            
    interface IEnumerable<'T> with
        member s.GetEnumerator() = s.GetEnumeratorImpl()

    interface System.Collections.IEnumerable with
        override s.GetEnumerator() = (s.GetEnumeratorImpl() :> System.Collections.IEnumerator)

and 
    [<NoEquality; NoComparison>]
    LazyCellStatus<'T> =
    | Delayed of (unit -> LazyListCell<'T> )
    | Value of LazyListCell<'T> 
    | Exception of System.Exception


and 
    [<NoEquality; NoComparison>]
    LazyListCell<'T> = 
    | CellCons of 'T * LazyList<'T> 
    | CellEmpty

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList = 

    let notlazy v = { status = Value v }
    
    type EmptyValue<'T>() = 
        static let value : LazyList<'T> = notlazy CellEmpty
        static member Value : LazyList<'T> = value
        
    [<NoEquality; NoComparison>]
    type LazyItem<'T> = Cons of 'T * LazyList<'T> | Empty
    type 'T item = 'T LazyItem

    let empty<'T> : LazyList<'T> = EmptyValue<'T>.Value

    let cons x (l : 'a LazyList) = l.Cons x

    let consDelayed x l = LazyList.lzy(fun () -> (LazyList.consc x (LazyList.lzy(fun () ->  (LazyList.force (l()))))))

    let uncons (x : LazyList<'T>) = x.Uncons

    let tryUncons (x : LazyList<'T>) = x.TryUncons

    let rec unfold f z = 
      LazyList.lzy(fun () -> 
          match f z with
          | None       -> CellEmpty
          | Some (x,z) -> CellCons (x,unfold f z))

    let append (l1 : LazyList<'T>)  (l2 : LazyList<'T>) = l1.Append l2

    let delayed f = LazyList.lzy(fun () ->  (LazyList.getCell (f())))
    
    let repeat x = 
      let rec s = cons x (delayed (fun () -> s)) in s

    let map f (x : LazyList<'T>) = x.Map f

    let map2 f (x1 : LazyList<'T1>) (x2 : LazyList<'T2>) = x1.Map2 f x2

    let zip (x1 : LazyList<'T1>) (x2 : LazyList<'T2>) = x1.Zip x2

    let rec concat s1 = 

        let rec append l1  l2 = LazyList.lzy(fun () ->  (appendc l1 l2))
        and appendc l1 l2 =
          match LazyList.getCell l1 with
          | CellEmpty -> LazyList.force l2
          | CellCons(a,b) -> LazyList.consc a (append b l2)

        LazyList.lzy(fun () -> 
        match LazyList.getCell s1 with
        | CellCons(a,b) -> appendc a (concat b)
        | CellEmpty -> CellEmpty)
      
    let filter p (x : LazyList<'T>) = x.Filter p
      
    let tryFind p (x : LazyList<'T>) = x.TryFind p

    let find p (x : LazyList<'T>) = x.Find p

    let scan f acc (x : LazyList<'T>) = x.Scan f acc

    let head (s : 'a LazyList) = s.Head

    let tryHead (x : LazyList<'T>) = x.TryHead

    let tail (x : LazyList<'T>) = x.Tail

    let tryTail (x : LazyList<'T>) = x.TryTail

    let isEmpty (x : LazyList<'T>) = x.IsEmpty

    let take n (x : LazyList<'T>) = x.Take n
    
    let rec tryTake n (x : LazyList<'T>) = x.TryTake n

    let skip n (x : LazyList<'T>) = x.Skip n

    let rec trySkip n (x : LazyList<'T>) = x.TrySkip n

    let mapAccum f s (x : LazyList<'T>) = x.MapAccum f s

    let rec ofList l = 
      LazyList.lzy(fun () -> 
        match l with [] -> CellEmpty | h :: t -> LazyList.consc h (ofList t))
      
    let toList (x : LazyList<'T>) = x.ToList()
      
    let rec iter f (x : LazyList<'T>) = x.Iter f 
      
    let rec copyFrom i a = 
      LazyList.lzy(fun () -> 
        if i >= Array.length a then CellEmpty 
        else LazyList.consc a.[i] (copyFrom (i+1) a))
      
    let copyTo (arr: _[]) (x : LazyList<'T>) i = x.CopyTo arr i

    let ofArray a = copyFrom 0 a

    let toArray (x : LazyList<'T>) = x.ToArray()

    let length (x : LazyList<'T>) = x.Length()

    let toSeq (x: LazyList<'T>) = x.ToSeq

    // Note: this doesn't dispose of the IEnumerator if the iteration is not run to the end
    let rec ofFreshIEnumerator (e : IEnumerator<_>) = 
      LazyList.lzy(fun () -> 
        if e.MoveNext() then 
          LazyList.consc e.Current (ofFreshIEnumerator e)
        else 
           e.Dispose()
           CellEmpty)
      
    let ofSeq (c : IEnumerable<_>) =
      ofFreshIEnumerator (c.GetEnumerator()) 
      
    let (|Cons|Nil|) l = match LazyList.getCell l with CellCons(a,b) -> Cons(a,b) | CellEmpty -> Nil