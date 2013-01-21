// bootstrapped queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/bootstrapped-queue
namespace NaiveDataStructures

type NonEmptyBootstrappedQueue1<'a> = {
    LenFM : int
    F : list<'a>
    M : BootstrappedQueue1<Lazy<list<'a>>>
    LenR : int
    R : list<'a>
}  

and BootstrappedQueue1<'a> = Empty | NonEmpty of NonEmptyBootstrappedQueue1<'a> with

    static member checkQ (q:NonEmptyBootstrappedQueue1<'a>) =
        if q.LenR <= q.LenFM then BootstrappedQueue1.checkF q 
        else
            { LenFM = (q.LenFM + q.LenR); F = q.F; M = BootstrappedQueue1.snoc (lazy List.rev q.R) q.M; LenR = 0; R = [] }
            |> BootstrappedQueue1.checkF 

    static member checkF (q:NonEmptyBootstrappedQueue1<'a>) : BootstrappedQueue1<'a> =
        match q.F, q.M with
        | [], Empty -> Empty
        | [], m ->
            { LenFM = q.LenFM; F = BootstrappedQueue1.head m |> Lazy.force; M = BootstrappedQueue1.tail m; LenR = q.LenR; R = q.R } 
            |> NonEmpty
        | _ -> NonEmpty q

    static member snoc (x:'a) : BootstrappedQueue1<'a> -> BootstrappedQueue1<'a> = function
        | Empty -> 
             NonEmpty { LenFM = 1; F = [x]; M = Empty; LenR = 0; R = [] } 
        | NonEmpty q ->
            if q.LenR < q.LenFM then 
                NonEmpty { LenFM = q.LenFM; F = q.F; M = q.M; LenR = q.LenR + 1; R = x::q.R } 
            else 
                let susp = BootstrappedQueue1.snoc (lazy List.rev (x::q.R)) q.M
                match q.F, susp with
                | [], Empty -> Empty
                | [], m ->
                    NonEmpty { LenFM = q.LenFM + q.LenR + 1; F = BootstrappedQueue1.head m |> Lazy.force; M = BootstrappedQueue1.tail m; LenR = 0; R = [] } 
                | _ -> NonEmpty { LenFM = q.LenFM + q.LenR + 1; F = q.F; M = susp; LenR = 0; R = [] }

    static member head : BootstrappedQueue1<'a> -> 'a = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q -> List.head q.F

    static member tryPeek : BootstrappedQueue1<'a> -> 'a option = function
        | Empty -> None
        | NonEmpty q -> Some (List.head q.F)

    static member tail : BootstrappedQueue1<'a> -> BootstrappedQueue1<'a> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            if q.LenR < q.LenFM then 
                BootstrappedQueue1.checkF { LenFM = q.LenFM - 1; F = List.tail q.F; M = q.M; LenR = q.LenR; R = q.R }
            else 
                { LenFM = q.LenFM + q.LenR - 1; F = List.tail q.F; M = BootstrappedQueue1.snoc (lazy List.rev q.R) q.M; LenR = 0; R = [] }
                |> BootstrappedQueue1.checkF 

    static member uncons : BootstrappedQueue1<'a> -> 'a * BootstrappedQueue1<'a> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            (List.head q.F), 
            ({ LenFM = q.LenFM - 1; F = List.tail q.F; M = q.M; LenR = q.LenR; R = q.R }
            |> BootstrappedQueue1<'a>.checkQ)

    static member tryTail : BootstrappedQueue1<'a> -> BootstrappedQueue1<'a> option = function
        | Empty -> None
        | NonEmpty q ->
            { LenFM = q.LenFM - 1; F = List.tail q.F; M = q.M; LenR = q.LenR; R = q.R }
            |> BootstrappedQueue1<'a>.checkQ
            |> Some

    static member length : BootstrappedQueue1<'a> -> int = function
        | Empty -> 0
        | NonEmpty q -> q.LenFM + q.LenR

    static member ofList (l:List<'a>) : BootstrappedQueue1<'a> = 
        { LenFM = l.Length; F = l; M = BootstrappedQueue1.Empty; LenR = 0; R = [] }
        |> NonEmpty

module BootstrappedQueue1 =
    ///O(1). Returns queue of no elements.
    let empty = Empty

    ///O(1). Returns true if the queue has no elements
    let isEmpty = function Empty -> true | _ -> false

    ///O(log* n). Returns a new queue with the element added to the end.
    let inline snoc x queue = BootstrappedQueue1.snoc x queue

    ///O(1), worst case. Returns the first element.
    let inline head queue = BootstrappedQueue1<'a>.head queue

    ///O(1), worst case.  Returns option first element.
    let inline tryGetHead queue = BootstrappedQueue1<'a>.tryPeek queue

    ///O(log* n), worst case. Returns a new queue of the elements trailing the first element.
    let inline dequeue queue : BootstrappedQueue1<'a> = BootstrappedQueue1<'a>.tail queue

    let inline uncons queue : 'a * BootstrappedQueue1<'a> = BootstrappedQueue1<'a>.uncons queue

    ///O(log* n), worst case. Returns option queue of the elements trailing the first element.
    //let inline tryGetTail queue = BootstrappedQueue1<'a>.tryGetTail queue

    ///O(1). Returns the count of elememts.
    let inline length queue = BootstrappedQueue1<'a>.length queue

    ///O(1). Returns a queue of the list.
    let inline ofList list = BootstrappedQueue1<'a>.ofList list