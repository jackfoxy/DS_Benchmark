// bootstrapped queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/bootstrapped-queue
namespace NaiveDataStructures

//open FSharpx

type Queue0<'a> = {
    FrontAndSuspensionsLength : int
    Front : list<'a>
    Suspensions : BootstrapQueue0<Lazy<list<'a>>>
    RBackLength : int
    RBack : list<'a>
} with
    static member create lenfm f m lenr r = {
        FrontAndSuspensionsLength = lenfm
        Front = f
        Suspensions = m
        RBackLength = lenr
        RBack = r
    }  

and BootstrapQueue0<'a> = Empty | NonEmpty of Queue0<'a> with
    // polymorphic recursion cannot be achieved through let-bound functions
    // hence we use static member methods 

    static member checkQ (q:Queue0<'a>) =
        if q.RBackLength <= q.FrontAndSuspensionsLength then BootstrapQueue0.checkF q else
        let susp = BootstrapQueue0.enqueue (lazy List.rev q.RBack) q.Suspensions
        Queue0<'a>.create (q.FrontAndSuspensionsLength + q.RBackLength) q.Front susp 0 []
        |> BootstrapQueue0.checkF 

    static member checkF (q:Queue0<'a>) : BootstrapQueue0<'a> =
        match q.Front, q.Suspensions with
        | [], Empty -> Empty
        | [], m ->
            let f = BootstrapQueue0.peek m |> Lazy.force
            let susp = BootstrapQueue0.dequeue0 m
            NonEmpty <| Queue0<'a>.create q.FrontAndSuspensionsLength f susp q.RBackLength q.RBack
        | _ -> NonEmpty q

    static member enqueue (x:'a) : BootstrapQueue0<'a> -> BootstrapQueue0<'a> = function
        | Empty -> NonEmpty <| Queue0<'a>.create 1 [x] Empty 0 []
        | NonEmpty q ->
            let lenr = q.RBackLength+1
            let r = x::q.RBack
            Queue0<'a>.create q.FrontAndSuspensionsLength q.Front q.Suspensions lenr r
            |> BootstrapQueue0<'a>.checkQ 

    static member peek : BootstrapQueue0<'a> -> 'a = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q -> List.head q.Front

    static member tryPeek : BootstrapQueue0<'a> -> 'a option = function
        | Empty -> None
        | NonEmpty q -> Some (List.head q.Front)

    static member dequeue0 : BootstrapQueue0<'a> -> BootstrapQueue0<'a> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            Queue0<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrapQueue0<'a>.checkQ

    static member dequeue : BootstrapQueue0<'a> -> 'a * BootstrapQueue0<'a> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            (List.head q.Front), 
            (Queue0<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack 
            |> BootstrapQueue0<'a>.checkQ)

    static member tryDequeue0 : BootstrapQueue0<'a> -> BootstrapQueue0<'a> option = function
        | Empty -> None
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            Queue0<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrapQueue0<'a>.checkQ
            |> Some

    static member length : BootstrapQueue0<'a> -> int = function
        | Empty -> 0
        | NonEmpty q -> q.FrontAndSuspensionsLength + q.RBackLength

    static member ofList (l:List<'a>) : BootstrapQueue0<'a> = 
        let b0 = BootstrapQueue0.Empty
        Queue0<'a>.create (l.Length) l b0 0 [] |> NonEmpty

module Queue0 =
    ///O(1). Returns queue of no elements.
    let empty = Empty

    ///O(1). Returns true if the queue has no elements
    let isEmpty = function Empty -> true | _ -> false

    ///O(log* n). Returns a new queue with the element added to the end.
    let inline enqueue x queue = BootstrapQueue0.enqueue x queue

    ///O(1), worst case. Returns the first element.
    let inline head queue = BootstrapQueue0<'a>.peek queue

    ///O(1), worst case.  Returns option first element.
    let inline tryGetHead queue = BootstrapQueue0<'a>.tryPeek queue

    ///O(log* n), worst case. Returns a new queue of the elements trailing the first element.
    let inline dequeue queue : BootstrapQueue0<'a> = BootstrapQueue0<'a>.dequeue0 queue

    let inline dequeue2 queue : 'a * BootstrapQueue0<'a> = BootstrapQueue0<'a>.dequeue queue

    ///O(log* n), worst case. Returns option queue of the elements trailing the first element.
    let inline tryDequeue queue = BootstrapQueue0<'a>.tryDequeue0 queue

    ///O(1). Returns the count of elememts.
    let inline length queue = BootstrapQueue0<'a>.length queue

    ///O(1). Returns a queue of the list.
    let inline ofList list = BootstrapQueue0<'a>.ofList list