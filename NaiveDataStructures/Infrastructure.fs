namespace NaiveDataStructures

//type IHeap<'a when 'a : comparison> =
//    inherit System.Collections.IEnumerable
//    inherit System.Collections.Generic.IEnumerable<'a>
//
//    ///returns the count of elememts
//    abstract member Count : int with get
//
//    ///returns the min or max element
//    abstract member Head : 'a with get
//
//    ///returns option first min or max element
//    abstract member TryGetHead : 'a option with get
//
//    ///returns true if the heap has no elements
//    abstract member IsEmpty : bool with get
//
//    ///returns true if the heap has max element at head
//    abstract member IsMaximalist : bool with get
//
//    ///returns the count of elememts
//    abstract member Length : int with get
//
//type IHeap<'c, 'a when 'c :> IHeap<'c, 'a> and 'a : comparison> =
//    inherit IHeap<'a>
//
//    ///returns a new heap with the element inserted
//    abstract member Insert : 'a -> 'c
//
//    ///returns heap from merging two heaps, both must have same isMaximalist
//    abstract member Merge : 'c -> 'c
//
//    ///returns heap option from merging two heaps
//    abstract member TryMerge : 'c -> 'c option
//
//    ///returns a new heap of the elements trailing the head
//    abstract member Tail : 'c with get
//
//    ///returns option heap of the elements trailing the head
//    abstract member TryGetTail : 'c option with get
//
//    ///returns the head element and tail
//    abstract member Uncons : 'a * 'c with get
//
//    ///returns option head element and tail
//    abstract member TryUncons : ('a * 'c) option with get

module LazyListHelpr =

    let rec private revAux r acc =
        match r with
        | LazyList.Nil -> acc
        | LazyList.Cons(hd, tl) -> revAux tl (LazyList.cons hd acc)

    let lLrev r =
        revAux r LazyList.empty

    let rec lLdrop n xs =
        if n < 0 then invalidArg "n" "n was negative"
        elif n > 0 then
            match xs with
            | LazyList.Cons(x, xs') -> lLdrop (n-1) xs'
            | _ -> LazyList.empty
        else
            xs

    let lLsplit (ll:LazyList<'a>) n  =
        let rec loop z (leftL:'a List) (ll':LazyList<'a>) = 
            match z with
            | 0 -> leftL, (LazyList.tail ll')
            | _ -> loop (z - 1)  ((LazyList.head ll')::leftL) (LazyList.tail ll')
        loop n List.empty ll

