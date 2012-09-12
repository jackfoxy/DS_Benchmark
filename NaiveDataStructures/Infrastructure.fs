namespace NaiveDataStructures

type IDeque<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>
    
    ///returns a new deque with the element added to the beginning
    abstract member Cons : 'a -> IDeque<'a>

    ///returns the first element
    abstract member Head : unit -> 'a

    ///returns a new deque of the elements before the last element
    abstract member Init : unit -> IDeque<'a>

    ///returns true if the deque has no elements
    abstract member IsEmpty : unit -> bool

    ///returns the last element
    abstract member Last : unit -> 'a

    ///returns the count of elememts
    abstract member Length : unit -> int

    ///returns a new deque with the element added to the end
    abstract member Snoc : 'a -> IDeque<'a>

    ///returns a new deque of the elements trailing the first element
    abstract member Tail : unit -> IDeque<'a>

    ///returns the first element and tail
    abstract member Uncons : unit -> 'a * IDeque<'a>

    ///returns init and the last element
    abstract member Unsnoc : unit -> IDeque<'a> * 'a

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

