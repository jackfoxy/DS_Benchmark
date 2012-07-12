namespace NaiveDataStructures

type 'a Stack =
    | EmptyStack 
    | Stack of 'a * 'a Stack 
    member x.isEmpty =
        match x with
        | EmptyStack -> true
        | Stack(hd, tl) -> false
    member x.peek =  
        match x with
        | EmptyStack -> failwith "Empty stack"
        | Stack(hd, tl) -> hd
    member x.pop = 
        match x with
        | EmptyStack -> failwith "Empty stack"
        | Stack(hd, tl) -> hd, tl
    member x.push hd = Stack(hd, x) 

module Stack =

    let rec contains x = function
        | EmptyStack -> false
        | Stack(hd, tl) -> hd = x || contains x tl
 
    let rec fold f seed = function
        | EmptyStack -> seed
        | Stack(hd, tl) -> fold f (f seed hd) tl

    let rec lookup index s =
        match index, s with
        | index, EmptyStack -> failwith "Index out of range"
        | 0, Stack(hd, tl) -> hd
        | n, Stack(hd, tl) -> lookup (index - 1) tl

    let rec map f = function
        | EmptyStack -> EmptyStack
        | Stack(hd, tl) -> Stack(f hd, map f tl)
 
    let ofArray (data:'a[]) =
        let rec loop (d:'a[]) (s:'a Stack) acc =
            match acc  with
            | _ when acc = d.Length -> s
            | _ -> loop d (s.push (d.[acc])) (acc + 1)
        loop data Stack.EmptyStack 0

    let ofList (data:'a list) =
        let rec loop (d:'a list) (s:'a Stack)=
            match d  with
            | hd::tl -> loop tl (s.push hd)
            | [] -> s
        loop data Stack.EmptyStack

    let ofSeq (data:'a seq) =
        let rec loop (d:'a seq) (s:'a Stack) dCount acc =
            match acc  with
            | _ when acc = dCount -> s
            | _ -> loop d (s.push (Seq.nth acc d)) dCount (acc + 1)
        loop data Stack.EmptyStack (Seq.length data) 0

    let rec rev s =
        let rec loop acc = function
            | EmptyStack -> acc
            | Stack(hd, tl) -> loop (Stack(hd, acc)) tl
        loop EmptyStack s

    let toList s =
        let rec loop (s2:'a Stack) (l:'a list) =
            match s2 with
            | EmptyStack -> l
            | Stack(hd, tl) -> loop tl (hd::l)
        loop s List.Empty

    let rec update index value s =
        match index, s with
        | index, EmptyStack -> failwith "Index out of range"
        | 0, Stack(hd, tl) -> Stack(value, tl)
        | n, Stack(hd, tl) -> Stack(hd, update (index - 1) value tl)