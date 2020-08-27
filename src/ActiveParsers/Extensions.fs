namespace ActiveParsers

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open ActiveParsers
open TypeEquality


#nowarn "0042" // retype

[<AutoOpen>]
module internal Prelude =
    let inline flip f x y = f y x
    let inline const' k _ = k
    let inline tupleToOption x = match x with true, value -> Some value | _ -> None
    let inline retype (x: 'T) : 'U = (# "" x: 'U #)
    let inline opaqueId x = Unchecked.defaultof<_>; x

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end                  
                    
[<Extension; Sealed>]
type Plus =     
    inherit Default1
//        static member inline ``+`` (x: 'Plus             , y: 'Plus             ,             _mthd: Default2) = (^Plus :  (static member (<|>) : _*_ -> _) x, y) : ^Plus
//
    static member inline ``+`` (x: 'Plus             , y: 'Plus             , [<Optional>]_mthd: Default1) = x + y : ^Plus
//        static member inline ``+`` (_: ^t when ^t: null and ^t: struct, _: ^t   , [<Optional>]_mthd: Default1) = id

    static member        ``+`` (x: list<_>           , y                    , [<Optional>]_mthd: Plus    ) = x @ y
    static member        ``+`` (x: array<_>          , y                    , [<Optional>]_mthd: Plus    ) = Array.append x y
    static member        ``+`` (()                   , ()                   , [<Optional>]_mthd: Plus    ) = ()
    static member        ``+`` (x: bool              , y: bool              , [<Optional>]_mthd: Plus    ) = x <> y
    static member        ``+`` (x: Set<_>            , y                    , [<Optional>]_mthd: Plus    ) = Set.union x y

    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)

type Plus with
    static member inline ``+`` (x: option<_>, y, [<Optional>]_mthd: Plus) =
                    match x, y with
                    | (Some a , Some b) -> Some (Plus.Invoke a b)
                    | (Some a , None  ) -> Some a
                    | (None   , Some b) -> Some b
                    | _                 -> None

module Extensions1 =
    
    let stringCharPath = Teq.path<string,char> (fun s -> if s.Length = 1 then s.Chars(0) else failwith "string to char") (fun c -> string c)
    let stringUnitPath = Teq.path<string,unit> (fun s -> failwith "string unit") (fun _ -> "")
    
    let optionUnitPath = Teq.path<option<char>,unit> (fun c -> if c.IsNone then () else failwith "option char to unit") (fun _ -> None)
    let optionStringPath = Teq.path<option<char>,string> (fun c -> if c.IsNone then "" else string c.Value) (fun s -> if s.Length = 1 then Some(s.Chars(0)) else if s.Length = 0 then None else failwith "string to char option")
    
    let charListStringPath = Teq.path<char list,string> (fun ls -> String.Concat ls) (fun s -> seq { for i in 0..(s.Length - 1) do yield s.Chars(i)} |> Seq.toList )
    let charSeqStringPath = Teq.path<char seq,string> (fun ls -> String.Concat ls) (fun s -> seq { for i in 0..(s.Length - 1) do yield s.Chars(i)} )
    
    let charArrayStringPath = Teq.path<char [],string> (fun ls -> String.Concat ls) (fun s -> seq { for i in 0..(s.Length - 1) do yield s.Chars(i)} |> Seq.toArray )
    
    let paths : Dictionary<Type * Type,obj> = new Dictionary<Type * Type,obj>([
        KeyValuePair((typeof<string>,typeof<char>),stringCharPath :> obj)
    ])
    
    let tryPath<'a, 'b> : Teq<'a, 'b> option =
        if paths.ContainsKey(typeof<'a>, typeof<'b>) then
            paths.Item(typeof<'a>, typeof<'b>) |> unbox |> Some
        else
            None

module Extensions =

         
    let mapV (p1 : ActiveParser<'a,char>) (map : 'a -> 'b) : ActiveParser<'b,char> =
            fun stream ->
                match p1 stream with
                | None -> None
                | Some(v,next) -> Some(map v, next)         
    
    type Uplift = Uplift with
        static member inline ($) (Uplift, x) = mapV x (fun () -> "")
        static member inline ($) (Uplift, x) = mapV x (fun chr -> string chr)
        static member inline ($) (Uplift, x) = mapV x (List.map string >> String.Concat)
        static member inline ($) (Uplift, x) = mapV x (fun (x : string list) -> String.Concat x)
        static member inline ($) (Uplift, x) = mapV x (fun opt -> match opt with Some(x) -> x | None -> "")
        static member inline ($) (Uplift, x : ActiveParser<string,char>) = x
        static member inline ($) (Uplift, x : ActiveParser<char,char>) = x
    let inline uplift (x : ActiveParser< ^A,char>) = (($) Uplift) x
    
    let inline  (<|>) (p1 : ActiveParser< ^A,char>) (p2 : ActiveParser< ^B,char>)  =
            fun stream ->
                match (uplift p1) stream with
                | None ->
                    match (uplift p2) stream with
                    | None -> None
                    | Some(v2,next) -> Some(v2,next) 
                | Some(v1,next) -> Some(v1,next)
                
    let inline  (<||>) (p1 : ActiveParser<char,char>) (p2 : ActiveParser<char,char>)  =
            fun stream ->
                match p1 stream with
                | None ->
                    match p2 stream with
                    | None -> None
                    | Some(v2,next) -> Some(v2,next) 
                | Some(v1,next) -> Some(v1,next)
                
    let inline (>>) (p1 : ActiveParser< ^A,char>) (p2 : ActiveParser< ^B,char>)  =
            fun stream ->
                match (uplift p1) stream with
                | None -> None
                | Some(v1,next) ->
                    match (uplift p2) next with
                    | None -> None
                    | Some(v2,next2) -> Some(v1 + v2,next2)
                    
    let inline (!?) (p1 : ActiveParser< ^A,char>)  =
        fun stream ->
            match (uplift p1) stream with
            | None -> Some(None,stream)
            | Some(v1,next) -> Some(Some(v1),next)

    let inline until p1 p2 =
        (uplift (Combinators.until (uplift p1) (uplift p2)))