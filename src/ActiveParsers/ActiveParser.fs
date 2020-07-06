namespace ActiveParsers
open System
open Stream
open System.Collections.Generic

type ActiveParser<'a,'t> = IStream<'t> -> ('a * IStream<'t>) option

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type private Unfold<'a> = 
  | Continue 
  | ContinueWith of ContinueWith : 'a
  | Break
  | BreakWith of BreakWith : 'a

  with 
    static member Unfold (fn : IStream<'t> -> Unfold<'a> * IStream<'t>) =
      fun stream ->
        let mutable loop = true
        let mutable state = []
        let mutable s = stream
        while loop do
            match fn s with
            | Continue,stream -> 
                s <- stream
            | ContinueWith(item),stream ->
                state <- List.append state [item]
                s <- stream
            | Break,stream -> 
                loop <- false
                s <- stream
            | BreakWith(item),stream ->
                loop <- false
                state <- List.append state [item]
                s <- stream
        Some(state, s)

    static member Lift (p : ActiveParser<'a,'t>) : IStream<'t> -> Unfold<'a> * IStream<'t> =
      fun stream -> 
        match p stream with
        | Some(x,next) -> ContinueWith(x),next
        | None -> Unfold<'a>.Break,stream

module ParserBuilder =
  [<Sealed>]
  type ParserBuilder() = 
    member inline this.Return(x : 'a) : ActiveParser<'a, 's> = fun stream -> Some(x, stream)
    member inline this.ReturnFrom(x : ActiveParser<'a, 's>) : ActiveParser<'a, 's> = x

    member inline this.Bind
        (m : ActiveParser<'a, 's>,
         f : 'a -> ActiveParser<'b, 's>)
        : ActiveParser<'b, 's> =
        fun stream ->
        match m stream with
        | Some(x, rest) -> (f x) rest
        | None -> None

    member inline this.Zero() : ActiveParser<unit, 's> = fun stream -> Some((),stream)
    //failwith "empty else branch in monadic-style parser"

    // member inline this.Combine
    //     (m : ActiveParser<'a, 's>,
    //      m2 : ActiveParser<'b, 's>)
    //     : ActiveParser<'b, 's> =
    //   this.Bind(m,fun _ -> m2)

    member inline this.Combine
      (m : ActiveParser< ^a, 's>,
      m2 : ActiveParser< ^b, 's>)
      : ActiveParser< ^b, 's> when ^a :> unit =
      this.Bind(m,fun _ -> m2)

    member inline this.Combine
      (m : ActiveParser< ^a, 's>,
      m2 : ActiveParser< ^b, 's>)
      : ActiveParser< ^a, 's> when ^b :> unit =
      this.Bind(m,fun a -> this.Bind(m2,fun _ -> this.Return a))

    member inline this.Combine
      (m : ActiveParser< ^a, 's>,
      m2 : ActiveParser< ^b, 's>)
      : ActiveParser< ^a * ^b, 's>  =
      this.Bind(m,fun a -> this.Bind(m2,fun b -> this.Return (a,b)))

    member inline this.Delay(f : unit -> 'a) : unit -> 'a = f
    member inline this.Run(f : unit -> 'a) : 'a = f ()

    member inline this.TryWith
        (m : ActiveParser<'a, 's>,
         h : exn -> ActiveParser<'a, 's>)
        : ActiveParser<'a, 's> =
      try this.ReturnFrom(m) with e -> h e

    member inline this.TryFinally
        (m : ActiveParser<'a, 's>,
         compensation : unit -> unit)
        : ActiveParser<'a, 's> =
      try this.ReturnFrom(m) finally compensation ()

    member inline this.Using
        (res : #IDisposable,
         body : #IDisposable -> ActiveParser<'a, 's>)
        : ActiveParser<'a, 's> =
      this.TryFinally(body res, fun () ->
        match res with null -> () | disp -> disp.Dispose())

    member this.While
        (guard : unit -> bool,
         f : unit -> ActiveParser<'a, 's>)
        : ActiveParser<'b, 's> =
      if not (guard()) then this.Zero() else
      this.Bind(f(), fun _ -> this.While(guard, f))

    member inline this.For
        (sequence : seq<'a>,
         body : 'a -> ActiveParser<'b, 's>)
        : ActiveParser<'c, 's> =
      this.Using(sequence.GetEnumerator(), fun enum ->
        this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    // [<CustomOperation("loop",IsLikeZip=true)>]
    // member inline _.Loop (body : 'a -> ActiveParser<'b, 's>) : ActiveParser<'c, 's> =
    //   body

  

  let parser = ParserBuilder()

module Combinators =
    open ParserBuilder

    let inline Return (x: 'a): ActiveParser<'a,'t> =
      fun stream -> Some(x, stream)

    let inline Head (stream : IStream<'t>) : ('t * IStream<'t>) option =
          stream.Head() |> Option.bind (fun x -> Some(x,stream.Consume(1)))  

    let inline Bind (p: ActiveParser<'a,'t>) (f: 'a -> ActiveParser<'b,'t>) : ActiveParser<'b,'t> =
      fun stream ->
        match p stream with
        | Some(x, rest) -> (f x) rest
        | None -> None

    let inline CombineRight (p: ActiveParser<'a,'t>) (p2: ActiveParser<'b,'t>) : ActiveParser<'b,'t> =
      fun stream ->
        match p stream with
        | Some(_,rest) -> p2 rest
        | None -> None

    let inline Either (p1: ActiveParser<'a,'t>) (p2: ActiveParser<'a,'t>) : ActiveParser<'a,'t> =
      fun stream ->
        match p1 stream with
        | None -> p2 stream
        | res -> res

    let (>>=) = Bind

    let (>>%) p x : ActiveParser<'b,'t> =
          p >>= (fun _ -> Return x)

    let (>>.) p1 p2 : ActiveParser<'b,'t> =
        p1 >>= (fun _ -> p2)

    let (.>>) p1 p2 : ActiveParser<'a,'t> =
        p1 >>= (fun x -> p2 >>% x)

    let (.>>.) p1 p2: ActiveParser<'a*'b,'t> =
        p1 >>= (fun x -> p2 >>= (fun y -> Return (x, y)))

    let (<|>) = Either

    let cons (p: ActiveParser<'a,'t>) (pM: ActiveParser<'a list,'t>) = 
        parser {
          let! r = p
          let! rs = pM
          return r::rs
        } 

    let unfold (p: ActiveParser<'a,'t>) (stream : IStream<'t>) = 
      Seq.unfold p stream

    /// Combines (0 or more) applications of parser p  
    let many (p: ActiveParser<'a,'t>) = Unfold<'a>.Unfold (Unfold<'a>.Lift p)

    /// Combines (1 or more) applications of parser p
    let many1 p = cons p (many p)

    let repeatN n (p: ActiveParser<'a,'t>) =
      let rec repeat n values stream = 
        match n, (p stream) with
        | 1, Some(v,next) -> Some(v::values,next)
        | 0, _ -> Some([],stream)
        | x, Some(v,next) when x > 0 -> repeat n (v::values) next
        | _, _ -> None
      repeat n []

    /// Combines 1 or more applications of parser p separated by parser sep
    let sepby1 p sep = cons p (many (sep >>. p))

    /// Combines 0 or more applications of parser p separated by parser sep
    let sepby p sep = sepby1 p sep<|> Return []

    /// Chain 0 or more applications of parser p separated by applications of parser op
    let rec chainl p op a = (chainl1  p op) <|> Return a
    /// Chain 1 or more applications of parser p separated by applications of parser op
    and chainl1 p op =  
        let rec rest r = 
            parser {
                let! f = op
                let! r' = p
                return! rest (f r r')
            } <|> Return r
    
        parser { let! a = p in return! rest a }
       
    let inline skip (p: ActiveParser<'a,'t>) = 
      fun stream -> match p stream with Some(_,s) -> Some((),s) | None -> None


    //choose, maybe, pipe..., between, followedBy, notFollowedBy, manyTill, skip, skipManyTill, chairr, chainr1 



module StringParsers =
    open Combinators

    let Char (c : char) : ActiveParser<char,char> =
      fun stream ->
          match stream.Head() with
          | Some(chr) when chr = c -> Some(chr, stream.Consume(1))
          | _ -> None
    
    let PChar (c : char) : ActiveParser<unit,char> =
      fun stream ->
          match stream.Head() with
          | Some(chr) when chr = c -> Some((), stream.Consume(1))
          | _ -> None

    let PString (s : string) : ActiveParser<unit,char> =
      fun stream -> 
        match stream.Read(s.Length) with
        | Some(str) when String(str) = s -> Some((),stream.Consume(s.Length))
        | _ -> None    

    let SplitWith (s : string) : ActiveParser<char [],char> =
      fun stream ->
        let pos_init = stream.Position()
        match stream.SubSearch ((=),s.ToCharArray()) with
        | Some(position) -> 
          match stream.Read (position - stream.Position()) with
          | Some(chars) -> 
            Some(chars,stream.Consume(position + s.Length - pos_init))
          | None -> Some([||],stream.Consume(s.Length))
        | _ -> None

    let Between (s : string) = PString s >>. SplitWith s


module StringActivePatterns =     
    let (|PChar|_|) (char : char) = StringParsers.PChar char

    let (|PString|_|) (pattern : string) = StringParsers.PString pattern

    let (|Between|_|) = StringParsers.Between 

    let (|Char|_|) (input : IStream<char>) = input.Head() |> Option.map (fun x -> x,input.Consume(1))

/// <summary> Initial module </summary>
module Say =
    /// <summary>
    /// Adds two integers <paramref name="a"/> and <paramref name="b"/> and returns the result.
    /// </summary>
    ///
    /// <remarks>
    /// This usually contains some really important information that you'll miss if you don't read the docs.
    /// </remarks>
    ///
    /// <param name="a">An integer.</param>
    /// <param name="b">An integer.</param>
    ///
    /// <returns>
    /// The sum of two integers.
    /// </returns>
    ///
    /// <exceptions cref="M:System.OverflowException">Thrown when one parameter is max
    /// and the other is greater than 0.</exceptions>
    let add a b =
        a + b


