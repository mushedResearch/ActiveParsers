namespace ActiveParsers

open System
open Stream

module StringParsers =
    open Combinators
    type StringParser = ActiveParser<unit,char>
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
        
    let String (s : string) : ActiveParser<string,char> =
      fun stream -> 
        match stream.Read(s.Length) with
        | Some(str) when String(str) = s -> Some(s,stream.Consume(s.Length))
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
    
    let Range (c1 : char) (c2 : char) : ActiveParser<char,char> =
        fun stream ->
          match stream.Head() with
          | Some(chr) when chr >= c1 && chr <= c2 -> Some(chr, stream.Consume(1))
          | _ -> None


module StringActivePatterns =     
    let (|PChar|_|) (char : char) = StringParsers.PChar char

    let (|PString|_|) (pattern : string) = StringParsers.PString pattern

    let (|Between|_|) = StringParsers.Between 

    let (|Char|_|) (input : IStream<char>) = input.Head() |> Option.map (fun x -> x,input.Consume(1))
    
    let (|Range|_|) (c1 : char) (c2 : char) = StringParsers.Range c1 c2
