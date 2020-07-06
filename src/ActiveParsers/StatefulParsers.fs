namespace ActiveParsers
open System
open Stream
open System.Collections.Generic

[<StructuralEquality; StructuralComparison>]
[<Struct>] 
type Reply<'a> = 
  { 
    State : 'a
    Ok : bool
  }

type StatefulParser<'a,'t> = 'a*IStream<'t> -> Reply<'a>*IStream<'t>