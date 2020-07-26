module Tests

open System
open Expecto
open ActiveParsers
open ActiveParsers.Combinators
open ActiveParsers.ParserBuilder
open ActiveParsers.StringParsers

[<Tests>]
let tests =
    testList "samples" [
        testCase "Add two integers" <| fun _ ->
            let p1 : ActiveParser<unit,char> = parser {
                //let! q = 
                for x in ['a' .. 'c'] do PChar x

            }

            let subject = Say.add 1 2
            Expect.equal subject 3 "Addition works"
       ]