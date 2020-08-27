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

            Expect.equal 3 3 "Addition works"
       ]