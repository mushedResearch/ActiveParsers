namespace ActiveParsers.ANTLR

open System
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

[<RequireQualifiedAccess>]
module Generator =
        type ANTLRAttribute(filePath : string) =
            inherit Attribute()
            let mutable _filePath = filePath
            member this.FilePath = filePath
            new () = ANTLRAttribute(null : string)
            //new (``type``: Type) = ANTLRAttribute(``type``.Name)

[<MyriadGenerator("ANTLR")>]
type ANTLRGenerator() =

    interface IMyriadGenerator with
        member __.Generate(namespace', ast: ParsedInput) =
            let namespaceAndRecords = Ast.extractTypeDefn
            let modules : SynAttribute*SynTypeDefn =
                namespaceAndRecords
                |> List.collect (fun (ns, dus) ->
                                    dus
                                    |> List.filter (Ast.hasAttribute<Generator.ANTLRAttribute>)
                                    |> List.map (fun x -> x |> Ast.getAttribute<Generator.ANTLRAttribute> |> Option.get, x))
            for attribute,expr in modules do
                let filePath =
                    match attribute.ArgExpr with
                    | SynExpr.Const(SynConst.String(path,_),_) -> path
                //load antlr file
                //generate antlr parsers

            //use nested types expression that have the attribute and generate a parser for each from the antlr parsers                 
                   
            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = [] }

            namespaceOrModule