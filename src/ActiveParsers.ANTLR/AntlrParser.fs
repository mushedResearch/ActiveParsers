module ActiveParsers.ANTLR.AntlrParser

open ActiveParsers
open ActiveParsers.StringParsers
open ActiveParsers.Combinators

module LexParser =
    let InCharSet (ls : char list) : ActiveParser<char,char> =
        let chrSet = Set.ofList ls
        fun stream ->
            match stream.Head() with
            | Some(chr) when chrSet.Contains(chr) -> Some(chr,stream.Consume(1))
            | _ -> None
            
    let notInCharSet (ls : char list) : ActiveParser<char,char> =
        let chrSet = Set.ofList ls
        fun stream ->
            match stream.Head() with
            | Some(chr) when not <| chrSet.Contains(chr) -> Some(chr,stream.Consume(1))
            | _ -> None
            
    let EOF (stream : Stream.IStream<char>) =
        match stream.Head() with
        | None -> Some((),stream)
        | _ -> None
        
    let AnyChar (stream : Stream.IStream<char>) =
        match stream.Head() with
        | Some(chr) -> Some(chr,stream)
        | _ -> None

module rec LexBasic =
    open LexParser
    let Ws =
        Hws <|> Vws
    let Hws =
        InCharSet [' ';'\t']
        
    let Vws =
        InCharSet ['\r';'\n';'\f']
    //BlockComment
    //DocComment
    let LineComment =
        String "//" >>. many (notInCharSet ['\r';'\n'])
    let EscSeq =
        Esc >>. (InCharSet ['b';'t';'n';'r';'"';'\'';'\\'] <|> UnicodeEsc <|> AnyChar <|> EOF)
    let EscAny =
        Esc >>. many (AnyChar)
    //UnicodeEsc
    //DecimalNumeral
    //HexDigit
    //DecDigit
    //BoolLiteral
    //CharLiteral
    //SQuoteLiteral
    //DQuoteLiteral
    let USQuoteLiteral =
        SQuote >>. many (EscSeq <|> (notInCharSet ['\''; '\r'; '\n'; '\\']))
    
    let NameChar : ActiveParser<char,char> =
        NameStart
        <|> StringParsers.Range '0' '9'
        <|> Underscore
        <|> Char '\u00B7'
        <|> StringParsers.Range '\u0300' '\u036F'
        <|> StringParsers.Range '\u203F' '\u2040'
    
    let NameStart : ActiveParser<char,char> =
        StringParsers.Range 'A' 'Z'
        <|> StringParsers.Range 'a' 'z'
        <|> StringParsers.Range '\u00C0' '\u00D6'
        <|> StringParsers.Range '\u00D8' '\u00F6'
        <|> StringParsers.Range '\u00F8' '\u02FF'
        <|> StringParsers.Range '\u0370' '\u037D'
        <|> StringParsers.Range '\u037F' '\u1FFF'
        <|> StringParsers.Range '\u200C' '\u200D'
        <|> StringParsers.Range '\u2070' '\u218F'
        <|> StringParsers.Range '\u2C00' '\u2FEF'
        <|> StringParsers.Range '\u3001' '\uD7FF'
        <|> StringParsers.Range '\uF900' '\uFDCF'
        <|> StringParsers.Range '\uFDF0' '\uFFFD'
    
    let Int = String "int"
    let Esc = Char '\\'
    let Colon = Char ':'
    let DColon = String "::"
    let SQuote = Char '\''
    let DQuote = Char '"'
    let LParen = Char '('
    let RParen = Char ')'
    let LBrace = Char '{'
    let RBrace = Char '}'
    let LBrack = Char '['
    let RBrack  = Char ']'
    let RArrow = String "->"
    let Lt = Char '<'
    let Gt = Char '>'
    let Equal = Char '='
    let Question = Char '?'
    let Star = Char '*'
    let Plus = Char '+'
    let PlusAssign = String "+=" 
    let Underscore = Char '_'
    let Pipe = Char '|'
    let Dollar = Char '$'
    let Comma = Char ','
    let Semi = Char ';'
    let Dot  = Char '.'
    let Range = String ".."
    let At = Char '@'
    let Pound = Char '#'
    let Tilde = Char '~'
