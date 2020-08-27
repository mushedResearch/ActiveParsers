module ActiveParsers.ANTLR.AntlrParser

open System.Collections.Generic
open ActiveParsers
open ActiveParsers.StringParsers
open ActiveParsers.Combinators
open ActiveParsers.Extensions
open Stream




[<StructuralEquality; StructuralComparison>]
[<Struct>]
type AntlrToken<'a> =
    {
        Value : 'a option
        Begin : int
        Len : int
    }

[<StructuralEquality; StructuralComparison>]
type AntlrState<'a> =
    {
        mutable Channels : (string * AntlrToken<'a> list) list
        mutable DefaultChannel : AntlrToken<'a> list
    }

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type ANTLR<'a> =
  { Value : AntlrToken<'a>
    Channel : string option
    Continue : bool
    Values : AntlrState<'a> option
     }


type ANTLRTokenizer<'t> = IStream<char> -> ANTLR<'t> * IStream<char>

//    static member LiftUntil (p1 : ActiveParser<'a,'t>) (p2 : ActiveParser<'a,'t>)  : IStream<'t> -> Unfold<'a> * IStream<'t> =
//      fun stream -> 
//        match p2 stream with
//        | Some(x,next) -> BreakWith(x),next
//        | None ->
//            match p1 stream with
//            | Some(x,next) -> ContinueWith(x),next
//            | None -> Unfold<'a>.Break,stream>



[<RequireQualifiedAccess>]
module ANTLR =
    let WithChannel chan x = {x with Channel = Some(chan)}
      
    let Value v pre post = { Value = { Value = Some(v); Begin = (pre.Position()); Len = (post.Position() - pre.Position())}; Channel = None; Continue = true; Values = None}
    
    let Break x = {x with Continue = false }
    
    let AT_None = { Value = None; Begin = 0; Len = 0}
    let Done = {Value = AT_None; Channel = None; Continue = false; Values = None}
    
    let Join st = {Values = Some st; Value = AT_None; Channel = None; Continue = true}
    
    let Process (fn : IStream<'t> -> ANTLR<'a> * IStream<'t>) =
      fun stream ->
        let mutable loop = true
        let state : AntlrState<'a> =
          {
            Channels = []
            DefaultChannel = []
          }
        let mutable s = stream
        while loop do
            match fn s with
            | {Values = Some(st) },stream ->
                let m1 = FSharp.Collections.Map(state.Channels)
                let m2 = FSharp.Collections.Map(st.Channels)
                let keys = [state.Channels |> List.map fst;  st.Channels |> List.map fst]  |> List.concat |> List.distinct
                state.Channels <-
                    keys |> List.map (fun key ->
                        match m1.TryFind(key), m2.TryFind(key) with
                        | Some(v1),Some(v2) -> key,List.append v1 v2
                        | Some(v1),_ -> key,v1
                        | _,Some(v2) -> key,v2
                        | _,_ -> key,[]
                        )
                state.DefaultChannel <- List.append state.DefaultChannel st.DefaultChannel
            | {Continue = true; Value = vOpt; Channel = cOpt},stream -> 
                s <- stream
                match vOpt,cOpt with
                | { Value = Some(_)} as value,Some(channel) ->
                   if List.exists (fun (chan,_) -> chan = channel) state.Channels then
                       state.Channels <- List.map (fun (chan,ls) -> if chan = channel then (chan,List.append ls [value]) else (chan,ls)) state.Channels
                   else
                       state.Channels <- (channel,[value]) :: state.Channels
                | { Value = Some(_)} as value,None ->
                   state.DefaultChannel <- List.append state.DefaultChannel [value]
            | {Continue = false; Value = vOpt; Channel = cOpt},stream ->
              loop <- false
              s <- stream
              match vOpt,cOpt with
              | { Value = Some(_)} as value,Some(channel) ->
                  if List.exists (fun (chan,_) -> chan = channel) state.Channels then
                       state.Channels <- List.map (fun (chan,ls) -> if chan = channel then (chan,List.append ls [value]) else (chan,ls)) state.Channels
                   else
                       state.Channels <- (channel,[value]) :: state.Channels
              | { Value = Some(_)} as value,None ->
                 state.DefaultChannel <- List.append state.DefaultChannel [value]
        Some(state, s)
        
    let Lift (p : ActiveParser<'a,'t>) : IStream<'t> -> ANTLR<'a> * IStream<'t> =
      fun stream -> 
        match p stream with
        | Some(x,next) -> Value x stream next,next
        | None -> Done,stream

module LexParser =
    let InCharSet (ls : char list) : StringParser<char> =
        let chrSet = Set.ofList ls
        fun stream ->
            match stream.Head() with
            | Some(chr) when chrSet.Contains(chr) -> Some(chr,stream.Consume(1))
            | _ -> None
            
    let notInCharSet (ls : char list) : StringParser<char> =
        let chrSet = Set.ofList ls
        fun stream ->
            match stream.Head() with
            | Some(chr) when not <| chrSet.Contains(chr) -> Some(chr,stream.Consume(1))
            | _ -> None
            
    let EOF : StringParser<unit> =
        fun (stream : Stream.IStream<char>) ->
            match stream.Head() with
            | None -> Some((),stream)
            | _ -> None
        
    let AnyChar (stream : Stream.IStream<char>) =
        match stream.Head() with
        | Some(chr) -> Some(chr,stream)
        | _ -> None
        


module rec LexBasic =
    open LexParser
    let Ws : StringParser<string> =
        Hws <|> Vws
    let Hws : StringParser<char> =
        InCharSet [' ';'\t']
        
    let Vws : StringParser<char> =
        InCharSet ['\r';'\n';'\f']
    let BlockComment : StringParser<string> = //will parse doc comments too?
        String "/*" >> until (AnyChar) (String "*/" <|> EOF)
        
    let DocComment : StringParser<string> =
        String "/**" >> until (AnyChar) (String "*/" <|> EOF)

    let LineComment : StringParser<string> =
        String "//" >> many (notInCharSet ['\r';'\n'])
    let EscSeq =
        Esc >> (InCharSet ['b';'t';'n';'r';'"';'\'';'\\'] <|> UnicodeEsc <|> AnyChar <|> EOF)
    let EscAny : StringParser<string> =
        Esc >> many (AnyChar)
    let UnicodeEsc =
        Char 'u' >> ( !? HexDigit)
    let DecimalNumeral : StringParser<string> =
        Char '0' <|> (StringParsers.Range '1' '9' >> many DecDigit )
    
    let HexDigit =
        StringParsers.Range 'a' 'f' <|> StringParsers.Range 'A' 'F' <|> StringParsers.Range '0' '9'
        
    let DecDigit =
        StringParsers.Range '0' '9'
        
    let BoolLiteral : StringParser<string> =
        String "true" <|> String "false"
        
    let CharLiteral : StringParser<string> =
        SQuote >> (EscSeq <|> notInCharSet ['\'';'\r';'\n';'\\']) >> SQuote
    let SQuoteLiteral : StringParser<string> =
        SQuote >> many (EscSeq <|> notInCharSet ['\'';'\r';'\n';'\\']) >> SQuote
        
    let DQuoteLiteral : StringParser<string> =
        DQuote >> many (EscSeq <|> notInCharSet ['\"';'\r';'\n';'\\']) >> DQuote
        
    let USQuoteLiteral : StringParser<string> =
        SQuote >> many (EscSeq <|> (notInCharSet ['\''; '\r'; '\n'; '\\']))

    let NameChar : StringParser<char> =
        NameStartChar
        <||> StringParsers.Range '0' '9'
        <||> Underscore
        <||> Char '\u00B7'
        <||> StringParsers.Range '\u0300' '\u036F'
        <||> StringParsers.Range '\u203F' '\u2040'
    
    let NameStartChar :  StringParser<char> =
        StringParsers.Range 'A' 'Z'
        <||> StringParsers.Range 'a' 'z'
        <||> StringParsers.Range '\u00C0' '\u00D6'
        <||> StringParsers.Range '\u00D8' '\u00F6'
        <||> StringParsers.Range '\u00F8' '\u02FF'
        <||> StringParsers.Range '\u0370' '\u037D'
        <||> StringParsers.Range '\u037F' '\u1FFF'
        <||> StringParsers.Range '\u200C' '\u200D'
        <||> StringParsers.Range '\u2070' '\u218F'
        <||> StringParsers.Range '\u2C00' '\u2FEF'
        <||> StringParsers.Range '\u3001' '\uD7FF'
        <||> StringParsers.Range '\uF900' '\uFDCF'
        <||> StringParsers.Range '\uFDF0' '\uFFFD'
    
    let Int : StringParser<string> = String "int"
    let Esc : StringParser<char> = Char '\\'
    let Colon : StringParser<char> = Char ':'
    let DColon : StringParser<string> = String "::"
    let SQuote : StringParser<char> = Char '\''
    let DQuote : StringParser<char> = Char '"'
    let LParen : StringParser<char> = Char '('
    let RParen : StringParser<char> = Char ')'
    let LBrace : StringParser<char> = Char '{'
    let RBrace : StringParser<char> = Char '}'
    let LBrack : StringParser<char> = Char '['
    let RBrack : StringParser<char> = Char ']'
    let RArrow : StringParser<string> = String "->"
    let Lt : StringParser<char> = Char '<'
    let Gt : StringParser<char> = Char '>'
    let Equal : StringParser<char> = Char '='
    let Question : StringParser<char> = Char '?'
    let Star : StringParser<char> = Char '*'
    let Plus : StringParser<char> = Char '+'
    let PlusAssign : StringParser<string> = String "+=" 
    let Underscore : StringParser<char> = Char '_'
    let Pipe : StringParser<char> = Char '|'
    let Dollar : StringParser<char> = Char '$'
    let Comma : StringParser<char> = Char ','
    let Semi : StringParser<char> = Char ';'
    let Dot : StringParser<char> = Char '.'
    let Range : StringParser<string> = String ".."
    let At : StringParser<char> = Char '@'
    let Pound : StringParser<char> = Char '#'
    let Tilde : StringParser<char> = Char '~'

module rec AntlrLex =
    open LexBasic
    open LexParser
    let Id : StringParser<string> = NameStartChar >> many NameChar
    
    //Channels? Alternate streams to push tokens too?
    
    type ModeLexerCharSet = //corresponds to mode
        | LEXER_CHAR_SET_BODY of string
        | LEXER_CHAR_SET
        | UNTERMINATED_CHAR_SET
        
    type ModeChannels =
        | CHN_DOC_COMMENT
        | CHN_BLOCK_COMMENT
        | CHN_LINE_COMMENT
        | CHN_LBRACE
        | CHN_RBRACE
        | CHN_ID
        | CHN_DPT
        | CHN_COMMA
        | CHN_WS
        
    type ModeTokens =
        | TOK_DOC_COMMENT
        | TOK_BLOCK_COMMENT
        | TOK_LINE_COMMENT
        | TOK_LBRACE
        | TOK_RBRACE
        | TOK_ID
        | TOK_DPT
        | TOK_COMMA
        | TOK_WS
        
    type ModeOptions =
        | OPT_DOC_COMMENT of string
        | OPT_BLOCK_COMMENT of string
        | OPT_LINE_COMMENT of string
        | OPT_LBRACE
        | OPT_RBRACE
        | OPT_ID of string
        | OPT_DOT
        | OPT_ASSIGN
        | OPT_STRING_LITERAL of string
        | OPT_INT of string
        | OPT_STAR
        | OPT_SEMI
        | OPT_WS of string

    type ModeAction =
        | NESTED_ACTION
        | ACTION_ESCAPE
        | ACTION_STRING_LITERAL
        | ACTION_CHAR_LITERAL
        | ACTION_DOC_COMMENT
        | ACTION_BLOCK_COMMENT
        | ACTION_LINE_COMMENT
        | END_ACTION
        | UNTERMINATED_ACTION
        | ACTION_CONTENT

    type ModeArgument =
        | NESTED_ARGUMENT
        | ARGUMENT_ESCAPE
        | ARGUMENT_STRING_LITERAL
        | ARGUMENT_CHAR_LITERAL
        | END_ARGUMENT
        | UNTERMINATED_ARGUMENT
        | ARGUMENT_CONTENT
        
    
    type Token =
        | DOC_COMMENT of string
        | BLOCK_COMMENT of string
        | LINE_COMMENT of string
        | INT of string
        | STRING_LITERAL of string
        | UNTERMINATED_STRING_LITERAL of string
        | BEGIN_ARGUMENT
        | BEGIN_ACTION
        | OPTIONS
        | TOKENS
        | CHANNELS
        | IMPORT
        | FRAGMENT
        | LEXER
        | PARSER
        | GRAMMAR
        | PROTECTED
        | PUBLIC
        | PRIVATE
        | RETURNS
        | LOCALS
        | THROWS
        | CATCH
        | FINALLY
        | MODE
        | COLON
        | COLONCOLON
        | COMMA
        | SEMI
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        | RARROW
        | LT
        | GT
        | ASSIGN
        | QUESTION
        | STAR
        | PLUS_ASSIGN
        | PLUS
        | OR
        | DOLLAR
        | RANGE
        | DOT
        | AT
        | POUND
        | NOT
        | ID of string
        | WS of string
        | ERRCHAR of char
        

        | LEXER_CHAR_SET_BODY of string
        | UNTERMINATED_CHAR_SET
        

        | CHN_DOC_COMMENT
        | CHN_BLOCK_COMMENT
        | CHN_LINE_COMMENT
        | CHN_LBRACE
        | CHN_RBRACE
        | CHN_ID
        | CHN_DPT
        | CHN_COMMA
        | CHN_WS
        

        | TOK_DOC_COMMENT
        | TOK_BLOCK_COMMENT
        | TOK_LINE_COMMENT
        | TOK_LBRACE
        | TOK_RBRACE
        | TOK_ID
        | TOK_DPT
        | TOK_COMMA
        | TOK_WS
        

        | OPT_DOC_COMMENT of string
        | OPT_BLOCK_COMMENT of string
        | OPT_LINE_COMMENT of string
        | OPT_LBRACE
        | OPT_RBRACE
        | OPT_ID of string
        | OPT_DOT
        | OPT_ASSIGN
        | OPT_STRING_LITERAL of string
        | OPT_INT of string
        | OPT_STAR
        | OPT_SEMI
        | OPT_WS of string


        | NESTED_ACTION
        | ACTION_ESCAPE
        | ACTION_STRING_LITERAL
        | ACTION_CHAR_LITERAL
        | ACTION_DOC_COMMENT
        | ACTION_BLOCK_COMMENT
        | ACTION_LINE_COMMENT
        | END_ACTION
        | UNTERMINATED_ACTION
        | ACTION_CONTENT of string

        | NESTED_ARGUMENT
        | ARGUMENT_ESCAPE
        | ARGUMENT_STRING_LITERAL
        | ARGUMENT_CHAR_LITERAL
        | END_ARGUMENT
        | UNTERMINATED_ARGUMENT
        | ARGUMENT_CONTENT of string

        //imaginary tokens (Lazy AnyString -> Which captures in context on first pass to be used on second pass) usefull for variables
        //first pass captures strings in the grammer
        //second pass matches those strings as references
        | TOKEN_REF
        | RULE_REF
        | LEXER_CHAR_SET
        
    let (| DOC_COMMENT |_|) = Map DocComment Token.DOC_COMMENT
    let (| BLOCK_COMMENT |_|) = Map BlockComment Token.BLOCK_COMMENT
    let (| LINE_COMMENT |_|) = Map LineComment Token.LINE_COMMENT
    let (| INT |_|) = Map DecimalNumeral Token.INT
    let (| STRING_LITERAL |_|) = Map SQuoteLiteral Token.STRING_LITERAL
    let (| UNTERMINATED_STRING_LITERAL |_|) = Map USQuoteLiteral Token.UNTERMINATED_STRING_LITERAL
    let (| BEGIN_ARGUMENT |_|) = LBrack >>% Token.BEGIN_ARGUMENT
    let (| BEGIN_ACTION |_|) = LBrace >>% Token.BEGIN_ACTION
    let (| OPTIONS |_|) = String "options" >>% Token.OPTIONS //pushMode
    let (| TOKENS |_|) = String "tokens" >>% Token.TOKENS
    let (| CHANNELS |_|) = String "channels" >>% Token.CHANNELS
    let (| IMPORT |_|) = String "import" >>% Token.IMPORT
    let (| FRAGMENT |_|) = String "fragment" >>% Token.FRAGMENT
    let (| LEXER |_|) = String "lexer" >>% Token.LEXER
    let (| PARSER |_|) = String "parser" >>% Token.PARSER
    let (| GRAMMAR |_|) = String "grammar" >>% Token.GRAMMAR
    let (| PROTECTED |_|) = String "protected" >>% Token.PROTECTED
    let (| PUBLIC |_|) = String "public" >>% Token.PUBLIC
    let (| PRIVATE |_|) = String "private" >>% Token.PRIVATE
    let (| RETURNS |_|) = String "returns" >>% Token.RETURNS
    let (| LOCALS |_|) = String "locals" >>% Token.LOCALS
    let (| THROWS |_|) = String "throws" >>% Token.THROWS
    let (| CATCH |_|) = String "catch" >>% Token.CATCH
    let (| FINALLY |_|) = String "finally" >>% Token.FINALLY
    let (| MODE |_|) = String "mode" >>% Token.MODE
    let (| COLON |_|) = Colon >>% Token.COLON
    let (| COLONCOLON |_|) = DColon >>% Token.COLONCOLON
    let (| COMMA |_|) = Comma >>% Token.COMMA
    let (| SEMI |_|) = Semi >>% Token.SEMI
    let (| LPAREN |_|) = LParen >>% Token.LPAREN
    let (| RPAREN |_|) = RParen >>% Token.RPAREN
    let (| LBRACE |_|) = LBrace >>% Token.LBRACE
    let (| RBRACE |_|) = RBrace >>% Token.RBRACE
    let (| RARROW |_|) = RArrow >>% Token.RARROW
    let (| LT |_|) = Lt >>% Token.LT
    let (| GT |_|) = Gt >>% Token.GT
    let (| ASSIGN |_|) = Equal >>% Token.ASSIGN
    let (| QUESTION |_|) = Question >>% Token.QUESTION
    let (| STAR |_|) = Star >>% Token.STAR
    let (| PLUS_ASSIGN |_|) = PlusAssign >>% Token.PLUS_ASSIGN
    let (| PLUS |_|) = Plus >>% Token.PLUS
    let (| OR |_|) = Pipe >>% Token.OR
    let (| DOLLAR |_|) = Dollar >>% Token.DOLLAR
    let (| RANGE |_|) = Range >>% Token.RANGE
    let (| DOT |_|) = Dot >>% Token.DOT
    let (| AT |_|) = At >>% Token.AT
    let (| POUND |_|) = Pound >>% Token.POUND
    let (| NOT |_|) = Tilde >>% Token.NOT
    let (| ID |_|) = Map Id Token.ID
    let (| WS |_|) =  Map (uplift (many Ws)) Token.WS
    let (| ERRCHAR |_|) = Map AnyChar Token.ERRCHAR
    
    let parseToken : ANTLRTokenizer<Token> =
        fun stream ->
            match stream with
            | DOC_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
            | BLOCK_COMMENT(t,next) -> ANTLR.Value t stream next|> ANTLR.WithChannel "COMMENT",next
            | LINE_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
            | INT(t,next) -> ANTLR.Value t stream next,next
            | STRING_LITERAL(t,next) -> ANTLR.Value t stream next,next
            | UNTERMINATED_STRING_LITERAL(t,next) -> ANTLR.Value t stream next,next
            | BEGIN_ARGUMENT(t,next) ->
                match ANTLR.Process parseAction next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | BEGIN_ACTION(t,next) ->
                match ANTLR.Process parseAction next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | OPTIONS(t,next) ->
                match ANTLR.Process parseOption next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | TOKENS(t,next) -> 
                match ANTLR.Process parseOption next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | CHANNELS(t,next) -> 
                match ANTLR.Process parseOption next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | IMPORT(t,next) -> ANTLR.Value t stream next,next
            | FRAGMENT(t,next) -> ANTLR.Value t stream next,next
            | LEXER(t,next) -> ANTLR.Value t stream next,next
            | PARSER(t,next) -> ANTLR.Value t stream next,next
            | GRAMMAR(t,next) -> ANTLR.Value t stream next,next
            | PROTECTED(t,next) -> ANTLR.Value t stream next,next
            | PUBLIC(t,next) -> ANTLR.Value t stream next,next
            | PRIVATE(t,next) -> ANTLR.Value t stream next,next
            | RETURNS(t,next) -> ANTLR.Value t stream next,next
            | LOCALS(t,next) -> ANTLR.Value t stream next,next
            | THROWS(t,next) -> ANTLR.Value t stream next,next
            | CATCH(t,next) -> ANTLR.Value t stream next,next
            | FINALLY(t,next) -> ANTLR.Value t stream next,next
            | MODE(t,next) -> ANTLR.Value t stream next,next
            | COLON(t,next) -> ANTLR.Value t stream next,next
            | COLONCOLON(t,next) -> ANTLR.Value t stream next,next
            | COMMA(t,next) -> ANTLR.Value t stream next,next
            | SEMI(t,next) -> ANTLR.Value t stream next,next
            | LPAREN(t,next) -> ANTLR.Value t stream next,next
            | RPAREN(t,next) -> ANTLR.Value t stream next,next
            | LBRACE(t,next) -> ANTLR.Value t stream next,next
            | RBRACE(t,next) -> ANTLR.Value t stream next,next
            | RARROW(t,next) -> ANTLR.Value t stream next,next
            | LT(t,next) -> ANTLR.Value t stream next,next
            | GT(t,next) -> ANTLR.Value t stream next,next
            | ASSIGN(t,next) -> ANTLR.Value t stream next,next
            | QUESTION(t,next) -> ANTLR.Value t stream next,next
            | STAR(t,next) -> ANTLR.Value t stream next,next
            | PLUS_ASSIGN(t,next) -> ANTLR.Value t stream next,next
            | PLUS(t,next) -> ANTLR.Value t stream next,next
            | OR(t,next) -> ANTLR.Value t stream next,next
            | DOLLAR(t,next) -> ANTLR.Value t stream next,next
            | RANGE(t,next) -> ANTLR.Value t stream next,next
            | DOT(t,next) -> ANTLR.Value t stream next,next
            | AT(t,next) -> ANTLR.Value t stream next,next
            | POUND(t,next) -> ANTLR.Value t stream next,next
            | NOT(t,next) -> ANTLR.Value t stream next,next
            | ID(t,next) -> ANTLR.Value t stream next,next
            | WS(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "OFF_CHANNEL",next
            | ERRCHAR(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "HIDDEN",next
            | _ -> ANTLR.Done,stream
    
    let parseTokens =
        fun stream -> ANTLR.Process parseToken stream
    
    let (| OPT_DOC_COMMENT |_|) = Map DocComment Token.DOC_COMMENT
    let (| OPT_BLOCK_COMMENT |_|) = Map BlockComment Token.BLOCK_COMMENT
    let (| OPT_LINE_COMMENT |_|) = Map LineComment Token.LINE_COMMENT
    let (| OPT_LBRACE |_|) = LBrace >>% Token.LBRACE
    let (| OPT_RBRACE |_|) = RBrace >>% Token.RBRACE
    let (| OPT_ID |_|) = Map Id Token.ID 
    let (| OPT_DOT |_|) = Dot >>% Token.DOT
    let (| OPT_ASSIGN |_|) = Equal >>% Token.ASSIGN
    let (| OPT_STRING_LITERAL |_|) = Map SQuoteLiteral Token.STRING_LITERAL
    let (| OPT_INT |_|) = Map DecimalNumeral Token.INT
    let (| OPT_STAR |_|) = Star >>% Token.STAR
    let (| OPT_SEMI |_|) = Semi >>% Token.SEMI
    let (| OPT_WS |_|) = Map (uplift (many Ws)) Token.WS
    
    let parseOption  =
            fun stream ->
                match stream with
                | OPT_DOC_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
                | OPT_BLOCK_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
                | OPT_LINE_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
                | OPT_LBRACE(t,next) -> ANTLR.Value t stream next,next
                | OPT_RBRACE(t,next) -> ANTLR.Value t  stream next|> ANTLR.Break,next
                | OPT_ID(t,next) -> ANTLR.Value t stream next,next
                | OPT_DOT(t,next) -> ANTLR.Value t stream next,next
                | OPT_ASSIGN(t,next) -> ANTLR.Value t stream next,next
                | OPT_STRING_LITERAL(t,next) -> ANTLR.Value t stream next,next
                | OPT_INT(t,next) -> ANTLR.Value t stream next,next
                | OPT_STAR(t,next) -> ANTLR.Value t stream next,next
                | OPT_SEMI(t,next) -> ANTLR.Value t stream next,next
                | OPT_WS(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "OFF_CHANNEL",next
                | _ -> ANTLR.Done,stream
    let (| ParseOptions |_|) =
        fun stream -> ANTLR.Process parseOption stream

    //let (| ParseOptions |_|) = (ParseOptions)
    
    
    let (| NESTED_ARGUMENT |_|) = Map (uplift LBrack) Token.ARGUMENT_CONTENT 
    let (| ARGUMENT_ESCAPE |_|) =  Map EscAny Token.ARGUMENT_CONTENT 
    let (| ARGUMENT_STRING_LITERAL |_|) =  Map DQuoteLiteral Token.ARGUMENT_CONTENT 
    let (| ARGUMENT_CHAR_LITERAL |_|) = Map SQuoteLiteral Token.ARGUMENT_CONTENT 
    let (| END_ARGUMENT |_|) = RBrack >>% Token.END_ARGUMENT

    let (| UNTERMINATED_ARGUMENT |_|) = EOF 

    let (| ARGUMENT_CONTENT |_|) = Map (uplift AnyChar) Token.ARGUMENT_CONTENT

    let rec parseArgument stream =
            match stream with
            | NESTED_ARGUMENT(t,next) ->
                match ANTLR.Process parseArgument next with
                | Some(st,next2) -> ANTLR.Join st, next2
                | None -> failwith ""
            | ARGUMENT_ESCAPE(t,next) -> ANTLR.Value t stream next,next
            | ARGUMENT_STRING_LITERAL(t,next) -> ANTLR.Value t stream next,next
            | ARGUMENT_CHAR_LITERAL(t,next) -> ANTLR.Value t stream next,next
            | END_ARGUMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.Break,next
            | UNTERMINATED_ARGUMENT(t,next) -> ANTLR.Done,next
            | ARGUMENT_CONTENT(t,next) -> ANTLR.Value t stream next,next
   
    let (| NESTED_ACTION |_|) = Map (uplift LBrace) Token.ACTION_CONTENT
    let (| ACTION_ESCAPE |_|) = Map EscAny Token.ACTION_CONTENT
   
    let (| ACTION_STRING_LITERAL |_|) = Map DQuoteLiteral Token.ACTION_CONTENT
   
    let (| ACTION_CHAR_LITERAL |_|) = Map SQuoteLiteral Token.ACTION_CONTENT
   
    let (| ACTION_DOC_COMMENT |_|) = Map DocComment Token.ACTION_CONTENT
   
    let (| ACTION_BLOCK_COMMENT |_|) = Map BlockComment Token.ACTION_CONTENT
   
    let (| ACTION_LINE_COMMENT |_|) = Map LineComment Token.ACTION_CONTENT
   
    let (| END_ACTION |_|) = RBrace

    let (| UNTERMINATED_ACTION |_|) = EOF
   
    let (| ACTION_CONTENT |_|) = Map (uplift AnyChar) Token.ACTION_CONTENT
    
    let rec parseAction stream =
        match stream with
        | NESTED_ACTION(t,next) ->
            match ANTLR.Process parseAction next with
            | Some(st,next2) -> ANTLR.Join st, next2
            | None -> failwith ""
        | ACTION_ESCAPE(t,next) -> ANTLR.Value t stream next,next
        | ACTION_STRING_LITERAL(t,next) -> ANTLR.Value t stream next,next
        | ACTION_CHAR_LITERAL(t,next) -> ANTLR.Value t stream next,next
        | ACTION_DOC_COMMENT(t,next) -> ANTLR.Value t stream next,next
        | ACTION_BLOCK_COMMENT(t,next) -> ANTLR.Value t stream next,next
        | ACTION_LINE_COMMENT(t,next) -> ANTLR.Value t stream next,next
        | END_ACTION(t,next) -> ANTLR.Done,next
        | UNTERMINATED_ACTION(t,next) -> ANTLR.Done,next
        | ACTION_CONTENT(t,next) -> ANTLR.Value t stream next,next
    
    let (| TOK_DOC_COMMENT |_|) = Map DocComment Token.DOC_COMMENT

    let (| TOK_BLOCK_COMMENT |_|) = Map BlockComment Token.BLOCK_COMMENT 
   
    let (| TOK_LINE_COMMENT |_|) = Map LineComment Token.LINE_COMMENT

    let (| TOK_LBRACE |_|) = LBrace >>% Token.LBRACE
   
    let (| TOK_RBRACE |_|) = RBrace >>% Token.RBRACE
  
    let (| TOK_ID |_|) = Map Id Token.ID

    let (| TOK_DOT |_|) = Dot >>% Token.DOT

    let (| TOK_COMMA |_|) = Comma >>% Token.COMMA

    let (| TOK_WS |_|) = Map Ws Token.WS
   
    let rec parseToken stream =
        match stream with
        | TOK_DOC_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | TOK_BLOCK_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | TOK_LINE_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | TOK_LBRACE(t,next) -> ANTLR.Value t stream next,next
        | TOK_RBRACE(t,next) -> ANTLR.Value t stream next |> ANTLR.Break,next
        | TOK_ID(t,next) -> ANTLR.Value t stream next,next
        | TOK_DOT(t,next) -> ANTLR.Value t stream next,next
        | TOK_COMMA(t,next) -> ANTLR.Value t stream next,next
        | TOK_WS(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "OFF_CHANNEL",next
    
    let (| CHN_DOC_COMMENT |_|) = Map DocComment Token.DOC_COMMENT
   

    let (| CHN_BLOCK_COMMENT |_|) = Map BlockComment Token.BLOCK_COMMENT
   

    let (| CHN_LINE_COMMENT |_|) = Map LineComment Token.LINE_COMMENT
   

    let (| CHN_LBRACE |_|) = LBrace >>% Token.LBRACE

    let (| CHN_RBRACE |_|) = RBrace >>% Token.RBRACE

    let (| CHN_ID |_|) = Map Id Token.ID

    let (| CHN_DOT |_|) = Dot >>% Token.DOT

    let (| CHN_COMMA |_|) = Comma >>% Token.COMMA

    let (| CHN_WS |_|) = Map Ws Token.WS

    let rec parseChannel stream =
        match stream with
        | CHN_DOC_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | CHN_BLOCK_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | CHN_LINE_COMMENT(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "COMMENT",next
        | CHN_LBRACE(t,next) -> ANTLR.Value t stream next,next
        | CHN_RBRACE(t,next) -> ANTLR.Value t stream next |> ANTLR.Break,next
        | CHN_ID(t,next) -> ANTLR.Value t stream next,next
        | CHN_DOT(t,next) -> ANTLR.Value t stream next,next
        | CHN_COMMA(t,next) -> ANTLR.Value t stream next,next
        | CHN_WS(t,next) -> ANTLR.Value t stream next |> ANTLR.WithChannel "OFF_CHANNEL",next
    
    let (| LEXER_CHAR_SET_BODY |_|) = many ((uplift (notInCharSet [']';'\\'])) <|> (uplift EscAny)) //more?

    let (| LEXER_CHAR_SET |_|) = RBrack 
   
    let (| UNTERMINATED_CHAR_SET |_|) = EOF
   
module rec AntlrParser =
    open AntlrLex
    open LexBasic
    open LexParser
    open ActiveParsers
    
    let (|TOKEN_REF|_|) = 
    
    let terminal = TOKEN_REF .>>. (maybe elementOptions) 
    let elementOptions = (|LT|_|) >>. elementOption .>>. (many ((|COMMA|_|) >>. elementOption)) .>> (|GT|_|)
    let elementOption = identifier <|> (identifier .>>. (|ASSIGN|_|) .>>. (identifier <|> (|STRING_LITERAL|_|) ))
    let identifier = RULE_REF <|> TOKEN_REF