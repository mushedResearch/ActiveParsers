module Stream

open System
open System.Text

type IStream<'t> =
    abstract Read : int -> 't [] option
    abstract Consume : int -> IStream<'t>
    abstract Head : unit -> 't option
    abstract SubSearch : ('t -> 't -> bool) * 't [] -> int option
    abstract Search : ('t -> 't -> bool) *'t -> int option
    abstract Length : unit -> int
    abstract Position : unit -> int
    

[<Struct>]    
type StreamState<'a> =
  { Pos : int
    TotalLength : int
    Cache : 'a []
  }


type Stream<'a>( model : 'a [] ref, cacheSize : int, state : StreamState<'a>) =
    
    let mutable state = state

    new( model : 'a [] ref, cacheSize : int) = 
      let len = if model.Value.Length < cacheSize then model.Value.Length else cacheSize
      Stream<'a>(model,cacheSize, { Pos = 0; TotalLength = model.Value.Length; Cache = Array.sub model.Value 0 len })

    interface IStream<'a> with
        member this.Length(): int = 
          state.TotalLength

        member this.Position(): int = 
          state.Pos

        member this.SubSearch(comp, arg: 'a []): int option = 
            let cfn = (fun x y  -> if comp x y then 0 else -1) 
            let mutable target = Array.zeroCreate arg.Length
            let mutable src = (fun _ -> state.Pos) ()
            let mutable result = None
            while result.IsNone
             && src <= (state.TotalLength - arg.Length) do
              Array.blit model.Value src target 0 arg.Length
              if Array.compareWith cfn target arg = 0 then
                result <- Some(src)
              src <- src + 1
            result

        member this.Search(comp, arg : 'a): int option =
          let mutable src = (fun _ -> state.Pos) ()
          let mutable result = None
          while result.IsNone
           && src < state.TotalLength do
            if (comp (model.Value.[src]) arg) then result <- Some(src)
            src <- src + 1
          result


        member this.Consume len = 
            match state.Cache.Length, len with
            | _,b when b < 0 -> failwith "cannot consume negative length"
            | a,b when a > b -> 
                 Stream<'a>(model,cacheSize,{ state with 
                    Pos = state.Pos + len; 
                    Cache = Array.skip len state.Cache }) :> IStream<'a>                
            | a,b when state.Pos + b <= state.TotalLength && a <= b -> 
                let readlen = 
                  if state.Pos + state.Cache.Length + cacheSize > state.TotalLength 
                  then state.TotalLength - state.Pos - state.Cache.Length 
                  else cacheSize
                let temp = Array.sub model.Value (state.Pos + state.Cache.Length) readlen
                let temp2 = Array.append state.Cache temp
                Stream<'a>(model,cacheSize,{ state with 
                  Pos = state.Pos + len; 
                  Cache = Array.skip len temp2}) :> IStream<'a>                   
            | _,_ -> failwith "stream consume exceeds limit"


        member this.Read length =
            match state.Cache.Length, length with
            | _,len when len < 0 -> failwith "cannot read negative length"
            | _,0 -> None
            | cl,len when cl >= len -> Some (Array.sub state.Cache 0 len)
            | cl,len when cl < len && state.Pos + len <= state.TotalLength ->
                let readlen = 
                  let temp = if len > cacheSize then len else cacheSize
                  if temp + state.Pos > state.TotalLength then state.TotalLength - state.Pos else temp

                let n_ary = Array.sub model.Value state.Pos readlen
                state <- { state with Cache = n_ary}
                Some (Array.sub n_ary 0 len)
            | a,b when a < b -> None //outofbounds

        member this.Head(): 'a option = 
          if state.Cache.Length >= 1 && state.Pos <> state.TotalLength then Some(state.Cache.[0])
          else (this :> IStream<'a>).Read 1 |> Option.map Array.head

[<Struct>]
type CachelessStream<'a>( model : 'a [] ref, position : int) =
    
    interface IStream<'a> with
        member this.Consume(length) = CachelessStream(model,position + length) :> IStream<'a>

        member this.Head() =
          if position <> model.Value.Length then Some(model.Value.[position]) else None

        member this.Length() = model.Value.Length

        member this.Position() = position

        member this.Search(comp, arg) = 
          let mutable src = position
          let mutable result = None
          while result.IsNone
           && src < model.Value.Length do
            if (comp (model.Value.[src]) arg) then result <- Some(src)
            src <- src + 1
          result

        member this.SubSearch(comp, arg) = 
          let cfn = (fun x y  -> if comp x y then 0 else -1) 
          let mutable target = Array.zeroCreate arg.Length
          let mutable src = position
          let mutable result = None
          while result.IsNone
           && src <= (model.Value.Length - arg.Length) do
            Array.blit model.Value src target 0 arg.Length
            if Array.compareWith cfn target arg = 0 then
              result <- Some(src)
            src <- src + 1
          result

        member this.Read length = 
          if length + position > model.Value.Length then None
          else Some (Array.sub model.Value position length)




