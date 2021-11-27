module Frame
open Fable.Core
open System

type Column<'a> = {
    name: string
    typedData: 'a array
}

type FloatCol = Column<float>
type IntCol = Column<int>
type StrCol = Column<string>
type DateCol = Column<System.DateTime>

type UntypedColumn = {
    name: string
    data: string array
}

type RawRow = string array
type RawCol = string array

type Frame = {
    columns: UntypedColumn list
}
 
 exception MissingData of string

// let col_names (f:Frame<obj>) : string list = List.map (fun col -> col.name) f.columns

let rowFromStr (s:string) = s.Split(',')

let buildCol (name:string) (i:int) (rows: string array array) =
    { 
        // (Array.map (fun (row: string array) -> row.[i]) rows)
        data= (Array.map (fun (row: string array) -> row.[i]) rows)
        name=name
    }

let toIntCol c (fill: int option) =
    let data = c.data |> Array.filter (fun s -> s <> "")
    {
        name=c.name;
        typedData=(Array.map (fun (x:string) -> 
            let succ, res = Int32.TryParse x
            if succ then
                res
            else
                match fill with
                | None -> raise (MissingData("sdfdf"))
                | Some v -> v
            ) data)
    }

let toFloatCol (c:UntypedColumn) (fill: float option) =
    {
        name=c.name;
        typedData=(Array.map (fun (x:string) -> 
            match x with
            | "" -> 
                match fill with
                    | None -> nan
                    | _ -> fill.Value
            | _ -> (JS.parseFloat x))
            c.data)
    }

let toStrCol c =
    {
        name=c.name;
        typedData=c.data
    }

let tryParseBool (s:string) =
    let l = s.ToLower()
    match l with
    | "false" -> false
    | "true" -> true
    | _ -> invalidArg s $"Invalid boolean {s}"

let toBoolCol c =
    {
        name=c.name;
        typedData=(Array.map tryParseBool c.data)
    }

let toDateTimeCol c =
    let verboseParse s =
        let parsed, d = System.DateTime.TryParse s
        match parsed with
        | true -> d
        | false -> invalidArg s $"Invalid datetime:{s}"
    let data = c.data |> Array.filter (fun s -> s <> "")
    let parsed = (Array.map verboseParse data)

    {
        name=c.name;
        typedData=parsed
    }

let getStrCol (cols: UntypedColumn list) (idx:int) : StrCol =
    let untypedCol = cols.[idx]
    untypedCol |> toStrCol

let getDateCol (cols: UntypedColumn list) (idx:int) : DateCol =
    let untypedCol = cols.[idx]
    untypedCol |> toDateTimeCol

let getIntCol (cols: UntypedColumn list) (idx:int) (fill: int option) : IntCol =
    let untypedCol = cols.[idx]
    toIntCol untypedCol fill

let getFloatCol (cols: UntypedColumn list) (idx:int) (fill: float option): FloatCol =
    let untypedCol = cols.[idx]
    toFloatCol untypedCol fill
