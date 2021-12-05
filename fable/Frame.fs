module Freedle.Frame
open Fable.Core
open System
open Fable.Core
open FSharp.Reflection
type Column<'a> = {
    name: string
    typedData: 'a array
}

let getRecordFieldTuples (recc: Type) : (string * string)[] = 
    let fields = FSharp.Reflection.FSharpType.GetRecordFields(recc)
    
    let fieldNames = 
        fields 
        |> Array.map (fun field -> (field.Name, field.PropertyType.FullName) )
    fieldNames

let getMapFromRecordType (value: Type) : Map<string, obj> =
    FSharpType.GetRecordFields(value)
    |> Seq.fold
        (fun acc field ->
            let fieldValue = field.GetValue value
            acc.Add (field.Name, fieldValue)
        )
        Map.empty
    
type FloatCol = Column<float>
type IntCol = Column<int>
type StrCol = Column<string>
type DateCol = Column<DateTime>
type DateOptionCol = Column<DateTime option>
type BoolCol = Column<bool>

type TypedColumn = 
    | F of FloatCol 
    | I of IntCol 
    | S of StrCol
    | D of DateCol
    | DO of DateOptionCol
    | B of BoolCol

type TypedFrame = 
    {
    columns: TypedColumn list}
    
    member this.GetColumnByName(name:string) =
        this.columns
        |> List.filter (fun col -> 
            let colname =
                match col with 
                | F c -> c.name
                | I c -> c.name
                | S c -> c.name
                | D c -> c.name 
                | DO c -> c.name
                | B c -> c.name
            colname = name
            )
        |> List.head

type RawRow = string array
type RawCol = string array
type UntypedColumn = {
    name: string
    data: string array
}    

type UntypedFrame = 
    {
    columns: UntypedColumn list}
    
    member this.GetColumnByName(name:string) =
        this.columns
        |> List.filter (fun col -> col.name = name)
        |> List.head
 
exception MissingDataException of string

// let col_names (f:Frame<obj>) : string list = List.map (fun col -> col.name) f.columns

let rowFromStr (s:string) : string[] = s.Split(',')

let buildCol (name:string) (i:int) (rows: string array array) : UntypedColumn =
    { 
        // (Array.map (fun (row: string array) -> row.[i]) rows)
        data= (Array.map (fun (row: string array) -> row.[i]) rows)
        name=name
    }

let toIntCol c (fill: int option) : IntCol =
    let data = c.data |> Array.filter (fun s -> s <> "")
    {
        name=c.name;
        typedData=(Array.map (fun (x:string) -> 
            let succ, res = Int32.TryParse x
            if succ then
                res
            else
                match fill with
                | None -> raise (MissingDataException("Data is empty."))
                | Some v -> v
            ) data)
    }

let inline toFloatCol (c:UntypedColumn) (fill: float option) =
    {
        name=c.name;
        typedData=(Array.map (fun (x:string) -> 
            match x with
            | "" -> 
                match fill with
                    | None -> nan
                    | _ -> fill.Value
            | _ -> 
                let res:float =
                    #if FABLE_COMPILER
                        JS.parseFloat(x)
                    #else
                        Double.Parse(x)
                    #endif
                res
        )
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

let toBoolCol c (fill: bool option) =
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

let toDateTimeOptionCol c =
    let verboseParse s =
        let parsed, d = System.DateTime.TryParse s
        match parsed with
        | true -> Some d
        | false -> None
    let data = c.data |> Array.filter (fun s -> s <> "")
    let parsed = (Array.map verboseParse data)

    {
        name=c.name;
        typedData=parsed
    }

// TODO: return type should be optional
let getStrCol (cols: UntypedColumn list) (idx:int) : StrCol =
    let untypedCol = cols.[idx]
    untypedCol |> toStrCol

// TODO: return type should be optional
let getDateCol (cols: UntypedColumn list) (idx:int) : DateCol =
    let untypedCol = cols.[idx]
    untypedCol |> toDateTimeCol

let getDateOptionCol (cols: UntypedColumn list) (idx:int) : DateOptionCol =
    let untypedCol = cols.[idx]
    untypedCol |> toDateTimeOptionCol

// TODO: return type should be optional
let getIntCol (cols: UntypedColumn list) (idx:int) (fillNa: int option) : IntCol =
    let untypedCol = cols.[idx]
    toIntCol untypedCol fillNa

// TODO: return type should be optional
let getFloatCol (cols: UntypedColumn list) (idx:int) (fillNa: float option): FloatCol =
    let untypedCol = cols.[idx]
    toFloatCol untypedCol fillNa

let getBoolCol (cols: UntypedColumn list) (idx:int) (fillNa: bool option): BoolCol =
    let untypedCol = cols.[idx]
    toBoolCol untypedCol fillNa

let emptyCol (name:string) =
    {
        name=name
        typedData=[||]
    }     

let rowWiseSum (cols: FloatCol list) : float array = 
    let firstColLength = cols.Head.typedData.Length
    let res : float array = Array.zeroCreate firstColLength
    let data = List.map (fun col -> col.typedData) cols
    for i = 0 to firstColLength do
        let rowdata: float list = List.map (fun (c: float array) -> c.[i] ) data
        res.[i] <- List.sum rowdata
    res

let replaceColNames (ids: string array) (labels: string array) (countCols: FloatCol list) = 
    // for each column, if we have the id in our ids list,
    // get the index of the match and pluck it out of the names list
    List.map(fun (col: FloatCol) ->
        let idx: int = Array.IndexOf(ids, col.name)
        // if we have a mapping, override it for display
        if idx <> -1 then
             { col with name = labels.[idx] }
        else
            col
    ) countCols

let filterColumn (col:Column<'a>) (predicate: ('a -> bool)): Column<'a> =
    let filteredData = Array.filter predicate col.typedData
    {col with typedData = filteredData}

let getIndexesMatchingPredicate (col:Column<'a>) (predicate: ('a -> bool)): bool array =
    let len = col.typedData.Length
    let indexes : bool array = Array.zeroCreate len
    for i = 0 to len-1 do
        if (predicate col.typedData.[i]) = true then
            indexes.[i] <- true
    indexes

let filterByIndex (col:Column<'a>) (indexArr: bool array) : Column<'a> =
    let len = col.typedData.Length
    let filtered = 
        col.typedData
        |> Array.mapi(fun i item -> 
            if indexArr.[i] = true then
                Some item
            else None
        ) 
        |> Array.filter Option.isSome
        |> (Array.map Option.get)
    
    { col with typedData = filtered}

type Filter<'a> = {
    col:Column<'a>
    predicate: ('a -> bool)
}
type FilterList<'a> = Filter<'a> list

let filterCol (col:Column<'a>) (predicate: ('a -> bool)) : Column<'a> =
    let indexes = getIndexesMatchingPredicate col predicate
    filterByIndex col indexes

let aggArrays (typedDataArrs : 'a array array) (aggFn: 'a array -> 'a) =
    typedDataArrs
    |> Array.map aggFn
    |> aggFn


let getColumnByName (name:string) (frame:UntypedFrame) =
    frame.columns
    |> List.filter (fun col -> col.name = name)
    |> List.head

let genTypedFrame (rowType: 'a) (untypedFrame: UntypedFrame) : TypedFrame =
    let fields = getRecordFieldTuples(rowType.GetType())
    let cols = 
        fields
        |> Array.map (fun (fieldName, fieldType) -> 
            match fieldType with
            | "System.Double" -> 
                (F (toFloatCol (getColumnByName fieldName untypedFrame) (Some 0.0)) )
            | "System.String" -> 
                (S (toStrCol (getColumnByName fieldName untypedFrame)))
            | "System.Boolean" -> 
                (B (toBoolCol (getColumnByName fieldName untypedFrame) (Some false)) )                
            | _ ->
                failwith $"Unrecognised field type {fieldType}"
            )
        |> Array.toList
    {columns=cols}
