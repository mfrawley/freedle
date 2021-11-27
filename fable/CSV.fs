module Freedle.CSV
open Frame
<<<<<<< HEAD
open Types
=======
>>>>>>> a32213c (fix compilation order)

let parseCSV (s:string) : UntypedFrame =
    let lines = s.Split('\n')
    let headerRow: string array = (rowFromStr lines.[0])
    let dataRows: string array array = 
        lines.[1..] 
        |> Array.filter (fun l -> l <> "")
        |> Array.map rowFromStr
        
    let cols = seq { for i in 0..headerRow.Length-1 -> (buildCol headerRow.[i] i dataRows)} |> Seq.toList
    {columns=cols}



