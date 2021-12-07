namespace fable.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Freedle.Frame
open Freedle.CSV
open FSharp.Reflection

type FooRecord = {
    yeah: string
    nope: int
}
type AbRecord = 
    {
    a: double
    b: bool option }
    static member Default =
        { a = 0.0
          b = Some false }

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestRowFromString () =
        let r = rowFromStr ""
        Assert.IsTrue(r.Length = 1)
        
        let r1 = rowFromStr "a,b"
        Assert.IsTrue(r1.Length = 2)
        Assert.AreEqual(r1.[0], "a")
        Assert.AreEqual(r1.[1], "b")

    [<TestMethod>]
    member this.TestTypedFrame () =
        let f : TypedFrame = {
            columns=[
            (F {
                name="foo"
                typedData=[|0.0|]
            })
            (S {
                name="bar"
                typedData=[|"sdfdsf"|]
            })            
        ]}
        Assert.IsTrue true


    [<TestMethod>]
    member _.TestgetRecordFieldTuples () =
        let fooInst = {
            yeah="sf"
            nope=234
        }
        let fieldNames = getRecordFieldTuples(fooInst.GetType())
        
        Assert.AreEqual ((fst fieldNames.[0]), "yeah")
        let s:Type = (snd fieldNames.[0])
        printfn "s: %A" s
        Assert.AreEqual(s, typeof<string>)

        Assert.AreEqual ((fst fieldNames.[1]), "nope")
        Assert.AreEqual ((snd fieldNames.[1]), typeof<int32>)

    [<TestMethod>]
    member _.TestGetRecordFieldsOnCol () =
        let myFloatCol = {
            name="sfds"
            typedData = [|0.0|]
        }
        let fieldNames = getRecordFieldTuples(myFloatCol.GetType())

        Assert.AreEqual ((fst fieldNames.[0]), "name")
        Assert.AreEqual ((snd fieldNames.[0]), typeof<string> )


    [<TestMethod>]
    member _.TestGenTypedFrame () =
        let rawStr="a,b\n0.0,true\n2.0,"
        let untyped = parseCSV rawStr
        Assert.AreEqual(["a";"b"], untyped.columns |> List.map (fun c -> c.name))

        let typedFrame = (genTypedFrame AbRecord.Default untyped)
        
        Assert.AreEqual(typedFrame.columns.Length, 2)

        let typedCol = typedFrame.GetColumnByName("a")
        let expected = [|0.0; 2.0|]
        match typedCol with
        | F c -> Assert.AreEqual(c.typedData.[0], expected.[0])
        | _ -> Assert.IsTrue(false)

        let typedCol = typedFrame.GetColumnByName("b")
        let expectedBO = [|Some true; None|]
        match typedCol with
        | BO c -> Assert.AreEqual(c.typedData.[1], expectedBO.[1])
        | _ -> Assert.IsTrue(false)
        
