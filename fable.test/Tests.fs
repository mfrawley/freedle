namespace fable.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Freedle.Frame

type FooRecord = {
    yeah: string
    nope: int
}
    
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
        let f : TypedFrame = [
            (F {
                name="foo"
                typedData=[|0.0|]
            })
            (S {
                name="bar"
                typedData=[|"sdfdsf"|]
            })            
        ]
        Assert.IsTrue true


    [<TestMethod>]
    member _.TestgetRecordFieldTuples () =
        let fooInst = {
            yeah="sf"
            nope=234
        }
        let fieldNames = getRecordFieldTuples(fooInst.GetType())
        
        Assert.AreEqual ((fst fieldNames.[0]), "yeah")
        Assert.AreEqual ((snd fieldNames.[0]), "System.String")

        Assert.AreEqual ((fst fieldNames.[1]), "nope")
        Assert.AreEqual ((snd fieldNames.[1]), "System.Int32")

    [<TestMethod>]
    member _.TestgetRecordFieldsOnCol () =
        let myFloatCol = {
            name="sfds"
            typedData = [|0.0|]
        }
        let fieldNames = getRecordFieldTuples(myFloatCol.GetType())
        printfn "%A" fieldNames
        Assert.AreEqual ((fst fieldNames.[0]), "name")
        Assert.AreEqual ((snd fieldNames.[0]), "System.String")

        Assert.AreEqual ((fst fieldNames.[1]), "typedData")
        Assert.AreEqual ((snd fieldNames.[1]), "System.Double[]")