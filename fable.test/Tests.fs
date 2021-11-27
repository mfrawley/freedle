namespace fable.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Frame

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
