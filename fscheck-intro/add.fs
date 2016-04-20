module add


let add a b = a + b

open FsCheck
open FsCheck.NUnit
open FsUnit

[<Property>]
let ``neutral element 0``(a,b) =
    add a b
    |> should equal (add b a)

        
module customGenerators =
    let generator = Arb.Default.Int32().Generator

    open NUnit.Framework

    [<Test>]
    let ``0 is neutral element - take 1``() =
        fun a -> add a 0 |> should equal a
        |> Prop.forAll (generator |> Arb.fromGen)
        |> Check.VerboseThrowOnFailure


    
    open FsCheckExtensions

    [<Test>]
    let ``0 is neutral element - take 2``() =
        fun a -> add a 0 |> should equal a
        |> Check.withGen generator


//
//
//type mygen =
//    static member g = 
//        gen {
//            let! a = Arb.Default.Int32().Generator
//            let! b = Arb.Default.Int32().Generator
//            let! c = Arb.Default.Int32().Generator
//            return (a, b, c)
//        }
//
//
//
//module customGenerators1 =
//    [<Property(Arbitrary=[|typeof<mygen>|])>]
//    let ``commutativity``(a,b) =
//        add a b
//        |> should equal (add b a)
//
//    open NUnit.Framework
//    open FsCheckExtensions
//
//    [<Test>]
//    let ``2nd try commutativity``() =
//        fun (a,b) -> add a b
//                     |> should equal (add b a)
//        |> Check.withGen mygen.g
//
//    [<Property(Arbitrary=[|typeof<mygen>|])>]
//    let ``associativity``(a,b,c) =
//        add a (add b c)
//        |> should equal (add (add a b) c)