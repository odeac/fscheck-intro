module fibonacci

open NUnit.Framework
open FsCheck.NUnit
open FsUnit
open FsCheck
open FsCheckExtensions

let fibs : int seq = 
    let rec generator current next = 
        seq {
            yield current
            yield! generator next (current + next)
        }
    generator 0 1




[<Test>]
let ``First 5 fibs are [0;1;2;3;5]`` () =
    fibs
    |> Seq.take 5
    |> List.ofSeq
    |> should equal [0;1;1;2;3]



[<Property>]
let ``consecutive fibs a,b,c satisfy c = a + b - take 1`` (n : int) =
    let [a;b;c] = fibs
                    |> Seq.take (n + 3)
                    |> Seq.skip n
                    |> List.ofSeq
    a + b
    |> should equal c


[<Property>]
let ``consecutive fibs a,b,c satisfy c = a + b - take 2`` (n : int) =
    n >= 0 ==> lazy (let [a;b;c] = fibs
                                     |> Seq.take (n + 3)
                                     |> Seq.skip n
                                     |> List.ofSeq
                     a + b
                     |> should equal c)


[<Property>]
let ``consecutive fibs a,b,c satisfy c = a + b - take 3`` (n : PositiveInt) =
    let [a;b;c] = fibs
                    |> Seq.skip n.Get
                    |> Seq.take 3
                    |> List.ofSeq
    a + b
    |> should equal c

[<Property>]
let ``All fibs are positive``(n : PositiveInt) =
    fibs
    |> Seq.nth n.Get
    |> should be (greaterThanOrEqualTo 0)