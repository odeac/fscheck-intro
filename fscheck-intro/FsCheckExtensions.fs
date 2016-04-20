module FsCheckExtensions

open FsCheck

module Check =
    let withGen g f = 
        let arb = Arb.fromGen g
        Prop.forAll arb f
        |> Check.VerboseThrowOnFailure
        
module Gen =
    open System

    let nonNullString = 
        Arb.Default.NonNull<string>().Generator
        |> Gen.map (fun x ->  x.Get)

    let nonNullStringList = Gen.listOf nonNullString

    let listUpToLength maxLen generator =
        gen {
            let! length = Gen.choose (0,maxLen)
            return! Gen.listOfLength length generator
        }

    let stringUpToLength (maxLen : int) (availableChars : char seq) =
        gen {
            let! lst = 
                Gen.elements availableChars
                |> listUpToLength maxLen 
            return String.Concat lst
        }