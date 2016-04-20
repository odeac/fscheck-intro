module paranthesis

open FSharpExtensions

/// Note: the implementations already consider a string which can contain other characters 
/// besides paranthesis.


/// Implementation based on higher order functions
/// More readable but with 2 iterations over the input string
let isWellFormedHof (s:string) : bool =
    let accumulator level = function 
        | '(' -> level + 1
        | ')' -> level - 1
        | _ -> level

    let levels = Seq.scan accumulator 0 s

    let tooManyClosed = Seq.exists (fun level -> level < 0) levels

    let tooManyOpened = (Seq.last levels) > 0

    not tooManyClosed && not tooManyOpened

/// Implementation based on reccursion
/// Less readable but it does only one iteration over the input string
let isWellFormedReccursive (s:string) : bool =
    let rec impl level (chars : char seq) =
        match Seq.tryHead chars with
        | None -> level = 0
        | Some head ->
            let rest = Seq.tail chars
            match head with
            | '(' -> impl (level + 1) rest
            | ')' -> level > 0 && impl (level - 1) rest
            | _ -> impl level rest
    impl 0 s


let isWellFormed = isWellFormedHof

module tests =
    open NUnit.Framework
    open FsCheck
    open FsCheck.NUnit
    open FsCheckExtensions
    open FsUnit

    open System

    /// For the property tests input strings up to 10 characters in length
    /// are good enough in this case
    let [<Literal>] maxLength = 10

    module Gen =
        /// generator that produces random strings containing only paranthesis 
        let parens = Gen.stringUpToLength maxLength ['('; ')']

        /// generator that produces random strings without paranthesis 
        let nonParens = 
            let nonParens = [Char.MinValue..Char.MaxValue]
                            |> Seq.filter (fun c -> c <> '(' && c <> ')')
            Gen.stringUpToLength maxLength nonParens
            
    [<Property>]
    let ``GIVEN any non-null string EXPECT the two implementations are equivalent`` (s : NonNull<string>) =
        isWellFormedHof s.Get = (isWellFormedReccursive s.Get)
            
    [<Test>]
    let ``GIVEN any parenthesis string EXPECT the two implementations are equivalent`` () =
        fun s -> isWellFormedHof s = (isWellFormedReccursive s)
        |> Check.withGen Gen.parens

    [<Test>]
    let ``GIVEN input with odd number of paranthesis EXPECT it is incorrect`` () =
        fun (s : string) -> s.Length % 2 = 1 ==> (isWellFormed s |> not)
        |> Check.withGen Gen.parens

    [<Test>]
    let ``GIVEN s1 correct and s2 random WHEN inserting s1 anywhere inside s2 EXPECT doesn't affect correctness`` () =
        let g = gen {
                    let! s1 = Gen.parens
                    let! s2 = Gen.parens
                    let! position = Gen.choose (0, s2.Length)
                    return (s1, s2, position)
                }

        fun (correct : string, random : string, position : int)->
            isWellFormed correct
            ==> lazy (isWellFormed random = isWellFormed (random.Insert(position, correct)))

        |> Check.withGen g

    [<Test>]
    let ``GIVEN s1 EXPECT (s1) is the same`` () =
        fun (s : string) ->
            (isWellFormed s) ==> (isWellFormed ("(" + s + ")"))

        |> Check.withGen Gen.parens

    [<Test>]
    let ``GIVEN s1 correct and s2 without paranthesis WHEN inserting s2 anywhere EXPECT doesn't affect correctness`` () =
        let g = gen {
                    let! s1 = Gen.parens
                    let! s2 = Gen.nonParens
                    let! position = Gen.choose (0, s1.Length)
                    return (s1, s2, position)
                }

        fun (withParens : string, noParens : string, position : int)->
            isWellFormed withParens = isWellFormed (withParens.Insert(position, noParens))
        |> Check.withGen g
