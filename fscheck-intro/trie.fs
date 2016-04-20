module trie

open FSharpExtensions

type Trie = Trie of Map<char option, Trie>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Trie =
    let empty = Trie Map.empty

    let rec addWord (trie : Trie) (word : char seq) : Trie =
        let (Trie trieImpl) = trie

        if Seq.isEmpty word
        then
            Map.add None empty trieImpl
        else
            let firstChar = Seq.head word |> Some
            let remainingChars = Seq.tail word

            let addTail subTrie = addWord subTrie remainingChars |> Some

            if Map.containsKey firstChar trieImpl
            then Map.updateWith addTail firstChar trieImpl
            else 
                 let newChild = addWord empty remainingChars
                 Map.add firstChar newChild trieImpl
        |> Trie

    /// Creates a Trie containing the given words
    let ofWords (words : string seq): Trie = Seq.fold addWord empty words

    ///Returns true if the word can be found in the Trie
    let rec contains (word : char seq) (t : Trie) : bool = 
        let (Trie trieImpl) = t

        let firstChar = Seq.tryHead word

        Map.tryFind firstChar trieImpl
        |> function
        | None -> false
        | Some subTrie-> (Seq.isEmpty word) || (contains (Seq.tail word) subTrie)

    /// Returns the next chars after the prefix based on the words stored in the Trie
    let getNextChars (prefix: char seq) (trie : Trie) : char seq =
        let rec impl charsLeft trieLeft =
            if Seq.isEmpty charsLeft
            then
                Map.keys trieLeft
                |> Seq.choose id
            else
                let firstChar = Seq.head charsLeft |> Some
                let remainingChars = Seq.tail charsLeft

                Map.tryFind firstChar trieLeft
                |> function
                | Some (Trie subTrie)-> impl remainingChars subTrie
                | _ -> Seq.empty

        let (Trie trieImpl) = trie
        impl prefix trieImpl

module tests =
    open NUnit.Framework
    open FsCheck
    open FsCheckExtensions
    open System

    [<Test>]
    let ``trie behaves like a set of words`` () =
        fun (wordsToAdd : string list, extraWords : string list) ->
            let trieRepr = Trie.ofWords wordsToAdd
            let setRepr = Set.ofList wordsToAdd

            let trieBehavesLikeSet word =
                (setRepr.Contains word) = (trieRepr |> Trie.contains word)

            List.concat [wordsToAdd; extraWords]
            |> List.forall trieBehavesLikeSet

        |> Check.withGen (Gen.two Gen.nonNullStringList)

    [<Test>]
    let ``After Insert prefix + suffix getNextChars returns first chars in suffixes`` () =
        let generator = 
            gen {
                let! prefix = Gen.nonNullString
                let! suffixes = Gen.nonNullStringList
                return (prefix, suffixes)
            }

        fun (prefix : string, suffixes : string list) ->
            let trie = 
                suffixes
                |> List.map (fun suffix -> String.Concat [prefix; suffix])
                |> Trie.ofWords

            let expectedChars = 
                suffixes
                |> Seq.choose Seq.tryHead
                |> Seq.distinct
                |> Seq.sort
                |> List.ofSeq

            let nextChars = 
                Trie.getNextChars prefix trie
                |> Seq.sort
                |> List.ofSeq 

            nextChars = expectedChars

        |> Check.withGen generator