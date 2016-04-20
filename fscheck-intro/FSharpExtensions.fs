module FSharpExtensions


module Seq =
    let tryHead (s : 't seq) = 
        if Seq.isEmpty s
        then None
        else Some (Seq.head s)

    let tail (s : 't seq) = s |> Seq.skip 1

module Map =
    let updateWith f key map =
        let inner v map =
            match f v with
            | Some value -> map |> Map.add key value
            | None -> map |> Map.remove key
        match Map.tryFind key map with
        | Some v -> inner v map
        | None -> map

    /// Retrieves the keys from a Map    
    let keys (map : Map<'T,'b>) = 
        map |> Map.toSeq |> Seq.map fst