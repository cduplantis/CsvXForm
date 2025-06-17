namespace CsvXForm

[<AutoOpen>]
module Validation =

    // Computation expression builder for accumulating errors
    type ValidatedOrderBuilder() =
        member _.Return(x) = Ok x
        member _.ReturnFrom(x) = x

        member _.Bind(x, f) =
            match x with
            | Ok v -> f v
            | Error e -> Error e

        member _.MergeSource(t1, t2) =
            match t1, t2 with
            | Ok v1, Ok v2 -> Ok(v1, v2)
            | Error e, Ok _ -> Error e
            | Ok _, Error e -> Error e
            | Error e1, Error e2 -> Error(e1 + "; " + e2)

        member this.Source(x) = x

    let validated = ValidatedOrderBuilder()



[<AutoOpen>]
module Utils =
    open System.Collections.Generic
    // Helper function to safely access dictionary values
    let getField (fields: Dictionary<string, string>) key =
        match fields.TryGetValue(key) with
        | true, value -> Some value
        | _ ->
            printfn "Warning: Missing field '%s'" key
            None
