namespace Temper

open Temper.Tree

module Template =

    let fromString str =
        match Parser.parseFragments str with
        | Result.Ok frags -> Ok { Body = frags }
        | Result.Error msg -> Error msg