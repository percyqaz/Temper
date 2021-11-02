module Temper.Tests

open NUnit.Framework
open Temper
open Temper.Data

[<SetUp>]
let Setup () =
    ()

let getTemplateExpectNoWarning str =
    match Template.fromString str with
    | Ok tmp -> tmp
    | x -> failwithf "Expected template parse with no warnings but got %A" x

let reader = Reader.build
let writer = Writer.build
let read r str = match Reader.read r str with Result.Ok vars -> vars | Result.Error x -> failwithf "Expected successful read: %s" x
let write = Writer.toString

[<Test>]
let Test1 () =
    let tmp =
        getTemplateExpectNoWarning
            """
            module %ModuleName:ident% =
                let %Ident:ident% = %:ModuleName|"()"%
            """
    printfn "%A" tmp
    let r = reader tmp
    Assert.AreEqual(
        Map.ofList [("ModuleName", String "TestModule"); ("Ident", String "Hello")],
        read r 
            """
            module TestModule =
                let Hello = TestModule
            """
    )
    Assert.AreEqual(
        Map.ofList [("ModuleName", String "TestModule"); ("Ident", String "Hello")],
        read r 
            """
            module TestModule =
                let Hello = ()
            """
    )
