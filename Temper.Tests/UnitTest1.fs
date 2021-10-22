module Temper.Tests

open NUnit.Framework
open Temper

[<SetUp>]
let Setup () =
    ()

let getTemplateExpectNoWarning str =
    match Template.Parser.parseTemplate str with
    | Ok (tmp, []) -> tmp
    | x -> failwithf "Expected template parse with no warnings but got %A" x

let reader = Template.Reader.build
let writer = Template.Writer.build
let read = Template.Reader.read
let write = Template.Writer.toString

[<Test>]
let Test1 () =
    let tmp =
        getTemplateExpectNoWarning
            "module {{ModuleName:ident}} =\n\
            \tlet {{Ident:ident}} = {{:ModuleName|\"()\"}}"
    let r = reader tmp
    Assert.AreEqual(
        Map.ofList [("ModuleName", "TestModule"); ("Ident", "Hello")],
        read r "module TestModule = \n\
                \tlet Hello = TestModule"
    )
