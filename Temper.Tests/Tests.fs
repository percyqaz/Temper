module Temper.Tests

open NUnit.Framework
open Temper
open Temper.Tree

// Test categories to do:
//  Parsing + Sem checks
//  Reading
//  Writing

[<SetUp>]
let Setup () =
    ()

let getReaderExpectNoWarning str =
    match Template.fromString str with
    | Ok tmp -> printfn "Template info -- %A" tmp; tmp
    | x -> failwithf "Expected template parse with no warnings but got %A" x
    |> Read.reader

let empty_ctx = Context.Empty
//let reader = Reader.build
//let writer = Writer.build
let read r str = match Read.read r empty_ctx str with Result.Ok vars -> vars | Result.Error x -> failwithf "Expected successful read: %s" x

let exGoodRead r str =
    match Read.read r empty_ctx str with
    | Result.Ok vars -> printfn "Good read -- %A" vars
    | Result.Error x -> failwithf "Expected successful read: %s" x

let exBadRead r str =
    match Read.read r empty_ctx str with
    | Result.Ok vars -> failwithf "Expected read to fail: %A" vars
    | Result.Error x -> printfn "Intended error -- %s" x

//let write = Writer.toString

[<Test>]
let Test1 () =
    let r =
        getReaderExpectNoWarning
            """%* Comment *%
            module % ModuleName: ident % =
                let % Ident: ident % = %: ModuleName | "()" %
            """
    Assert.AreEqual(
        Map.ofList [("ModuleName", Val.Str "TestModule"); ("Ident", Val.Str "Hello")],
        read r 
            """
            module TestModule =
                let Hello = TestModule
            """
    )
    Assert.AreEqual(
        Map.ofList [("ModuleName", Val.Str "TestModule"); ("Ident", Val.Str "Hello")],
        read r 
            """
            module TestModule =
                let Hello = ()
            """
    )

[<TestFixture>]
module Patterns =

    [<Test>]
    let ExactString () =
        let r = getReaderExpectNoWarning """ %VAR:"HelloWorld"% World %:<VAR>% """

        exGoodRead r """ HelloWorld World HelloWorld """
        exBadRead r """ HelloWorld World . """
        exBadRead r """ HelloWorld World """
        exBadRead r """ HelloWorld HelloWorld """
        exBadRead r """ . World HelloWorld """

    [<Test>]
    let CIString () =
        let r = getReaderExpectNoWarning """ %VAR:^"Hello"% World %:<VAR>% """

        exGoodRead r """ hello World hello """
        exGoodRead r """ Hello World Hello """
        exGoodRead r """ HELLO World HELLO """
        exBadRead r """ hello World Hello """
        exBadRead r """ Hello World HELLO """

    [<Test>]
    let Regex () =
        let r = getReaderExpectNoWarning """ %VAR:r"[a-z.]*"% X %:<VAR>% """

        exGoodRead r """ hello X hello """
        exGoodRead r """ zoo.wee.mama X zoo.wee.mama """
        exGoodRead r """ ... X ... """
        exBadRead r """ .. X . """
        exBadRead r """ aA X aA """

    [<Test>]
    let Optional () =
        let r = getReaderExpectNoWarning """ %VAR:"Hello"?% World %:<VAR>% """

        exGoodRead r """ Hello World Hello """
        exGoodRead r """  World  """
        exBadRead r """ Hello World  """
        exBadRead r """  World Hello """
        exBadRead r """ World """

    [<Test>]
    let List () =
        let r = getReaderExpectNoWarning """ %VAR:"Hello"*%World%:[VAR|<it>]% """

        exGoodRead r """ HelloHelloHelloWorldHelloHelloHello """
        exGoodRead r """ HelloWorldHello """
        exGoodRead r """ World """
        exBadRead r """ HelloWorld """
        exBadRead r """ WorldHelloHello """

    [<Test>]
    let Definition () =
        let r = getReaderExpectNoWarning """
            %-# HelloWorlds = (^"Hello" | "World")* -%
            %- VAR: #HelloWorlds % | %: [VAR|<it>] -%
            """

        exGoodRead r """HellohelloWorldHELLO | HellohelloWorldHELLO"""
        exGoodRead r """ | """
        exGoodRead r """World | World"""
        exBadRead r """HELLO | """
        exBadRead r """helloWorld | HelloWorld"""

    [<Test>]
    let Object () =
        let r = getReaderExpectNoWarning """
            %-#Obj = <#let %N-% = %-V-%#> -%
            %-#ObjR = <#let %:<N>-% = %-:<V>-%#> -%
            % VAR: #Obj % | %: {VAR|#ObjR} -%
            """

        exGoodRead r """let x = y | let x = y"""
        exGoodRead r """let 12345 = 0 | let 12345 = 0"""
        exBadRead r """let 5 = 1 | let 5 = 0"""
        exBadRead r """let 5 = | let 5 = 0"""