module Temper.Tests

open NUnit.Framework
open Temper
open Temper.Data

// Test categories to do:
//  Parsing + Sem checks
//  Reading
//  Writing

[<SetUp>]
let Setup () =
    ()

let getReaderExpectNoWarning str =
    match Template.fromString str with
    | Ok tmp -> printfn "%A" tmp; tmp
    | x -> failwithf "Expected template parse with no warnings but got %A" x
    |> Reader.build

let reader = Reader.build
let writer = Writer.build
let read r str = match Reader.read r str with Result.Ok vars -> vars | Result.Error x -> failwithf "Expected successful read: %s" x

let exGoodRead r str =
    match Reader.read r str with
    | Result.Ok vars -> printfn "%A" vars
    | Result.Error x -> failwithf "Expected successful read: %s" x

let exBadRead r str =
    match Reader.read r str with
    | Result.Ok vars -> failwithf "Expected read to fail: %A" vars
    | Result.Error x -> printfn "%s" x

let write = Writer.toString

[<Test>]
let Test1 () =
    let r =
        getReaderExpectNoWarning
            """
            module % ModuleName: ident % =
                let % Ident: ident % = %: ModuleName | "()" %
            """
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

[<TestFixture>]
module Reading =

    [<Test>]
    let ExactString () =
        let r = getReaderExpectNoWarning """ %VAR:"HelloWorld"% World %:VAR% """

        exGoodRead r """ HelloWorld World HelloWorld """
        exBadRead r """ HelloWorld World . """
        exBadRead r """ HelloWorld World """
        exBadRead r """ HelloWorld HelloWorld """
        exBadRead r """ . World HelloWorld """

    [<Test>]
    let CIString () =
        let r = getReaderExpectNoWarning """ %VAR:^"Hello"% World %:VAR% """

        exGoodRead r """ hello World hello """
        exGoodRead r """ Hello World Hello """
        exGoodRead r """ HELLO World HELLO """
        exBadRead r """ hello World Hello """
        exBadRead r """ Hello World HELLO """

    [<Test>]
    let Optional () =
        let r = getReaderExpectNoWarning """ %VAR:"Hello"?% World %:VAR% """

        exGoodRead r """ Hello World Hello """
        exGoodRead r """  World  """
        exBadRead r """ Hello World  """
        exBadRead r """  World Hello """
        exBadRead r """ World """

    [<Test>]
    let List () =
        let r = getReaderExpectNoWarning """ %VAR:"Hello"*%World%:VAR% """

        exGoodRead r """ HelloHelloHelloWorldHelloHelloHello """
        exGoodRead r """ HelloWorldHello """
        exGoodRead r """ World """
        exBadRead r """ HelloWorld """
        exBadRead r """ WorldHelloHello """