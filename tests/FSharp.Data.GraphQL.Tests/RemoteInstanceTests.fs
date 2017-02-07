module RemoteInstanceTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Relay

type IPet =
    interface
        abstract Name : string
    end 

type Dog = 
    { Name: string; Woofs: bool }
    interface IPet with
        member x.Name = x.Name

type Cat = 
    { Name: string; Meows: bool }
    interface IPet with
        member x.Name = x.Name

type Human = { Name: string; }

type Pet =
    | DogCase of Dog
    | CatCase of Cat


type CustomRemoteInstance() = 
    let remoteStrategy: ResolveFieldContext -> obj -> AsyncVal<obj> = Unchecked.defaultof<ResolveFieldContext -> obj -> AsyncVal<obj>>

    interface RemoteInstance with
        member x.RemoteStrategy resolveCtx obj = remoteStrategy resolveCtx obj

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () = 
    let remoteInstace = CustomRemoteInstance() :> RemoteInstance
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String) ])
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ c -> c.Name)
            Define.Field("meows", Boolean, fun _ c -> c.Meows)
        ])
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, (fun _ _ -> [ { Name = "Odie"; Woofs = true } :> IPet ; upcast { Name = "Garfield"; Meows = false } ]), remoteInstace)
        ]), 
        config = { SchemaConfig.Default with Types = [CatType; DogType] })
    let schemaProcessor = SchemaProcessor(schema)
    let query = """{
      pets {
        name
        ... on Dog {
          woofs
        }
        ... on Cat {
          meows
        }
      }
    }"""
    let result = sync <| schemaProcessor.AsyncExecute(parse query)
    let expected =
      NameValueLookup.ofList [
          "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    result.["data"] |> equals (upcast expected)