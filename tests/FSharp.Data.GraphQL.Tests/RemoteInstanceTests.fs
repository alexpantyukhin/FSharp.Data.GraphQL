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
        abstract Weight: int
    end 

type Dog = 
    { Name: string; Woofs: bool; Weight: int }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight

type Cat = 
    { Name: string; Meows: bool; Weight: int }
    interface IPet with
        member x.Name = x.Name
        member x.Weight = x.Weight

type Human = { Name: string; }

type Pet =
    | DogCase of Dog
    | CatCase of Cat


type CustomRemoteInstance() = 
    let remoteStrategy resolveCtx obj = 
//        Unchecked.defaultof<AsyncVal<obj>>
        AsyncVal.wrap(
                    NameValueLookup.ofList [
                              "pets", upcast [
                                NameValueLookup.ofList [
                                    "name", "Odie" :> obj
                                    "woofs", upcast true ] :> obj
                                upcast NameValueLookup.ofList [
                                    "name", "Garfield" :> obj
                                    "meows", upcast false]]] :> obj
                      )

    interface RemoteInstance with
        member x.RemoteStrategy resolveCtx obj = remoteStrategy resolveCtx obj

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () = 
    let remoteInstace = CustomRemoteInstance() :> RemoteInstance
    //let 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String) ])
    let DogType =
      Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ d -> d.Name)
            Define.Field("woofs", Boolean, fun _ d -> d.Woofs)
            Define.Field("weight", Int, fun _ d -> d.Weight)
        ])
    let CatType =
      Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String, fun _ c -> c.Name)
            Define.Field("meows", Boolean, fun _ c -> c.Meows)
            Define.Field("weight", Int, fun _ d -> d.Weight)
        ])
    let schema =
      Schema(
        query = Define.Object("Query", fun () ->
        [
            Define.Field("pets", ListOf PetType, (fun _ _ -> [ { Name = "Odie"; Woofs = true; Weight=10 } :> IPet ; upcast { Name = "Garfield"; Meows = false; Weight=20 } ]))//, remoteInstace)
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



//type Widget = 
//    { Id: string;
//      Name: string }

//type User = 
//    { Id: string;
//      Name: string;
//      Widgets: Widget list }

//[<Fact>]
//let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface1111`` () = 
//    let rec Widget = Define.Object<Widget>(
//        name = "Widget",
//        description = "A shiny widget",
//        interfaces = [ Node ],
//        fields = [
//            Define.GlobalIdField(fun _ w -> w.Id)
//            Define.Field("name", String, fun _ w -> w.Name)])

//    and User = Define.Object<User>(
//        name = "User",
//        description = "A person who uses our app",
//        interfaces = [ Node ],
//        fields = [
//            Define.GlobalIdField(fun _ w -> w.Id)
//            Define.Field("name", String, fun _ w -> w.Name)
//            Define.Field("widgets", ConnectionOf Widget, "A person's collection of widgets", Connection.allArgs, fun ctx user -> 
//                let widgets = user.Widgets |> List.toArray
//                Connection.ofArray widgets )])

//    and Node = Define.Node<obj>(fun () -> [ User; Widget ])
//    ()

