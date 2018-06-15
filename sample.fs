module Presentation
open System
open System.Drawing

// simple
type Nothing = unit
type ChannelId = int
type KeyList = list<string> // or... = string list
type MyFun = int -> string

let keys: KeyList = ["12"; "key2"; "key3"]

// Product type
type Timestamp = int * DateTime
type Message =
    { ts: Timestamp
      text: string }
let helloMessage = {ts = (0, DateTime.Now); text = "hello"}

// Sum Types
type ServerCommand =
  | Join of name: string
  | Create of name: string
  | Leave of int
  | Ping

let join = Join "dotnet-flood"

type List<'T> =
    | Nil
    | Cons of 'T * List<'T>
let list = Cons(1, Cons(2, Cons (3, Nil)))

let foo = 1 :: 2 :: 3 :: List.Empty
let listToo = [1; 2; 3]


// DDD
type Contact = {
    FirstName: string;
    MiddleInitial: string;
    LastName: string;
    EmailAddress: string;
    IsEmailVerified: bool;
}

module Refactored =
    type PersonalName = {
        FirstName: string;
        MiddleInitial: string option;
        LastName: string;
        }
    type EmailAddress = EmailAddress of string
    type EmailContact =
        | Unverified of EmailAddress
        | Verified of EmailAddress * DateTime
    type ContactMethod = 
        | Email of EmailContact 
        | PostalAddress of unit // FIXME
    type Contact = {
        Name: PersonalName;
        PrimaryContact: ContactMethod
        }


// Pattern matching
type Shape =
    | Circle of float
    | Square of float
    | Rectangle of float * float

let area s =
    match s with
    | Circle r -> Math.PI * r * r
    | Square x -> x * x
    | Rectangle (w,h) -> w * h
   


let rec listLen ls =
    match ls with
    | Nil -> 0
    | Cons (head, tail) -> 1 + (listLen tail)

let rec isEven ls =
    match ls with
    | Nil -> true
    | Cons (_, Cons (_, tail)) -> isEven tail
    | _ -> false

let rec pairProduct ls =
    match ls with
    | Cons (a, Cons (b, tail)) -> Cons(a * b, pairProduct tail)
    | other -> other

let (|IsChannelId|_|) s = 
    match Int32.TryParse s with
    | true, value -> Some value
    | _ -> None

let (|IsChanId|_|) = 
    Int32.TryParse >> function
    | true, value -> Some value
    | _ -> None

let probeChan = function
    | IsChannelId chanId -> printf "%i" chanId
    | _ -> printf "not a channel identifier"


type UserInfo = {ident: int; name: string}
// immutability
type Channel = {
    Messages: Message list
    Info: string
    Users: UserInfo list
    PostText: string
}
let chan = {Messages = []; Info = "None"; Users = []; PostText = ""}

type ChannelMessage =
    | Init of string * UserInfo list
    | Update of string
    | AppendMessage of Message

let update (msg: ChannelMessage) (channel: Channel) : Channel =

    match msg with
    | Init (info, userlist) ->
        { Info = info; Messages = []; PostText = ""; Users = userlist}
    | Update info ->
        { channel with Info = info }

    | AppendMessage message ->
        { channel with Messages = channel.Messages @ [message] }


/// type inference

let f = (1 = 2)
// bool

let add a b = a + b
// int -> int -> int

let Or a b = a || b
// bool -> bool -> bool

let addFloat (a: float) b : float = a + (b : float)
// float -> float -> float


// computation expressions (in Fable + power pack)
let fetchEntity url =
    promise {
        let! fetched = fetch url []
        let! response = fetched.text()
        return response
    }

//

type Simple = JsonProvider<""" { "name":"John", "age":94 } """>
let simple = Simple.Parse(""" { "name":"Tomas", "age":4 } """)
simple.Age
simple.Name

// elmish
type Model = int
type Msg = | Inc | Dec

let init() : Model = 0

let view model dispatch =
    div []
        [ button [ OnClick (fun _ -> dispatch Dec) ] [ "-" ]
          div [] [ model.ToString() ]
          button [ OnClick (fun _ -> dispatch Inc) ] [ "+" ] ]

let update msg (model: Model) =
    match msg with
    | Inc -> model + 1
    | Dec -> model - 1

Program.mkProgram init update view
|> Program.withDebugger
|> Program.withHMR
|> Program.withReact "elmish-app"
|> Program.run