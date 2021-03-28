// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
module ArithmeticExpression =  

    type Result<'T> =
    | Value of 'T
    | Error of string

    type Parenthes =
    | Open
    | Close
    type OperatorKind =
    |Unary
    |Binary

    type Operator = char
    type Number = char

    type LiteralExpression =
    | Number of Number
    | Operator of Operator
    | Dot
    | Parenthes of Parenthes

    let unfoldResult (res:Result<'T>) =
        match res with
        | Value s -> Some s
        | _ -> None

    let (|Number|_|) (input:char) =
        match input |> Char.IsDigit with
        | true -> Some(input)
        | _ -> None

    let (|Operator|_|) (input:char) =
        match input with
        | '+' | '-' | '*' | '/' -> Operator input |> Some
        | _ -> None

    let (|Parenthes|_|) = function
        | '(' -> Open |> Some
        | ')' -> Close |> Some
        | _ -> None
        
    let matchExpression = function
        | Number n -> n |> Number |> Value
        | Operator op -> op |> Operator |> Value
        | Parenthes p -> p |> Parenthes |> Value
        | '.' -> Dot |> Value        
        | sym -> Error(sprintf "Invalid symbol %A" sym)

    let parseLiterals (input:string): Result<LiteralExpression list> =    
        if String.IsNullOrEmpty(input) then Error("Expression cannot be empty")
        else
            let expressions = 
                input 
                |> Seq.toList 
                |> (fun lst -> lst @ [' '])
                |> List.map matchExpression
            let errors = 
                expressions                
                |> List.choose(fun res ->
                    match res with
                    |Error err -> Some err
                    | _ -> None)                
            if errors |> List.isEmpty then 
                expressions 
                |> List.choose unfoldResult
                |> Value
            else
                errors
                |> Seq.fold (sprintf "%s\n%s") String.Empty
                |> Error

open ArithmeticExpression
[<EntryPoint>]
let main _ =
    Console.WriteLine("Input arithmetic expression")
    match Console.ReadLine() |> parseLiterals  with
    | Value e -> e |> Seq.iter(fun exp -> Console.WriteLine(exp))
    | Error err -> Console.WriteLine(err)
    0 // return an integer exit code