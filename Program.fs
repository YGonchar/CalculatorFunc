// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

module ArithmeticExpression =  

    type Result<'T> =
    | Value of 'T
    | Error of string

    type Parenthes =
    | Open
    | Close

    type Operator = char
    type Number = char

    type LiteralExpression =
    | Number of Number
    | Operator of Operator
    | Dot
    | Parenthes of Parenthes

    type ValueExpr = 
        { Values:LiteralExpression list }
    type OperationExpr = 
        { Operation:Operator 
          Priority:int }
    type Expression = 
        | ValueExp of ValueExpr 
        | OperationExp of OperationExpr

    let unfoldResult (res:Result<'T>) =
        match res with
        | Value s -> Some s
        | _ -> None

    let (|NumberPat|_|) (input:char) =
        match input |> Char.IsDigit with
        | true -> Some(input)
        | _ -> None

    let (|OperatorPat|_|) (input:char) =
        match input with
        | '+' | '-' | '*' | '/' -> OperatorPat input |> Some
        | _ -> None

    let (|ParenthesPat|_|) = function
        | '(' -> Open |> Some
        | ')' -> Close |> Some
        | _ -> None
        
    let matchLiterals = function
        | NumberPat n -> n |> Number |> Value
        | OperatorPat op -> op |> Operator |> Value
        | ParenthesPat p -> p |> Parenthes |> Value
        | '.' -> Dot |> Value        
        | sym -> Error(sprintf "Invalid symbol %A" sym)

    let parseLiterals (input:string): Result<LiteralExpression seq> =    
        if String.IsNullOrEmpty(input) then Error("Expression cannot be empty")
        else
            let expressions = 
                input 
                |> Seq.map matchLiterals
            let errors = 
                expressions                
                |> Seq.choose(function
                    |Error err -> Some err
                    | _ -> None)                
            if errors |> Seq.isEmpty then 
                expressions 
                |> Seq.choose unfoldResult
                |> Value
            else
                errors
                |> Seq.fold (sprintf "%s\n%s") String.Empty
                |> Error

    let rec setPriorities (literals:LiteralExpression list) (expressions:Expression list) (level:int): unit = 
        let matchVals = function
            |Number n, ValueExp vex -> ()
            |Number n, OperationExp oex -> ()
            |Operator o, ValueExp vex -> ()
            |Operator o, OperationExp oex -> ()
            |Dot, ValueExp vex -> ()
            |Dot, OperationExp vex -> ()
            |Parenthes p, ValueExp vex -> ()
            |Parenthes p, OperationExp oex -> ()
        ()


open ArithmeticExpression
[<EntryPoint>]
let main _ =
    Console.WriteLine("Input arithmetic expression")
    let getInitialValues (literals:LiteralExpression list):(LiteralExpression list * Expression list) = 
        match literals with
        | Operator o :: rest when o = '-' -> (rest, [OperationExp { Operation = '-'; Priority = 0}])
        | all -> (all, [OperationExp { Operation = '+'; Priority = 0}])
    
    match Console.ReadLine() |> parseLiterals with
    | Value e -> 
        let literals, expressions = e |> Seq.toList |> getInitialValues
        setPriorities literals expressions 0 |> ignore
        ()
    | Error err -> Console.WriteLine(err)
    0 // return an integer exit code