module JSON


type JSObj = 
    internal |Content of List<string*JSVal>
    with 
        static member ofList items = 
            JSVal.JSObj (JSObj.Content (items))
        static member toList = function |JSObj.Content l -> l
        member x.ToList = JSObj.toList x
        member private x.Map = x.ToList |> Map.ofList
        member x.tryFind name = x.Map |> Map.tryFind name
            
and JSVal = 
    |JSStr of string
    |JSBool of bool
    |JSArr of JSVal list
    |JSObj of JSObj
    |JSInt of int
    |JSNull
        
        
module private Parse =
    type token =
        | WhiteSpace
        | Symbol of char
        | StrToken of string
        | NumToken of int
        | BoolToken of bool

    let (|Match|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some m.Value else None

    let bool = System.Boolean.Parse
    let unquote (s:string) = s.Substring(1,s.Length-2)

    let toToken = function
        | Match @"^\s+" s -> s, WhiteSpace
        | Match @"^""[^""\\]*(?:\\.[^""\\]*)*""" s -> s, s |> unquote |> StrToken
        | Match @"^\{|^\}|^\[|^\]|^:|^," s -> s, s.[0] |> Symbol
        | Match @"^\d+(\.\d+)?|\.\d+" s -> s, s |> int |> NumToken
        | Match @"^true|false" s -> s, s |> bool |> BoolToken
        | _ -> invalidOp "Unknown token"

    let tokenize s =
        let rec tokenize' index (s:string) =
            if index = s.Length then []
            else
                let next = s.Substring index 
                let text, token = toToken next
                token :: tokenize' (index + text.Length) s
        tokenize' 0 s
        |> List.choose (function WhiteSpace -> None | t -> Some t)


    let rec (|ValueRec|_|) = function
        | NumToken n::t -> Some(JSInt n, t)
        | BoolToken b::t -> Some(JSBool b, t)
        | StrToken s::t -> Some(JSStr s, t)
        | Symbol '['::ValuesRec(vs, Symbol ']'::t) -> Some(JSArr vs,t)
        | Symbol '{'::PairsRec(ps, Symbol '}'::t) -> Some(JSObj ps,t)
        | [] -> Some(JSNull,[])
        | _ -> None
    and (|ValuesRec|_|) = function
        | ValueRec(p,t) ->
            let rec aux p' = function
                | Symbol ','::ValueRec(p,t) -> aux (p::p') t
                | t -> p' |> List.rev,t
            Some(aux [p] t)
        | _ -> None
    and (|PairRec|_|) = function
        | StrToken k::Symbol ':'::ValueRec(v,t) -> Some((k,v), t)
        | _ -> None
    and (|PairsRec|_|) = function
        | PairRec(p,t) ->
            let rec aux p' = function
                | Symbol ','::PairRec(p,t) -> aux (p::p') t
                | t -> p' |> List.rev,t
            let (items,c) = aux [p] t
            let a = (JSObj.Content (items))
            Some(a,c)
        | _ -> None

let tryParse s = 
    Parse.tokenize s |> function 
    | Parse.ValueRec(v,[]) -> Some v
    | _ -> None


let rec private formatStr (jsVal:JSVal) = 
    match jsVal with
    |JSStr s -> sprintf "\"%s\"" (s.Replace("\"", "\\\""))
    |JSNull -> "null"
    |JSInt i -> sprintf "%i" i
    |JSBool b -> if b then "true" else "false"
    |JSArr a -> sprintf "[%s]" (a |> List.map formatStr |> String.concat ",")
    |JSObj o -> 
        JSObj.toList o
        |> List.map (fun (k,v) -> sprintf "\"%s\":%s" k (formatStr v))
        |> String.concat ","
        |> (fun x -> sprintf "{%s}" x)

let formatToString jsVal = formatStr jsVal
