namespace HttpApp

type UploadedFile = {
    Filename : string
    Content : unit -> System.IO.Stream 
}

type HttpRequestForm = {
    Files : Map<string, UploadedFile> 
    Fields : Map<string,string list>
}


type CookieKey = string
type CookieValue = string

type Cookie = {
  Key : CookieKey
  Value : CookieValue
  Expiration : System.DateTime option
}

type HttpRequest = {
    Method : string
    Header : Map<string, string>
    Uri : System.Uri
    Cookies : Map<CookieKey, CookieValue> 
    ContentType : string
    Body : System.IO.Stream
    Querystring : Lazy<Map<string, string list>>
    Server : Lazy<Map<string, string list>>
}

type HttpResponse = {
    Content : System.IO.Stream -> unit
    ContentType : string
    HttpStatus : int
    Cookies : Cookie list
    Header : Map<string, string>
}


type HttpApplication<'a> = 'a -> HttpRequest-> HttpResponse option

module internal Dry= 
    module Common = 
        let parseKeyValuesEx (pairSplitter:char) (keyValueSplitter:char) (str:string) = 
            let pairs = str.Split([|pairSplitter|], System.StringSplitOptions.RemoveEmptyEntries)
            pairs 
            |> Array.map 
                (fun x -> 
                    let keyValues = x.Split(keyValueSplitter)
                    match keyValues with 
                    |[|key; value|] -> key, System.Uri.UnescapeDataString(value)
                    |a -> failwithf "Unexpected length of %A" a
                )
            |> Seq.groupBy fst
            |> Seq.map (fun (k,values) -> k, values |> Seq.map snd |> Seq.toList)
            |> Map.ofSeq 

        let parseKeyValues (str:string) = parseKeyValuesEx '&' '=' str


module Utils = 
//    open System.Net.Http
//    open System.Net.Http.Formatting 
    open System.Globalization

    let readUTF8Stream (body:System.IO.Stream) = 
        if body.CanSeek then
            body.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
        let inp = new System.IO.StreamReader(body, System.Text.Encoding.UTF8)
        let inpStr = inp.ReadToEnd()
        inpStr 


    let parseQuerystring (uri:System.Uri) = 
        Dry.Common.parseKeyValues (if uri.Query.Length > 0 then (uri.Query.Substring(1)) else "") 

    let parseRequestForm (contentType:string) (body:System.IO.Stream)  = 
        let e = body.Seek(0L, System.IO.SeekOrigin.Begin) 
        if System.String.IsNullOrEmpty(contentType) then
            //Read querystring
            { Files = Map.empty; Fields = Map.empty }
        else 
            let isMultiPart = if contentType.StartsWith("multipart/form-data;") then true else false
            match isMultiPart with
            |false -> 
                let all = body |> readUTF8Stream
                let v =  all |>  Dry.Common.parseKeyValues
        //        v, Map.empty 
                { Files = Map.empty; Fields = v }
            |true ->
    //            body.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                //if we use instead of let it disposes and we cant reuse body
                failwithf "Not implemented"
//                let streamContent = new System.Net.Http.StreamContent(body)
//                streamContent.Headers.ContentType <- System.Net.Http.Headers.MediaTypeHeaderValue.Parse(contentType);
//                let mutable result = None
//                //Sjukligt hackigt, fixa en egen parser
//                let a : System.Action<obj> = System.Action<obj>((fun a -> result <- Some (streamContent.ReadAsMultipartAsync().Result)))
//                System.Threading.Tasks.Task.Factory.StartNew(a, 1, System.Threading.CancellationToken.None, System.Threading.Tasks.TaskCreationOptions.LongRunning, System.Threading.Tasks.TaskScheduler.Default).Wait()
//                let form = result |> Option.get
//                let (fields, files) = 
//                    form.Contents 
//                    |> Seq.fold 
//                        (fun (fields, files) x -> 
//                            let fn (s:string) = if s = null then "" else s.Substring(1).Substring(0, s.Length - 2)
//                            let name = x.Headers.ContentDisposition.Name |> fn 
//                            if System.String.IsNullOrWhiteSpace(x.Headers.ContentDisposition.FileName |> fn)  then
//                                let s = x.ReadAsStreamAsync() |> Async.AwaitTask |> Async.RunSynchronously
//                                let c = readUTF8Stream s
//                            
//                                match fields |> Map.tryFind name with
//                                |Some e -> 
//                                    (fields |> Map.add name (c :: e), files)
//                                |None -> 
//                                    (fields |> Map.add name [c], files)    
//
//                            
//                            else
//                                let s = (fun () -> (x.ReadAsStreamAsync() |> Async.AwaitTask |> Async.RunSynchronously))
//                                (fields, (name, { UploadedFile.Filename = x.Headers.ContentDisposition.FileName |> fn; UploadedFile.Content = s }) :: files)
//                        )
//                        (Map.empty,[])
//                let fields = fields 
//                let files = files |> Map.ofList 
//                { Files = files; Fields = fields }


    let zip<'a> (a:HttpApplication<'a>) (b:HttpApplication<'a>) : HttpApplication<'a> = fun state r -> match a state r with |Some s -> Some s |None -> b state r
    let list<'a> (a:HttpApplication<'a> list) : HttpApplication<'a> = 
        (fun state r -> 
            a |> List.tryPick (fun x -> x state r)
        )



module Response =
    let strUtf8Response status contentType (str:string) headers =
        {
            HttpStatus = status
            ContentType = contentType
            Content =
                (fun s ->
                    let sw = new System.IO.StreamWriter(s, System.Text.Encoding.UTF8)
                    sw.Write(str)
                    sw.Flush()
                )
            Cookies = []
            Header = headers
       }
    let text (str:string) = strUtf8Response 200 "text/plain; charset=utf-8" str Map.empty
    let html (str:string) = strUtf8Response 200 "text/html; charset=utf-8" str Map.empty
    let json (str:string) = strUtf8Response 200 "application/json; charset=utf-8" str Map.empty

    let notFound404 = strUtf8Response 404 "text/plain; charset=utf-8" "404 Not found" Map.empty
    let redirect toLocation = 
        strUtf8Response 303 "text/plain; charset=utf-8" "300 Moved" (["Location", toLocation] |> Map.ofList)
    let unauthorized = strUtf8Response 401 "text/plain; charset=utf-8" "401 Unauthorized" Map.empty

    let withNoCache (r:HttpResponse) = 
        { 
            r with 
                Header = 
                r.Header 
                |> Map.add "Cache-Control" "no-cache, no-store, must-revalidate"
                |> Map.add "Pragma" "no-cache"
                |> Map.add "Expires" "0"
        }


    let file (stream:System.IO.Stream) contentType fileName disposeWhenDone =
        {
          Content = 
            (fun s -> 
                stream.CopyTo(s)
                if disposeWhenDone then
                    stream.Dispose()
            )
          ContentType = contentType
          Cookies = List.empty
          HttpStatus = 200
          Header = 
            ["Content-Disposition", "inline; filename=\"" + fileName + "\""]
            |> Map.ofList
        }

    let fileByte (data:byte[]) contentType fileName =
        let ms = new System.IO.MemoryStream(data)
        file ms contentType fileName true

    

  
module Route = 
    type ArgType = 
        |String 
        |Int32
        |Decimal of (System.Globalization.NumberStyles*System.IFormatProvider)

    type ArgValue = 
        |String of string
        |Int32 of int
        |Decimal of decimal

    type Segment = |Path of string |Arg of ArgType
    type UrlMatch = Segment list
    type ArgMatch = |Hit of ArgValue option |Miss 

    // "/test/{int32:orderid}"

    let private isMatch (segment:Segment) (sourcePath:string) = 
        match segment with
        |Path targetPath -> if targetPath = sourcePath then (Hit None) else Miss
        |Arg ArgType.String -> Hit (Some (ArgValue.String (sourcePath)))
        |Arg ArgType.Int32 -> match System.Int32.TryParse sourcePath with |true,v -> Hit (Some (ArgValue.Int32 v)) |_ -> Miss
        |Arg (ArgType.Decimal (ns, fp)) -> match System.Decimal.TryParse(sourcePath, ns, fp) with |true,v -> Hit (Some (ArgValue.Decimal v)) |_ -> Miss

    let rec innerUrlMatch (urlMatch:UrlMatch) (source:string list) (arguments:ArgValue option list) = 
        match urlMatch, source with
        |(tHead :: tBody), (sHead :: sBody) -> 
            match isMatch tHead sHead with
            |Hit (Some v) -> innerUrlMatch tBody sBody ((Some v) :: arguments)
            |Hit None -> innerUrlMatch tBody sBody ((None) :: arguments)
            |Miss -> None
        |_ :: _, [] -> None
        |[], _ :: _ -> None
        |[], [] -> Some (arguments |> List.rev)

    let urlListMatch urlMatch source = 
        innerUrlMatch urlMatch source []

    let urlMatch urlMatch (uri:System.Uri) = 
        
        let source = System.Uri.UnescapeDataString(uri.AbsolutePath).Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        innerUrlMatch urlMatch source []

    type Route<'a> = {
        HttpMethod : string
        Url : UrlMatch
        Action : 'a -> HttpRequest -> ArgValue list -> HttpResponse
    }

    let routeApplication<'a> (routes:Route<'a> list) =
        (fun state (request:HttpRequest) -> 
            routes
            |> List.tryPick 
                (fun r -> 
                    if request.Method = r.HttpMethod then
                        match urlMatch r.Url request.Uri with 
                        |Some s -> Some (r.Action state request (s |> List.choose id))
                        |None -> None
                     else None
                )
        )

    module Arg = 
        let string = Arg ArgType.String
        let int32 = Arg ArgType.Int32
        let decimalSwe = Arg (ArgType.Decimal (System.Globalization.NumberStyles.None ||| System.Globalization.NumberStyles.AllowDecimalPoint, upcast System.Globalization.CultureInfo.GetCultureInfo("sv-SE")))
        let decimal = Arg (ArgType.Decimal (System.Globalization.NumberStyles.None ||| System.Globalization.NumberStyles.AllowDecimalPoint, upcast System.Globalization.CultureInfo.InvariantCulture))

    //urlMatch [Path "Test"] (System.Uri("http://www.test.se/Test"))
    //urlMatch [Path "Test"; Arg ArgType.String] (System.Uri("http://www.test.se/Test/test"))
    //urlMatch [Path "Test"; Arg ArgType.Int32] (System.Uri("http://www.test.se/Test/56"))
    //urlMatch [Path "Test"; Arg.decimal] (System.Uri("http://www.test.se/Test/56.10"))
    //urlMatch [Path "Test"; Arg.decimal] (System.Uri("http://www.test.se/Test/56,10"))
    //urlMatch [Path "Test"; Arg.decimalSwe] (System.Uri("http://www.test.se/Test/56,10"))


    //let tupleToList<'a> (arg1:'a) = [arg1]
    //let listToTuple<'a> (args:'a list) =  
    //    match args with
    //    |[] -> unbox ()
    //    |[a] -> unbox (a)
    //    |[a1; a2] -> unbox (a1, a2)
    //    |a -> 
    //        let tupleDef = a |> List.map (fun _ -> typeof<'a>) |> List.toArray
    //        let tupleType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(tupleDef)
    //        Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(args |> List.map unbox |> List.toArray, tupleType)


    //Kan eg ligga utanför
    //type HttpMethod = string
    //type Route = {
    //    UrlMatch : UrlMatch
    //    HttpMethod : HttpMethod
    //}


    let private argTypeString (arg:obj) = 
        function
            |ArgType.String -> (arg :?> string)
            |(ArgType.Decimal (s,f)) -> (arg :?> decimal).ToString(f)
            |ArgType.Int32 -> (arg :?> int).ToString()

    let rec private getPath2 (route:UrlMatch) values parts = 
        match route, values with 
        |[], [] -> parts
        |[], h :: [] when h = null -> parts //Special case for unit
        |[], h -> failwithf "route at end, but we get values %A" h
        |Segment.Path a :: ar, [] -> getPath2 ar [] (a :: parts)
        |Segment.Path a :: ar, br -> getPath2 ar br (a :: parts)
        |Segment.Arg a :: ar, b :: br -> getPath2 ar br (argTypeString b a :: parts)
        |Segment.Arg a :: ar, [] -> failwithf "We receive arguments, but values are at end"



    let private unboxArgValue =
        function 
                |ArgValue.Decimal a -> unbox a
                |ArgValue.String a -> unbox a
                |ArgValue.Int32 a -> unbox a


    let route httpMethod url action = 
        {
            Action = action
            Url = url
            HttpMethod = httpMethod
        }

    module Reflection = 
        type TypedRoute<'a, 'state> = {
            Route : Route<'state>
            GetUrl : 'a -> string
        }
        type DefinedUrl<'a, 'state> = {
            Implementet : ('state -> HttpRequest -> 'a -> HttpResponse) -> Route<'state>
            GetUrl : 'a -> string
        }
        let getPathFromValue<'a> (route:UrlMatch) = 
            let values = 
                if Microsoft.FSharp.Reflection.FSharpType.IsTuple typeof<'a> then          
                    Microsoft.FSharp.Reflection.FSharpValue.PreComputeTupleReader typeof<'a>
                    >> List.ofArray
                else (fun x -> [x])

            (fun (x:'a) -> 
                getPath2 route (values x) [] |> List.rev
                |> String.concat "/"
                |> sprintf "/%s"
            )

        let getValueFromArgs<'a> : ArgValue list -> 'a = 
            if Microsoft.FSharp.Reflection.FSharpType.IsTuple typeof<'a> then          
                let crtr = Microsoft.FSharp.Reflection.FSharpValue.PreComputeTupleConstructor typeof<'a>
                (fun a -> a |> Array.ofList |> Array.map unboxArgValue |> crtr |> unbox)
            elif typeof<'a> = typeof<unit> then
                (fun _ -> unbox null)
            else
                function |[a] -> unboxArgValue a |_ -> failwithf "" 

        let typedRoute<'a, 'state> httpMethod url action = 
            let route = 
                route httpMethod url
                    (fun (state:'state) r p -> 
                        let t = getValueFromArgs<'a> p
                        action state r t
                    )
            let b = {
                Route = route
                GetUrl = getPathFromValue<'a> url
            }
            b

        let defineRoute<'a, 'state> httpMethod url = 
            let b : DefinedUrl<'a, 'state> = {
                Implementet = 
                    (fun implementor-> 
                        route httpMethod url
                            (fun (state:'state) r p -> 
                                let t = getValueFromArgs<'a> p
                                implementor state r t
                            )
                    )
                GetUrl = getPathFromValue<'a> url
            }
            b


    //Reflection.typedRoute "GET" [] (fun r () -> Response.text "Test")

    
    //let a = Reflection.getPathFromValue<decimal> [Path "Test"; Arg.decimal] 5M 
    //let b = a.Split([|'/'|]) |> Array.toList
    //let c = urlListMatch [Path "Test"; Arg.decimal] b
    //let d = Reflection.getValueFromArgs<decimal> (c |> Option.get |> List.choose id)
    
    
    //let a2 = Reflection.getPathFromValue<decimal*int> [Path "Test"; Arg.decimal; Path "Test2"; Arg.int32] (5M,67)
    //let b2 = a2.Split([|'/'|]) |> Array.toList
    //let c2 = urlListMatch [Path "Test"; Arg.decimal; Path "Test2"; Arg.int32] b2
    //let d2 = Reflection.getValueFromArgs<decimal*int> (c2 |> Option.get |> List.choose id)


