namespace HttpApp.Swagger

(*
TODO:

- deserialiser borde vara av typen: JSVal -> 'a Result
- Skriva tester
- Stöd för authentication. Någon sorts SwaggerApplication-impl.?
- Stöd för option-typer!
- Typade Url:ar? (= öppen för t.ex. typprovider impl.)

*)

open JSON
open HttpApp

//// Route.fs
//module Route =
//    type DecimalFormat = (System.Globalization.NumberStyles*System.IFormatProvider)
//    type ArgType = 
//        |String 
//        |Int32
//        |Decimal of DecimalFormat
    
//    type ArgValue = 
//        |String of string
//        |Int32 of int
//        |Decimal of decimal
    
//    type Segment = |Path of string |Arg of ArgType
//    type UrlMatch = Segment list
//    type ArgMatch = |Hit of ArgValue option |Miss 
    
//    let isMatch (segment:Segment) (sourcePath:string) = 
//        match segment with
//        |Path targetPath -> if targetPath = sourcePath then (Hit None) else Miss
//        |Arg ArgType.String -> Hit (Some (ArgValue.String sourcePath))
//        |Arg ArgType.Int32 -> match System.Int32.TryParse sourcePath with |true,v -> Hit (Some (ArgValue.Int32 v)) |_ -> Miss
//        |Arg (ArgType.Decimal (numberStyles, formatProvider)) -> 
//            System.Decimal.TryParse(sourcePath, numberStyles, formatProvider)
//            |> function |(true, v) -> Hit (Some (ArgValue.Decimal v)) 
//                        |_ -> Miss
    
//    let rec innerUrlMatch (urlMatch:UrlMatch) (source:string list) (arguments:ArgValue option list) = 
//        match urlMatch, source with
//        |(tHead :: tBody), (sHead :: sBody) -> 
//            match isMatch tHead sHead with
//            |Hit (Some v) -> innerUrlMatch tBody sBody ((Some v) :: arguments)
//            |Hit None -> innerUrlMatch tBody sBody ((None) :: arguments)
//            |Miss -> None
//        |_ :: _, [] -> None
//        |[], _ :: _ -> None
//        |[], [] -> Some arguments
    
//    let urlMatch urlMatch source = innerUrlMatch urlMatch source []
//    let urlGetParts (uri:System.Uri) = 
//        if (System.String.IsNullOrWhiteSpace(System.Web.HttpRuntime.AppDomainAppVirtualPath)) then
//            uri.AbsolutePath.Split('/') |> Array.toList
//            |> List.filter (fun s -> not (System.String.Equals (s,"")))
//        else
//            let appRelPath = System.Web.VirtualPathUtility.ToAppRelative(uri.LocalPath)
//            appRelPath.Split ('/') |> Array.toList
//            |> List.filter (fun s -> not (System.String.Equals (s,"")))
//            |> List.filter (fun s -> not (System.String.Equals (s,"~")))         

//    let urlMatchByUri urlMatch (uri:System.Uri) = 
//        let source = urlGetParts uri
//        innerUrlMatch urlMatch source []
    
//    module Arg = 
//        let string = Arg ArgType.String
//        let int32 = Arg ArgType.Int32
//        let internal sweCulture = System.Globalization.CultureInfo.GetCultureInfo("SV-se")
//        let internal invCulture = System.Globalization.CultureInfo.InvariantCulture
//        let decimalSwe = Arg (ArgType.Decimal (System.Globalization.NumberStyles.Any, upcast sweCulture.NumberFormat))
//        let decimal = Arg (ArgType.Decimal (System.Globalization.NumberStyles.Any, upcast invCulture.NumberFormat))
    
//    //urlMatch [Path "Test"] (System.Uri("http://www.test.se/Test"))
//    //urlMatch [Path "Test"; Arg ArgType.String] (System.Uri("http://www.test.se/Test/test"))
//    //urlMatch [Path "Test"; Arg ArgType.Int32] (System.Uri("http://www.test.se/Test/56"))
//    //urlMatch [Path "Test"; Arg.decimal] (System.Uri("http://www.test.se/Test/56.10"))
//    //urlMatch [Path "Test"; Arg.decimal] (System.Uri("http://www.test.se/Test/56,10"))
//    //urlMatch [Path "Test"; Arg.decimalSwe] (System.Uri("http://www.test.se/Test/56,10"))
    
    
//    //let tupleToList<'a> (arg1:'a) = [arg1]
//    //let listToTuple<'a> (args:'a list) =  
//    //    match args with
//    //    |[] -> unbox ()
//    //    |[a] -> unbox (a)
//    //    |[a1; a2] -> unbox (a1, a2)
//    //    |a -> 
//    //        let tupleDef = a |> List.map (fun _ -> typeof<'a>) |> List.toArray
//    //        let tupleType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(tupleDef)
//    //        Microsoft.FSharp.Reflection.FSharpValue.MakeTuple(args |> List.map unbox |> List.toArray, tupleType)
    
    
//    //Kan eg ligga utanför
//    //type HttpMethod = string
//    type Route<'a> = {
//        UrlMatch : UrlMatch
//        HttpMethod : string //HttpMethod
//        Method : 'a
//    }

// Swagger.fs


module Specification = 
    //type TypeKey = TypeKey of string

    type IntegerFormat = 
        |Int32
        |Int64
        |Decimal

    type NumberFormat = 
        |Float
        |Double
   
    type StringFormat = 
        |NoFormat
        |Byte
        |Binary
        |Date
        |DateTime
        |Password

    type SchemaObjectMember = {
        Schema : Schema
        Required : bool
        Name : string
    }
    and SchemaObject = {
        Name : string
        Members : SchemaObjectMember list
    }
    and PrimitiveType = 
        |Integer of IntegerFormat
        |Number of NumberFormat
        |Boolean
        |String of StringFormat
        |Object of SchemaObject
        |Array of Schema 
        |Null 
    and Schema = 
        |Type of PrimitiveType
        |Reference of string * SchemaObject // TypeKey*SchemaObject

    //type DefinitionsObject = 

    type BaseSchema = {
        Schema : Schema
        Definitions : Map<string, SchemaObject> // DefinitionsObject
    }

    type ResponseObject = {
        Description : string
        Schema : Schema
    }

    type ParameterPrimitive = |String|Number|Integer|Boolean|Array|File
    type ParameterObjectLocation = 
        |Query of ParameterPrimitive
        |Header of ParameterPrimitive
        |Path of ParameterPrimitive
        |FormData of ParameterPrimitive
        |Body of Schema

    type ParameterObject = {
        Name : string
        In : ParameterObjectLocation
        Description : string
        Required : bool
    }
        with    
            member x.Validate () = ()

    type OperationObject = {
        Description : string
        Tags : string list
        Deprecated : bool
        Responses : Map<int, ResponseObject>
        Parameters : ParameterObject list
    }

    type InfoObject = {
        Title : string
        Description : string
        Version : string
    }

    type PathItemObject = {
        GET : OperationObject option
        PUT : OperationObject option
        POST : OperationObject option
        DELETE : OperationObject option
        OPTIONS : OperationObject option
        HEAD : OperationObject option
        PATCH : OperationObject option
    }

    type PathWithPathTemplating = string

    type PathItemsObject = Map<PathWithPathTemplating, PathItemObject>

    type SwaggerObject = {
        Info : InfoObject 
        Paths : PathItemsObject
        Definitions : Map<string, SchemaObject> //DefinitionsObject
        Host : string option
        BasePath : string option
    }

open Specification

module private Internal = 
    let private formatIntegerFormat = function |IntegerFormat.Int32 -> "int32" |IntegerFormat.Int64 -> "int64" | IntegerFormat.Decimal -> "number"
    let private formatNumberFormat = function |NumberFormat.Double -> "double" |NumberFormat.Float -> "float"
    let private formatStringFormat = 
        function 
            |NoFormat -> None
            |Byte -> Some "byte"
            |Binary -> Some "binary"
            |Date -> Some "date"
            |DateTime -> Some "date-time"
            |Password -> Some "password"


    let extractPathParameters (patternUrl:string) = 
        patternUrl.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.choose 
            (fun x -> 
                if x.StartsWith("{") && x.EndsWith("}") then
                    Some (x.Substring(1, x.Length - 2))
                else
                    None
            
            )  
        |> Seq.toList

    let generateDefinitionName (_definitions : Map<string, SchemaObject>) (typeKey : string) : string = 
        //let (objInfo) = definitions |> Map.find typeKey
        
        //TODO: The Name should be unique even though the same Type.Name Exists in different Namespaces
        //       Maybee include namespace as well (There still could be collisions if they are from same namespace but different assemblies)
        //       a nice aproach would be to generate the name over a function of definitions 
        typeKey


    let rec schemaToJSVal (baseSchema:BaseSchema) = 
        let definitions = baseSchema.Definitions
        let prop = baseSchema.Schema
    //    let definitions = match definitions with |Some s -> s |None -> Map.empty 
        let baseInfo = 
            match prop with
            |Schema.Type (PrimitiveType.Integer format) -> ["type", JSStr "integer"; "format", JSStr (formatIntegerFormat format)]
            |Schema.Type (PrimitiveType.Number format)  -> ["type", JSStr "number"; "format", JSStr (formatNumberFormat format)]
            |Schema.Type (PrimitiveType.Boolean)        -> ["type", JSStr "boolean"]
            |Schema.Type (PrimitiveType.String format)  -> ("type", JSStr "string") :: (match formatStringFormat format with |Some s -> ["format", JSStr s] |None -> List.empty)
            |Schema.Type (PrimitiveType.Object objInfo) -> 
                ["type", JSStr "object"
                 "title", JSStr objInfo.Name
                 "additionalProperties", JSBool false
                 "properties",  objInfo.Members |> List.map (fun x -> x.Name, schemaToJSVal { Schema=x.Schema; Definitions = definitions } ) |> JSObj.ofList 
                 "required", JSArr (objInfo.Members |> List.choose (fun x -> if x.Required then x.Name |> JSStr |> Some else None))
                ]

            |Schema.Type (PrimitiveType.Array ofType) -> ["type", JSStr "array"; "items", schemaToJSVal { Schema=ofType; Definitions = definitions } ]
            |Schema.Type (PrimitiveType.Null) -> ["type", JSStr "null"]
            |Schema.Reference (typeKey ,_reference) ->
                ["$ref", JSStr ("#/definitions/" + generateDefinitionName definitions typeKey)]

        baseInfo
        |> JSObj.ofList

    let definitionsToJSVal (definitions: Map<string, SchemaObject> option) = 
        let definitions = match definitions with |Some s -> s |None -> Map.empty 
        definitions
        |> Map.toList
        |> List.map (fun (typeKey, (objectDefinition)) -> 
                let name = generateDefinitionName definitions typeKey
                let schema = schemaToJSVal 
                                { BaseSchema.Definitions = definitions; Schema = (Schema.Type (Object objectDefinition)) }
                name, schema
            )
        |> JSObj.ofList



    let formatSwaggerPath (patternUrl) definitions (operations:PathItemObject)  = 
        let response (responseObject:ResponseObject) = 
            [
                "description", JSStr responseObject.Description
                "schema",  schemaToJSVal 
                            { Schema =  responseObject.Schema
                              Definitions = definitions }
            ]
            |> JSObj.ofList

        let urlPathParameter p = 
            {
                ParameterObject.Name = p
                ParameterObject.Description = ""
                ParameterObject.In = ParameterObjectLocation.Path ParameterPrimitive.String
                ParameterObject.Required = true
            }

        let urlPathParameters = extractPathParameters patternUrl |> List.map urlPathParameter
        let formatParameterPrimitiv = 
            function 
            |ParameterPrimitive.String -> "string"
            |ParameterPrimitive.Number -> "number"
            |ParameterPrimitive.Integer -> "integer"
            |ParameterPrimitive.Boolean -> "boolean"
            |ParameterPrimitive.Array -> "array"
            |ParameterPrimitive.File -> "file"

        let urlPathParameterToJSVal (x:ParameterObject) = 
            [
                "name"       , JSStr x.Name
                "in"         , match x.In with 
                               | ParameterObjectLocation.Query _    -> "query"
                               | ParameterObjectLocation.Header _   -> "header"
                               | ParameterObjectLocation.Path _     -> "path"
                               | ParameterObjectLocation.FormData _ -> "formData"
                               | ParameterObjectLocation.Body _     -> "body"
                               |> JSStr
                "description", JSStr x.Description
                "required"   , JSBool x.Required
                (match x.In with 
                 | ParameterObjectLocation.Query b    -> "type"  , JSStr (formatParameterPrimitiv b)
                 | ParameterObjectLocation.Header b   -> "type"  , JSStr (formatParameterPrimitiv b)
                 | ParameterObjectLocation.Path b     -> "type"  , JSStr (formatParameterPrimitiv b)
                 | ParameterObjectLocation.FormData b -> "type"  , JSStr (formatParameterPrimitiv b)
                 | ParameterObjectLocation.Body b     -> "schema", schemaToJSVal { Definitions = definitions; Schema = b })
            ]
            |> JSObj.ofList            

        let resp (operationObject:OperationObject) = 
            ["description", JSStr operationObject.Description
             "deprecated" , JSBool operationObject.Deprecated
             "tags"       , operationObject.Tags |> List.map JSStr |> JSArr            
             "produces"   , [JSStr "application/json"] |> JSArr
             "parameters" , operationObject.Parameters
                            |> List.map urlPathParameterToJSVal
                            |> JSArr
             "responses"  , operationObject.Responses
                            |> Map.toList
                            |> List.map (fun (httpStatus, resp) -> 
                                    string httpStatus, response resp
                                )
                            |> JSObj.ofList]
            |> JSObj.ofList

        ("parameters", urlPathParameters |> List.map urlPathParameterToJSVal |> JSArr) :: 
        (
            [   
                operations.GET     |> Option.map (fun x -> "get", resp x)
                operations.PUT     |> Option.map (fun x -> "put", resp x)
                operations.POST    |> Option.map (fun x -> "post", resp x)
                operations.DELETE  |> Option.map (fun x -> "delete", resp x)
                operations.OPTIONS |> Option.map (fun x -> "options", resp x)
                operations.HEAD    |> Option.map (fun x -> "head", resp x)
                operations.PATCH   |> Option.map (fun x -> "patch", resp x)
            ] |> List.choose id
        )       
        |> JSObj.ofList
        
open Internal



module JSON = 
    let swaggerObjectToJSVal (app:SwaggerObject) = 
        let info = 
            [
                "title", JSStr app.Info.Title 
                "description", JSStr app.Info.Description
                "version", JSStr app.Info.Version
            ]
            |> JSObj.ofList      

        
        [
            [
                app.Host |> function |Some s -> Some ("host", JSStr s) |None -> None
                app.BasePath |> function |Some s -> Some ("basePath", JSStr s) |None -> None
            ]
            |> List.choose id

            [
                "swagger", JSStr "2.0"  
                "info", info
                "paths", 
                        app.Paths
                        |> Map.toList
                        |> List.map (fun (url, o) -> url, formatSwaggerPath url app.Definitions o)
                        |> JSObj.ofList
                "definitions", definitionsToJSVal (Some app.Definitions)
            ]
        ]
        |> List.concat 
        |> JSObj.ofList      


open Route
open Specification
//open Svea.Dry.JSON
//open Svea.Dry

module Reflection =
    let private isOptionType (t : System.Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>>  

    type UrlPatternMatch = 
        abstract GetString : string -> string option 
        
//        member x.GetInt16                 key = x.GetString key >>= Parse.Int16
//        member x.GetInt32                 key = x.GetString key >>= Parse.Int32
//        member x.GetDecimal decimalFormat key = x.GetString key >>= Parse.Decimal decimalFormat
//        member x.GetInt32                 key = x.GetString key >>= Parse.Int32
        
    
    type SwaggerRoute<'a> = {
        BodyInput : System.Type
        BodyOutput : System.Type
        Route : Route<'a>
        Description : string
        Deprecated : bool
        Tags : string list
        UrlPath : string
    }
    
    
    module Mapping =    
        let simpleProperty primitive = 
            Schema.Type primitive
    
        type TypeMapper<'a> = {
            Schema : Schema
            Serialize : 'a -> JSVal
            Deserialize : JSVal -> 'a
        }
            with 
                member x.Type = typeof<'a>
    
        type TypeMapper = {
            Type : System.Type
            Schema : Schema
            Serialize : obj -> JSVal
            Deserialize : JSVal -> obj // Borde vara Result<obj, string> 
        }
    
        let boxTypeMapper (mapper:TypeMapper<'a>) : TypeMapper = 
            {
                Type = mapper.Type
                Schema = mapper.Schema
                Serialize = (fun x -> mapper.Serialize (unbox x))
                Deserialize = (fun x -> mapper.Deserialize x |> box)
            }
    
        let unboxTypeMapper (mapper:TypeMapper) : TypeMapper<'a> = 
            {
                Schema = mapper.Schema
                Serialize = (fun x -> mapper.Serialize (box x))
                Deserialize = (fun x -> unbox (mapper.Deserialize x))
            }
    
        let mapping<'a> primitive serialize deserialize : TypeMapper<'a> =
            {
                Schema = simpleProperty primitive
                Serialize = serialize
                Deserialize = deserialize
            }
    open Mapping
    
    ///Must generate a uniquekey for each type
    let private typeToKey (a:System.Type) : string = a.ToString()
  
    
    let internal isList (aType:System.Type) = aType.IsGenericType && aType.GetGenericTypeDefinition() = typedefof<List<_>>
    let internal listType (aType:System.Type) = aType.GetGenericArguments().[0]
    
    //Curtisy of http://www.fssnip.net/1L with small modifications for peroformance and the reverse
    type internal ReflectiveListBuilder = 
        static member UnboxList<'a> (args: obj list) = 
            [ for a in args do yield a :?> 'a ] 
        static member BoxList<'a> (args: 'a list) = 
            [ for a in args do yield box a ] 
    
        static member BuildUnboxList lType = 
            let fnc = 
                typeof<ReflectiveListBuilder>
                    .GetMethod("UnboxList")
                    .MakeGenericMethod([|lType|])
            (fun (args: obj list) -> fnc.Invoke(null, [|args|]))
    
        static member BuildBoxList lType = 
            let fnc = 
                typeof<ReflectiveListBuilder>
                    .GetMethod("BoxList")
                    .MakeGenericMethod([|lType|])
            (fun (args: obj) -> fnc.Invoke(null, [|args|]) :?> obj list)                
                
    
    
    
    let rec generateSchema (lookup:Map<string,TypeMapper>) (aType:System.Type) (definitions:Map<string, SchemaObject> option) : bool*Schema*Map<string, SchemaObject> option = 
        match definitions |> (function |Some s -> s |None -> Map.empty) |> Map.tryFind (typeToKey aType) with
        |Some (ref) -> (true, Schema.Reference ((typeToKey aType),ref), definitions)
        |None -> 
            let mapping = lookup |> Map.tryFind (typeToKey aType)
            match mapping, aType with 
            |Some a, _ -> (true, a.Schema, definitions)
            |None,a when isList a -> 
                let aListType = listType a
                let (required, schema, newDefs) = generateSchema lookup aListType definitions
                required, Schema.Type (PrimitiveType.Array schema), newDefs
    
            |None, a when isOptionType a -> 
                let optionType = a.GetGenericArguments().[0]
                //let (schema, newDefs) = generateSchema lookup optionType definitions
                let (required, schema, newDefs) = generateSchema lookup optionType definitions
                false, schema, newDefs
                
            |None, a when typeof<unit> = a -> (false, Schema.Type PrimitiveType.Null,definitions)
            |None,a when Microsoft.FSharp.Reflection.FSharpType.IsRecord(a) -> 
                let (newDefinitions, members) = 
                            Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(a)
                            |> Array.fold 
                                (fun (defs, items) a -> 
                                    let (required, schema, newDefinitions) = generateSchema lookup a.PropertyType defs
                                    (newDefinitions, { Schema = schema; Required = required; Name = a.Name } :: items)
                                )
                                (definitions, [])
                let obj = { Name = a.Name; Members = members}
                match newDefinitions with
                |Some a -> 
                    true, Schema.Reference ((typeToKey aType), obj), Some (a |> Map.add (typeToKey aType) (obj) )
                |None -> 
                    true, Schema.Type (PrimitiveType.Object obj), newDefinitions
            
            |None, a -> failwithf "Not supported type %A" a
    
    let rec createSerializer (lookup:Map<string,TypeMapper>) (aType:System.Type) = 
        let mapping = lookup |> Map.tryFind (aType.ToString())
    
        match mapping, aType with 
        |Some a, _ -> a.Serialize
        |None,a when isList a -> 
            let arraySerializer = createSerializer lookup (listType a)
            let listBuilder = ReflectiveListBuilder.BuildBoxList (listType a)
            (fun x -> 
                listBuilder x
                |> List.map arraySerializer
                |> JSArr
            )
        |None,a when typeof<unit> = a -> (fun _ -> JSNull)
        |None,a when isOptionType a -> 
            let optionType = a.GetGenericArguments().[0]
            let nonNullFn = createSerializer lookup optionType
            
            (fun x -> 
                let (union,args) = FSharp.Reflection.FSharpValue.GetUnionFields(x, a)
                if union.Name = "Some" then
                    nonNullFn args.[0]
                else
                    JSNull
                //if x = null then JSNull else nonNullFn x
            )
            
        |None,a when Microsoft.FSharp.Reflection.FSharpType.IsRecord(a) -> 
            let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(a)
            let formatters = fields |> Array.map (fun f -> createSerializer lookup f.PropertyType)
            (fun x -> 
                (fields,formatters)
                ||> Array.map2 (fun p f -> p.Name, f (p.GetValue(x)))
                |> List.ofArray
                |> JSObj.ofList      
            )
        |None, a -> failwithf "Not supported type %A" a
       
    let failDes expected json =
        failwithf "'%s' found where %s was expected" (formatToString json) expected

    let rec createDeserializer (lookup:Map<string,TypeMapper>) (aType:System.Type) = 
        let mapping = lookup |> Map.tryFind (aType.ToString())
    
        match mapping, aType with 
        |Some a, _ -> a.Deserialize
        |None,a when isList a -> 
            let arrayDeserializer = createDeserializer lookup (listType a)
            let listBuilder = ReflectiveListBuilder.BuildUnboxList (listType a) 
            (function JSArr a -> 
                        a
                        |> List.map arrayDeserializer
                        |> listBuilder
                    | json  -> failDes "JSArr" json)
        |None,a when typeof<unit> = a -> (fun _ -> box null)
        |None,a when isOptionType a -> 
            let optionType = a.GetGenericArguments().[0]
            let createSome = 
                FSharp.Reflection.FSharpType.GetUnionCases(a)
                |> Array.find (fun x -> x.Name = "Some")
            let createSomeFn = FSharp.Reflection.FSharpValue.PreComputeUnionConstructor(createSome)
            let nonNullFn = createDeserializer lookup optionType
            (function |JSNull -> null 
                      |a -> 
                        let obj = nonNullFn a
                        createSomeFn [| obj |]
            )          
        |None,a when Microsoft.FSharp.Reflection.FSharpType.IsRecord(a) -> 
            let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(a)
            let formatters = fields |> Array.map (fun f -> createDeserializer lookup f.PropertyType)
            let ctor = Microsoft.FSharp.Reflection.FSharpValue.PreComputeRecordConstructor(a)
            (function JSObj o -> 
                        let a = 
                            (fields,formatters) 
                            ||> Array.map2 (fun x f -> f (match o.tryFind x.Name with Some v -> v | None -> JSNull))
                        ctor a
                    | json  -> failDes "JSObj" json)
            
        |None, a -> failwithf "Not supported type %A" a
    
    //[<Literal>]
    //let anyFormat = DecimalFormat.DecimalMarkComma ||| DecimalFormat.DecimalMarkDot

    //  http://swagger.io/specification/    
    let mappers = 
        [
            boxTypeMapper <| mapping<string> (PrimitiveType.String StringFormat.NoFormat) JSStr 
                                             (function JSStr s -> s | x -> failDes "JSStr" x)
                                                                               
            boxTypeMapper <| mapping<int>    (PrimitiveType.Integer IntegerFormat.Int32)  JSInt 
                                             (function JSInt s -> s | x -> failDes "JSInt" x) 
            //JSInt borde vara en Int64 eg anser jag nog kanske...
            //boxTypeMapper <| mapping<int64>  (PrimitiveType.Integer IntegerFormat.Int64) (int >> JSInt) 
            //                                 (function JSInt s -> int64 s
            //                                         | JSNumber (Int64Str s) -> s 
            //                                         | x -> failDes "JSInt" x)
            //boxTypeMapper <| mapping<decimal> 
            //                                 (PrimitiveType.Integer IntegerFormat.Decimal) (int >> JSInt) 
            //                                 // Denna är kanske väl flexibel utifrån den genererade dokumentationen...
            //                                 (function JSInt s -> decimal s 
            //                                         | JSNumber (DecimalStr anyFormat s) -> s
            //                                         | JSStr (DecimalStr anyFormat s) -> s 
            //                                         | x -> failDes "JSInt" x)
            boxTypeMapper <| mapping<bool>   PrimitiveType.Boolean JSBool 
                                             (function JSBool s -> s | x -> failDes "JSBool" x)
            boxTypeMapper <| mapping<System.DateTime> 
                                             (PrimitiveType.String StringFormat.DateTime)
                                             (fun x -> JSStr (System.Xml.XmlConvert.ToString(x, System.Xml.XmlDateTimeSerializationMode.Local))) 
                                             (function JSStr s -> System.Xml.XmlConvert.ToDateTime(s, System.Xml.XmlDateTimeSerializationMode.Local)
                                                     | x -> failDes "JSStr" x)
        ]
        |> List.map (fun x -> x.Type.ToString(), x)
        |> Map.ofList
            
    
    let schema<'a> = generateSchema mappers typeof<'a>    
    let serializer<'a> = let a = createSerializer mappers typeof<'a> in (fun (x:'a) -> a (box x))
    let deserializer<'a> =  let a = createDeserializer mappers typeof<'a> in (fun x -> a x :?> 'a) 
    
    
    
    let patternToUrlMatch (patternUrl:string) : UrlMatch = 
        let patternArray = patternUrl.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries)
        Array.foldBack 
            (fun (p : string) s -> 
                if p.StartsWith("{") && p.EndsWith("}") then 
                    //let newParam = p.Substring(1, p.Length-2)
                    Segment.Arg ArgType.String :: s
                else
                    Segment.Path p :: s
            )
            patternArray
            []
    
    //
    ////TODO: Ta bort
    //let matchPatternUrl (patternUrl:string) (targetUrl:string) = 
    //    let targetUrlArray = targetUrl.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries)
    //    let patternArray = patternUrl.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries)
    //    if targetUrlArray.Length <> patternArray.Length then false, []
    //    else
    //        Array.zip patternArray targetUrlArray
    //        |> Array.fold 
    //            (fun (result, parameters) (p,t) -> 
    //                if result then 
    //                    if p.StartsWith("{") && p.EndsWith("}") then 
    //                        let newParam = p.Substring(1, p.Length-2), t
    //                        (result, newParam :: parameters)
    //                    elif p.ToUpperInvariant() = t.ToUpperInvariant() then
    //                        (result, parameters)
    //                    else
    //                        (false, parameters)
    //                else (result, parameters)
    //            )
    //            (true, [])
            
    
    //open Svea.Dry.Http
    
    let extractParmeters (patternUrl:string) (parameters:ArgValue list) = 
        let patternArray = patternUrl.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.rev |> Array.toList
    
        //List.zip  parameters
        patternArray
        |> List.fold
            (fun (s,remain) (p) -> 
                if p.StartsWith("{") && p.EndsWith("}") then      
                    let name = p.Substring(1, p.Length-2)
                    match remain with
                    |b :: newRemain -> 
                        let c = 
                            match b with
                            |ArgValue.Int32 a -> sprintf "%i" a
                            |ArgValue.Decimal a -> sprintf "%M" a
                            |ArgValue.String a -> a
                        s |> Map.add name c, newRemain
                    |[] -> failwithf "Expected more parameters"
                else (s,remain)
            )
            (Map.empty, parameters)
        |> fst
    
    //open Route
    open HttpApp
    type Mapper<'inp, 'out> = {
        FromHttpRequest : HttpRequest -> 'inp
        ToHttpResponse : 'out -> HttpResponse
    }
        
    let typedMapper<'inp, 'out> = 
        let deserializer = createDeserializer mappers typeof<'inp>
        let serializer = createSerializer mappers typeof<'out>        

        let deserializeHttpRequest  =
            (fun (req:HttpRequest) -> 
                let jsVal = 
                    HttpApp.Utils.readUTF8Stream req.Body
                    |> JSON.tryParse 
                let input = (jsVal |> function |Some s -> s |None -> JSNull) |> deserializer 
                input :?> 'inp)

        let serializeHttpResponse = 
            (fun (s:'out) -> 
                let resp = 
                    serializer s
                    |> formatToString
                    |> HttpApp.Response.json
                resp
            )
        {
            FromHttpRequest = deserializeHttpRequest
            ToHttpResponse = serializeHttpResponse
        }
    
    let swaggerCustom<'state, 'inp, 'out> httpMethod tags (patternUrl:string) description (fn : 'state -> HttpRequest -> UrlPatternMatch -> HttpResponse) =
        let route = 
            {
                Route.Url = patternToUrlMatch patternUrl
                Route.HttpMethod = httpMethod
                Route.Action = 
                    (fun state req parameters -> 
                        let urlParameters = extractParmeters patternUrl parameters
                        let b = { new UrlPatternMatch with
                                        member x.GetString key = urlParameters.TryFind key }
                        fn state req b)
            }        
        {
            BodyInput = typeof<'inp>
            BodyOutput = typeof<'out>
            Route = route
            Description = description
            Tags = tags
            Deprecated = false
            UrlPath = patternUrl
        }
        
    let swaggerErrorResponse errorStr = 
        let resp = 
            ["Error", JSStr errorStr]
            |> JSObj.ofList
        resp
        |> formatToString
        |> HttpApp.Response.json
    
    let swaggerResult<'state, 'inp, 'out> httpMethod tags (patternUrl:string) description (fn : 'state -> HttpRequest -> UrlPatternMatch -> 'inp -> Result<'out,string>) =
        let mapper = typedMapper<'inp, 'out>
        swaggerCustom<'state, 'inp, 'out> httpMethod tags patternUrl description (fun state req pattern -> 
            let input = mapper.FromHttpRequest req
            let output = fn state req pattern input
            output
            |> function 
                |Result.Ok s -> mapper.ToHttpResponse s
                |Result.Error f -> swaggerErrorResponse f
            )
        
    let swagger<'state, 'inp, 'out> httpMethod tags (patternUrl:string) description (fn : 'state -> HttpRequest -> UrlPatternMatch -> 'inp -> 'out) =
        swaggerResult<'state, 'inp, 'out> httpMethod tags (patternUrl:string) description 
            (fun state req parameters inp -> Result.Ok (fn state req parameters inp))

   
    
    
    let createPathObjects definitions (routes:SwaggerRoute<'a> list) = 
        let createPathObject (x:SwaggerRoute<'a>) schema definitions  = 
            let Response200 = 
                {
                    ResponseObject.Description = "OK Response"
                    Schema = schema    
                }
    
            let bodyParams =
                if x.BodyInput <> typeof<unit> then
                    //Bara om det finns någon bodyinput kanske
                    let (required, bodyInputSchema, newDefinitions) = generateSchema mappers x.BodyInput definitions
                    let bodyParams = {
                        Name = "Body input"
                        In = 
                            ParameterObjectLocation.Body bodyInputSchema
                        Description = ""
                        Required = required
                    }
                    Some (bodyParams,newDefinitions)
                else None
    
            
            let operation = 
                {
                    OperationObject.Description = x.Description
                    Tags = x.Tags
                    Deprecated = x.Deprecated
                    Responses = 
                        [200, Response200]    
                        |> Map.ofList
    
                    OperationObject.Parameters = 
                        match bodyParams with 
                        |Some s -> [s |> fst]
                        |None -> []
                            
                }
            operation, match bodyParams with |Some (_,d) -> d |None -> definitions
    
        
        let createPathItemObject definitions (routes:SwaggerRoute<'a> list) = 
            let l = 
                routes 
                |> Seq.groupBy (fun x -> x.Route.HttpMethod.ToUpperInvariant())
                |> Seq.map (fun (k,v) -> k, v |> Seq.toList)
                |> List.ofSeq
    
            l |> List.iter (fun (k,v) -> if v |> List.length > 1 then failwithf "One url can only have one HttpMethod %A" (k, v))
    
            let (operationMap, newDefinitions) = 
                l 
                |> List.map (fun (k,v) -> k,v.Head)
                |> List.fold 
                    (fun (m, d) (k,x) -> 
                        let (required, schema, newD) = generateSchema mappers x.BodyOutput d
                        let (pathObject, newd) = createPathObject x schema newD
                        ((m |> Map.add k pathObject), newd)
                    )
                    (Map.empty, definitions)
            {
                GET = operationMap |> Map.tryFind "GET"
                PUT = operationMap |> Map.tryFind "PUT"
                POST = operationMap |> Map.tryFind "POST"
                DELETE = operationMap |> Map.tryFind "DELETE"
                OPTIONS = operationMap |> Map.tryFind "OPTIONS"
                HEAD = operationMap |> Map.tryFind "HEAD"
                PATCH = operationMap |> Map.tryFind "PATCH"
            }, newDefinitions
    
        routes
        |> Seq.groupBy (fun x -> x.UrlPath)
        |> Seq.map (fun (k,v) -> k, v |> Seq.toList)
        |> List.ofSeq
        |> List.fold 
            (fun (d, m) (url, routes) -> 
                let (o, newD) = createPathItemObject d routes 
                newD, m |> Map.add url o
            )
            (definitions, Map.empty)
        
    
    
    let createRouteFinder (swaggerRoutes:SwaggerRoute<'a> list)  = 
        swaggerRoutes
        |> List.map (fun x -> x.Route)


open HttpApp

type RouteTrace<'state> = {
    Matched : bool
    OriginalUri : System.Uri
    Parts : string list
    Route : Route<'state>
}
type RouteLogger<'state> = {
    Log : RouteTrace<'state> -> unit
}

type SwaggerApplication<'state> = { 
        SwaggerObject : SwaggerObject
        Route : RouteLogger<'state> option -> 'state -> HttpRequest -> HttpResponse option
    }
    with
        member x.SwaggerDefinition = JSON.swaggerObjectToJSVal x.SwaggerObject 
        // Implementation in Svea.Dry.Http
        static member createFromReflection (info : InfoObject) (swaggerRoutes : Reflection.SwaggerRoute<'state> list) =
            let (definitions, paths) = Reflection.createPathObjects (Some Map.empty) swaggerRoutes
            let swaggerObject = 
                {
                    SwaggerObject.Info = info
                    Paths = paths
                    Definitions = (definitions |> function |Some s -> s |None -> Map.empty)
                    Host = None
                    BasePath = None
                }              
            //let sDef = JSON.swaggerObjectToJSVal a 
            
            let routes = Reflection.createRouteFinder swaggerRoutes

            let tryFindRoute (routes:Route.Route<'state> list) (routeLogger:RouteLogger<'state> option) (state:'state) (request:HttpRequest) : HttpResponse option = 
                //Den här skulle kunna optimeras så man letar igenom Route listan på effektivare sätt
                let httpMethod = request.Method
                //let uri = request.Url.Uri
                let source = urlGetParts request.Uri
                routes
                |> List.tryPick 
                    (fun x -> 
                        let methodHit = x.HttpMethod.ToUpperInvariant() = httpMethod.ToUpperInvariant()
                        let urlResult = Route.urlMatchByUri x.Url request.Uri

                        match methodHit, urlResult with
                        |true, Some args -> 
                            routeLogger|> Option.iter (fun l -> l.Log { Matched = true; OriginalUri = request.Uri; Parts = source; Route = x }) 
                            Some (x,args)
                        |_ -> 
                            routeLogger|> Option.iter (fun l -> l.Log { Matched = false; OriginalUri = request.Uri; Parts = source; Route = x }) 
                            None
                    )
                |> Option.map 
                    (fun (x,args) -> 
                        let response = x.Action state request (args |> List.choose id)
                        response
                    )

            { SwaggerObject = swaggerObject
              Route = 
                tryFindRoute routes
            }