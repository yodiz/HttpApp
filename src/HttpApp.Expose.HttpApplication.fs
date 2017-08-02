namespace HttpApp.Expose.HttpApplication
open HttpApp
//
//type HttpApp<'a> = {
//    HttpApplication : unit -> HttpApplication<'a>
//}
//    
//module Util = 
//    let getRequest (req:System.Web.HttpRequest) = 
//        let cookies =
//            req.Cookies
//            |> Seq.cast<string> 
//            |> Seq.map (fun x -> x, (req.Cookies.Item x).Value) 
//            |> Map.ofSeq 
//
//        let headers = 
//            req.Headers 
//            |> Seq.cast<string> 
//            |> Seq.map (fun x -> x, (req.Headers.Item x))
//            |> Map.ofSeq
//
//
//        let serverVariables = 
//            req.ServerVariables
//            |> Seq.cast<string> 
//            |> Seq.map (fun x -> x, (req.ServerVariables.GetValues x) |> Array.toList)
//            |> Map.ofSeq
//
//        let myRequest = { 
//            Method = req.HttpMethod
//            Header = headers
//            Uri = req.Url
//            Cookies = cookies
//            Body = req.InputStream 
//            ContentType = req.ContentType 
//            Querystring = lazy (parseQuerystring req.Url)
//            Server = lazy serverVariables
//        }    
//        myRequest
//
//type HttpApplicationHost<'a> (application:HttpApp<'a>, state:'a) as this =
//    inherit System.Web.HttpApplication()
//
//    let applyResponse (res:System.Web.HttpResponse) (resp:HttpResponse)  = 
//        resp.Cookies
//        |> List.iter (fun x -> 
//                        let cookie = System.Web.HttpCookie(x.Key, x.Value)
//                        match x.Expiration with |Some e -> cookie.Expires <- e |None -> ()
//                        res.Cookies.Add(cookie))
//
//        res.ContentType <- resp.ContentType 
//        res.StatusCode <- resp.HttpStatus
//        resp.Header
//        |> Map.iter (fun k v -> res.Headers.Add(k, v))
//        resp.Content res.OutputStream
//
//
//
//    let fromContext state (c:System.Web.HttpContext) (application:HttpApp<'a>) = 
//        let req = Utils.getRequest c.Request
//        let app = application.HttpApplication ()
//        match app state req with
//        |Some r ->           
//            applyResponse c.Response r
//            true
//        |None -> false
//    
//    do
//        this.BeginRequest.Add
//            (fun e -> 
//                if fromContext state this.Context application then
//                    this.CompleteRequest()
//                else
//                    ()
//                ()
//            )  
