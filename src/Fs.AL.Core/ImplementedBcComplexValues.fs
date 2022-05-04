namespace rec Fs.AL.Core

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open Fs
open System.Reflection
open Fs.AL.Core.Abstract

[<AutoOpen>]
module Http =
    
    [<CompiledName("HttpHeaders")>]
    type ALHttpHeaders() =
        inherit ALComplexValue()
        
        member val internal _impl = new Dictionary<string,string>() with get, set
        member this.Add (key:string,value:string) : bool = this._impl.TryAdd(key,value)
        member this.TryAddWithoutValidation (key:string,value:string) : bool = this._impl.TryAdd(key,value)
        member this.Contains (key:string) : bool = this._impl.ContainsKey key
        member this.Clear ()  : unit = this._impl.Clear()
        member this.Remove (key:string) : bool = this._impl.Remove(key)
        member this.GetValues (key:string,resultValues:ResizeArray<string>) : bool =
            match this._impl.TryGetValue(key) with
            | true, value -> resultValues.Add(value); true 
            | _ -> false
        member this.Keys () : seq<string> = this._impl.Keys
        

    [<CompiledName("HttpContent")>]
    type ALHttpContent() =
        inherit ALComplexValue()
        
        member val internal _impl = "" with get, set
        member this.Clear ()  : unit = this._impl <- ""
        member this.GetHeaders (headersReference:ref<ALHttpHeaders>) : bool = failwith "todo" 
        member this.ReadAs (text:ref<string>) : bool =
            text.Value <- this._impl
            not (String.IsNullOrEmpty(this._impl)) 

        member this.ReadAs (inStream:ref<Stream>) : bool =
            let ms =
                this._impl
                |> Encoding.UTF8.GetBytes
                |> (fun f -> new MemoryStream(f))
            inStream.Value <- ms
            ms.Length <> 0

        
        member this.WriteFrom (text:string) : unit = this._impl <- text
        member this.WriteFrom (stream:Stream) : unit =
            use sr = new StreamReader(stream)
            this._impl <- sr.ReadToEnd()

        member this.Assign (other:ALHttpContent) : unit = failwith "todo"
        
        static member ofHttpContent (httpContent:HttpContent) =
            let c = ALHttpContent()
            c.WriteFrom(httpContent.ReadAsStringAsync().Result)
            c
            

    
    [<CompiledName("HttpRequestMessage")>]
    type ALHttpRequestMessage() =
        inherit ALComplexValue()
        member val internal _impl = new HttpRequestMessage() with get, set

        member this.Clear ()  : unit =
            this._impl.Dispose()
            this._impl <- new HttpRequestMessage()

        member this.SetRequestUri (value:string) : bool =
            let uri = Uri(value)
            this._impl.RequestUri <- uri
            uri.IsWellFormedOriginalString()

        [<Obsolete("not implemented")>] member this.GetHeaders (headersReference:ref<ALHttpHeaders>) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Assign (other:ALHttpRequestMessage) : unit = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Content : ALHttpContent = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Method : string = failwith "todo" 

        [<Obsolete("not implemented")>] member this.GetRequestUri : string = failwith "todo" 

    [<CompiledName("HttpResponseMessage")>]
    type ALHttpResponseMessage() =
        inherit ALComplexValue()
        
        member val internal _statusCode = 0 with get, set
        member val internal _content = ALHttpContent() with get, set
        member val internal _headers = ALHttpHeaders() with get, set
        
        member this.Clear () : unit =
            this._statusCode <- 0
            this._content <- ALHttpContent()
            this._headers <- ALHttpHeaders()

        [<Obsolete("not implemented")>] member this.Assign (other:ALHttpResponseMessage) : unit = failwith "todo" 

        member this.Content : ALHttpContent = this._content

        [<Obsolete("not implemented")>] member this.Headers : ALHttpHeaders = failwith "todo" 

        member this.IsSuccessStatusCode : bool = this._statusCode >= 200 && this._statusCode < 300

        [<Obsolete("not implemented")>] member this.IsBlockedByEnvironment : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.ReasonPhrase : string = failwith "todo" 
        [<Obsolete("not implemented")>] member this.HttpStatusCode : int = failwith "todo"
        
        member internal this._assignFromHttpResponseMessage (result:HttpResponseMessage) =
            this._content._impl <- result.Content.ReadAsStringAsync().Result
            this._statusCode <- int result.StatusCode
            let headersDict = Dictionary() 
            result.Headers
            |> Seq.iter (fun f -> headersDict.Add(f.Key,f.Value |> Seq.head) )
            this._headers._impl <- headersDict

        
    [<CompiledName("HttpClient")>]
    type ALHttpClient() =
        inherit ALComplexValue()

        let mutable _impl =
            {|
              headers = ALHttpHeaders()
              client = new HttpClient()
              |}
        [<Obsolete("not implemented")>] member this.Clear ()  : unit = failwith "todo" 

        [<Obsolete("not implemented")>] member this.SetBaseAddress (value:string) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Get (path:string,response:ref<ALHttpResponseMessage>) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Delete (path:string,response:ref<ALHttpResponseMessage>) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Put (path:string,content:ALHttpContent,response:ref<ALHttpResponseMessage>) : bool = failwith "todo" 

        member this.Post (path:string,content:ALHttpContent,response:ref<ALHttpResponseMessage>) : bool =
            _impl.client.DefaultRequestHeaders.Clear()
            _impl.headers._impl
            |> Seq.iter (fun f -> _impl.client.DefaultRequestHeaders.Add(f.Key,f.Value) ) 
            let result = _impl.client.PostAsync(path,new StringContent(content._impl)).Result
            response.Value._assignFromHttpResponseMessage(result)
            response.Value.IsSuccessStatusCode            

        [<Obsolete("not implemented")>] member this.Send (request:ALHttpRequestMessage,response:ref<ALHttpResponseMessage>) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.AddCertificate (certificateContent:string,password:string) : unit = failwith "todo" 

        [<Obsolete("not implemented")>] member this.UseDefaultNetworkWindowsAuthentication ()  : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.UseWindowsAuthentication (userName:string,password:string,domain:string) : bool = failwith "todo" 

        [<Obsolete("not implemented")>] member this.Assign (other:ALHttpClient) : unit = failwith "todo" 

        [<Obsolete("not implemented")>] member this.GetBaseAddress : string = failwith "todo" 

        member this.DefaultRequestHeaders : ALHttpHeaders = _impl.headers

        [<Obsolete("not implemented")>] member this.Timeout : TimeSpan = failwith "todo" 

