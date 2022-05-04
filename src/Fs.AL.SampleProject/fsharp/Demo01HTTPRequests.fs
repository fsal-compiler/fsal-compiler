module Fs.AL.SampleProject.Demo01HTTPRequests

// AL Source code
// https://github.com/PacktPublishing/Mastering-Microsoft-Dynamics-365-Business-Central/blob/master/Chapter%206/TranslateCustomers/TranslationManagement.al

open Fs.AL.Core
open Fs.AL.Core.ALComplexValues
open Fs.AL.Core.ALFunctions
open Fs.AL.Core.ALSimpleValues
open Fs.AL.Packages

#nowarn "20" // to ignore return values

// generate strongly typed code for the json sample response
[<Literal>]
let JsonSample =
    __SOURCE_DIRECTORY__
    + "/Samples/api-response.json"

type APIResponseProvider = Fable.JsonProvider.Generator<JsonSample>

type Customer = BaseApplicationALPackage.Tables.Customer

type TranslationManagement() =
    inherit ALCodeunit()
    override this.ObjectId = 50100

    member this.getTokenAsObject(jsonObject: ALJsonObject, tokenKey: string, error: string) =
        let jtoken = ref (ALJsonToken())
        if not (jsonObject.SelectToken(tokenKey, jtoken)) then
            if error <> "" then failwith error
        jtoken.Value.AsObject()

    member this.getTokenAsArray(jsonObject: ALJsonObject, tokenKey: string, error: string) =
        let jtoken = ref (ALJsonToken())
        if not (jsonObject.SelectToken(tokenKey, jtoken)) then
            if error <> "" then failwith error
        jtoken.Value.AsArray()

    member this.getArrayElementAsObject(jsonArray: ALJsonArray, index: int, error: string) =
        let jtoken = ref (ALJsonToken())
        if not (jsonArray.Get(index, jtoken)) then
            if error <> "" then failwith error
        jtoken.Value.AsObject()

    member this.handleErrors(jContent: ALJsonObject) =
        let details =
            this.getTokenAsObject(jContent, "details", "Invalid response from Web Service")
        let locations =
            this.getTokenAsArray(details, "locations", "No locations available")
        let location =
            this.getArrayElementAsObject(locations, 0, "Location not available")
        ()
    
    
    // this is not implemented - 
//    member this.handleErrorsPatternMatch(response: API.Root) =
//        match response with
//        // details dont exist
//        | { details = None } ->
//            failwith "Invalid response from Web Service"
//        // details exist without a location list
//        | { details = Some { locations = None } } ->
//            failwith "No locations available"
//        // details exist with an empty location list
//        | { details = Some { locations = Some [] } } ->
//            failwith "Location not available"
//        | _ ->
//            () // the response is correct
    
    member this.ReadJsonUsingTypeProvider(json:string,customer:byref<Customer>) =
        let response = APIResponseProvider(json)

        let details = response.details
        let location = details.locations.[0]
        let phone = details.phones.[0]

        customer.Name <- response.name
        customer.Address <- location.addressLine1
        customer.``Post Code`` <- location.postalCode
        customer.``Country/Region Code`` <- location.countryCode
        customer.County <- location.country
        customer.``Phone No.`` <- phone.value

    member this.LookupAddressInfo(name: string, customer: byref<Customer>) =
        let httpContent = ALHttpContent()
        httpContent.WriteFrom("{\"domain\":\"" + name + "\"}") // string interpolation not yet supported
        let httpClient = ALHttpClient()
        httpClient.DefaultRequestHeaders.Add("Authorization", "Bearer <YOUR KEY>")
        let httpResponse = ref (ALHttpResponseMessage())
        httpClient.Post("https://api.fullcontact.com/v3/company.enrich", httpContent, httpResponse)

        if not httpResponse.Value.IsSuccessStatusCode then
            failwith "Error connecting to the Web Service."

        let responseText = ref ""
        httpResponse.Value.Content.ReadAs(responseText)

        // handling the errors the same way as in the book
        let jContent = ALJsonObject()
        jContent.ReadFrom(responseText.Value)
        this.handleErrors (jContent)

        // from here on using the Json Type Provider
        this.ReadJsonUsingTypeProvider(responseText.Value,&customer)




    // this is to test with replexample.fsx
    member this.ProcedureForTesting(input: string) =

        let httpContent = ALHttpContent()
        httpContent.WriteFrom("{\"domain\":\"" + input + "\"}")
        let httpClient = ALHttpClient()
        httpClient.DefaultRequestHeaders.Add("Authorization", "Bearer <YOUR KEY>")
//        let httpResponse = ref (ALHttpResponseMessage())
        let httpResponse = ref (ALHttpResponseMessage())
        httpClient.Post("https://api.fullcontact.com/v3/company.enrich", httpContent, httpResponse)

        if not httpResponse.Value.IsSuccessStatusCode then
            failwith "Error connecting to the Web Service."

        let responseText = ref ""
        httpResponse.Value.Content.ReadAs(responseText)

        "the response:" + responseText.Value



         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         