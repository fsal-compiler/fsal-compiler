codeunit 50100 TranslationManagement
{
    procedure getTokenAsObject(jsonObject: JsonObject; tokenKey: Text; error: Text): JsonObject
    var
        jtoken: JsonToken;
    begin
        if (not jsonObject.SelectToken(tokenKey,jtoken)) then 
        if (error <> '') then Error(error); 
        exit(jtoken.AsObject());
    end;

    procedure getTokenAsArray(jsonObject: JsonObject; tokenKey: Text; error: Text): JsonArray
    var
        jtoken: JsonToken;
    begin
        if (not jsonObject.SelectToken(tokenKey,jtoken)) then 
        if (error <> '') then Error(error); 
        exit(jtoken.AsArray());
    end;

    procedure getArrayElementAsObject(jsonArray: JsonArray; index: Integer; error: Text): JsonObject
    var
        jtoken: JsonToken;
    begin
        if (not jsonArray.Get(index,jtoken)) then 
        if (error <> '') then Error(error); 
        exit(jtoken.AsObject());
    end;

    procedure handleErrors(jContent: JsonObject)
    var
        details: JsonObject;
        locations: JsonArray;
        location: JsonObject;
    begin
        details := getTokenAsObject(jContent,'details','Invalid response from Web Service');
        locations := getTokenAsArray(details,'locations','No locations available');
        location := getArrayElementAsObject(locations,0,'Location not available');
    end;

    procedure ReadJsonUsingTypeProvider(json: Text; var customer: Record Customer)
    var
        response: JsonToken;
        details: JsonToken;
        _jtoken: JsonToken;
        location: JsonToken;
        _var4: JsonArray;
        phone: JsonToken;
        _var6: JsonArray;
        _var7: Text;
        _var8: Text;
        _var9: Text;
        _var10: Text;
        _var11: Text;
        _var12: Text;
    begin
        response.ReadFrom(json);
        response.SelectToken('details',_jtoken);
        details := _jtoken;
        details.SelectToken('locations',_jtoken);
        _var4 := _jtoken.AsArray();
        _var4.Get(0,_jtoken);
        location := _jtoken;
        details.SelectToken('phones',_jtoken);
        _var6 := _jtoken.AsArray();
        _var6.Get(0,_jtoken);
        phone := _jtoken;
        response.SelectToken('name',_jtoken);
        _var7 := _jtoken.AsValue().AsText();
        customer.Name := _var7;
        location.SelectToken('addressLine1',_jtoken);
        _var8 := _jtoken.AsValue().AsText();
        customer.Address := _var8;
        location.SelectToken('postalCode',_jtoken);
        _var9 := _jtoken.AsValue().AsText();
        customer."Post Code" := _var9;
        location.SelectToken('countryCode',_jtoken);
        _var10 := _jtoken.AsValue().AsText();
        customer."Country/Region Code" := _var10;
        location.SelectToken('country',_jtoken);
        _var11 := _jtoken.AsValue().AsText();
        customer.County := _var11;
        phone.SelectToken('value',_jtoken);
        _var12 := _jtoken.AsValue().AsText();
        customer."Phone No." := _var12;
    end;

    procedure LookupAddressInfo(name: Text; var customer: Record Customer)
    var
        httpContent: HttpContent;
        httpClient: HttpClient;
        httpResponse: HttpResponseMessage;
        responseText: Text;
        jContent: JsonObject;
    begin
        httpContent.WriteFrom('{"domain":"' + name + '"}');
        httpClient.DefaultRequestHeaders.Add('Authorization','Bearer <YOUR KEY>');
        httpClient.Post('https://api.fullcontact.com/v3/company.enrich',httpContent,httpResponse);
        if (not httpResponse.IsSuccessStatusCode) then Error('Error connecting to the Web Service.'); 
        httpResponse.Content.ReadAs(responseText);
        jContent.ReadFrom(responseText);
        handleErrors(jContent);
        ReadJsonUsingTypeProvider(responseText,customer);
    end;

    procedure ProcedureForTesting(input: Text): Text
    var
        httpContent: HttpContent;
        httpClient: HttpClient;
        httpResponse: HttpResponseMessage;
        responseText: Text;
    begin
        httpContent.WriteFrom('{"domain":"' + input + '"}');
        httpClient.DefaultRequestHeaders.Add('Authorization','Bearer <YOUR KEY>');
        httpClient.Post('https://api.fullcontact.com/v3/company.enrich',httpContent,httpResponse);
        if (not httpResponse.IsSuccessStatusCode) then Error('Error connecting to the Web Service.'); 
        httpResponse.Content.ReadAs(responseText);
        exit('the response:' + responseText);
    end;
}