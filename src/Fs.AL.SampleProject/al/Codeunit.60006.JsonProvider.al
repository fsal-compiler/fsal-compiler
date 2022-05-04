codeunit 60006 JsonProvider
{
    SingleInstance=true;
    procedure ReadJsonResponse(content: Text): Text
    var
        token: JsonToken;
        _var1: Text;
        _jtoken: JsonToken;
        _var3: Text;
    begin
        token.ReadFrom(content);
        token.SelectToken('firstName',_jtoken);
        _var1 := _jtoken.AsValue().AsText();
        token.SelectToken('lastName',_jtoken);
        _var3 := _jtoken.AsValue().AsText();
        exit(_var1 + ' ' + _var3);
    end;

    procedure TypeCasting(content: Text): Integer
    var
        token: JsonToken;
        ageAsInteger: Integer;
        _jtoken: JsonToken;
        ageAsDecimal: Decimal;
        ageAsText: Text;
        ageIn5Years: Integer;
    begin
        token.ReadFrom(content);
        token.SelectToken('age',_jtoken);
        ageAsInteger := _jtoken.AsValue().AsInteger();
        token.SelectToken('age',_jtoken);
        ageAsDecimal := _jtoken.AsValue().AsDecimal();
        token.SelectToken('age',_jtoken);
        ageAsText := _jtoken.AsValue().AsText();
        ageIn5Years := ageAsInteger + 5;
        exit(ageIn5Years);
    end;
}