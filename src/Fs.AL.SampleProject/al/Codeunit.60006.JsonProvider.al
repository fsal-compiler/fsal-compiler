codeunit 60006 JsonProvider
{
    SingleInstance=true;
    procedure getJsonProp1(content: Text): Text
    var
        _jtoken: JsonToken;
        reader: JsonToken;
        ".json.fname": JsonToken;
        fname: Text;
    begin
        reader.ReadFrom(content);
        _jtoken.SelectToken('firstName',".json.fname");
        fname := ".json.fname".AsValue().AsText();
        exit(fname);
    end;

    procedure getJsonProp2(content: Text): Text
    var
        _jtoken: JsonToken;
        reader: JsonToken;
        ".json.firstName2": JsonToken;
        "@firstName2": Text;
        ".json.lastName4": JsonToken;
        "@lastName4": Text;
        fname2: Text;
    begin
        reader.ReadFrom(content);
        begin
            _jtoken.SelectToken('firstName',".json.firstName2");
            "@firstName2" := ".json.firstName2".AsValue().AsText();
            _jtoken.SelectToken('lastName',".json.lastName4");
            "@lastName4" := ".json.lastName4".AsValue().AsText();
        end;
        fname2 := "@firstName2" + 'test' + "@lastName4";
        exit(fname2);
    end;

    procedure getJsonProp3(content: Text): Decimal
    var
        _jtoken: JsonToken;
        token: JsonToken;
        ".json.ageAsDecimal": JsonToken;
        ageAsDecimal: Decimal;
    begin
        token.ReadFrom(content);
        _jtoken.SelectToken('age',".json.ageAsDecimal");
        ageAsDecimal := ".json.ageAsDecimal".AsValue().AsDecimal();
        exit(ageAsDecimal);
    end;
}