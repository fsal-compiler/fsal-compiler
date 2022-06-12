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
        "@j@reader.firstName_2": JsonToken;
        "@reader.firstName_3": Text;
        "@j@reader.lastName_4": JsonToken;
        "@reader.lastName_5": Text;
        fname2: Text;
    begin
        reader.ReadFrom(content);
        begin
            _jtoken.SelectToken('firstName',"@j@reader.firstName_2");
            "@reader.firstName_3" := "@j@reader.firstName_2".AsValue().AsText();
            _jtoken.SelectToken('lastName',"@j@reader.lastName_4");
            "@reader.lastName_5" := "@j@reader.lastName_4".AsValue().AsText();
        end;
        fname2 := "@reader.firstName_3" + 'test' + "@reader.lastName_5";
        exit(fname2);
    end;

    procedure getJsonProp3(content: Text): Decimal
    var
        _jtoken: JsonToken;
        token: JsonToken;
        ".json.ageAsDecimal": JsonToken;
        ageAsDecimal: Decimal;
        "@j@token.age_4": JsonToken;
        "@token.age_5": Decimal;
        ageAsInteger: Integer;
        "@j@token.age_7": JsonToken;
        "@token.age_8": Decimal;
        "@ageAsTextTemp": Text;
        ageAsText: Text;
    begin
        token.ReadFrom(content);
        _jtoken.SelectToken('age',".json.ageAsDecimal");
        ageAsDecimal := ".json.ageAsDecimal".AsValue().AsDecimal();
        begin
            _jtoken.SelectToken('age',"@j@token.age_4");
            "@token.age_5" := "@j@token.age_4".AsValue().AsDecimal();
        end;
        ageAsInteger := "@token.age_5";
        begin
            _jtoken.SelectToken('age',"@j@token.age_7");
            "@token.age_8" := "@j@token.age_7".AsValue().AsDecimal();
            "@ageAsTextTemp" := format("@token.age_8");
        end;
        ageAsText := "@ageAsTextTemp";
        exit(ageAsDecimal);
    end;

    procedure getJsonProp4(content: Text): Text
    var
        _jtoken: JsonToken;
        token: JsonToken;
        ".json.children": JsonToken;
        children: List of [Text];
        firstChild: Text;
    begin
        token.ReadFrom(content);
        _jtoken.SelectToken('children',".json.children");
        foreach _jtoken in ".json.children".AsArray() do
            children.Add(_jtoken.AsValue().AsText());
        firstChild := children.Get(1);
        exit(firstChild);
    end;
}