codeunit 60001 SimpleCodeunit
{
    procedure getNumber(): Integer
    var
    begin
        exit(5);
    end;

    procedure getText(): Text
    var
    begin
        exit('asdasd');
    end;

    procedure getTime(): DateTime
    var
    begin
        exit(CurrentDateTime());
    end;

    procedure getDeclaredMember(): Integer
    var
        declaredmember: Integer;
    begin
        declaredmember := getNumber();
        exit(declaredmember);
    end;

    procedure assignment(input: Integer): Integer
    var
        value: Integer;
        value3: Integer;
        value2: Integer;
    begin
        value := input;
        value3 := 5;
        value2 := input;
        value2 := 5;
        value2 := 10;
        exit(value);
    end;
}