codeunit 60001 SimpleMethods
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

    procedure assignIfElse(condition: Boolean): Integer
    var
        ifelseassignment: Integer;
    begin
        if (condition) then ifelseassignment := getNumber() else ifelseassignment := 7;
        exit(ifelseassignment + 123);
    end;

    procedure recursiveExample(n: Integer): Integer
    var
        newvalue: Integer;
    begin
        if (n > 1) then newvalue := n + recursiveExample(n - 1) else newvalue := n;
        exit(newvalue);
    end;
}