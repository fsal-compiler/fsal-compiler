codeunit 60004 LoopsCodeunit
{
    SingleInstance=true;
    procedure recursiveExample(n: Integer): Integer
    var
        "@s@LoopsCodeunit": Codeunit LoopsCodeunit;
        newvalue: Integer;
    begin
        if (n > 1) then 
            newvalue := n + "@s@LoopsCodeunit".recursiveExample(n - 1)
        else 
            newvalue := n;
        exit(newvalue);
    end;

    procedure whileLoopExample()
    var
        i: Integer;
    begin
        i := 0;
        while i < 5 do 
        begin
            Dialog.Message('hello');
            i := i + 1;
        end;
    end;
}