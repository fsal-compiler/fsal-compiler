codeunit 60004 SingleInstanceModule
{
    SingleInstance=true;
    procedure add2(x: Integer): Integer
    var
    begin
        exit(x + 2);
    end;
}