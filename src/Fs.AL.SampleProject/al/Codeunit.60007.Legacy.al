codeunit 60007 Legacy
{
    SingleInstance=true;
    procedure substrings(content: Text): Text
    var
        substring1: Text;
        someInteger: Integer;
        substring2: Text;
    begin
        substring1 := content.Substring(3);
        someInteger := 6;
        substring2 := content.Substring(1 + someInteger);
        exit(substring2);
    end;
}