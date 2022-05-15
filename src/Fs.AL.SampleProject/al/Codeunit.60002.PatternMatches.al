codeunit 60002 PatternMatches
{
    procedure patternmatch1(someNumber: Integer)
    var
        result: Text;
        v: Integer;
    begin
        v := someNumber;
        if (someNumber = 1) then 
        result := 'one two or three' else 
        if (someNumber = 2) then 
        result := 'one two or three' else 
        if (someNumber = 3) then 
        result := 'one two or three' else 
        if (v > 500) then 
        result := 'greater than 500' else 
        if (v < 0) then 
        result := 'less than 0' else 
        result := 'other';
        Dialog.Message('matched result:' + result);
    end;

    procedure patternmatch2(someText: Text)
    var
        result: Text;
        v: Text;
    begin
        v := someText;
        if (someText = 'a') then 
        result := 'a, b or c' else 
        if (someText = 'b') then 
        result := 'a, b or c' else 
        if (someText = 'c') then 
        result := 'a, b or c' else 
        if (someText = 'someText') then 
        result := 'compile-time constant' else 
        if (v.StartsWith('d')) then 
        result := 'starts with d' else 
        if (v.EndsWith('e')) then 
        result := 'ends with e' else 
        result := 'other text';
        Dialog.Message('matched result:' + result);
    end;
}