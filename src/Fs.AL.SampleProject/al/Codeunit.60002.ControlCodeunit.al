codeunit 60002 ControlCodeunit
{
    procedure patternmatch3(someNumber: Integer)
    var
        v: Integer;
        result: Text;
    begin
        if (someNumber = 1) then 
            result := 'one two or three'
        else 
            if (someNumber = 2) then 
                result := 'one two or three'
            else 
                if (someNumber = 3) then 
                    result := 'one two or three'
                else 
                    begin
                    v := someNumber;
                    if (v > 500) then 
                        begin
                            result := 'greater than 500';
                            v := someNumber;
                        end
                    else 
                        result := 'other';
                    end;
        Dialog.Message('matched result:' + result);
    end;
}