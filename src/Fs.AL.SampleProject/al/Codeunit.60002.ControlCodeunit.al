codeunit 60002 ControlCodeunit
{
    procedure patternmatch4(someText: Text)
    var
        v: Text;
        result: Text;
    begin
        if (someText = 'a') then 
            result := 'a, b or c'
        else 
            if (someText = 'b') then 
                result := 'a, b or c'
            else 
                if (someText = 'c') then 
                    result := 'a, b or c'
                else 
                    if (someText = 'someText') then 
                        result := 'compile-time constant'
                    else 
                        if (someText = 'Literal-string') then 
                            result := 'compile-time constant'
                        else 
                            begin
                            v := someText;
                            if (v.StartsWith('d')) then 
                                begin
                                    result := 'starts with d';
                                    v := someText;
                                end
                            else 
                                begin
                                v := someText;
                                if (v.EndsWith('e')) then 
                                    begin
                                        result := 'ends with e';
                                        v := someText;
                                    end
                                else 
                                    result := 'other text';
                                end;
                            end;
        Dialog.Message('matched result:' + result);
    end;
}