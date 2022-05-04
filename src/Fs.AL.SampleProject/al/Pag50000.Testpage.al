page 50000 "Testpage"
{
    Caption = 'Testpage';
    PageType = Card;
    
    layout
    {
        area(content)
        {
            group(General)
            {
            }
        }
    }

    trigger OnOpenPage()
    var 
        strlist: List of [Text];
        jsonlist: List of [JsonToken];
        d : Integer;
        cu: Codeunit Legacy;
        debug : text;
    begin
        debug := cu.substrings('some text');
        Message(debug);
        d := 5
    end;
}
