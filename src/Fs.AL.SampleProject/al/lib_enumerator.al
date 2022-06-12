codeunit 50001 FS_Enumerator
{
    var 
        i : Integer;
        size : Integer;

        currText : Text;
        currInt : Integer;
 
    /// ///////////////////
    /// Text
    /// //////////////////
    
    procedure GetEnumerator(data: List of [Text]) : List of [Text]
    begin
        size := data.Count;
        i := 0;
        exit(data);
    end;

    procedure Current(var data: List of [Text]) : Text
    begin
        exit(currText) 
    end;

    procedure MoveNext(data: List of [Text]) : Boolean
    begin
        i += 1;
        if i > size then exit(false)
        else currText := data.Get(i);
        exit(true)
    end;

    procedure Dispose(data: List of [Text])
    begin
        i := 0;
        size := 0;
    end;

    /// ///////////////////
    /// Integer
    /// //////////////////
    
    procedure GetEnumerator(data: List of [Integer]) : List of [Integer]
    begin
        size := data.Count;
        i := 0;
        exit(data);
    end;

    procedure Current(var data: List of [Integer]) : Integer
    begin
        exit(currInt) 
    end;

    procedure MoveNext(data: List of [Integer]) : Boolean
    begin
        i += 1;
        if i > size then exit(false)
        else currInt := data.Get(i);
        exit(true)
    end;

    procedure Dispose(data: List of [Integer])
    begin
        i := 0;
        size := 0;
    end;
}
