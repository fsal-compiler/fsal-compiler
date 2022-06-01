codeunit 60003 Constructors
{
    procedure newObject1(): Integer
    var
        record: Record SimpleRecord;
    begin
        record.Id := 12;
        record.SomeNumber := 20;
        exit(record.SomeNumber);
    end;

    procedure newObject2(): Integer
    var
        ".record2": Record SimpleRecord;
        record2: Record SimpleRecord;
    begin
        begin
            ".record2".Id := 15;
            ".record2".SomeNumber := 25;
        end;
        record2 := ".record2";
        exit(record2.SomeNumber);
    end;

    procedure newObject4(): Record SimpleRecord
    var
        returnVal: Record SimpleRecord;
    begin
        begin
            returnVal.Id := 12;
            returnVal.SomeNumber := 20;
            exit(returnVal);
        end;
    end;

    procedure newObject5(): Record SimpleRecord
    var
        ".rec1": Record SimpleRecord;
        rec1: Record SimpleRecord;
        ".rec2": Record SimpleRecord;
        rec2: Record SimpleRecord;
        rec3: Record SimpleRecord;
    begin
        begin
            ".rec1".Id := 12;
            ".rec1".SomeNumber := 20;
        end;
        rec1 := ".rec1";
        begin
            ".rec2".Id := 15;
            ".rec2".SomeNumber := 25;
        end;
        rec2 := ".rec2";
        rec3 := rec1;
        rec3 := rec2;
        exit(rec3);
    end;
}