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
        record2: Record SimpleRecord;
        returnVal: Record SimpleRecord;
    begin
        returnVal.Id := 15;
        returnVal.SomeNumber := 25;
        record2 := returnVal;
        exit(record2.SomeNumber);
    end;

    procedure newObject3(): Integer
    var
        record: Record SimpleRecord;
        innerRecord: Record SimpleRecord;
    begin
        innerRecord.Id := 12;
        innerRecord.SomeNumber := 20;
        record := innerRecord;
        exit(record.SomeNumber);
    end;

    procedure newObject4(): Record SimpleRecord
    var
        returnVal: Record SimpleRecord;
    begin
        returnVal.Id := 12;
        returnVal.SomeNumber := 20;
        exit(returnVal);
    end;
}