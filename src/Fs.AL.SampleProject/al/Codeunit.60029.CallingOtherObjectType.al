codeunit 60029 CallingOtherObjectType
{
    procedure callRecordProcedure(): Integer
    var
        instance: Record SimpleRecord;
        result: Integer;
    begin
        result := instance.SampleFunction('someinput');
        exit(result);
    end;
}