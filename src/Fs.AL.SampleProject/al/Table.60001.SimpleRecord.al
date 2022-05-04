table 60001 SimpleRecord
{
    fields
    {
        field(1;Id;Integer)
        {
        }
        field(2;SomeNumber;Integer)
        {
        }
        field(3;SomeNumber2;Integer)
        {
        }
        field(4;SomeNumber3;Integer)
        {
        }
    }
    procedure SampleFunction(input: Text): Integer
    var
        sum: Integer;
    begin
        sum := SomeNumber + SomeNumber2 + SomeNumber3;
        exit(sum);
    end;

}