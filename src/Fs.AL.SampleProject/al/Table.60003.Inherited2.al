table 60003 Inherited2
{
    fields
    {
        field(1;Id;Integer)
        {
        }
        field(2;Shared1;Text[200])
        {
        }
        field(3;Shared2;Integer)
        {
        }
        field(4;Int;Integer)
        {
        }
    }
    procedure SharedProcedure(): DateTime
    var
    begin
        exit(CurrentDateTime());
    end;

    procedure AssignShared1(data: Text)
    var
        customer: Record Customer;
    begin
        customer.Get(data);
        customer.Address := 'asfdsfgsd';
        customer.Modify();
        Dialog.Message('tehtud');
    end;

}