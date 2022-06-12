codeunit 60005 StrongTypedFunctions
{
    SingleInstance=true;
    procedure GetSampleCustomerName(): Text
    var
        customer: Record Customer;
    begin
        customer.Get('customer1');
        exit(customer.Name);
    end;

    procedure CreateSampleCustomer()
    var
        ".sampleCustomer": Record Customer;
        sampleCustomer: Record Customer;
    begin
        begin
            ".sampleCustomer"."No." := 'customer1';
            ".sampleCustomer".Name := 'John';
            ".sampleCustomer"."E-Mail" := 'johnsmith@example.com';
            ".sampleCustomer"."Phone No." := '123123123';
        end;
        sampleCustomer := ".sampleCustomer";
        sampleCustomer.Insert();
    end;

    procedure createEmployee(firstname: Text; lastname: Text): Record Employee
    var
        returnVal: Record Employee;
    begin
        begin
            returnVal."No." := lastname;
            returnVal."First Name" := firstname;
            returnVal."E-Mail" := firstname + '.' + lastname + '@business.com';
            returnVal."Phone No." := '123123123';
            exit(returnVal);
        end;
    end;
}