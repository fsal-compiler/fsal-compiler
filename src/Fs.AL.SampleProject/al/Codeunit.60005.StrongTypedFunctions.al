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
        sampleCustomer: Record Customer;
        returnVal: Record Customer;
    begin
        returnVal."No." := 'customer1';
        returnVal.Name := 'John';
        returnVal."E-Mail" := 'johnsmith@example.com';
        returnVal."Phone No." := '123123123';
        sampleCustomer := returnVal;
        sampleCustomer.Insert();
    end;

    procedure createEmployee(firstname: Text; lastname: Text): Record Employee
    var
        returnVal: Record Employee;
    begin
        returnVal."No." := lastname;
        returnVal."First Name" := firstname;
        returnVal."E-Mail" := firstname + '.' + lastname + '@business.com';
        returnVal."Phone No." := '123123123';
        exit(returnVal);
    end;
}