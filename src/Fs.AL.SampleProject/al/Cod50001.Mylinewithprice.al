codeunit 50001 Mylinewithprice implements "Line With Price"
{
    var mytable : Record "Sales Line";
    procedure GetTableNo(): Integer
    begin
    end;
    
    procedure SetLine(PriceType: Enum "Price Type"; Line: Variant)
    begin
    end;
    
    procedure SetLine(PriceType: Enum "Price Type"; Header: Variant; Line: Variant)
    begin
    end;
    
    procedure SetSources(var NewPriceSourceList: Codeunit "Price Source List")
    begin
    end;
    
    procedure GetLine(var Line: Variant)
    begin
    end;
    
    procedure GetLine(var Header: Variant; var Line: Variant)
    begin
    end;
    
    procedure GetAssetType(): Enum "Price Asset Type"
    begin
    end;
    
    procedure GetPriceType(): Enum "Price Type"
    begin
    end;
    
    procedure IsPriceUpdateNeeded(AmountType: Enum "Price Amount Type"; FoundPrice: Boolean; CalledByFieldNo: Integer): Boolean
    begin
    end;
    
    procedure IsDiscountAllowed(): Boolean
    begin
    end;
    
    procedure Verify()
    begin
    end;
    
    procedure SetAssetSourceForSetup(var DtldPriceCalculationSetup: Record "Dtld. Price Calculation Setup"): Boolean
    begin
    end;
    
    procedure CopyToBuffer(var PriceCalculationBufferMgt: Codeunit "Price Calculation Buffer Mgt."): Boolean
    begin
    end;
    
    procedure Update(AmountType: Enum "Price Amount Type")
    begin
    end;
    
    procedure SetPrice(AmountType: Enum "Price Amount Type"; PriceListLine: Record "Price List Line")
    begin
    end;
    
    procedure ValidatePrice(AmountType: Enum "Price Amount Type")
    begin
    end;
    
}
