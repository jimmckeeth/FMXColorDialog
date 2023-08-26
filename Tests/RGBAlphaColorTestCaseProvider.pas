unit RGBAlphaColorTestCaseProvider;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.Types,
  DUnitX.InternalDataProvider,
  DUnitX.TestDataProvider,
  DUnitX.TestFramework;

type
  RGBAlphaColorProvider = class(TTestDataProvider)
  private
    colorNames: TStringList;
    procedure GetColorName(const ColorName: String);
  public
    constructor Create; override;
    //Get the amount of cases we are creating
    function GetCaseCount(const methodName : string) : Integer; override;
    //Get the name of the cases, depending on the Test-Function
    function GetCaseName(const methodName : string; const caseNumber : integer) : string; override;
    //Get the Params for calling the Test-Function;Be aware of the order !
    function GetCaseParams(const methodName : string ; const caseNumber : integer) : TValuearray; override;
    //Cleanup the instance
    Destructor Destroy; override;
  end;

implementation

{ TColorTestCaseProvider }

uses System.UITypes, System.UIConsts;

constructor RGBAlphaColorProvider.Create;
begin
  inherited;
  colorNames := TStringList.Create;
  GetAlphaColorValues(GetColorName);

end;

destructor RGBAlphaColorProvider.Destroy;
begin
  colorNames.Free;
  inherited;
end;

function RGBAlphaColorProvider.GetCaseCount(const methodName: string): Integer;
begin
  Result := colorNames.Count
end;

function RGBAlphaColorProvider.GetCaseName(const methodName : string; const caseNumber : integer) : string;
begin
  Result :=  Format('RBG Alpha Color "%s"',[colorNames[caseNumber]]);

end;

function RGBAlphaColorProvider.GetCaseParams(const methodName: string; const caseNumber: integer): TValuearray;
begin
  Result := [colorNames[caseNumber], StringToAlphaColor(colorNames[caseNumber])];
end;

procedure RGBAlphaColorProvider.GetColorName(const ColorName: String);
begin
  colorNames.add(colorName);
end;

initialization
   TestDataProviderManager.RegisterProvider('RGBAlphaColorProvider',RGBAlphaColorProvider);

end.
