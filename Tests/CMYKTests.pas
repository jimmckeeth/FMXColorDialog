unit CMYKTests;

interface

uses
  SysUtils, DUnitX.TestFramework, RGBAlphaColorTestCaseProvider, System.UITypes;

type
  [TestFixture]
  TMyTestObject = class
  public
    [TestCaseProvider(RGBAlphaColorProvider)]
    procedure TestRGBRoundTrip(const ColorName: string; const AlphaColor: TAlphaColor);

    [TestCase('0','0')]
    [TestCase('1','1')]
    [TestCase('127','127')]
    [TestCase('254','254')]
    [TestCase('255','255')]
    procedure TestAlphaValues(Alpha: Integer);

    [TestCase('Red',    '$FF0000,   0, 100, 100,   0')]
    [TestCase('Orange', '$F29918,   0,  37,  90,  05')]
    [TestCase('Green',  '$00FF00, 100,   0, 100,   0')]
    [TestCase('Blue',   '$0000FF, 100, 100,   0,   0')]
    [TestCase('AFBlue', '$5D8AA8,  45,  18,   0,  34')]
    [TestCase('Azure',  '$0080FF, 100,  50,   0,   0')]
    [TestCase('Magenta','$FF00FF,   0, 100,   0,   0')]
    [TestCase('Cyan',   '$00FFFF, 100,   0,   0,   0')]
    [TestCase('Yellow', '$FFFF00,   0,   0, 100,   0')]
    [TestCase('Teal',   '$008080, 100,   0,   0,  50')]
    procedure TestRGBHex(const Hex: String; C,M,Y,K: Integer );

    [Test] procedure TestAlphaBlack;
    [Test] procedure TestZero;
  end;

implementation

{ TMyTestObject }

uses cmykutil, System.UIConsts;

function Round1000(const S: Single): Int64;
begin
  result := Round(S * 1000);
end;

procedure TMyTestObject.TestAlphaValues(Alpha: Integer);
begin
  var rgbOrange := claOrange;
  TAlphaColorRec(rgbOrange).A := Alpha;
  var cmykOrange := TCMYKColor.RGBToCMYK(rgbOrange);
  Assert.AreEqual(Round1000(TAlphaColorRec(rgbOrange).A), Round1000(cmykOrange.A * 255));
end;

procedure TMyTestObject.TestAlphaBlack;
begin
  var black := TCMYKColor.RGBToCMYK(claBlack);
  Assert.AreEqual(Single(0), black.C, 'Black should have Cyan of 0');
  Assert.AreEqual(Single(0), black.M, 'Black should have Magenta of 0');
  Assert.AreEqual(Single(0), black.Y, 'Black should have Yellow of 0');
  Assert.AreEqual(Single(1), black.K, 'Black should have Black of 100%');
  Assert.AreEqual(Single(1), black.A, 'Black should have Alpha of 100%');
end;

procedure TMyTestObject.TestZero;
begin
  var null := TCMYKColor.RGBToCMYK(0);
  Assert.AreEqual(Single(0), null.C, 'Null should have Cyan of 0');
  Assert.AreEqual(Single(0), null.M, 'Null should have Magenta of 0');
  Assert.AreEqual(Single(0), null.Y, 'Null should have Yellow of 0');
  Assert.AreEqual(Single(0), null.A, 'Null should have Alpha of 0');
  Assert.AreEqual(Single(1), null.K, 'Null should have Black of 100%');
end;

procedure TMyTestObject.TestRGBRoundTrip(const ColorName: string; const AlphaColor: TAlphaColor);
begin
  var cmyk := TCMYKColor.RGBToCMYK(AlphaColor);
  Assert.AreEqual(AlphaColor, cmyk.ToRGB, ColorName);
end;

procedure TMyTestObject.TestRGBHex(const Hex: String; C, M, Y, K: Integer);
begin
  var rgb: TAlphaColor;
  if Length(Hex) = 7 then
    rgb := TAlphaColorRec.Alpha or TAlphaColor(StrToInt(hex))
  else
    rgb := TAlphaColor(StrToInt(hex));

  var cmyk := TCMYKColor.Create(C/100, M/100, Y/100, K/100);
  Assert.AreEqual(IntToHex(rgb,8), IntToHex(cmyk.ToRGB, 8));
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
  TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Pause;
end.
