(*
  Based on work by Matthew Hipkin

  https://github.com/hippy2094/codelib/blob/master/cymk-delphi-lazarus/cmykutil.pas
  https://www.matthewhipkin.co.uk/codelib/convert-tcolor-cmyk-with-delphi-lazarus/

  Used under MIT License
*)

unit CMYKutil;

interface

uses SysUtils, System.UITypes;

type
  TCMYKColor = record
  private
    C: Single;
    M: Single;
    Y: Single;
    K: Single;
    A: Single;
  public
    property Cyan: Single read C write C;
    property Magenta: Single read M write M;
    property Yellow: Single read Y write Y;
    property Black: Single read K write K;
    property Alpha: Single read A write A;

    procedure FromRGB(RGBColor: TAlphaColor);
    function ToRGB: TAlphaColor;
    function ToString: String;
    procedure FromHSL(H, S, L: Single);

    class function RGBToCMYK(RGBColor: TAlphaColor): TCMYKColor; static;
    class function CMYKToRGB(CMYKColor: TCMYKColor): TAlphaColor; static;
    class function CMYKToString(CMYKColor: TCMYKColor): String; static;
    class function HSLToCMYK(H, S, L: Single): TCMYKColor; static;

    constructor Create(const C, M, Y: Single; const K: Single = 1; const A: Single = 1);
  end;

implementation

uses
  Math, System.UIConsts;

class function TCMYKColor.RGBToCMYK(RGBColor: TAlphaColor): TCMYKColor;
begin
  Result.A := TAlphaColorRec(RGBColor).A / 255;
  if (RGBColor = claBlack) or (RGBColor = 0) then
  begin
    Result.C := 0.00;
    Result.M := 0.00;
    Result.Y := 0.00;
    Result.K := 1.00;
  end
  else
  begin
    var r := 1 - (TAlphaColorRec(RGBColor).R / 255);
    var g := 1 - (TAlphaColorRec(RGBColor).G / 255);
    var b := 1 - (TAlphaColorRec(RGBColor).B / 255);
    var k := Min(r, Min(g, b));
    Result.C := (r - k) / (1 - k);
    Result.M := (g - k) / (1 - k);
    Result.Y := (b - k) / (1 - k);
    Result.K := k;
  end;
end;

class function TCMYKColor.CMYKToRGB(CMYKColor: TCMYKColor): TAlphaColor;
begin
  var CR: TAlphaColorRec;
  CR.R := Round(255 * (1-CMYKColor.C) * (1-CMYKColor.K));
  CR.G := Round(255 * (1-CMYKColor.M) * (1-CMYKColor.K));
  CR.B := Round(255 * (1-CMYKColor.Y) * (1-CMYKColor.K));
  CR.A := Round(255 * CMYKColor.A);
  Result := TAlphaColor(CR);
end;

class function TCMYKColor.CMYKToString(CMYKColor: TCMYKColor): String;
begin
  Result := FloatToStrF(CMYKColor.C, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(CMYKColor.M, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(CMYKColor.Y, ffGeneral, 3, 3);
  Result := Result + ',';
  Result := Result + FloatToStrF(CMYKColor.K, ffGeneral, 3, 3);
end;

constructor TCMYKColor.Create(const C, M, Y, K, A: Single);
begin
  Self.C := C;
  Self.M := M;
  Self.Y := Y;
  Self.K := K;
  Self.A := A;
end;

function TCMYKColor.ToRGB: TAlphaColor;
begin
  Result := TCMYKColor.CMYKToRGB(Self)
end;

function TCMYKColor.ToString: String;
begin
  Result := TCMYKColor.CMYKToString(Self);
end;

procedure TCMYKColor.FromHSL(H, S, L: Single);
begin
  Self := HSLToCMYK(H, S, L);
end;

procedure TCMYKColor.FromRGB(RGBColor: TAlphaColor);
begin
  Self := RGBToCMYK(RGBColor);
end;

class function TCMYKColor.HSLToCMYK(H, S, L: Single): TCMYKColor;
begin
  Result := TCMYKColor.RGBToCMYK(HSLToRGB(H, S, L));
  Result.A := 1;
end;

end.