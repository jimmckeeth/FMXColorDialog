unit FullColor;

interface

uses
  SysUtils, System.UITypes, System.UIConsts;

type
  THSLColor = record
    Hue, Saturation, Luminance, Alpha: Single;
  end;
  {$SCOPEDENUMS ON}
  TFullColor = record
  public
    type TColorType = (RGB, HSL, CMYK);
  private
    FRed, FGreen, FBlue: Byte;
    FAlpha: Single;
    FHue, FSaturation, FLuminance: Single;
    FCyan, FMagenta, FYellow, FBlack: Single;
    FColorType: TColorType;
    procedure SetBlue(const Value: Byte);
    procedure SetGreen(const Value: Byte);
    procedure SetHue(const Value: Single);
    procedure SetLuminance(const Value: Single);
    procedure SetRed(const Value: Byte);
    procedure SetSaturation(const Value: Single);
    procedure UpdateConversion;
  public
    function AsColor: TColor;
    function AsAlphaColor: TAlphaColor;
    constructor CreateFromColor(Color: TColor);
    constructor CreateFromAlphaColor(AlphaColor: TAlphaColor);
    constructor CreateFromRGBA(R, G, B: Byte; A: Byte = 255);
    constructor CreateFromHSLA(H, S, L: Single; A: Single = 1);
    constructor CreateFromCMYK(C, M, Y, K: Single; A: Single = 1);
    property ColorType: TColorType read FColorType;
    property Red: Byte read FRed write SetRed;
    property Green: Byte read FGreen write SetGreen;
    property Blue: Byte read FBlue write SetBlue;
    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
    property Luminance: Single read FLuminance write SetLuminance;
    property Cyan: Single read FCyan;
    property Magenta: Single read FMagenta;
    property Yellow: Single read FYellow;
    property Black: Single read FBlack;

  class function RGBAToAlphaColor(R, G, B: Byte; A: Byte = 255): TAlphaColor; static;
  class function RGBToColor(R, G, B: Byte): TColor; static;

  end;

implementation

{ TFullColor }

function TFullColor.AsAlphaColor: TAlphaColor;
begin
  Result := RGBAToAlphaColor(FRed, FGreen, FBlue, Trunc(FAlpha*$FF));
end;

function TFullColor.AsColor: TColor;
begin
  Result := RGBToColor(FRed, FGreen, FBlue);
end;

constructor TFullColor.CreateFromAlphaColor(AlphaColor: TAlphaColor);
begin
  FColorType := TColorType.RGB;
  var ACR := TAlphaColorRec(AlphaColor);
  FRed := ACR.R;
  FGreen := ACR.G;
  FBlue := ACR.B;
  FAlpha := ACR.A/255;
  UpdateConversion;
end;

constructor TFullColor.CreateFromCMYK(C, M, Y, K, A: Single);
begin
  FColorType := TColorType.CMYK;
  UpdateConversion;
end;

constructor TFullColor.CreateFromColor(Color: TColor);
begin
  FColorType := TColorType.RGB;
  var CR := TColorRec(Color);
  FRed := CR.R;
  FGreen := CR.G;
  FBlue := CR.B;
  FAlpha := 1;
  UpdateConversion;
end;

constructor TFullColor.CreateFromHSLA(H, S, L, A: Single);
begin
  FColorType := TColorType.HSL;
  FHue := H;
  FSaturation := S;
  FLuminance := L;
  FAlpha := A;
  UpdateConversion;
end;

constructor TFullColor.CreateFromRGBA(R, G, B, A: Byte);
begin
  FColorType := TColorType.RGB;
  FRed := R;
  FGreen := G;
  FBlue := B;
  FAlpha := A/255;
  UpdateConversion;
end;

class function TFullColor.RGBAToAlphaColor(R, G, B, A: Byte): TAlphaColor;
begin

end;

class function TFullColor.RGBToColor(R, G, B: Byte): TColor;
begin

end;

procedure TFullColor.SetBlue(const Value: Byte);
begin

end;

procedure TFullColor.SetGreen(const Value: Byte);
begin

end;

procedure TFullColor.SetHue(const Value: Single);
begin

end;

procedure TFullColor.SetLuminance(const Value: Single);
begin

end;

procedure TFullColor.SetRed(const Value: Byte);
begin

end;

procedure TFullColor.SetSaturation(const Value: Single);
begin

end;

procedure TFullColor.UpdateConversion;
begin
  case FColorType of
    TColorType.RGB: begin
    end;
    TColorType.HSL: begin
      var AC := HSLtoRGB(FHue, FSaturation, FLuminance);
      var ACR := TAlphaColorRec(AC);
      FRed := ACR.R;
      FGreen := ACR.G;
      FBlue := ACR.B;
    end;
    TColorType.CMYK: ;
  end;
end;

end.
