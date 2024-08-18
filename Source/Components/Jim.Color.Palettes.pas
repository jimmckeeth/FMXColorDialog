unit Jim.Color.Palettes;

interface

uses
  System.UITypes, System.SysUtils;

type
  TPaletteType = (ptAnalogous, ptTriad, ptSplitComplementary, ptQuad, ptRectangle, ptComplementary, ptSpectrum, ptAll);
  TRGBA = record
    case Cardinal of
      0: (Color: TAlphaColor);
      2: (HiWord, LoWord: Word);
      3: (B, G, R, A: System.Byte);
  end;
  TBGRA = record
    case Cardinal of
      0: (Color: TColor);
      2: (HiWord, LoWord: Word);
      3: (R, G, B, A: System.Byte);
  end;

  TColorPalette = record
  private
    FColors: TArray<TAlphaColor>;
    FColorCount: Integer;
    FPaletteType: TPaletteType;
    function GetColor(Index: Integer): TAlphaColor;
    function GetBGRColor(Index: Integer): TColor;
    procedure PopulatePalette(const BaseColor: TAlphaColor);
  public
    constructor Create(AColor: TAlphaColor; AType: TPaletteType);
    property Colors[Index: Integer]: TAlphaColor read GetColor; default;
    property BGRColors[Index: Integer]: TColor read GetBGRColor;
    property ColorCount: Integer read FColorCount;
    property PaletteType: TPaletteType read FPaletteType;
  end;

function GetComplementaryColor(AColor: TAlphaColor): TAlphaColor;
function GetAllPalette(AColor: TAlphaColor): TColorPalette;
function GetAnalogousPalette(AColor: TAlphaColor): TColorPalette;
function GetTriadPalette(AColor: TAlphaColor): TColorPalette;
function GetQuadPalette(AColor: TAlphaColor): TColorPalette;
function GetRectanglePalette(AColor: TAlphaColor): TColorPalette;
function GetSplitComplementaryPalette(AColor: TAlphaColor): TColorPalette;

function AlphaColorToColor(AlphaColor: TAlphaColor): TColor;
function ColorToAlphaColor(Color: TColor): TAlphaColor;
function ColorToIntColor(Color: TColor): Longint;

implementation

uses
  System.UIConsts;

function ColorToIntColor(Color: TColor): Longint;
begin
  Result := Longint(ColorToAlphaColor(Color));
end;

function AlphaColorToColor(AlphaColor: TAlphaColor): TColor;
begin
  TBGRA(Result).R := TRGBA(Alphacolor).R;
  TBGRA(Result).G := TRGBA(Alphacolor).G;
  TBGRA(Result).B := TRGBA(Alphacolor).B;
  TBGRA(Result).A := 0;
end;

function ColorToAlphaColor(Color: TColor): TAlphaColor;
begin
  TRGBA(Result).R := TBGRA(Color).R;
  TRGBA(Result).G := TBGRA(Color).G;
  TRGBA(Result).B := TBGRA(Color).B;
  TRGBA(Result).A := 255;
end;

function GetComplementaryColor(AColor: TAlphaColor): TAlphaColor;
var
  H, S, L: Single;
begin
  // Convert the color to HSL
  RGBToHSL(AColor, H, S, L);

  // Rotate the hue by 0.5 (180 degrees in the 0 to 1 range) to get the complementary color
  H := H + 0.5;
  if H > 1 then
    H := H - 1;

  // Convert back to RGB
  Result := HSLToRGB(H, S, L);
end;

// Standalone functions to create specific palettes
function GetAnalogousPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptAnalogous);
end;

function GetTriadPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptTriad);
end;

function GetSplitComplementaryPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptSplitComplementary);
end;

function GetQuadPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptQuad);
end;

function GetRectanglePalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptRectangle);
end;

function GetComplementaryPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptComplementary);
end;

function GetAllPalette(AColor: TAlphaColor): TColorPalette;
begin
  Result := TColorPalette.Create(AColor, ptAll);
end;

function TColorPalette.GetBGRColor(Index: Integer): TColor;
begin
  Result := AlphaColorToColor(GetColor(Index));
end;

function TColorPalette.GetColor(Index: Integer): TAlphaColor;
begin
  if (Index >= 0) and (Index < FColorCount) then
    Result := FColors[Index]
  else
    raise ERangeError.CreateFmt('Color index (%d) out of bounds', [Index]);
end;

constructor TColorPalette.Create(AColor: TAlphaColor; AType: TPaletteType);
begin
  FPaletteType := AType;
  case FPaletteType of
    ptAnalogous, ptTriad, ptSplitComplementary: FColorCount := 3;
    ptQuad, ptRectangle: FColorCount := 4;
    ptComplementary: FColorCount := 2;
    ptSpectrum: FColorCount := 7;
    ptAll: FColorCount := 12;
  else
    raise ENotImplemented.Create('Unimplemented palette type');
  end;
  SetLength(FColors, FColorCount);
  PopulatePalette(AColor);
end;

procedure TColorPalette.PopulatePalette(const BaseColor: TAlphaColor);
var
  H, S, L: Single;
  I: Integer;

  procedure AddColor(Hue: Single);
  begin
    if Hue > 1 then
      Hue := Hue - 1
    else if Hue < 0 then
      Hue := Hue + 1;
    Assert(I < Length(FColors), 'Attempting to add too many colors to palette!');
    FColors[I] := HSLToRGB(Hue, S, L);
    Inc(I);
  end;

begin
  RGBToHSL(BaseColor, H, S, L);

  I := 0;
  AddColor(H);
  case FPaletteType of
    ptAnalogous:
    begin
      AddColor(H + 1/12); // 30 degrees
      AddColor(H - 1/12); // -30 degrees
    end;
    ptTriad:
    begin
      AddColor(H + 1/3); // 120 degrees
      AddColor(H + 2/3); // 240 degrees
    end;
    ptSplitComplementary:
    begin
      AddColor(H + 5/12); // 150 degrees
      AddColor(H - 5/12); // 210 degrees
    end;
    ptQuad:
    begin
      AddColor(H + 1/4); // 90 degrees
      AddColor(H + 1/2); // 180 degrees
      AddColor(H + 3/4); // 270 degrees
    end;
    ptRectangle:
    begin
      AddColor(H + 1/6); // 60 degrees
      AddColor(H + 1/2); // 180 degrees
      AddColor(H + 5/6); // 300 degrees
    end;
    ptComplementary:
    begin
      AddColor(H + 1/2); // 180 degrees
    end;
    ptSpectrum:
    begin
      AddColor(H + 1/7);
      AddColor(H + 2/7);
      AddColor(H + 3/7);
      AddColor(H + 4/7);
      AddColor(H + 5/7);
      AddColor(H + 6/7);
    end;
    ptAll:
    begin
      {030} AddColor(H + 1/12);  // 30 degrees
      {060} AddColor(H + 1/6);   // 60 degrees
      {090} AddColor(H + 1/4);   // 90 degrees
      {120} AddColor(H + 1/3);   // 120 degrees
      {150} AddColor(H + 5/12);  // 150 degrees
      {180} AddColor(H + 1/2);   // 180 degrees
      {210} AddColor(H + 7/12);  // 210 degrees
      {240} AddColor(H + 2/3);   // 240 degrees
      {270} AddColor(H + 3/4);   // 270 degrees
      {300} AddColor(H + 5/6);   // 300 degrees
      {330} AddColor(H + 11/12); // 330 degrees
    end
    else
      raise ENotImplemented.Create('Unimplemented palette type');
  end;
end;

initialization
  System.UITypes.TColorRec.ColorToRGB := ColorToIntColor;

end.
