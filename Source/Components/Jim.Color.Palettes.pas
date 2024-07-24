unit Jim.Color.Palettes;

interface

uses
  System.UITypes, System.SysUtils;

type
  TPaletteType = (ptAnalogous, ptTriad, ptSplitComplementary, ptQuad, ptRectangle, ptComplementary, ptAll);
  TRGBA = record
    case Cardinal of
      0: (Color: TAlphaColor);
      2: (HiWord, LoWord: Word);
      3: (B, G, R, A: System.Byte);
  end;

  TColorPalette = record
  private
    FColors: TArray<TAlphaColor>;
    FColorCount: Integer;
    FPaletteType: TPaletteType;
    procedure SetBaseColor(const Value: TAlphaColor);
    function GetColor(Index: Integer): TAlphaColor;
    procedure UpdatePalette;
  public
    constructor Create(AColor: TAlphaColor; AType: TPaletteType);
    property Colors[Index: Integer]: TAlphaColor read GetColor; default;
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

implementation

uses
  System.UIConsts;

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

constructor TColorPalette.Create(AColor: TAlphaColor; AType: TPaletteType);
begin
  FPaletteType := AType;
  case FPaletteType of
    ptAnalogous, ptTriad, ptSplitComplementary: FColorCount := 3;
    ptQuad, ptRectangle: FColorCount := 4;
    ptComplementary: FColorCount := 2;
    ptAll: FColorCount := 3 + 3 + 3 + 4 + 4 + 2 + 4; // Sum of all distinct colors in the other types
  else
    FColorCount := 3; // Default to 3 colors if unspecified
  end;
  SetLength(FColors, FColorCount);
  SetBaseColor(AColor);
end;

procedure TColorPalette.SetBaseColor(const Value: TAlphaColor);
begin
  FColors[0] := Value;
  UpdatePalette;
end;

function TColorPalette.GetColor(Index: Integer): TAlphaColor;
begin
  if (Index >= 0) and (Index < FColorCount) then
    Result := FColors[Index]
  else
    raise ERangeError.CreateFmt('Color index (%d) out of bounds', [Index]);
end;

procedure TColorPalette.UpdatePalette;
var
  H, S, L: Single;
  I, Count: Integer;

  procedure AddColor(Hue: Single);
  begin
    if Hue > 1 then
      Hue := Hue - 1
    else if Hue < 0 then
      Hue := Hue + 1;
    FColors[I] := HSLToRGB(Hue, S, L);
    Inc(I);
  end;

begin
  RGBToHSL(FColors[0], H, S, L);

  I := 0;
  case FPaletteType of
    ptAnalogous:
    begin
      AddColor(H);
      AddColor(H + 1/12); // 30 degrees
      AddColor(H - 1/12); // -30 degrees
    end;
    ptTriad:
    begin
      AddColor(H);
      AddColor(H + 1/3); // 120 degrees
      AddColor(H + 2/3); // 240 degrees
    end;
    ptSplitComplementary:
    begin
      AddColor(H);
      AddColor(H + 5/12); // 150 degrees
      AddColor(H - 5/12); // 210 degrees
    end;
    ptQuad:
    begin
      AddColor(H);
      AddColor(H + 1/4); // 90 degrees
      AddColor(H + 1/2); // 180 degrees
      AddColor(H + 3/4); // 270 degrees
    end;
    ptRectangle:
    begin
      AddColor(H);
      AddColor(H + 1/6); // 60 degrees
      AddColor(H + 1/2); // 180 degrees
      AddColor(H + 5/6); // 300 degrees
    end;
    ptComplementary:
    begin
      AddColor(H);
      AddColor(H + 1/2); // 180 degrees
    end;
    ptAll:
    begin
      var TempPalette: TColorPalette;
      var Hue: Single;

      TempPalette := TColorPalette.Create(FColors[0], ptAnalogous);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      TempPalette := TColorPalette.Create(FColors[0], ptTriad);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      TempPalette := TColorPalette.Create(FColors[0], ptSplitComplementary);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      TempPalette := TColorPalette.Create(FColors[0], ptQuad);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      TempPalette := TColorPalette.Create(FColors[0], ptRectangle);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      TempPalette := TColorPalette.Create(FColors[0], ptComplementary);
      for Count := 0 to TempPalette.ColorCount - 1 do
      begin
        RGBToHSL(TempPalette.Colors[Count], Hue, S, L);
        AddColor(Hue);
      end;

      // Remove duplicates
      var UniqueColors: TArray<TAlphaColor>;
      SetLength(UniqueColors, FColorCount);
      var UniqueCount: Integer := 0;
      for Count := 0 to I - 1 do
      begin
        var Found: Boolean := False;
        for var J: Integer := 0 to UniqueCount - 1 do
        begin
          if UniqueColors[J] = FColors[Count] then
          begin
            Found := True;
            Break;
          end;
        end;
        if not Found then
        begin
          UniqueColors[UniqueCount] := FColors[Count];
          Inc(UniqueCount);
        end;
      end;
      SetLength(UniqueColors, UniqueCount);
      FColorCount := UniqueCount;
      FColors := UniqueColors;
      I := UniqueCount;
    end;
  end;

  SetLength(FColors, I);
end;

end.
