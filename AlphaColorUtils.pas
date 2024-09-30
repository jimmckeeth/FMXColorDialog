unit AlphaColorUtils;

interface

uses SysUtils, System.UITypes,
  System.Math.Vectors;

const
  AllAlphaColorArray: array of TAlphaColor = [TAlphaColors.Aliceblue, TAlphaColors.Antiquewhite, TAlphaColors.Aqua, TAlphaColors.Aquamarine, TAlphaColors.Azure, TAlphaColors.Beige, TAlphaColors.Bisque, TAlphaColors.Black, TAlphaColors.Blanchedalmond, TAlphaColors.Blue, TAlphaColors.Blueviolet, TAlphaColors.Brown, TAlphaColors.Burlywood, TAlphaColors.Cadetblue, TAlphaColors.Chartreuse, TAlphaColors.Chocolate, TAlphaColors.Coral, TAlphaColors.Cornflowerblue, TAlphaColors.Cornsilk, TAlphaColors.Crimson, TAlphaColors.Cyan, TAlphaColors.Darkblue, TAlphaColors.Darkcyan, TAlphaColors.Darkgoldenrod, TAlphaColors.Darkgray, TAlphaColors.Darkgreen, TAlphaColors.Darkgrey, TAlphaColors.Darkkhaki, TAlphaColors.Darkmagenta, TAlphaColors.Darkolivegreen, TAlphaColors.Darkorange, TAlphaColors.Darkorchid, TAlphaColors.Darkred, TAlphaColors.Darksalmon, TAlphaColors.Darkseagreen, TAlphaColors.Darkslateblue, TAlphaColors.Darkslategray, TAlphaColors.Darkslategrey, TAlphaColors.Darkturquoise, TAlphaColors.Darkviolet, TAlphaColors.Deeppink, TAlphaColors.Deepskyblue, TAlphaColors.Dimgray, TAlphaColors.Dimgrey, TAlphaColors.Dodgerblue, TAlphaColors.Firebrick, TAlphaColors.Floralwhite, TAlphaColors.Forestgreen, TAlphaColors.Fuchsia, TAlphaColors.Gainsboro, TAlphaColors.Ghostwhite, TAlphaColors.Gold, TAlphaColors.Goldenrod, TAlphaColors.Gray, TAlphaColors.Green, TAlphaColors.Greenyellow, TAlphaColors.Grey, TAlphaColors.Honeydew, TAlphaColors.Hotpink, TAlphaColors.Indianred, TAlphaColors.Indigo, TAlphaColors.Ivory, TAlphaColors.Khaki, TAlphaColors.Lavender, TAlphaColors.Lavenderblush, TAlphaColors.Lawngreen, TAlphaColors.Lemonchiffon, TAlphaColors.Lightblue, TAlphaColors.Lightcoral, TAlphaColors.Lightcyan, TAlphaColors.Lightgoldenrodyellow, TAlphaColors.Lightgray, TAlphaColors.Lightgreen, TAlphaColors.Lightgrey, TAlphaColors.Lightpink, TAlphaColors.Lightsalmon, TAlphaColors.Lightseagreen, TAlphaColors.Lightskyblue, TAlphaColors.Lightslategray, TAlphaColors.Lightslategrey, TAlphaColors.Lightsteelblue, TAlphaColors.Lightyellow, TAlphaColors.LtGray, TAlphaColors.MedGray, TAlphaColors.DkGray, TAlphaColors.MoneyGreen, TAlphaColors.LegacySkyBlue, TAlphaColors.Cream, TAlphaColors.Lime, TAlphaColors.Limegreen, TAlphaColors.Linen, TAlphaColors.Magenta, TAlphaColors.Maroon, TAlphaColors.Mediumaquamarine, TAlphaColors.Mediumblue, TAlphaColors.Mediumorchid, TAlphaColors.Mediumpurple, TAlphaColors.Mediumseagreen, TAlphaColors.Mediumslateblue, TAlphaColors.Mediumspringgreen, TAlphaColors.Mediumturquoise, TAlphaColors.Mediumvioletred, TAlphaColors.Midnightblue, TAlphaColors.Mintcream, TAlphaColors.Mistyrose, TAlphaColors.Moccasin, TAlphaColors.Navajowhite, TAlphaColors.Navy, TAlphaColors.Oldlace, TAlphaColors.Olive, TAlphaColors.Olivedrab, TAlphaColors.Orange, TAlphaColors.Orangered, TAlphaColors.Orchid, TAlphaColors.Palegoldenrod, TAlphaColors.Palegreen, TAlphaColors.Paleturquoise, TAlphaColors.Palevioletred, TAlphaColors.Papayawhip, TAlphaColors.Peachpuff, TAlphaColors.Peru, TAlphaColors.Pink, TAlphaColors.Plum, TAlphaColors.Powderblue, TAlphaColors.Purple, TAlphaColors.Red, TAlphaColors.Rosybrown, TAlphaColors.Royalblue, TAlphaColors.Saddlebrown, TAlphaColors.Salmon, TAlphaColors.Sandybrown, TAlphaColors.Seagreen, TAlphaColors.Seashell, TAlphaColors.Sienna, TAlphaColors.Silver, TAlphaColors.Skyblue, TAlphaColors.Slateblue, TAlphaColors.Slategray, TAlphaColors.Slategrey, TAlphaColors.Snow, TAlphaColors.Springgreen, TAlphaColors.Steelblue, TAlphaColors.Tan, TAlphaColors.Teal, TAlphaColors.Thistle, TAlphaColors.Tomato, TAlphaColors.Turquoise, TAlphaColors.Violet, TAlphaColors.Wheat, TAlphaColors.White, TAlphaColors.Whitesmoke, TAlphaColors.Yellow, TAlphaColors.Yellowgreen];
  DistinctColors: array of TAlphaColor = [
    TAlphaColors.Green,
    TAlphaColors.Cyan,
    TAlphaColors.Brown,
    TAlphaColors.Magenta,
    TAlphaColors.Pink,
    TAlphaColors.Teal,
    TAlphaColors.Peachpuff,
    TAlphaColors.Orange,
    TAlphaColors.Purple,
    TAlphaColors.Maroon,
    TAlphaColors.Lavender,
    TAlphaColors.Navy,
    TAlphaColors.Olive,
    TAlphaColors.Yellow,
    TAlphaColors.Chartreuse,
    TAlphaColors.Beige,
    TAlphaColors.Blue,
    TAlphaColors.Red,
    TAlphaColors.Lime
  ];
function BlendRGB(const Color1, Color2 : TAlphaColor; ratio: Single = 0.5): TAlphaColor;
function BlendHSL(const RGBColor1, RGBColor2: TAlphaColor): TAlphaColor;
function BlendMultiply(const Color1, Color2: TAlphaColor): TAlphaColor;

// The subtractive functions don't work right
//function BlendHSLSubtractive(const Color1, Color2: TAlphaColor; Ratio: Single = 0.5): TAlphaColor;
//function BlendSubtractive(const Color1, Color2: TAlphaColor; Ratio: Single = 0.5): TAlphaColor;
//function RGBtoRYB(R, G, B: Single): TPoint3D;
//function RYBtoRGB(RYB_R, RYB_Y, RYB_B: Single): TPoint3D;

implementation

uses
  UIConsts,
  Math;

function BlendHSLSubtractive(const Color1, Color2: TAlphaColor; Ratio: Single = 0.5): TAlphaColor;
var
  H1, S1, L1: Single;
  H2, S2, L2: Single;
  HBlend, SBlend, LBlend: Single;
  A1, A2, ABlend: Byte;
begin
  // Ensure Ratio is within 0..1
  Ratio := EnsureRange(Ratio, 0, 1);

  // Convert RGB to HSL
  RGBtoHSL(Color1, H1, S1, L1);
  RGBtoHSL(Color2, H2, S2, L2);

  // Correct hue wrapping
  if Abs(H1 - H2) > 0.5 then
  begin
    if H1 > H2 then
      H2 := H2 + 1
    else
      H1 := H1 + 1;
  end;

  // Blend HSL components
  HBlend := H1 * (1 - Ratio) + H2 * Ratio;
  SBlend := S1 * (1 - Ratio) + S2 * Ratio;
  LBlend := L1 * (1 - Ratio) + L2 * Ratio;

  // Wrap hue back into [0,1]
  HBlend := HBlend - Floor(HBlend);

  // Blend alpha components
  A1 := TAlphaColorRec(Color1).A;
  A2 := TAlphaColorRec(Color2).A;
  ABlend := Round(A1 * (1 - Ratio) + A2 * Ratio);

  // Convert back to RGB
  Result := HSLtoRGB(HBlend, SBlend, LBlend);
  TAlphaColorRec(Result).A := ABlend;
end;

function BlendMultiply(const Color1, Color2: TAlphaColor): TAlphaColor;
var
  r1, g1, b1, a1, r2, g2, b2, a2: single;
begin
  // Decompose colors
  r1 := TAlphaColorRec(Color1).R;
  g1 := TAlphaColorRec(Color1).G;
  b1 := TAlphaColorRec(Color1).B;
  a1 := TAlphaColorRec(Color1).A;

  r2 := TAlphaColorRec(Color2).R;
  g2 := TAlphaColorRec(Color2).G;
  b2 := TAlphaColorRec(Color2).B;
  a2 := TAlphaColorRec(Color2).A;

  // Multiply components and normalize
  TAlphaColorRec(Result).R := round((r1 * r2) /255);
  TAlphaColorRec(Result).G := round((g1 * g2) /255);
  TAlphaColorRec(Result).B := round((b1 * b2) /255);
  TAlphaColorRec(Result).A := round((a1 * a2) /255);
end;

function BlendRGB(const Color1, Color2: TAlphaColor; Ratio: Single = 0.5): TAlphaColor;
var
  r1, g1, b1, a1, r2, g2, b2, a2: Byte;
  rBlend, gBlend, bBlend, aBlend: Byte;
begin
  Ratio := EnsureRange(Ratio, 0, 1);

  // Decompose colors
  r1 := TAlphaColorRec(Color1).R;
  g1 := TAlphaColorRec(Color1).G;
  b1 := TAlphaColorRec(Color1).B;
  a1 := TAlphaColorRec(Color1).A;

  r2 := TAlphaColorRec(Color2).R;
  g2 := TAlphaColorRec(Color2).G;
  b2 := TAlphaColorRec(Color2).B;
  a2 := TAlphaColorRec(Color2).A;

  // Adjust the blending formula to simulate additive blending
  rBlend := Min(Round(r1 + (r2 - r1) * Ratio), 255);
  gBlend := Min(Round(g1 + (g2 - g1) * Ratio), 255);
  bBlend := Min(Round(b1 + (b2 - b1) * Ratio), 255);
  aBlend := Min(Round(a1 + (a2 - a1) * Ratio), 255);

  // Reassemble the color
  TAlphaColorRec(Result).R := rBlend;
  TAlphaColorRec(Result).G := gBlend;
  TAlphaColorRec(Result).B := bBlend;
  TAlphaColorRec(Result).A := aBlend;
end;

function AverageHue(H1, H2: Single): Single;
begin
  if Abs(H1 - H2) > 0.5 then
  begin
    if H1 > H2 then
      H2 := H2 + 1
    else
      H1 := H1 + 1;
  end;

  Result := (H1 + H2) / 2;

  if Result >= 1 then
    Result := Result - 1;
end;

function BlendHSL(const RGBColor1, RGBColor2: TAlphaColor): TAlphaColor;
var
  H1, S1, L1, H2, S2, L2, A1, A2: Single;
  HBlend, SBlend, LBlend, ABlend: Single;
begin
  // Convert RGB to HSL
  RGBtoHSL(RGBColor1, H1, S1, L1);
  RGBtoHSL(RGBColor2, H2, S2, L2);
  A1 := TAlphaColorRec(RGBColor1).A / 255;
  A2 := TAlphaColorRec(RGBColor2).A / 255;

  // Correct hue averaging
  HBlend := Min(AverageHue(H1, H2), 255);
  SBlend := Min((S1 + S2) / 2, 255);
  LBlend := Min((L1 + L2) / 2, 255);
  ABlend := Min((A1 + A2) / 2, 255);

  // Convert back to RGB
  Result := HSLtoRGB(HBlend, SBlend, LBlend);
  TAlphaColorRec(Result).A := Round(ABlend * 255);
end;

// Based on Scott Burns work
function RGBtoRYB(R, G, B: Single): TPoint3D;
var
  // Normalize RGB to [0,1]
  Rn, Gn, Bn: Single;
  // Variables for the conversion
  W, MaxG, Y, MinG, RYB_R, RYB_Y, RYB_B: Single;
begin
  Rn := R / 255;
  Gn := G / 255;
  Bn := B / 255;

  // Remove the whiteness from the color
  W := Min(Rn, Min(Gn, Bn));
  Rn := Rn - W;
  Gn := Gn - W;
  Bn := Bn - W;

  // Get the yellow out of the red+green
  Y := Min(Rn, Gn);
  Rn := Rn - Y;
  Gn := Gn - Y;

  // If blue and green are dominant
  if (Bn > 0) and (Gn > 0) then
  begin
    Bn := Bn + Gn;
    Gn := 0;
  end;

  // Redistribute the remaining green
  MinG := Min(Rn, Bn);
  Rn := Rn - MinG;
  Bn := Bn - MinG;
  Gn := Gn + MinG;

  // Normalize to the maximum component
  MaxG := Max(Rn, Max(Gn, Bn));
  if MaxG > 0 then
  begin
    Rn := Rn / MaxG;
    Gn := Gn / MaxG;
    Bn := Bn / MaxG;
  end;

  // Apply the color transformations
  RYB_R := Rn + Y;
  RYB_Y := Y + Gn;
  RYB_B := Bn;

  // Add the white back in
  RYB_R := RYB_R + W;
  RYB_Y := RYB_Y + W;
  RYB_B := RYB_B + W;

  // Return the RYB color
  Result := TPoint3D.Create(RYB_R, RYB_Y, RYB_B);
end;

function RYBtoRGB(RYB_R, RYB_Y, RYB_B: Single): TPoint3D;
var
  // Remove the whiteness from the color
  W: Single;
  Rn, Gn, Bn: Single;
  MaxG, Y, MinG: Single;
begin
  W := Min(RYB_R, Min(RYB_Y, RYB_B));
  RYB_R := RYB_R - W;
  RYB_Y := RYB_Y - W;
  RYB_B := RYB_B - W;

  MaxG := Max(RYB_R, Max(RYB_Y, RYB_B));

  // Normalize the colors
  if MaxG > 0 then
  begin
    RYB_R := RYB_R / MaxG;
    RYB_Y := RYB_Y / MaxG;
    RYB_B := RYB_B / MaxG;
  end;

  // Convert RYB to RGB
  Y := RYB_Y;
  Rn := RYB_R - Y;
  Gn := RYB_Y - Y;
  Bn := RYB_B;

  MinG := Min(Rn, Bn);
  Rn := Rn - MinG;
  Bn := Bn - MinG;
  Gn := Gn + MinG;

  // Redistribute the remaining green
  if (Bn > 0) and (Gn > 0) then
  begin
    Gn := Gn + Bn;
    Bn := 0;
  end;

  // Normalize to the maximum component
  MaxG := Max(Rn, Max(Gn, Bn));
  if MaxG > 0 then
  begin
    Rn := Rn / MaxG;
    Gn := Gn / MaxG;
    Bn := Bn / MaxG;
  end;

  // Add the yellow back in
  Rn := Rn + Y;
  Gn := Gn + Y;

  // Add the white back in
  Rn := Rn + W;
  Gn := Gn + W;
  Bn := Bn + W;

  // Scale to [0, 255]
  Rn := Rn * 255;
  Gn := Gn * 255;
  Bn := Bn * 255;

  // Return the RGB color
  Result := TPoint3D.Create(Rn, Gn, Bn);
end;

function BlendSubtractive(const Color1, Color2: TAlphaColor; Ratio: Single = 0.5): TAlphaColor;
var
  R1, G1, B1, R2, G2, B2: Single;
  RYB_R1, RYB_Y1, RYB_B1: Single;
  RYB_R2, RYB_Y2, RYB_B2: Single;
  Blended_RYB_R, Blended_RYB_Y, Blended_RYB_B: Single;
  Blended_RGB: TPoint3D;
  A1, A2, ABlend: Byte;
begin
  // Ensure Ratio is within [0, 1]
  Ratio := EnsureRange(Ratio, 0, 1);

  // Extract RGB components from Color1 and normalize to [0,255]
  R1 := TAlphaColorRec(Color1).R;
  G1 := TAlphaColorRec(Color1).G;
  B1 := TAlphaColorRec(Color1).B;
  A1 := TAlphaColorRec(Color1).A;

  // Extract RGB components from Color2 and normalize to [0,255]
  R2 := TAlphaColorRec(Color2).R;
  G2 := TAlphaColorRec(Color2).G;
  B2 := TAlphaColorRec(Color2).B;
  A2 := TAlphaColorRec(Color2).A;

  // Convert both colors from RGB to RYB
  with RGBtoRYB(R1, G1, B1) do
  begin
    RYB_R1 := X;
    RYB_Y1 := Y;
    RYB_B1 := Z;
  end;

  with RGBtoRYB(R2, G2, B2) do
  begin
    RYB_R2 := X;
    RYB_Y2 := Y;
    RYB_B2 := Z;
  end;

  // Perform blending in RYB space
  Blended_RYB_R := RYB_R1 * (1 - Ratio) + RYB_R2 * Ratio;
  Blended_RYB_Y := RYB_Y1 * (1 - Ratio) + RYB_Y2 * Ratio;
  Blended_RYB_B := RYB_B1 * (1 - Ratio) + RYB_B2 * Ratio;

  // Convert blended RYB color back to RGB
  Blended_RGB := RYBtoRGB(Blended_RYB_R, Blended_RYB_Y, Blended_RYB_B);

  // Assemble the final blended color
  TAlphaColorRec(Result).R := Round(EnsureRange(Blended_RGB.X, 0, 255));
  TAlphaColorRec(Result).G := Round(EnsureRange(Blended_RGB.Y, 0, 255));
  TAlphaColorRec(Result).B := Round(EnsureRange(Blended_RGB.Z, 0, 255));
  ABlend := Round(A1 * (1 - Ratio) + A2 * Ratio);
  TAlphaColorRec(Result).A := ABlend;
end;


end.



