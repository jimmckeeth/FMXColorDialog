unit TriangleColorWheelMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  System.Types,
  System.UITypes, Vcl.StdCtrls;

type
  TForm27 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    Button1: TButton;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

uses
  CodeSiteLogging,
  SkiaHelpers,
  System.Math,
  System.UIConsts;

{$R *.dfm}

const Names30DegreeColors: array of string = ['Red','Orange','Yellow','Chartreuse','Green','Spring','Cyan','Azure','Blue','Violet','Magenta','Rose'];

function CalculateTriangleVertices(const Center: TPointF; const Radius, Angle: Single): TArray<TPointF>;
begin
  var SideLength := Radius / Sqrt(3);

  var AdjustedAngle := (Angle + Pi / 2);

  // The top vertex of the triangle (touching the circle)
  var Triangle0 := TPointF.Create(
    Center.X + Radius * Cos(AdjustedAngle),
    Center.Y + Radius * Sin(AdjustedAngle)
  );

  // The other two vertices based on the side length and adjusted angle
  var Triangle1 := TPointF.Create(
    Triangle0.X - SideLength * Cos(AdjustedAngle - Pi / 6),
    Triangle0.Y - SideLength * Sin(AdjustedAngle - Pi / 6)
  );

  var Triangle2 := TPointF.Create(
    Triangle0.X - SideLength * Cos(AdjustedAngle + Pi / 6),
    Triangle0.Y - SideLength * Sin(AdjustedAngle + Pi / 6)
  );

  Result := TArray<TPointF>.Create(Triangle0, Triangle1, Triangle2);
end;

function RadiansToDegrees(const Radians: Single): Single;
begin
  Result := Radians * (180 / Pi);
end;

function ModFloat(X, Y: Double): Double;
begin
  Result := X - Y * Floor(X / Y);
end;

procedure DrawTriangle(const ACanvas: ISkCanvas; const Vertices: TArray<TPointF>;
  const Color: TAlphaColor);
begin
  var PathBuilder := TSkPathBuilder.Interfaced;
  PathBuilder.MoveTo(Vertices[0]);
  PathBuilder.LineTo(Vertices[1]);
  PathBuilder.LineTo(Vertices[2]);
  PathBuilder.Close;

  var path := PathBuilder.Detach;

  var Paint := TSKPaint.Interfaced;
  Paint.Color := Color;
  ACanvas.DrawPath(path, Paint);

//  paint.Color := TAlphaColors.White;
//  paint.Style := TSkPaintStyle.Stroke;
//  ACanvas.DrawPath(path, Paint);
end;

procedure RenderTriangle(const AIndex, ANumTriangles: Integer;
  const ACenter: TPointF; const ARadius: Single;
  const ACanvas: ISkCanvas);
begin
  var Angle := AIndex * 2 * Pi / ANumTriangles;
  var Vertices := CalculateTriangleVertices(ACenter, ARadius, Angle);
  var Color := HSLToRGB((Angle + pi) / (2 * Pi), 1, 0.5);
  DrawTriangle(ACanvas, Vertices, Color);
end;

function GetTextHeight(AText: String; AFont: ISkFont): Single;
begin
  var bounds := AFont.GetBounds(AFont.GetGlyphs(AText));
  Result := 0;
  for var bound in bounds do
  begin
    if bound.Height > Result then
      Result := bound.Height;
  end;

end;

procedure RenderColorWheel(const ACanvas: ISkCanvas;
  const AOpacity: Single; const ADest: TRectF);

begin
  const NumTriangles = 12;
  var Center :=
    TPointF.Create(
      (ADest.Left + ADest.Right) / 2,
      (ADest.Top + ADest.Bottom) / 2);
  var Radius := Min(ADest.Width, ADest.Height) / 2;
  var TriangleHeight := 0.85 * Radius;

  var Paint := TSKPaint.Interfaced;
  Paint.AlphaF := AOpacity;

  // First pass: Draw every other triangle starting at 30 degrees
  for var i := 1 to NumTriangles - 1 do
    if i mod 2 = 1 then
      RenderTriangle(i, NumTriangles, Center, TriangleHeight, ACanvas);

  // Second pass: Draw the remaining triangles
  for var i := 0 to NumTriangles do
    if i mod 2 = 0 then
      RenderTriangle(i, NumTriangles, Center, TriangleHeight, ACanvas);


  var DegreeFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu', TSkFontStyle.Normal), Radius*0.04);
  var NameFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu', TSkFontStyle.Normal), Radius*0.07);
  var HexFont := TSKFont.Interfaced(
    TSkTypeface.MakeFromName('Ubuntu Mono', TSkFontStyle.Bold), Radius*0.06);

  var HexPosition := Min(ADest.Width, ADest.Height) / 4;
  var DegreePosition := DegreeFont.Size;
  var NamePosition := DegreePosition + NameFont.Size;



  for var i := 0 to NumTriangles-1 do
  begin
    paint.Color := TAlphaColors.Black;

    var offsetAngle := Trunc(360/NumTriangles)*i;

    var flip := (offsetAngle > 90) and (offsetAngle < 270);


    // Degrees
    var X := Center.X - DegreeFont.MeasureText(offsetAngle.ToString) / 2;

    if flip then ACanvas.Rotate(180, Center.X, DegreePosition/2);
    ACanvas.DrawSimpleText(offsetAngle.ToString + #$0B0, X, DegreePosition, DegreeFont, paint);
    if flip then ACanvas.Rotate(180, Center.X, DegreePosition/2);

    // Name
    if NumTriangles = Length(Names30DegreeColors) then
    begin
      var height := GetTextHeight(Names30DegreeColors[i], NameFont);
      X := Center.X - NameFont.MeasureText(Names30DegreeColors[i]) / 2;
      if flip then ACanvas.Rotate(180, Center.X, NamePosition-NameFont.Size/2);
      ACanvas.DrawSimpleText(Names30DegreeColors[i], X, NamePosition, NameFont, paint);
      if flip then ACanvas.Rotate(180, Center.X, NamePosition-NameFont.Size/2);
    end;

    // Hex
    var color := HSLtoRGB(offsetAngle/360, 1.0, 0.5);
    var hex := '#'+IntToHex(color).Remove(0,2);

    x := Center.X - HexFont.MeasureText(hex) / 2;
    if flip then ACanvas.Rotate(180, Center.X, hexPosition+5);
    paint.Color := TAlphaColors.Black;
    paint.Style := TSkPaintStyle.Stroke;
    paint.StrokeWidth := 2;

    ACanvas.DrawSimpleText(hex, x, hexPosition+10, HexFont, paint);
    paint.Style := TSkPaintStyle.Fill;
    paint.Color := TAlphaColors.White;
    ACanvas.DrawSimpleText(hex, x, hexPosition+10, HexFont, paint);
    if flip then ACanvas.Rotate(180, Center.X, hexPosition+5);

    ACanvas.Rotate(360/NumTriangles, Center.X, Center.Y);
  end;

end;

procedure CreateSVG(const AOutputFileName: string; const ADest: TRectF);
begin
  var LStream := TFileStream.Create(AOutputFileName, fmCreate);
  try
    var LCanvas := TSkSVGCanvas.Make(ADest, LStream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
    RenderColorWheel(LCanvas, 1, ADest);
    LCanvas := nil;
  finally
    LStream.Free;
  end;
end;

procedure TForm27.Button1Click(Sender: TObject);
begin
  CreateSVG('TriangeColorWheel(RGB-HSL).svg', TRectF.Create(0, 0, 1000, 1000));
end;

procedure TForm27.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  RenderColorWheel(ACanvas, AOpacity, ADest);
end;

end.
