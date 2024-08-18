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
  System.Math,
  System.UIConsts;

{$R *.dfm}

function CalculateTriangleVertices(const Center: TPointF; const Radius, Angle: Single): TArray<TPointF>;
  begin
  var TriangleHeight := Radius ;

  // Calculate the correct side length based on the triangle height
  var SideLength := 2 * TriangleHeight / Sqrt(3);

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

function CalculateHue(const Angle: Single): TAlphaColor;
begin
//  // Convert angle from radians to degrees
//  var Hue := RadiansToDegrees(Angle) - 30;
//
//  // Normalize the hue to the range [0, 360]
//  Hue := Hue - Floor(Hue / 360) * 360;
//
//  // Convert the hue to an AlphaColor using HSLToRGB
//  Result := HSLToRGB(Hue / 360, 1.0, 0.5);

  Result := HSLToRGB((Angle + pi) / (2 * Pi), 1.0, 0.5);
end;

procedure DrawTriangle(const ACanvas: ISkCanvas; const Vertices: TArray<TPointF>; const Color: TAlphaColor);
begin
  var PathBuilder: ISkPathBuilder := TSkPathBuilder.Create;
  PathBuilder.MoveTo(Vertices[0]);
  PathBuilder.LineTo(Vertices[1]);
  PathBuilder.LineTo(Vertices[2]);
  PathBuilder.Close;

  var path := PathBuilder.Detach;

  var Paint: ISkPaint := TSKPaint.Create;
  Paint.Color := Color;
  ACanvas.DrawPath(path, Paint);
  paint.Color := TAlphaColors.White;
  paint.Style := TSkPaintStyle.Stroke;
  ACanvas.DrawPath(path, Paint);
end;

procedure DrawCenteredSquare(const ACanvas: ISkCanvas; const Center: TPointF; const Size: Single);
begin
  var HalfSize := Size / 2;

  // Define the four corners of the square
  var TopLeft := TPointF.Create(Center.X - HalfSize, Center.Y - HalfSize);

  // Set the paint for the square (white and unfilled)
  var Paint: ISkPaint := TSKPaint.Create;
  Paint.Color := TAlphaColors.Black;
  Paint.Style := TSKPaintStyle.Stroke; // Unfilled, just the border

  // Draw the square
  ACanvas.DrawRect(TRectF.Create(TopLeft, size, size), Paint);
end;


procedure RenderColorWheel(const ACanvas: ISkCanvas; const ADest: TRectF);
begin
  const NumTriangles = 4;
  var Center := TPointF.Create((ADest.Left + ADest.Right) / 2, (ADest.Top + ADest.Bottom) / 2);
  var Radius := Min(ADest.Width, ADest.Height) / 2;

  var Paint: ISkPaint := TSKPaint.Create;

  for var i := 0 to NumTriangles - 1 do
  begin
    var Angle := (i * 2 * Pi / NumTriangles + (pi/3) * i)+pi;

    var Triangle0 := TPointF.Create(
      Center.X + Radius * Cos(Angle),
      Center.Y + Radius * Sin(Angle)
    );

    var Triangle1 := TPointF.Create(
      Center.X + Radius * Cos(Angle + (2 * Pi / 3)),
      Center.Y + Radius * Sin(Angle + (2 * Pi / 3))
    );

    var Triangle2 := TPointF.Create(
      Center.X + Radius * Cos(Angle - (2 * Pi / 3)),
      Center.Y + Radius * Sin(Angle - (2 * Pi / 3))
    );

    var PathBuilder: ISkPathBuilder := TSkPathBuilder.Create;
    PathBuilder.MoveTo(Triangle0);
    PathBuilder.LineTo(Triangle1);
    PathBuilder.LineTo(Triangle2);
    PathBuilder.Close;

    Paint.Color := CalculateHue(angle);
    ACanvas.DrawPath(PathBuilder.Detach, Paint);
  end;

end;

procedure CreateSVG(const AOutputFileName: string; const ADest: TRectF);
begin
  var LStream := TFileStream.Create(AOutputFileName, fmCreate);
  try
    var LCanvas : ISkCanvas := TSkSVGCanvas.Make(ADest, LStream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
    RenderColorWheel(LCanvas, ADest);
    LCanvas := nil;
  finally
    LStream.Free;
  end;
end;

procedure TForm27.Button1Click(Sender: TObject);
begin
  CreateSVG('trianges.svg', SkPaintBox1.ClientRect);
end;

procedure TForm27.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
const
  NumTriangles = 12;
begin
  var Center := TPointF.Create((ADest.Left + ADest.Right) / 2, (ADest.Top + ADest.Bottom) / 2);
  var Radius := Min(ADest.Width, ADest.Height) / 2 - 30;

  var Paint: ISkPaint := TSKPaint.Create;
  Paint.AlphaF := AOpacity;
  ACanvas.DrawCircle(Center.X, Center.Y, Radius, Paint);

  // First pass: Draw every other triangle starting at 30 degrees
  for var i := 1 to NumTriangles - 1 do
  begin
    if i mod 2 = 1 then
    begin
      var Angle := i * 2 * Pi / NumTriangles;
      var Vertices := CalculateTriangleVertices(Center, Radius, Angle);
      var Color := CalculateHue(Angle);
      DrawTriangle(ACanvas, Vertices, Color);
    end;
  end;

  // Second pass: Draw the remaining triangles
  for var i := 0 to NumTriangles do
  begin
    if i mod 2 = 0 then
    begin
      var Angle := i * 2 * Pi / NumTriangles;
      var Vertices := CalculateTriangleVertices(Center, Radius, Angle);
      var Color := CalculateHue(Angle);
      DrawTriangle(ACanvas, Vertices, Color);
    end;
  end;
  //RenderColorWheel(ACanvas, ADest);
//
  //DrawCenteredSquare(ACanvas, Center, Radius);
end;


end.
