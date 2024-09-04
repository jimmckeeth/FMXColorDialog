unit ColorMixDiagramMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia,
  Math;

type
  TForm4 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses SkiaHelpers, AlphaColorUtils;

type
  TCircleF = record
    Center: TPointF;
    Radius: Single;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
    constructor Create(const Center: TPointF; const Radius: Single); overload;
    constructor Create(const X, Y, Radius: Single); overload;
  end;

  TArcF = record
    Start, Finish, Center: TPointF;
    Radius: Single;
    StartAngle: Single;
    SweepAngle: Single;
    function IsEmpty: Boolean;
    function BoundingRect: TRectF;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
    procedure NormalizeSweep;
  end;

  TLensF = record
    Arc1, Arc2: TArcF;
    Point1, Point2: TPointF;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
  end;

  TReuleauxF = record
    Arcs: array of TArcF;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
  end;

  TVennDiagramF = record
    Circles: array of TCircleF;
    Lenses: array of TLensF;
    Lunes: array of TLensF;
    Reuleaux: TReuleauxF;
  end;

procedure TArcF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  ACanvas.DrawArc(BoundingRect, StartAngle, SweepAngle, False, APaint);
end;
//
//function TArcF.StartPoint: TPointF;
//begin
//  // Convert StartAngle from degrees to radians
//  var StartAngleRadians := DegToRad(StartAngle);
//
//  // Calculate the start point using trigonometric functions
//  Result.X := Center.X + Radius * Cos(StartAngleRadians);
//  Result.Y := Center.Y + Radius * Sin(StartAngleRadians);
//end;
//
//function TArcF.EndPoint: TPointF;
//begin
//  // Calculate the end angle (StartAngle + SweepAngle)
//  var EndAngleRadians := DegToRad(StartAngle + SweepAngle);
//
//  // Calculate the end point using trigonometric functions
//  Result.X := Center.X + Radius * Cos(EndAngleRadians);
//  Result.Y := Center.Y + Radius * Sin(EndAngleRadians);
//end;

procedure TArcF.NormalizeSweep;
begin
  // Ensure the sweep angle is within the range 0 to 360 degrees
  if SweepAngle < 0 then
    SweepAngle := SweepAngle + 360;
  if SweepAngle > 360 then
    SweepAngle := SweepAngle - 360;
end;

function TArcF.IsEmpty: Boolean;
begin
  Result := Radius = 0;
end;

function TArcF.BoundingRect: TRectF;
begin
  Result := TRectF.Create(Center.X - Radius, Center.Y - Radius, Center.X + Radius, Center.Y + Radius);
end;

constructor TCircleF.Create(const Center: TPointF; const Radius: Single);
begin
  Self.Center := Center;
  Self.Radius := Radius;
end;

constructor TCircleF.Create(const X, Y, Radius: Single);
begin
  Self.Center := TPointF.Create(X, Y);
  Self.Radius := Radius;
end;

procedure TCircleF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  ACanvas.DrawCircle(Center, Radius, APaint);
end;

function TCircleF.IsEmpty: Boolean;
begin
  Result := Radius = 0;
end;

function TReuleauxF.IsEmpty: Boolean;
begin
  for var arc in arcs do
    if arc.Radius = 0 then Exit(True);
  Result := False;
end;

procedure TReuleauxF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  if IsEmpty then
    Exit;

  var path := TSkPathBuilder.interfaced;
  for var Arc in Arcs do
  begin
    path.MoveTo(arc.Start);
    path.ArcTo(arc.BoundingRect, arc.StartAngle, arc.SweepAngle, False);
    path.MoveTo(arc.Finish);
  end;
  path.Close;
  path.FillType := TSkPathFillType.EvenOdd;

  ACanvas.DrawPath(path.Detach, APaint);
  APaint.Color := TAlphaColors.Black;
  APaint.Style := TSkPaintStyle.Fill;
  var cnt := 0;
  for var Arc in Arcs do
  begin
    ACanvas.DrawCircle(arc.Start, 5, APaint);
    ACanvas.DrawSimpleText('start ' +cnt.ToString, arc.Start.x + 10, arc.Start.Y + 10, TSkFont.interfaced(), APaint);
    ACanvas.DrawCircle(arc.Finish, 5, APaint);
    ACanvas.DrawSimpleText('end ' + cnt.ToString, arc.Finish.x + 10, arc.Finish.Y + 10, TSkFont.interfaced(), APaint);
    Inc(cnt);
  end;

end;

function TLensF.IsEmpty: Boolean;
begin
  Result := (Arc1.Radius = 0) or (Arc2.Radius = 0);
end;

procedure TLensF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  if IsEmpty then
    Exit;

  var path := TSkPathBuilder.interfaced;
  path.MoveTo(Point1);
  path.AddArc(Arc2.BoundingRect, Arc2.StartAngle, Arc2.SweepAngle);
  path.AddArc(Arc1.BoundingRect, Arc1.StartAngle, Arc1.SweepAngle);
  path.MoveTo(Point2);
  path.Close;
  path.FillType := TSkPathFillType.EvenOdd;

  ACanvas.DrawPath(path.Detach, APaint);
end;

function CircleIntersection(const Circle1, Circle2, Circle3: TCircleF): TVennDiagramF;
  // Helper function to handle two-circle intersections
  function HandleTwoCircleIntersection(const CircleA, CircleB: TCircleF): TLensF;
  var
    dist, a, height, x0, y0, x3, y3, x4: Single;
  begin
    dist := Sqrt(Sqr(CircleB.Center.X - CircleA.Center.X) + Sqr(CircleB.Center.Y - CircleA.Center.Y));

    if (dist > CircleA.Radius + CircleB.Radius) or (dist < Abs(CircleA.Radius - CircleB.Radius)) then
    begin
      // No intersection points, return an empty lens
      Result := Default(TLensF);
      Exit;
    end;

    // Calculate 'a', the distance from center of CircleA to the line joining the intersections
    a := (Sqr(CircleA.Radius) - Sqr(CircleB.Radius) + Sqr(dist)) / (2 * dist);

    // Calculate the height of the triangle formed by the two intersection points
    height := Sqrt(Sqr(CircleA.Radius) - Sqr(a));

    // Calculate the point (x0, y0) which is the midpoint between the intersection points
    x0 := CircleA.Center.X + a * (CircleB.Center.X - CircleA.Center.X) / dist;
    y0 := CircleA.Center.Y + a * (CircleB.Center.Y - CircleA.Center.Y) / dist;

    // Calculate the two intersection points
    x3 := x0 + height * (CircleB.Center.Y - CircleA.Center.Y) / dist;
    y3 := y0 - height * (CircleB.Center.X - CircleA.Center.X) / dist;

    x4 := x0 - height * (CircleB.Center.Y - CircleA.Center.Y) / dist;
    var y4 := y0 + height * (CircleB.Center.X - CircleA.Center.X) / dist;

    Result.Point1 := TPointF.Create(x3, y3);
    Result.Point2 := TPointF.Create(x4, y4);

    // Set the centers and radius in the lens
    Result.Arc1.Center := CircleA.Center;
    Result.Arc1.Radius := CircleA.Radius;
    Result.Arc2.Center := CircleB.Center;
    Result.Arc2.Radius := CircleB.Radius;

    // Calculate angles for CircleA (center at Center1)
    Result.Arc1.StartAngle := ArcTan2(y3 - CircleA.Center.Y, x3 - CircleA.Center.X) * 180 / Pi;
    Result.Arc1.SweepAngle := ArcTan2(y4 - CircleA.Center.Y, x4 - CircleA.Center.X) * 180 / Pi - Result.Arc1.StartAngle;
    Result.Arc1.NormalizeSweep;

    // Calculate angles for CircleB (center at Center2)
    Result.Arc2.StartAngle := ArcTan2(y3 - CircleB.Center.Y, x3 - CircleB.Center.X) * 180 / Pi;
    Result.Arc2.SweepAngle := ArcTan2(y4 - CircleB.Center.Y, x4 - CircleB.Center.X) * 180 / Pi - Result.Arc2.StartAngle - 360;
    Result.Arc2.NormalizeSweep;
  end;

  // Helper function to set the lunes for two intersecting circles
  procedure SetLunesForTwoCircles(var Lune1, Lune2: TLensF; const CircleA, CircleB: TCircleF; const Lens: TLensF);
  begin
    // Lune1: the area formed by CircleA, excluding the intersection
    Lune1.Arc1.Center := CircleA.Center;
    Lune1.Arc1.Radius := CircleA.Radius;
    Lune1.Arc1.StartAngle := Lens.Arc1.StartAngle;
    Lune1.Arc1.SweepAngle := Lens.Arc1.SweepAngle;
    Lune1.Arc1.NormalizeSweep;

    Lune1.Arc2.Center := CircleB.Center;
    Lune1.Arc2.Radius := CircleB.Radius;
    Lune1.Point1 := Lens.Point1;
    Lune1.Point2 := Lens.Point2;

    // Lune2: the area formed by CircleB, excluding the intersection
    Lune2.Arc1.Center := CircleB.Center;
    Lune2.Arc1.Radius := CircleB.Radius;
    Lune2.Arc1.StartAngle := Lens.Arc2.StartAngle;
    Lune2.Arc1.SweepAngle := Lens.Arc2.SweepAngle;
    Lune2.Arc1.NormalizeSweep;

    Lune2.Arc2.Center := CircleA.Center;
    Lune2.Arc2.Radius := CircleA.Radius;
    Lune2.Point1 := Lens.Point1;
    Lune2.Point2 := Lens.Point2;
  end;

begin
  // Calculate two-circle intersections
  SetLength(Result.Circles, 3);
  Result.Circles[0] := Circle1;
  Result.Circles[1] := Circle2;
  Result.Circles[2] := Circle3;

  SetLength(Result.Lenses, 3);
  Result.Lenses[0] := HandleTwoCircleIntersection(Circle1, Circle2);
  Result.Lenses[1] := HandleTwoCircleIntersection(Circle2, Circle3);
  Result.Lenses[2] := HandleTwoCircleIntersection(Circle3, Circle1);

  // Set up the lunes for each pair of circles
  SetLength(Result.Lunes, 6);
  SetLunesForTwoCircles(Result.Lunes[0], Result.Lunes[1], Circle1, Circle2, Result.Lenses[0]);
  SetLunesForTwoCircles(Result.Lunes[2], Result.Lunes[3], Circle2, Circle3, Result.Lenses[1]);
  SetLunesForTwoCircles(Result.Lunes[4], Result.Lunes[5], Circle3, Circle1, Result.Lenses[2]);

  // Reuleaux triangle - calculate intersection common to all three circles
  if not Result.Lenses[0].IsEmpty and not Result.Lenses[1].IsEmpty and not Result.Lenses[2].IsEmpty then
  begin
    SetLength(Result.Reuleaux.Arcs, 3);

    Result.Reuleaux.Arcs[0].Center := Circle1.Center;
    Result.Reuleaux.Arcs[0].Radius := Circle1.Radius;
    Result.Reuleaux.Arcs[0].StartAngle := Result.Lenses[0].Arc1.StartAngle;
    Result.Reuleaux.Arcs[0].SweepAngle := Result.Lenses[0].Arc1.SweepAngle;
    Result.Reuleaux.Arcs[0].NormalizeSweep;

    Result.Reuleaux.Arcs[1].Center := Circle2.Center;
    Result.Reuleaux.Arcs[1].Radius := Circle2.Radius;
    Result.Reuleaux.Arcs[1].StartAngle := Result.Lenses[1].Arc1.StartAngle;
    Result.Reuleaux.Arcs[1].SweepAngle := Result.Lenses[1].Arc1.SweepAngle;
    Result.Reuleaux.Arcs[1].NormalizeSweep;

    Result.Reuleaux.Arcs[2].Center := Circle3.Center;
    Result.Reuleaux.Arcs[2].Radius := Circle3.Radius;
    Result.Reuleaux.Arcs[2].StartAngle := Result.Lenses[2].Arc1.StartAngle;
    Result.Reuleaux.Arcs[2].SweepAngle := Result.Lenses[2].Arc1.SweepAngle;
    Result.Reuleaux.Arcs[2].NormalizeSweep;

  end;
end;

procedure TForm4.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var paint := TSkPaint.interfaced;
  paint.Style := TSkPaintStyle.Stroke;
  paint.Color := TAlphaColors.Black;

  var diameter := Min(ADest.Width, ADest.Height);
  var third := diameter/3;

  var Circle1 := TCircleF.Create(diameter/2, third, third);
  var Circle2 := TCircleF.Create(third, third*2, third);
  var Circle3 := TCircleF.Create(third*2, diameter-third, third);
  Circle1.Draw(ACanvas, paint);
  Circle2.Draw(ACanvas, paint);
  Circle3.Draw(ACanvas, paint);


  var colorIndex := 1;
  paint.StrokeWidth := 2;
  paint.Style := TSkPaintStyle.Stroke;
  var Venn := CircleIntersection(Circle1, Circle2, Circle3);
  for var lune in venn.Lunes do
  begin
    paint.Color := DistinctColors[colorIndex];
    Inc(ColorIndex);
    lune.Draw(ACanvas, paint);
  end;
  for var lens in venn.Lenses do
  begin
    paint.Color := DistinctColors[colorIndex];
    Inc(ColorIndex);
    lens.Draw(ACanvas, paint);
  end;
  paint.StrokeWidth := 4;
  paint.Color := TAlphaColors.Red;
  venn.Reuleaux.Draw(ACanvas, paint);

end;

end.
