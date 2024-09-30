unit SkiaCircleMath;

interface

uses
  Skia,
  Types;

{.$DEFINE DebugDraw}

type

  TCircleF = record
    TagString: string;
    Center: TPointF;
    Radius: Single;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
    constructor Create(const Center: TPointF; const Radius: Single; const TagString: string = ''); overload;
    constructor Create(const X, Y, Radius: Single; const TagString: string = ''); overload;
  end;

  TArcF = record
    TagString: string;
    Start, Finish, Center: TPointF;
    Radius: Single;
    StartAngle: Single;
    SweepAngle: Single;
    function IsEmpty: Boolean;
    function BoundingRect: TRectF;
    function AngleFromCenter: Single;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
    function GetArcStartPoint: TPointF;
    function GetArcEndPoint: TPointF;
  end;

  TLensF = record
    TagString: string;
    Arc1, Arc2: TArcF;
    Point1, Point2: TPointF;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
  end;

  TLuneF = record
    TagString: string;
    Arc1, Arc2: TArcF;
    Point1, Point2: TPointF;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
  end;

  TReuleauxF = record
    TagString: string;
    Arcs: array of TArcF;
    function IsEmpty: Boolean;
    procedure Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
  end;

  TVennDiagramF = record
    TagString: string;
    Circles: array of TCircleF;
    Lenses: array of TLensF;
    Lunes: array of TLuneF;
    Reuleaux: TReuleauxF;
  end;

function CircleIntersection(const Circle1, Circle2, Circle3: TCircleF): TVennDiagramF;
function FindReuleaux(const Circle1, Circle2, Circle3: TCircleF): TReuleauxF;
function FindLune(const Circle: TCircleF; const Lens: TLensF): TLuneF;
function FindLens(const CircleA, CircleB: TCircleF): TLensF;
procedure SortArcsClockwise(var Arcs: array of TArcF);
procedure DrawLensLune(const Arc1, Arc2: TArcF; const Point1, Point2: TPointF;
  const ACanvas: ISkCanvas; const APaint: ISkPaint; const TagString: string = '');
procedure RenderVenn(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);

implementation

uses SkiaHelpers, AlphaColorUtils,
  Math,
  UITypes,
  System.Generics.Collections,
  System.Generics.Defaults,
  SysUtils;

procedure DrawLensLune(const Arc1, Arc2: TArcF; const Point1, Point2: TPointF;
  const ACanvas: ISkCanvas; const APaint: ISkPaint; const TagString: string = '');
begin
  var Arc1EndPoint := Arc1.GetArcEndPoint;
  var Arc2StartPoint := Arc2.GetArcStartPoint;

  var path := TSkPathBuilder.interfaced;
  path.ArcTo(Arc1.BoundingRect, Arc1.StartAngle, Arc1.SweepAngle, False);
  if Arc1EndPoint <> Arc2StartPoint then
    path.ArcTo(Arc2.BoundingRect, Arc2.StartAngle + Arc2.SweepAngle, -Arc2.SweepAngle, False)
  else
    path.ArcTo(Arc2.BoundingRect, Arc2.StartAngle, Arc2.SweepAngle, False);

  path.Close;
  path.FillType := TSkPathFillType.EvenOdd;

  {$IFDEF DebugDraw}
  var x := min(Arc1.Start.X, Arc2.Start.X) + abs(Arc1.Start.X - Arc2.Start.X)/2;
  var y := min(Arc1.Start.Y, Arc2.Start.Y) + abs(Arc1.Start.Y - Arc2.Start.Y)/2;
  ACanvas.DrawSimpleText(TagString, x, y, TSkFont.interfaced(), APaint);
  ACanvas.DrawCircle(Point1, 10, APaint);
  ACanvas.DrawCircle(Point2, 10, APaint);
  {$ENDIF}

  ACanvas.DrawPath(path.Detach, APaint);
end;

procedure SortArcsClockwise(var Arcs: array of TArcF);
begin
  TArray.Sort<TArcF>(Arcs, TComparer<TArcF>.Construct(
    function(const A, B: TArcF): Integer
    begin
      // Sort arcs by their angle relative to the center
      if A.AngleFromCenter < B.AngleFromCenter then
        Result := -1
      else if A.AngleFromCenter > B.AngleFromCenter then
        Result := 1
      else
        Result := 0;
    end
  ));
end;

procedure TArcF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  ACanvas.DrawArc(BoundingRect, StartAngle, SweepAngle, False, APaint);
end;

function TArcF.GetArcStartPoint: TPointF;
begin
  Result := BoundingRect.CenterPoint +
            PointF(Cos(StartAngle * (Pi / 180)) * Radius,
                   Sin(StartAngle * (Pi / 180)) * Radius);
end;

function TArcF.GetArcEndPoint: TPointF;
begin
  Result := BoundingRect.CenterPoint +
            PointF(Cos((StartAngle + SweepAngle) * (Pi / 180)) * Radius,
                   Sin((StartAngle + SweepAngle) * (Pi / 180)) * Radius);
end;

function TArcF.IsEmpty: Boolean;
begin
  Result := Radius = 0;
end;

function TArcF.AngleFromCenter: Single;
begin
  Result := ArcTan2(Start.Y - Center.Y, Start.X - Center.X);
end;

function TArcF.BoundingRect: TRectF;
begin
  Result := TRectF.Create(Center.X - Radius, Center.Y - Radius, Center.X + Radius, Center.Y + Radius);
end;

constructor TCircleF.Create(const Center: TPointF; const Radius: Single; const TagString: string = '');
begin
  Self.Center := Center;
  Self.TagString := TagString;
  Self.Radius := Radius;
end;

constructor TCircleF.Create(const X, Y, Radius: Single; const TagString: string = '');
begin
  Self.Center := TPointF.Create(X, Y);
  Self.TagString := TagString;
  Self.Radius := Radius;
end;

procedure TCircleF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  ACanvas.DrawCircle(Center, Radius, APaint);
  {$IFDEF DebugDraw}
  ACanvas.DrawSimpleText(Self.TagString, center.x, center.y, TSkFont.interfaced(), APaint);
  {$ENDIF}
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

  SortArcsClockwise(arcs);

  var path := TSkPathBuilder.interfaced;
  for var Arc in Arcs do
  begin
    path.ArcTo(arc.BoundingRect, arc.StartAngle, arc.SweepAngle, False);
  end;

  path.Close;
  path.FillType := TSkPathFillType.Winding;


  ACanvas.DrawPath(path.Detach, APaint);
  {$IFDEF DebugDraw}
  APaint.Color := TAlphaColors.Black;
  APaint.Style := TSkPaintStyle.Fill;
  var cnt: Integer := 0;
  for var Arc in Arcs do
  begin
    ACanvas.DrawCircle(arc.Start, 5, APaint);
    ACanvas.DrawCircle(arc.Finish, 5, APaint);
    ACanvas.DrawSimpleText(Format('start %d', [cnt]), arc.Start.x + 10, arc.Start.Y + 10, TSkFont.interfaced(), APaint);
    ACanvas.DrawSimpleText(Format('finish %d', [cnt]), arc.Finish.x + 10, arc.Finish.Y + 10, TSkFont.interfaced(), APaint);
    Inc(cnt);
  end;
  {$ENDIF}
end;

function TLensF.IsEmpty: Boolean;
begin
  Result := (Arc1.Radius = 0) or (Arc2.Radius = 0);
end;

procedure TLensF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  if IsEmpty then
    Exit;
  DrawLensLune(Arc1, Arc2, Point1, Point2, ACanvas, APaint, TagString);
end;

function FindLens(const CircleA, CircleB: TCircleF): TLensF;
begin
  var
    dist := Sqrt(
        Sqr(CircleB.Center.X - CircleA.Center.X)
      + Sqr(CircleB.Center.Y - CircleA.Center.Y));

  if (dist > CircleA.Radius + CircleB.Radius) or
    (dist < Abs(CircleA.Radius - CircleB.Radius)) then
  begin
    Result := Default(TLensF);
    Exit;
  end;

  // Calculate 'a', the distance from center of CircleA to the line joining the intersections
  var aDist := (Sqr(CircleA.Radius) - Sqr(CircleB.Radius) + Sqr(dist)) / (2 * dist);

  // Calculate the height of the triangle formed by the two intersection points
  var height := Sqrt(Sqr(CircleA.Radius) - Sqr(aDist));

  // Calculate the point (x0, y0) which is the midpoint between the intersection points
  var x0 := CircleA.Center.X + aDist * (CircleB.Center.X - CircleA.Center.X) / dist;
  var y0 := CircleA.Center.Y + aDist * (CircleB.Center.Y - CircleA.Center.Y) / dist;

  // Calculate the two intersection points
  var x3 := x0 + height * (CircleB.Center.Y - CircleA.Center.Y) / dist;
  var y3 := y0 - height * (CircleB.Center.X - CircleA.Center.X) / dist;

  var x4 := x0 - height * (CircleB.Center.Y - CircleA.Center.Y) / dist;
  var y4 := y0 + height * (CircleB.Center.X - CircleA.Center.X) / dist;

  // Assign intersection points to lens points
  Result.Point1 := TPointF.Create(x3, y3);
  Result.Point2 := TPointF.Create(x4, y4);

  // Set the centers and radius in the lens
  Result.Arc1.Center := CircleA.Center;
  Result.Arc1.Radius := CircleA.Radius;
  Result.Arc2.Center := CircleB.Center;
  Result.Arc2.Radius := CircleB.Radius;

  // Set the Start and Finish points for Arc1 (CircleA)
  Result.Arc1.Start := TPointF.Create(x3, y3);
  Result.Arc1.Finish := TPointF.Create(x4, y4);

  // Set the Start and Finish points for Arc2 (CircleB)
  Result.Arc2.Start := TPointF.Create(x4, y4);
  Result.Arc2.Finish := TPointF.Create(x3, y3);

  // Calculate angles for CircleA (center at CircleA)
  Result.Arc1.StartAngle := ArcTan2(y3 - CircleA.Center.Y, x3 - CircleA.Center.X) * 180 / Pi;
  Result.Arc1.SweepAngle := ArcTan2(y4 - CircleA.Center.Y, x4 - CircleA.Center.X) * 180 / Pi - Result.Arc1.StartAngle;

  // Normalize the sweep angle to ensure it's a positive value within 0-360 degrees
  if Result.Arc1.SweepAngle < 0 then
    Result.Arc1.SweepAngle := Result.Arc1.SweepAngle + 360;
  if Result.Arc1.SweepAngle > 180 then
    Result.Arc1.SweepAngle := 180;  // Ensure it doesn't exceed the lens shape

  // Calculate angles for CircleB (center at CircleB)
  Result.Arc2.StartAngle := ArcTan2(y3 - CircleB.Center.Y, x3 - CircleB.Center.X) * 180 / Pi;
  Result.Arc2.SweepAngle := ArcTan2(y4 - CircleB.Center.Y, x4 - CircleB.Center.X) * 180 / Pi - Result.Arc2.StartAngle - 360;

  // Normalize the sweep angle to ensure it's a positive value within 0-360 degrees
  if Result.Arc2.SweepAngle < 0 then
    Result.Arc2.SweepAngle := Result.Arc2.SweepAngle + 360;
  if Result.Arc2.SweepAngle > 180 then
    Result.Arc2.SweepAngle := 180;  // Ensure it doesn't exceed the lens shape
end;

function FindLune(const Circle: TCircleF; const Lens: TLensF): TLuneF;
  // Local helper function to normalize the sweep angle
  function NormalizeAngle(Angle: Single): Single;
  begin
    if Angle < 0 then
      Result := Angle + 360
    else
      Result := Angle;
  end;

begin
  // Assign the intersection points (the same as Lens points)
  Result.Point1 := Lens.Point1;
  Result.Point2 := Lens.Point2;

  // Determine which arc from the lens to use based on the circle
  if (Circle.Center = Lens.Arc1.Center) and (Circle.Radius = Lens.Arc1.Radius) then
  begin
    Result.Arc1 := Lens.Arc1;
    Result.Arc2 := Lens.Arc2;
  end
  else if (Circle.Center = Lens.Arc2.Center) and (Circle.Radius = Lens.Arc2.Radius) then
  begin
    Result.Arc1 := Lens.Arc2;
    Result.Arc2 := Lens.Arc1;
  end
  else
    Exit(Default(TLuneF));

  // Calculate the sweep angle for the lune. We subtract the lens arc's sweep from 360.
  if Result.Arc1.SweepAngle < 0 then
  begin
    Result.Arc1.SweepAngle := 360 + Result.Arc1.SweepAngle;
  end
  else
  begin
    Result.Arc1.StartAngle := NormalizeAngle(ArcTan2(Lens.Point2.Y - Circle.Center.Y, Lens.Point2.X - Circle.Center.X) * 180 / Pi);
    Result.Arc1.SweepAngle := 360 - Result.Arc1.SweepAngle;
  end;

end;

function FindReuleaux(const Circle1, Circle2, Circle3: TCircleF): TReuleauxF;
var
  Lens12, Lens23, Lens31: TLensF;  // Lenses between pairs of circles
begin
  SetLength(Result.Arcs, 3);

  // Step 1: Calculate the lenses for each pair of circles
  Lens12 := FindLens(Circle1, Circle2);  // Lens between Circle1 and Circle2
  Lens23 := FindLens(Circle2, Circle3);  // Lens between Circle2 and Circle3
  Lens31 := FindLens(Circle3, Circle1);  // Lens between Circle3 and Circle1

  // Step 2: Create the first arc (from Circle1, using points from Lens12 and Lens31)
  Result.Arcs[0].Center := Circle1.Center;
  Result.Arcs[0].Radius := Circle1.Radius;

  // The start of the arc is from Lens12 (where Circle1 and Circle2 intersect)
  Result.Arcs[0].Start := Lens12.Point1;  // Intersection between Circle1 and Circle2

  // Finish point is now connected to the start of Arc 3
  Result.Arcs[0].Finish := Lens31.Point1;

  // Step 3: Create the second arc (from Circle2, using points from Lens23 and Lens12)
  Result.Arcs[1].Center := Circle2.Center;
  Result.Arcs[1].Radius := Circle2.Radius;

  // The start of the arc is from Lens23 (where Circle2 and Circle3 intersect)
  Result.Arcs[1].Start := Lens23.Point1;  // Intersection between Circle2 and Circle3

  // Finish point is now connected to the start of Arc 1
  Result.Arcs[1].Finish := Result.Arcs[0].Start;

  // Step 4: Create the third arc (from Circle3, using points from Lens31 and Lens23)
  Result.Arcs[2].Center := Circle3.Center;
  Result.Arcs[2].Radius := Circle3.Radius;

  // The start of the arc is from Lens31 (where Circle3 and Circle1 intersect)
  Result.Arcs[2].Start := Lens31.Point1;  // Intersection between Circle3 and Circle1

  // Finish point is now connected to the start of Arc 2
  Result.Arcs[2].Finish := Result.Arcs[1].Start;

  // Step 5: Calculate the angles and sweep angles for each arc

  // Arc1 (Circle1)
  Result.Arcs[0].StartAngle := ArcTan2(Result.Arcs[0].Start.Y - Result.Arcs[0].Center.Y, Result.Arcs[0].Start.X - Result.Arcs[0].Center.X) * 180 / Pi;
  Result.Arcs[0].SweepAngle := ArcTan2(Result.Arcs[0].Finish.Y - Result.Arcs[0].Center.Y, Result.Arcs[0].Finish.X - Result.Arcs[0].Center.X) * 180 / Pi - Result.Arcs[0].StartAngle;
  if Result.Arcs[0].SweepAngle < 0 then
    Result.Arcs[0].SweepAngle := Result.Arcs[0].SweepAngle + 360;
  if Result.Arcs[0].SweepAngle > 180 then
    Result.Arcs[0].SweepAngle := Result.Arcs[0].SweepAngle - 180;  // Keep it within 180 degrees

  // Arc2 (Circle2)
  Result.Arcs[1].StartAngle := ArcTan2(Result.Arcs[1].Start.Y - Result.Arcs[1].Center.Y, Result.Arcs[1].Start.X - Result.Arcs[1].Center.X) * 180 / Pi;
  Result.Arcs[1].SweepAngle := ArcTan2(Result.Arcs[1].Finish.Y - Result.Arcs[1].Center.Y, Result.Arcs[1].Finish.X - Result.Arcs[1].Center.X) * 180 / Pi - Result.Arcs[1].StartAngle;
  if Result.Arcs[1].SweepAngle < 0 then
    Result.Arcs[1].SweepAngle := Result.Arcs[1].SweepAngle + 360;
  if Result.Arcs[1].SweepAngle > 180 then
    Result.Arcs[1].SweepAngle := 180;  // Keep it within 180 degrees

  // Arc3 (Circle3)
  Result.Arcs[2].StartAngle := ArcTan2(Result.Arcs[2].Start.Y - Result.Arcs[2].Center.Y, Result.Arcs[2].Start.X - Result.Arcs[2].Center.X) * 180 / Pi;
  Result.Arcs[2].SweepAngle := ArcTan2(Result.Arcs[2].Finish.Y - Result.Arcs[2].Center.Y, Result.Arcs[2].Finish.X - Result.Arcs[2].Center.X) * 180 / Pi - Result.Arcs[2].StartAngle;
  if Result.Arcs[2].SweepAngle < 0 then
    Result.Arcs[2].SweepAngle := Result.Arcs[2].SweepAngle + 360;
  if Result.Arcs[2].SweepAngle > 180 then
    Result.Arcs[2].SweepAngle := 180;  // Keep it within 180 degrees
end;

function CircleIntersection(const Circle1, Circle2, Circle3: TCircleF): TVennDiagramF;
begin
  // Calculate two-circle intersections
  SetLength(Result.Circles, 3);
  Result.Circles[0] := Circle1;
  Result.Circles[1] := Circle2;
  Result.Circles[2] := Circle3;

  SetLength(Result.Lenses, 3);
  Result.Lenses[0] := FindLens(Circle1, Circle2);
  Result.Lenses[0].TagString := 'Lens of 1 & 2';
  Result.Lenses[1] := FindLens(Circle3, Circle2);
  Result.Lenses[1].TagString := 'Lens of 2 & 3';
  Result.Lenses[2] := FindLens(Circle3, Circle1);
  Result.Lenses[2].TagString := 'Lens of 3 & 1';

  // Set up the lunes for each pair of circles
  SetLength(Result.Lunes, 6);
  Result.Lunes[0] := FindLune(Circle1, Result.Lenses[0]);
  Result.Lunes[0].TagString := 'Lune 1 of 1 & 2';
  Result.Lunes[1] := FindLune(Circle1, Result.Lenses[2]);
  Result.Lunes[1].TagString := 'Lune 1 of 1 & 3';
  Result.Lunes[2] := FindLune(Circle2, Result.Lenses[1]);
  Result.Lunes[2].TagString := 'Lune 1 of 2 & 3';
  Result.Lunes[3] := FindLune(Circle2, Result.Lenses[0]);
  Result.Lunes[3].TagString := 'Lune 2 of 1 & 2';
  Result.Lunes[4] := FindLune(Circle3, Result.Lenses[2]);
  Result.Lunes[4].TagString := 'Lune 2 of 1 & 3';
  Result.Lunes[5] := FindLune(Circle3, Result.Lenses[1]);
  Result.Lunes[5].TagString := 'Lune 2 of 2 & 3';


  // Reuleaux triangle - calculate intersection common to all three circles
  if not Result.Lenses[0].IsEmpty and
     not Result.Lenses[1].IsEmpty and
     not Result.Lenses[2].IsEmpty then
  begin
    Result.Reuleaux := FindReuleaux(Circle1, Circle2, Circle3);

    Result.Reuleaux.Arcs[0].TagString := 'Reuleaux Arc of Circle 1';
    Result.Reuleaux.Arcs[1].TagString := 'Reuleaux Arc of Circle 2';
    Result.Reuleaux.Arcs[2].TagString := 'Reuleaux Arc of Circle 3';
  end;
end;

procedure RenderVenn(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var paint := TSkPaint.interfaced;

  const buffer = 10;

  var diameter := Min(ADest.Width, ADest.Height)-buffer*3;
  var third := (diameter/3);

  var Circle1 := TCircleF.Create(diameter/2 + buffer, third + buffer, third, 'Circle 1');
  var Circle2 := TCircleF.Create(third + buffer, third*2 + buffer, third, 'Circle 2');
  var Circle3 := TCircleF.Create(third*2 + buffer, diameter-third + buffer, third, 'Circle 3');

  paint.Style := TSkPaintStyle.Stroke;
  paint.StrokeWidth := 2;
  paint.Color := TAlphaColors.Black;
  Circle1.Draw(ACanvas, paint);
  Circle2.Draw(ACanvas, paint);
  Circle3.Draw(ACanvas, paint);

  paint.Style := TSkPaintStyle.StrokeAndFill;
  var Venn := CircleIntersection(Circle1, Circle2, Circle3);

  var colorIndex := 0;
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

  paint.StrokeWidth := 5;
  paint.Style := TSkPaintStyle.StrokeAndFill;
  paint.Color := DistinctColors[colorIndex];
  venn.Reuleaux.Draw(ACanvas, paint);

  paint.Style := TSkPaintStyle.Stroke;
  paint.StrokeWidth := 2;
  paint.Color := TAlphaColors.Black;
  Circle1.Draw(ACanvas, paint);
  Circle2.Draw(ACanvas, paint);
  Circle3.Draw(ACanvas, paint);
end;

{ TLuneF }

procedure TLuneF.Draw(const ACanvas: ISkCanvas; const APaint: ISkPaint);
begin
  DrawLensLune(Arc1, Arc2, Point1, Point2, ACanvas, APaint, TagString);
end;

function TLuneF.IsEmpty: Boolean;
begin
  Result := (Arc1.Radius = 0) or (Arc2.Radius = 0);
end;

end.
