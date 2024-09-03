unit SkGradientViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Objects;

type
  TWheelType = (wtPureHue, wtSaturation, wtLuminanceDark, wtLuminanceLight);
  TfrmSkGradientView = class(TForm)
    SkPaintBox1: TSkPaintBox;
    rbSaturation: TRadioButton;
    rbLuminance: TRadioButton;
    rbLuminanceInv: TRadioButton;
    rbNone: TRadioButton;
    edRGB: TEdit;
    lbHSL: TLabel;
    Circle1: TCircle;
    PalettePaintBox: TSkPaintBox;
    Button1: TButton;
    Layout1: TLayout;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure rbLuminanceInvChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkPaintBox1Resize(Sender: TObject);
    procedure PalettePaintBoxDraw(ASender: TObject;
      const ACanvas: ISkCanvas; const ADest: TRectF;
      const AOpacity: Single);
    procedure Button1Click(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure PalettePaintBoxClick(Sender: TObject);
  private
    { Private declarations }
    FBitmap: TBitmap;
    function GetColor(X, Y: Single): TAlphaColor;
    procedure UpdateColorMap;
    procedure MoveCircle(x, y: Single);
    procedure DrawCircle(ACanvas: ISkCanvas; center, origin: TPointF;
      Theta: Single);
    function GetWheelType: TWheelType;
  public
    { Public declarations }
  end;

var
  frmSkGradientView: TfrmSkGradientView;

implementation

uses
  System.UIConsts,
  System.Math,
  FMX.Skia.Canvas;

{$R *.fmx}


function HueGradientArray(Saturation: Single = 1; Luminance: Single = 0.5): TArray<TAlphaColor>;
const
  HueArray: Array of Single =
    [60,120,180,240,300,0,59.9];
//    [90,135,180,225,270,315,0,45,89.9];
//  [90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345,0,15,30,45,60,75,89.9];
begin
  SetLength(Result, Length(HueArray));
  for var idx := 0 to pred(Length(HueArray)) do
  Result[idx] := HSLtoRGB( HueArray[idx]/360, Saturation, Luminance);
end;

procedure RenderColorWheel(ACanvas: ISKCanvas; ADest: TRectF; const AWheelType: TWheelType = wtPureHue);
begin
  var maxdia := Min(ADest.Width/2, ADest.Height/2);
  const max= 100;
  for var dia := max downto 1 do
  begin
    var Saturation: Single := 1;
    var Luminance: Single := 0.5;

    case AWheelType of
      wtSaturation: Saturation := dia/max;
      wtLuminanceDark: Luminance := dia/max*0.5;
      wtLuminanceLight: Luminance := 1-dia/max*0.5;
    end;

    var curDia := dia*maxDia/max;

    var LPaint: ISkPaint := TSkPaint.Create;
    LPaint.Shader :=
      TSkShader.MakeGradientSweep(
        ADest.CenterPoint,
        HueGradientArray(Saturation, Luminance),
        nil,TSkTileMode.Repeat,0,360);
    ACanvas.DrawCircle(ADest.Width/2, ADest.Height/2, curDia, LPaint);
  end;
end;


function IsPointInCircle(x, y, Width, Height: single): Boolean;
begin
  // Calculate the center of the rectangle
  var Cx := Width/2;
  var Cy := Height/2;

  // Calculate the radius of the circle
  var R := Min(Cx, Cy);

  // Calculate the distance from the point to the center of the circle
  var Distance := Sqrt(Sqr(x - Cx) + Sqr(y - Cy));

  // Determine if the point is within the circle
  Result := Distance <= R;
end;

function GetRotatedPoint(StartPoint, Center: TPointF; Radius, Theta: Single): TPointF;
var
  AngleRad, CurrentAngle, NewAngle: Single;
  Distance: Single;
begin
  // Calculate the distance from StartPoint to Center
  Distance := Sqrt(Sqr(StartPoint.X - Center.X) + Sqr(StartPoint.Y - Center.Y));

  // Calculate the current angle (in radians) of StartPoint relative to Center
  CurrentAngle := ArcTan2(StartPoint.Y - Center.Y, StartPoint.X - Center.X);

  // Convert Theta to radians and calculate the new angle
  AngleRad := Theta * Pi / 180;
  NewAngle := CurrentAngle + AngleRad;

  // Calculate the new coordinates based on the new angle and distance
  Result.X := Center.X + Distance * Cos(NewAngle);
  Result.Y := Center.Y + Distance * Sin(NewAngle);
end;


procedure TfrmSkGradientView.FormCreate(Sender: TObject);
begin
  PalettePaintBox.Visible := True;
  PalettePaintBox.BringToFront;
end;

procedure TfrmSkGradientView.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

function TfrmSkGradientView.GetColor(X, Y: Single): TAlphaColor;
begin
  Result := TAlphaColors.Red;
  if not Assigned(FBitmap) then exit;

  var vBitMapData : TBitmapData;
  if FBitmap.Map (TMapAccess.Read, vBitMapData) then
  begin
    try
      Result := vBitmapData.GetPixel(
        trunc(X*FBitmap.BitmapScale),
        trunc(Y*FBitmap.BitmapScale));
    finally
      FBitmap.Unmap(vBitMapData);
    end;
  end;
end;

procedure TfrmSkGradientView.rbLuminanceInvChange(Sender: TObject);
begin
  UpdateColorMap;
end;

procedure TfrmSkGradientView.Button1Click(Sender: TObject);
begin
//  var Stream := TFileStream.Create('colorwheel.svg', fmCreate);
//  try
//    var svg: ISkCanvas := TSkSVGCanvas.Make(SkPaintBox1.BoundsRect, stream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
//    RenderColorWheel(svg, SkPaintBox1.BoundsRect, GetWheelType);
//  finally
//    Stream.Free;
//  end;

  Layout1.Visible := False;
  PalettePaintBox.Visible := False;
  try
    var bitmap := SkPaintBox1.MakeScreenshot;
    bitmap.SaveToFile('colorwheel.bmp');
  finally
    Layout1.Visible := True;
    PalettePaintBox.Visible := True;
  end;
end;

procedure TfrmSkGradientView.DrawCircle(ACanvas: ISkCanvas; center, origin: TPointF; Theta: Single);
begin
  var comp := GetRotatedPoint(origin, Center, Min(Center.x, Center.y), Theta);
  var paint: ISkPaint := TSkPaint.Create;
  paint.Color := TAlphaColors.Black;
  paint.Style := TSkPaintStyle.Stroke;
  paint.AntiAlias := True;
  paint.StrokeWidth := 2;

  ACanvas.DrawCircle(comp, 5, paint);
  ACanvas.DrawLine(origin, comp, paint);

  paint.Color := GetColor(comp.x, comp.y);
  paint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawCircle(comp, 5, paint);

end;

procedure TfrmSkGradientView.PalettePaintBoxClick(Sender: TObject);
begin

end;

//None (1)
//Complementary: 0°, 180°
//Split-complementary: 0°, 150°, 210°
//Analogous: 0°, 30°, 330°
//Triadic: 0°, 120°, 240°
//Tetradic: 0°, 60°, 180°, 240°
//Hexadic: 0°, 60°, 120°, 180°, 240° and 300°

procedure TfrmSkGradientView.PalettePaintBoxDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if Circle1.Visible then
  begin
    var Center := TPointF.Create(ADest.Width/2, ADest.Height/2);
    var origin := Circle1.Position.Point;
    origin.Offset(Circle1.Size.Size.cx/2, Circle1.Size.Size.cy/2);

    DrawCircle(ACanvas, center, origin, 120);
    DrawCircle(ACanvas, center, origin, 240);
    DrawCircle(ACanvas, center, origin, 0);
  end;
end;

procedure TfrmSkGradientView.MoveCircle(x, y: Single);
begin
  if not IsPointInCircle(x,y, SkPaintBox1.Width, SkPaintBox1.Height ) then exit;
  var color := GetColor(X,Y);
  Circle1.Fill.Color := color;
  Circle1.Position.X := X - Circle1.Width/2;
  Circle1.Position.Y := Y - Circle1.Height/2;
  Circle1.Visible := True;
  var colorString := AlphaColorToString(color);
  edRGB.Text := colorString.Remove(1,2);
  PalettePaintBox.Redraw;
end;

function TfrmSkGradientView.GetWheelType: TWheelType;
begin
  if rbSaturation.IsChecked then
    Result := wtSaturation
  else if rbLuminance.IsChecked then
    Result := wtLuminanceLight
  else if rbLuminanceInv.IsChecked then
    Result := wtLuminanceDark
  else
    Result := wtPureHue;
end;

procedure TfrmSkGradientView.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin

  if rbLuminanceInv.IsChecked  then
    Circle1.Stroke.Color := TAlphaColorRec.White
  else
    Circle1.Stroke.Color := TAlphaColorRec.Black;

  RenderColorWheel(ACanvas, ADest, GetWheelType);
end;

procedure TfrmSkGradientView.SkPaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
    MoveCircle(x,y);
end;

procedure TfrmSkGradientView.SkPaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (TShiftStateItem.ssLeft in Shift) then MoveCircle(X,Y);
end;

procedure TfrmSkGradientView.SkPaintBox1Resize(Sender: TObject);
begin
  UpdateColorMap;
end;

procedure TfrmSkGradientView.UpdateColorMap;
begin
  SkPaintBox1.Redraw;
  FreeAndNil(FBitmap);
  SkPaintBox1.Repaint;
  Circle1.Visible := False;
  FBitmap := SkPaintBox1.MakeScreenshot;
  Circle1.Visible := True;
  MoveCircle(
    Circle1.Position.X + Circle1.Width/2,
    Circle1.Position.Y + Circle1.Height/2);
end;

end.
