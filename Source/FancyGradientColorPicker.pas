unit FancyGradientColorPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia,
  UIConsts,
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

function CoordinateToIndex(x, y, w, h, offset: Integer): Integer;
var
  innerW, innerH: Integer;
begin
  // Calculate the dimensions of the inner rectangle
  innerW := w - 2 * offset;
  innerH := h - 2 * offset;

  // Check if the given coordinates are within the bounds of the inner rectangle
  if (x < offset) or (x >= w - offset) or (y < offset) or (y >= h - offset) then
  begin
    Result := -1; // Outside the inner rectangle
    Exit;
  end;

  // Adjust coordinates to be relative to the inner rectangle
  x := x - offset;
  y := y - offset;

  // Top row of the inner rectangle
  if y = 0 then
    Result := x
  // Right column of the inner rectangle
  else if x = innerW - 1 then
    Result := (innerW - 1) + y
  // Bottom row of the inner rectangle
  else if y = innerH - 1 then
    Result := (innerW - 1) + (innerH - 1) + (innerW - 1 - x)
  // Left column of the inner rectangle
  else if x = 0 then
    Result := 2 * (innerW - 1) + (innerH - 1) + (innerH - 1 - y)
  else
    Result := -1;  // This case should never occur for outer pixels of the inner rectangle
end;


procedure TForm4.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var outerSteps := 3;
  var maxSteps := 4;

  var paint: ISkPaint := TSkPaint.Create;
  var font: ISkFont := TSkFont.Create;
  paint.StrokeWidth := 0.1;
  paint.AntiAlias := True;
  paint.Color := TAlphaColors.Black;

  for var i := 1 to (maxSteps) do
  begin
    var step := outerSteps + (outerSteps * (i-1) * (i-1));
    var segWidth := (ADest.Width) / step;
    var segHeight := (ADest.Height) / step;
    var offset := (i-1)*(i-1);
    var segments := (step - offset * 2) * 4 - 4;
    for var x := 0 to step - 1 do
      for var y := 0 to step - 1 do
      begin
        var hueStep := CoordinateToIndex(x,y, step, step, offset);
        if hueStep <> -1 then
        begin
          var topLeft := TPointF.Create(x * segWidth, y * segHeight);
          paint.Style := TSkPaintStyle.Fill;
          paint.Color := HSLToRGB(1/(segments)*hueStep, 1, 0.5);
          ACanvas.DrawRect(TRectF.Create(TopLeft, segWidth, segHeight ), paint);

          paint.Color := TAlphaColors.Black;
          paint.Style := TSkPaintStyle.Stroke;
          ACanvas.DrawRect(TRectF.Create(TopLeft, segWidth, segHeight ), paint);
        end;
      end;
  end;
end;

end.
