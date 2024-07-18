unit SkGradientViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Objects;

type
  TSkGradientView = class(TForm)
    SkPaintBox1: TSkPaintBox;
    rbSaturation: TRadioButton;
    rbLuminance: TRadioButton;
    rbLuminanceInv: TRadioButton;
    rbNone: TRadioButton;
    edRGB: TEdit;
    lbHSL: TLabel;
    Circle1: TCircle;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure rbLuminanceInvChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkPaintBox1Resize(Sender: TObject);
  private
    { Private declarations }
    FBitmap: TBitmap;
    procedure GetColor(X,Y: Single);
    procedure UpdateColorMap;
  public
    { Public declarations }
  end;

var
  SkGradientView: TSkGradientView;

implementation

uses
  System.UIConsts,
  System.Math,
  FMX.Skia.Canvas;

{$R *.fmx}

function HueGradientArray(Saturation: Single = 1; Luminance: Single = 0.5): TArray<TAlphaColor>;
begin
  Result :=
    [HSLtoRGB(   0, Saturation, Luminance),
     HSLtoRGB( 1/7, Saturation, Luminance),
     HSLtoRGB( 2/7, Saturation, Luminance),
     HSLtoRGB( 3/7, Saturation, Luminance),
     HSLtoRGB( 4/7, Saturation, Luminance),
     HSLtoRGB( 5/7, Saturation, Luminance),
     HSLtoRGB( 6/7, Saturation, Luminance),
     HSLtoRGB(0.99, Saturation, Luminance)
    ];
end;

procedure TSkGradientView.FormCreate(Sender: TObject);
begin
  //
end;

procedure TSkGradientView.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TSkGradientView.GetColor(X, Y: Single);
begin
  var vBitMapData : TBitmapData;
  if FBitmap.Map (TMapAccess.Read, vBitMapData) then
  begin
    try
      var color := vBitmapData.GetPixel(trunc(X), trunc(Y));
      Circle1.Fill.Color := color;
      Circle1.Position.X := X - Circle1.Width/2;
      Circle1.Position.Y := Y - Circle1.Height/2;
      Circle1.Visible := True;
      edRGB.Text := IntToHex(color);
    finally
      FBitmap.Unmap(vBitMapData);
    end;
  end;
end;

procedure TSkGradientView.rbLuminanceInvChange(Sender: TObject);
begin
  UpdateColorMap;
end;

procedure TSkGradientView.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var maxdia := Min(SkPaintBox1.Width/2, SkPaintBox1.Height/2);
  const max= 100;
  for var dia := max downto 1 do
  begin
    var Saturation: Single := 1;
    var Luminance: Single := 0.5;

    if rbSaturation.IsChecked then
      Saturation := dia/max
    else if rbLuminanceInv.IsChecked then
      Luminance := dia/max*0.5
    else if rbLuminance.IsChecked then
      Luminance := 1-dia/max*0.5;

    if rbLuminanceInv.IsChecked then
      Circle1.Stroke.Color := TAlphaColorRec.White
    else
      Circle1.Stroke.Color := TAlphaColorRec.Black;

    var curDia := dia*maxDia/max;

    var LPaint: ISkPaint := TSkPaint.Create;
    LPaint.Shader :=
      TSkShader.MakeGradientSweep(
        ADest.CenterPoint,
        HueGradientArray(Saturation, Luminance));
    ACanvas.DrawCircle(
      SkPaintBox1.Width/2, SkPaintBox1.Height/2, curDia, LPaint);
  end;
end;

procedure TSkGradientView.SkPaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then GetColor(X,Y);
end;

procedure TSkGradientView.SkPaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (TShiftStateItem.ssLeft in Shift) then GetColor(X,Y);
end;

procedure TSkGradientView.SkPaintBox1Resize(Sender: TObject);
begin
  UpdateColorMap;
end;

procedure TSkGradientView.UpdateColorMap;
begin
  SkPaintBox1.Redraw;
  FreeAndNil(FBitmap);
  SkPaintBox1.Repaint;
  Circle1.Visible := False;
  FBitmap := SkPaintBox1.MakeScreenshot;
  Circle1.Visible := True;
  GetColor(
    Circle1.Position.X + Circle1.Width/2,
    Circle1.Position.Y + Circle1.Height/2);
end;

end.
