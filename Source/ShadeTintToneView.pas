unit ShadeTintToneView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Colors, FMX.Controls.Presentation, FMX.Objects,
  FMX.Layouts, System.Skia, FMX.Skia, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  System.UIConsts, FMX.ListBox;

type
  TfrmShadeTintTone = class(TForm)
    Hue: THueTrackBar;
    Layout1: TLayout;
    Layout2: TLayout;
    PaintShade: TSkPaintBox;
    PaintTone: TSkPaintBox;
    PaintTint: TSkPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinBox1: TSpinBox;
    Rectangle1: TRectangle;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    procedure PaintShadeDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure PaintTintDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure PaintToneDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure HueChange(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
  private
    procedure RefreshPaints;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmShadeTintTone: TfrmShadeTintTone;

implementation

{$R *.fmx}

procedure TfrmShadeTintTone.HueChange(Sender: TObject);
begin
  Rectangle1.Fill.Color := HSLtoRGB(Hue.Value, 1, 0.5);

  RefreshPaints;

end;

procedure TfrmShadeTintTone.RefreshPaints;
begin
  PaintShade.Redraw;
  PaintTone.Redraw;
  PaintTint.Redraw;
end;

procedure TfrmShadeTintTone.SpinBox1Change(Sender: TObject);
begin
  RefreshPaints;
end;

procedure TfrmShadeTintTone.PaintShadeDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var sampleCount := trunc(SpinBox1.Value);
  var sampleWidth := ADest.Width / sampleCount;
  var paint : ISkPaint := TSkPaint.Create;
  ListBox3.Clear;
  for var s := 0 to sampleCount do
  begin
    var L := s/sampleCount * 0.5;
    ListBox3.Items.Add(l.ToString);

    paint.Color := HSLToRGB(Hue.Value, 1, l);
    ACanvas.DrawRect(TRectF.Create(s * sampleWidth, ADest.Top,
      s * SampleWidth + SampleWidth, ADest.Height ), paint);
  end;
end;

procedure TfrmShadeTintTone.PaintToneDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var sampleCount := trunc(SpinBox1.Value);
  var sampleWidth := ADest.Width / sampleCount;
  var paint : ISkPaint := TSkPaint.Create;
  ListBox2.Clear;
  for var s := 0 to sampleCount do
  begin
    var sat := s/sampleCount;
    ListBox2.Items.Add(sat.ToString);
    paint.Color := HSLToRGB(Hue.Value, sat, 0.5);
    ACanvas.DrawRect(TRectF.Create(s * sampleWidth, ADest.Top,
      s * SampleWidth + SampleWidth, ADest.Height ), paint);
  end;
end;

procedure TfrmShadeTintTone.PaintTintDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var sampleCount := trunc(SpinBox1.Value);
  var sampleWidth := ADest.Width / sampleCount;
  var paint : ISkPaint := TSkPaint.Create;
  ListBox1.Clear;
  for var s := 0 to sampleCount do
  begin
    var L := 1-s/sampleCount * 0.5;
    ListBox1.Items.Add(l.ToString);
    paint.Color := HSLToRGB(Hue.Value, 1, l);
    ACanvas.DrawRect(TRectF.Create(s * sampleWidth, ADest.Top,
      s * SampleWidth + SampleWidth, ADest.Height ), paint);
  end;

end;

end.
