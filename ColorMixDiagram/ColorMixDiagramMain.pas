unit ColorMixDiagramMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Skia, FMX.Skia,
  Math, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  System.Generics.Collections,
  System.Generics.Defaults, FMX.Colors, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm4 = class(TForm)
    SkPaintBox1: TSkPaintBox;
    Layout1: TLayout;
    Button1: TButton;
    SkSvg1: TSkSvg;
    HueTrackBar1: THueTrackBar;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    chkSync: TCheckBox;
    HueTrackBar3: THueTrackBar;
    HueTrackBar2: THueTrackBar;
    Hex1: TMemo;
    Hex2: TMemo;
    Hex3: TEdit;
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HueTrackBar1Change(Sender: TObject);
    procedure HueTrackBar2Change(Sender: TObject);
    procedure HueTrackBar3Change(Sender: TObject);
  private
    procedure UpdateHex;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses SkiaHelpers, AlphaColorUtils, SkiaCircleMath,
  UIConsts;

procedure RenderVenn(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single = 1;
  const Color1: TAlphaColor = TAlphaColors.Red;
  const Color2: TAlphaColor = TAlphaColors.Lime;
  const Color3: TAlphaColor = TAlphaColors.Blue);
begin
  var paint := TSkPaint.interfaced;

  const spacing = 10;

  var diameter := Min(ADest.Width, ADest.Height)-spacing*3;
  var third := (diameter/3);
  var xoffset := (ADest.Width - diameter)/2;
  var yoffset := (ADest.Height - diameter)/2;

  var Circle1 := TCircleF.Create(
    xoffset+diameter/2 + spacing,
    yoffset+third + spacing,
    third, 'Circle 1');
  var Circle2 := TCircleF.Create(
    xoffset+third + spacing,
    yoffset+third*2 + spacing,
    third, 'Circle 2');
  var Circle3 := TCircleF.Create(
    xoffset+third*2 + spacing,
    yoffset+diameter-third + spacing,
    third, 'Circle 3');

  paint.Style := TSkPaintStyle.Fill;
  var Venn := CircleIntersection(Circle1, Circle2, Circle3);

  paint.StrokeWidth := 3;
  paint.Color := Color1;
  venn.lunes[1].Draw(ACanvas, paint);
  paint.Color := Color2;
  venn.lunes[3].Draw(ACanvas, paint);
  paint.Color := Color3;
  venn.lunes[5].Draw(ACanvas, paint);

  paint.Color := BlendHSL(Color1,Color2);
  venn.lenses[0].Draw(ACanvas, paint);
  paint.Color := BlendHSL(Color3,Color2);
  venn.lenses[1].Draw(ACanvas, paint);
  paint.Color := BlendHSL(Color3,Color1);
  venn.lenses[2].Draw(ACanvas, paint);

  paint.StrokeWidth := 5;
  paint.Color := TAlphaColors.White;
  venn.Reuleaux.Draw(ACanvas, paint);
//
  paint.Style := TSkPaintStyle.Stroke;
  paint.StrokeWidth := 1;
  paint.Color := TAlphaColors.Black;
  Circle1.Draw(ACanvas, paint);
  Circle2.Draw(ACanvas, paint);
  Circle3.Draw(ACanvas, paint);
end;


procedure CreateSVG(const AOutputFileName: string; const ADest: TRectF);
begin
  var LStream := TFileStream.Create(AOutputFileName, fmCreate);
  try
    var LCanvas := TSkSVGCanvas.Make(ADest, LStream, [TSkSVGCanvasFlag.ConvertTextToPaths]);
    RenderVenn(LCanvas, ADest, 1);
    LCanvas := nil;
  finally
    LStream.Free;
  end;
end;


procedure TForm4.Button1Click(Sender: TObject);
begin
  CreateSVG('Venn.svg',TRectF.Create(0,0,1000,1000));
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  SkSvg1.Visible := False;
end;

procedure TForm4.HueTrackBar1Change(Sender: TObject);
begin
  SkPaintBox1.Redraw;
  if chkSync.IsChecked then
  begin
    var val2 := HueTrackBar1.Value + 1/3;
    if val2 > 1 then val2 := val2-1;
    HueTrackBar2.Value := val2;

    var val3 := val2 + 1/3;
    if val3 > 1 then val3 := val3-1;
    HueTrackBar3.Value := val3;
  end;

end;

procedure TForm4.HueTrackBar2Change(Sender: TObject);
begin
  SkPaintBox1.Redraw;
end;

procedure TForm4.HueTrackBar3Change(Sender: TObject);
begin
  SkPaintBox1.Redraw;
end;

procedure TForm4.UpdateHex();
begin
  var C1 := HSLtoRGB(HueTrackBar1.Value, 1, 0.5);
  var C2 := HSLtoRGB(HueTrackBar2.Value, 1, 0.5);
  var C3 := HSLtoRGB(HueTrackBar3.Value, 1, 0.5);

  var B1 := BlendHSL(C1,C2);
  var B2 := BlendHSL(C1,C3);
  var B3 := BlendHSL(C3,C2);
  var C1S := IntToHex(C1).Remove(0,2);
  var C2S := IntToHex(C2).Remove(0,2);;
  var C3S := IntToHex(C3).Remove(0,2);;
  var B1S := IntToHex(C1).Remove(0,2);
  var B2S := IntToHex(C1).Remove(0,2);
  var B3S := IntToHex(C1).Remove(0,2);

  Hex1.Lines.Text := Format('  %s',[c1s]);
  Hex1.Lines.Add(Format('⊗ %s',[c2s]));
  Hex1.Lines.Add('---------');
  Hex1.Lines.Add(Format('= %s',[B1S]));

  Hex2.Lines.Text := Format('  %s',[c1s]);
  Hex2.Lines.Add(Format('⊗ %s',[c3s]));
  Hex2.Lines.Add('---------');
  Hex2.Lines.Add(Format('= %s',[B2S]));

  Hex3.Text := Format('%s ⊗ %s = %s',[c2s, c3s, b3s]);
end;

procedure TForm4.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  RenderVenn(ACanvas, ADest, AOpacity,
    HSLtoRGB(HueTrackBar1.Value, 1, 0.5),
    HSLtoRGB(HueTrackBar2.Value, 1, 0.5),
    HSLtoRGB(HueTrackBar3.Value, 1, 0.5));
  UpdateHex;
end;

end.
