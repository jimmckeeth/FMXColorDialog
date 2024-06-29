unit ColorSliderConverter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.StdCtrls, System.UIConsts,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Colors, Jim.FMX.TrackAndSpin,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.ListBox,
  Jim.Color.Palettes, System.Math.Vectors, FMX.Controls3D, FMX.Layers3D,
  FMX.Objects;

type
  TForm1 = class(TForm)
    RGBRed: TGradientTrackSpin;
    RGBGreen: TGradientTrackSpin;
    RGBBlue: TGradientTrackSpin;
    Layout9: TLayout;
    Layout10: TLayout;
    Label9: TLabel;
    RGBString: TEdit;
    Layout1: TLayout;
    HSLHue: THueTrackSpin;
    HSLSaturation: TGradientTrackSpin;
    HSLLuminance: TBWTrackSpin;
    Layout2: TLayout;
    Label1: TLabel;
    Layout3: TLayout;
    CMYKBlack: TGradientTrackSpin;
    Layout4: TLayout;
    Label2: TLabel;
    CMYKCyan: TGradientTrackSpin;
    CMYKMagenta: TGradientTrackSpin;
    CMYKYellow: TGradientTrackSpin;
    ColorBox1: TColorBox;
    AlphaTrackSpin1: TAlphaTrackSpin;
    ColorQuadPicker: TColorQuad;
    ColorTriad1: TColorBox;
    ColorTriad2: TColorBox;
    ColorQuad2: TColorBox;
    ColorQuad3: TColorBox;
    ColorQuad1: TColorBox;
    ColorSplit1: TColorBox;
    ColorSplit2: TColorBox;
    ColorAnalogous1: TColorBox;
    ColorAnalogous2: TColorBox;
    ColorRectangle1: TColorBox;
    ColorRectangle2: TColorBox;
    ColorRectangle3: TColorBox;
    Label3: TLabel;
    Label4: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ColorComplementary: TCircle;
    ColorListBox1: TColorListBox;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Splitter1: TSplitter;
    procedure ColorTrackChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ColorListBox1Change(Sender: TObject);
    procedure ColorComplementaryDblClick(Sender: TObject);
    procedure ColorSplit1DblClick(Sender: TObject);
  private
    { Private declarations }
    var Updating: Boolean;
    procedure UpdateHSL;
    procedure UpdateCMYK;
    procedure UpdateRGB;
    procedure UpdateUI;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  case random(3) of
    0:
    begin
      RGBRed.Value := Random(255);
      RGBGreen.Value := Random(255);
      RGBBlue.Value := Random(255);
    end;
    1:
    begin
      HSLHue.Value := Random(360);
      HSLSaturation.Value := Random(100);
      if random(5)=0 then
        HSLLuminance.Value := Random(100)
      else
        HSLLuminance.Value := 40 + Random(20);
    end;
    2:
    begin
      CMYKCyan.Value := Random(100);
      CMYKMagenta.Value := Random(100);
      CMYKYellow.Value := Random(100);
      if random(5)=0 then
        CMYKBlack.Value := Random(100)
      else
        CMYKBlack.Value := Random(40);
    end;
  end;
end;

procedure TForm1.ColorComplementaryDblClick(Sender: TObject);
begin
  ColorBox1.Color := (Sender as TShape).Fill.Color;
  UpdateHSL;
  UpdateUI;
end;

procedure TForm1.ColorListBox1Change(Sender: TObject);
begin
  ColorBox1.Color := ColorListBox1.Color;
  UpdateHSL;
  UpdateUI;
end;

procedure TForm1.ColorSplit1DblClick(Sender: TObject);
begin
  ColorBox1.Color := (Sender as TColorBox).Color;
  UpdateHSL;
  UpdateUI;
end;

procedure TForm1.ColorTrackChange(Sender: TObject);
begin
  if Updating then exit;
  Updating := True;
  try
    case (Sender as TCustomTrackSpin).Tag of
      1: // RGB
      begin
        RGBString.Text := Format('#%.2x%.2x%.2x%.2x',[
          Trunc(AlphaTrackSpin1.Value*2.55), Trunc(RGBRed.Value),
          Trunc(RGBGreen.Value), Trunc(RGBBlue.Value)]);
        ColorBox1.Color := StringToAlphaColor(RGBString.Text);
        UpdateHSL;
        UpdateCMYK;
      end;
      2: // HSL
      begin
        ColorQuadPicker.Hue := HSLHue.Value/360;
        ColorQuadPicker.Sat := HSLSaturation.Value/100;
        ColorQuadPicker.Lum := HSLLuminance.Value/100;
        UpdateHSL; // Trackbar
        UpdateCMYK;
        UpdateRGB;
      end;
      3: // CMYK
      begin
        var k := CMYKBlack.Value/100;
        var CR: TAlphaColorRec;
        CR.R := Round(255 * (1-CMYKCyan.Value/100) * (1-K));
        CR.G := Round(255 * (1-CMYKMagenta.Value/100) * (1-K));
        CR.B := Round(255 * (1-CMYKYellow.Value/100) * (1-K));
        CR.A := Round(AlphaTrackSpin1.Value * 2.55);
        ColorBox1.Color := TAlphaColor(CR);
        UpdateHSL;
        UpdateRGB;
      end;
    end;
    UpdateUI;
  finally
    Updating := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Updating := False;
  UpdateHSL;
  UpdateUI;
end;

procedure TForm1.UpdateCMYK;
begin
  var RGBColor := ColorBox1.Color;

  if (RGBColor = claBlack) or (RGBColor = 0) then
  begin
    CMYKCyan.Value := 0.00;
    CMYKMagenta.Value := 0.00;
    CMYKYellow.Value := 0.00;
    CMYKBlack.Value := 1.00;
  end
  else
  begin
    var r := 1 - (TAlphaColorRec(RGBColor).R / 255);
    var g := 1 - (TAlphaColorRec(RGBColor).G / 255);
    var b := 1 - (TAlphaColorRec(RGBColor).B / 255);
    var k := Min(r, Min(g, b));
    CMYKCyan.Value := (r - k) / (1 - k) * 100;
    CMYKMagenta.Value := (g - k) / (1 - k) * 100;
    CMYKYellow.Value := (b - k) / (1 - k) * 100;
    CMYKBlack.Value := k * 100;
  end;
end;

procedure TForm1.UpdateHSL;
begin
  var H, S, L: Single;
  RGBtoHSL(ColorBox1.Color, H, S, L);
  HSLHue.Value := H*360;
  HSLSaturation.Value := S*100;
  HSLLuminance.Value := L*100;
  // Trackbar
  HSLSaturation.ColorBegin := HSLtoRGB(H,0,L);
  HSLSaturation.ColorEnd := HSLtoRGB(H,1,L);
end;

procedure TForm1.UpdateRGB;
begin
  var RGBColor := ColorBox1.Color;
  RGBRed.Value := TAlphaColorRec(RGBColor).R;
  RGBGreen.Value := TAlphaColorRec(RGBColor).G;
  RGBBlue.Value := TAlphaColorRec(RGBColor).B;
  RGBString.Text := Format('#%.2x%.2x%.2x%.2x',[
    Trunc(AlphaTrackSpin1.Value*2.55), Trunc(RGBRed.Value),
    Trunc(RGBGreen.Value), Trunc(RGBBlue.Value)]);
end;

procedure TForm1.UpdateUI;
begin
  ColorQuadPicker.Hue := HSLHue.Value/360;
  ColorQuadPicker.Sat := HSLSaturation.Value/100;
  ColorQuadPicker.Lum := HSLLuminance.Value/100;

  ColorComplementary.Fill.Color := GetComplementaryColor(ColorBox1.Color);
  var Triad := GetTriadPalette(ColorBox1.Color);
  ColorTriad1.Color := triad[1];
  ColorTriad2.Color := triad[2];

  var quad := GetQuadPalette(ColorBox1.Color);
  ColorQuad1.Color := quad[1];
  ColorQuad2.Color := quad[2];
  ColorQuad3.Color := quad[3];

  var split := GetSplitComplementaryPalette(ColorBox1.Color);
  ColorSplit1.Color := split[1];
  ColorSplit2.Color := split[2];

  var Analogous := GetAnalogousPalette(ColorBox1.Color);
  ColorAnalogous1.Color := Analogous[1];
  ColorAnalogous2.Color := Analogous[2];

  var Rectangle := GetRectanglePalette(ColorBox1.Color);
  ColorRectangle1.Color := Rectangle[1];
  ColorRectangle2.Color := Rectangle[2];
  ColorRectangle3.Color := Rectangle[3];

  var all := GetAllPalette(ColorBox1.Color);

end;

end.

