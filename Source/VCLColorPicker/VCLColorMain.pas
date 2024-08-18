unit VCLColorMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, UITypes, UIConsts,
  Jim.Color.Palettes;

type
  TForm2 = class(TForm)
    tbRed: TTrackBar;
    tbGreen: TTrackBar;
    tbBlue: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    tbSat: TTrackBar;
    tbLum: TTrackBar;
    tbHue: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    pnlColor: TPanel;
    lblRed: TLabel;
    lblGreen: TLabel;
    lblBlue: TLabel;
    lblSat: TLabel;
    lblLum: TLabel;
    lblHue: TLabel;
    rdPalette: TRadioGroup;
    Button1: TButton;
    paintPalette: TPaintBox;
    paintShadeTint: TPaintBox;
    paintTone: TPaintBox;
    procedure tbRedChange(Sender: TObject);
    procedure tbSatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure paintPalettePaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure paintShadeTintPaint(Sender: TObject);
    procedure paintTonePaint(Sender: TObject);
  private
    { Private declarations }
    FUpdating: Boolean;
    FPalette: TColorPalette;
    procedure UpdateRGB;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  paintPalette.Invalidate;
  //paintShadeTint.Invalidate;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FUpdating := False;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  FPalette := TColorPalette.Create(
    ColorToAlphaColor( pnlColor.Color ),
    TPaletteType(rdPalette.ItemIndex));
end;

procedure TForm2.paintPalettePaint(Sender: TObject);
begin
  FPalette := TColorPalette.Create(
    ColorToAlphaColor( pnlColor.Color ),
    TPaletteType(rdPalette.ItemIndex));

  var w := paintPalette.Width div (FPalette.ColorCount-1);
  var cnvs := paintPalette.Canvas;
  var max := pred(FPalette.ColorCount);
  for var i := 1 to max do
  begin
    var ac := FPalette.Colors[i];
    var c := AlphaColorToColor(ac);
    cnvs.Brush.Color := c;
    if i = max then
      cnvs.Rectangle((i-1)*w, 0, paintPalette.Width, paintPalette.Height)
    else
      cnvs.Rectangle((i-1)*w, 0, (i-1)*w+w, paintPalette.Height);
  end;
end;

procedure TForm2.paintShadeTintPaint(Sender: TObject);
begin
  var h, s, l: Single;

  h := tbHue.Position/tbHue.Max;
  s := tbSat.Position/tbSat.Max;
  l := tbLum.Position/tbLum.Max;

  var cnvs := paintShadeTint.Canvas;
  var wt := paintShadeTint.Width;
  var ht := paintShadeTint.Height div 10;
  var rem := paintShadeTint.Height mod 10;
  var startHeight := 0;
  for var i := 0 to 9 do
  begin
    var localHeight := ht;
    if i mod rem > 0 then
      localHeight := localHeight + 1;

    var c := HSLtoRGB(h, s, i/9);
    cnvs.Brush.Color := AlphaColorToColor(c);
    cnvs.Rectangle(0, startHeight, wt, startHeight + localHeight);
    startHeight := startHeight + localHeight;
  end;
end;

procedure TForm2.paintTonePaint(Sender: TObject);
begin
  var h, s, l: Single;

  h := tbHue.Position/tbHue.Max;
  s := tbSat.Position/tbSat.Max;
  l := tbLum.Position/tbLum.Max;

  var cnvs := paintTone.Canvas;
  var wt := paintTone.Width;
  var ht := paintTone.Height div 10;
  var rem := paintTone.Height mod 10;
  var startHeight := 0;
  for var i := 0 to 9 do
  begin
    var localHeight := ht;
    if i mod rem > 0 then
      localHeight := localHeight + 1;

    var c := HSLtoRGB(h, i/9, l);
    cnvs.Brush.Color := AlphaColorToColor(c);
    cnvs.Rectangle(0, startHeight, wt, startHeight + localHeight);
    startHeight := startHeight + localHeight;
  end;
end;

procedure TForm2.UpdateRGB;
begin
  lblRed.Caption := tbRed.Position.ToString;
  lblGreen.Caption := tbGreen.Position.ToString;
  lblBlue.Caption := tbBlue.Position.ToString;
  var c: TColorRec;
  c.R := tbRed.Position;
  c.G := tbGreen.Position;
  c.B := tbBlue.Position;
  pnlColor.Color := c.Color;

  var ac := ColorToAlphaColor(c.Color);
  var h,s,l: Single;

  RGBtoHSL(ac, h, s, l);
  try
    FUpdating := True;
    tbHue.Position := Round(h * tbHue.Max);
    tbSat.Position := Round(s * tbSat.Max);
    tbLum.Position := Round(l * tbLum.Max);
  finally
    FUpdating := False;
  end;
end;

procedure TForm2.tbRedChange(Sender: TObject);
begin
  if FUpdating then exit;

  UpdateRGB;
end;

procedure TForm2.tbSatChange(Sender: TObject);
begin
  if FUpdating then exit;

  lblHue.Caption := tbHue.Position.ToString;
  lblSat.Caption := tbSat.Position.ToString;
  lblLum.Caption := tbLum.Position.ToString;
  var c := AlphaColorToColor(HSLToRGB(
    tbHue.Position/tbHue.Max,
    tbSat.Position/tbSat.Max,
    tbLum.Position/tbLum.Max));
  pnlColor.Color := c;

  try
    FUpdating := True;
    tbRed.Position := TColorRec(c).R;
    tbGreen.Position := TColorRec(c).G;
    tbBlue.Position := TColorRec(c).B;
  finally
    FUpdating := False;
  end;
end;

end.
