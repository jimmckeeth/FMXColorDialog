unit SimpleSliders;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors,
  System.UIConsts, Jim.FMX.MoreTrackbars, FMX.Layouts;

type
  TfrmSimpleSliders = class(TForm)
    Hue: THueTrackBar;
    Sat: TTrackBar;
    Rectangle1: TRectangle;
    Lum: TTrackBar;
    Layout1: TLayout;
    Red: TGradientTrackBar;
    Green: TGradientTrackBar;
    Blue: TGradientTrackBar;
    Label1: TLabel;
    procedure HueChange(Sender: TObject);
    procedure LumDblClick(Sender: TObject);
    procedure RedChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSimpleSliders: TfrmSimpleSliders;

implementation

{$R *.fmx}

procedure TfrmSimpleSliders.HueChange(Sender: TObject);
begin
  Rectangle1.Fill.Color := HSLToRGB(Hue.Value, Sat.Value, Lum.Value);
  Label1.Text := Format('HSL: %f %f %f', [Hue.Value, Sat.Value, Lum.Value]);
end;

procedure TfrmSimpleSliders.LumDblClick(Sender: TObject);
begin
  Lum.Value := 0.5;
end;

procedure TfrmSimpleSliders.RedChange(Sender: TObject);
begin
  var ACR: TAlphaColorRec;
  ACR.R := Trunc(Red.Value);
  ACR.G := Trunc(Green.Value);
  ACR.B := Trunc(Blue.Value);
  ACR.A := 255;
  Label1.Text := Format('RGB: %d %d %d', [ACR.R, ACR.G, ACR.B]);

  Rectangle1.Fill.Color := TAlphaColor(ACR);
end;

end.
