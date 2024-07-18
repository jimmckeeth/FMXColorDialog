unit CircularGradientDisplay;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, Generics.Collections,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  System.Skia, FMX.Skia;

type
  TForm2 = class(TForm)
    Circle1: TCircle;
    HueTrackBar1: THueTrackBar;
    GradientEdit1: TGradientEdit;
    ListBox1: TListBox;
    Button1: TButton;
    SpinBox1: TSpinBox;
    rbRadial: TRadioButton;
    rbLinear: TRadioButton;
    procedure GradientEdit1Change(Sender: TObject);
    procedure HueTrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure rbRadialChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateListBox;
    procedure ApplyGradient;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  System.UIConsts,
  System.Math;

{$R *.fmx}

procedure MakeGradientRainbow(const gradient: TGradient; points: UInt16);
begin
  gradient.Points.BeginUpdate;
  try
    gradient.Points.ClearAndResetID;
    for var p := 0 to pred(points) do
    begin
      var pt := TGradientPoint(gradient.Points.Add);
      pt.Offset := p * 1/(points-1);
      pt.Color := HSLtoRGB(gradient.Points[p].Offset,1,0.5);
    end;
  finally
    gradient.Points.EndUpdate;
  end;
end;

procedure TForm2.ApplyGradient;
begin
  UpdateListBox;
  GradientEdit1.Repaint;
  GradientEdit1.UpdateGradient;
  Circle1.Fill.Gradient.Assign(GradientEdit1.Gradient);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  MakeGradientRainbow(GradientEdit1.Gradient,trunc(SpinBox1.Value));
  ApplyGradient;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  MakeGradientRainbow(GradientEdit1.Gradient,7);
  ApplyGradient;
end;

procedure TForm2.GradientEdit1Change(Sender: TObject);
begin
  Circle1.Fill.Gradient.Assign(GradientEdit1.Gradient);
  ApplyGradient;
end;

procedure TForm2.HueTrackBar1Change(Sender: TObject);
begin
  GradientEdit1.Gradient.Points[GradientEdit1.CurrentPoint].Color := HSLtoRGB(HueTrackBar1.Value,1,0.5);
  ApplyGradient;
end;

procedure TForm2.ListBox1Change(Sender: TObject);
begin
  GradientEdit1.CurrentPoint := ListBox1.Selected.Index;
  var h,s,l: Single;
  RGBtoHSL(GradientEdit1.Gradient.Points[GradientEdit1.CurrentPoint].Color, h,s,l);

  HueTrackBar1.Value := h;
end;

procedure TForm2.rbRadialChange(Sender: TObject);
begin
  if rbRadial.IsChecked then
    Circle1.Fill.Gradient.Style := TGradientStyle.Radial
  else
    Circle1.Fill.Gradient.Style := TGradientStyle.Linear
end;


procedure TForm2.SpinBox1Change(Sender: TObject);
begin
  MakeGradientRainbow(GradientEdit1.Gradient,trunc(SpinBox1.Value));
  ApplyGradient;
end;

procedure TForm2.UpdateListBox;
begin
  ListBox1.BeginUpdate;
  try
    ListBox1.Clear;
    var grad := GradientEdit1.Gradient;
    for var p := 0 to pred(grad.Points.Count) do
    begin
      var pnt := grad.Points[p];
      ListBox1.Items.Add(Format('%d [%d] %6n%% #%s',
        [pnt.ID, pnt.Index, pnt.Offset*100, IntToHex(pnt.Color)]));
    end;
  finally
    ListBox1.EndUpdate;
  end;
end;

end.
