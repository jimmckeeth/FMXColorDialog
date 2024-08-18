unit FormPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Threading;

type
  TForm1 = class(TForm)
    rdCircularGradient: TRadioButton;
    rdColorDialog: TRadioButton;
    rdColorSlideConverter: TRadioButton;
    rdFancyGradient: TRadioButton;
    rdSkiaGriadient: TRadioButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    rdSimpleSliders: TRadioButton;
    Button1: TButton;
    rdShadeTintTone: TRadioButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    FAlreadyShown: Boolean;
  public
    { Public declarations }
    procedure OpenSelectedForm;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses GradientDisplay, cmykutil, ColorDialog, ColorSliderConverter,
  FancyGradientColorPicker, FullColor, HTMLColorData, Jim.Color.Palettes,
  Jim.FMX.MoreTrackbars, Jim.FMX.TrackAndSpin, SkGradientViewer,
  X11ColorData, SimpleSliders, ShadeTintToneView;

procedure TForm1.Button1Click(Sender: TObject);
begin
  rdSimpleSliders.IsChecked := True;
  OpenSelectedForm;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  rdCircularGradient.IsChecked := True;
  OpenSelectedForm;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  rdColorDialog.IsChecked := True;
  OpenSelectedForm;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  rdColorSlideConverter.IsChecked := True;
  OpenSelectedForm;

end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  rdSkiaGriadient.IsChecked := True;
  OpenSelectedForm;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  rdShadeTintTone.IsChecked := True;
  OpenSelectedForm;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAlreadyShown := False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not FAlreadyShown then
  begin
    FAlreadyShown := True;
    TTask.Run(procedure begin
        TThread.Queue(nil, procedure begin
          OpenSelectedForm;
        end);
    end);
  end;
end;

procedure TForm1.OpenSelectedForm;
begin
  var selected: TForm := self;
  if rdCircularGradient.IsChecked then
    selected := frmGradientDisplay;
  if rdColorDialog.IsChecked then
    selected := frmColorDialog;
  if rdFancyGradient.IsChecked then
    selected := frmFancyGradientColorPicker;
  if rdColorSlideConverter.IsChecked then
    selected := frmSlideConverter;
  if rdSkiaGriadient.IsChecked then
    selected := frmSkGradientView;
  if rdSimpleSliders.IsChecked then
    selected := frmSimpleSliders;

  if rdShadeTintTone.IsChecked then
    selected := frmShadeTintTone;


  selected.ShowModal;

end;

end.
