program FMXColorDialogSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorDialog in 'ColorDialog.pas' {frmColorDialog},
  cmykutil in 'cmykutil.pas',
  X11ColorData in 'X11ColorData.pas',
  HTMLColorData in 'HTMLColorData.pas',
  ColorSliderConverter in 'ColorSliderConverter.pas' {Form1},
  Jim.FMX.TrackAndSpin in 'Components\Jim.FMX.TrackAndSpin.pas',
  Jim.FMX.MoreTrackbars in 'Components\Jim.FMX.MoreTrackbars.pas',
  Jim.Color.Palettes in 'Components\Jim.Color.Palettes.pas',
  CircularGradientDisplay in 'CircularGradientDisplay.pas' {Form2},
  SkGradientViewer in 'SkGradientViewer.pas' {SkGradientView},
  FancyGradientColorPicker in 'FancyGradientColorPicker.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TSkGradientView, SkGradientView);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmColorDialog, frmColorDialog);
  Application.Run;
end.
