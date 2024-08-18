program FMXColorDialogSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  ColorDialog in 'ColorDialog.pas' {frmColorDialog},
  cmykutil in 'cmykutil.pas',
  X11ColorData in 'X11ColorData.pas',
  HTMLColorData in 'HTMLColorData.pas',
  ColorSliderConverter in 'ColorSliderConverter.pas' {frmSlideConverter},
  Jim.FMX.TrackAndSpin in 'Components\Jim.FMX.TrackAndSpin.pas',
  Jim.FMX.MoreTrackbars in 'Components\Jim.FMX.MoreTrackbars.pas',
  Jim.Color.Palettes in 'Components\Jim.Color.Palettes.pas',
  GradientDisplay in 'GradientDisplay.pas' {frmGradientDisplay},
  SkGradientViewer in 'SkGradientViewer.pas' {frmSkGradientView},
  FancyGradientColorPicker in 'FancyGradientColorPicker.pas' {frmFancyGradientColorPicker},
  FullColor in 'FullColor.pas',
  FormPicker in 'FormPicker.pas' {Form1},
  SimpleSliders in 'SimpleSliders.pas' {frmSimpleSliders},
  ShadeTintToneView in 'ShadeTintToneView.pas' {frmShadeTintTone};

{$R *.res}

begin
  GlobalUseSkia := False;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmSlideConverter, frmSlideConverter);
  Application.CreateForm(TfrmColorDialog, frmColorDialog);
  Application.CreateForm(TfrmFancyGradientColorPicker, frmFancyGradientColorPicker);
  Application.CreateForm(TfrmSkGradientView, frmSkGradientView);
  Application.CreateForm(TfrmGradientDisplay, frmGradientDisplay);
  Application.CreateForm(TfrmSimpleSliders, frmSimpleSliders);
  Application.CreateForm(TfrmShadeTintTone, frmShadeTintTone);
  Application.Run;
end.
