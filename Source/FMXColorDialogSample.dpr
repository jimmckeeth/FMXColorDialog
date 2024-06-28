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
  Jim.FMX.MoreTrackbars in 'Components\Jim.FMX.MoreTrackbars.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmColorDialog, frmColorDialog);
  Application.Run;
end.
