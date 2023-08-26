program FMXColorDialogSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorDialog in 'ColorDialog.pas' {frmColorDialog},
  cmykutil in 'cmykutil.pas',
  X11ColorData in 'X11ColorData.pas',
  HTMLColorData in 'HTMLColorData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmColorDialog, frmColorDialog);
  Application.Run;
end.
