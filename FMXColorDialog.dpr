program FMXColorDialog;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorDialog in 'ColorDialog.pas' {frmColorDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmColorDialog, frmColorDialog);
  Application.Run;
end.
