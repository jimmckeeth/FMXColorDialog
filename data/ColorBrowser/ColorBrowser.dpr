program ColorBrowser;

uses
  Vcl.Forms,
  ColorBrowserMain in 'ColorBrowserMain.pas' {Form3},
  cmykutil in '..\..\Source\cmykutil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
