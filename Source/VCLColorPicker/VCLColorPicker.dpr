program VCLColorPicker;

uses
  Vcl.Forms,
  VCLColorMain in 'VCLColorMain.pas' {Form2},
  Jim.Color.Palettes in '..\Components\Jim.Color.Palettes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
