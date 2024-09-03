program TriangleColorWheel;

uses
  Vcl.Forms,
  TriangleColorWheelMain in 'TriangleColorWheelMain.pas' {Form27},
  SkiaHelpers in '..\SkiaHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm27, Form27);
  Application.Run;
end.
