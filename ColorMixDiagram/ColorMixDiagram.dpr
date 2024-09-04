program ColorMixDiagram;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorMixDiagramMain in 'ColorMixDiagramMain.pas' {Form4},
  SkiaHelpers in '..\SkiaHelpers.pas',
  AlphaColorUtils in '..\AlphaColorUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
