program ColorMixDiagram;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorMixDiagramMain in 'ColorMixDiagramMain.pas' {Form4},
  SkiaHelpers in '..\SkiaHelpers.pas',
  AlphaColorUtils in '..\AlphaColorUtils.pas',
  SkiaCircleMath in '..\SkiaCircleMath.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
