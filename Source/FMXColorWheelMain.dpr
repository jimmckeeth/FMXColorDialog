program FMXColorWheelMain;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXColorWheel in 'FMXColorWheel.pas' {Form28};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm28, Form28);
  Application.Run;
end.
