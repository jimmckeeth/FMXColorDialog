unit ColorSliderConverter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.StdCtrls, System.UIConsts,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Colors, Jim.FMX.TrackAndSpin,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    JimTrackSpin1: TJimTrackSpin;
    JimTrackSpin2: TJimTrackSpin;
    JimTrackSpin3: TJimTrackSpin;
    Layout9: TLayout;
    Layout10: TLayout;
    Label9: TLabel;
    Edit2: TEdit;
    Layout1: TLayout;
    JimTrackSpin4: TJimTrackSpin;
    JimTrackSpin5: TJimTrackSpin;
    JimTrackSpin6: TJimTrackSpin;
    Layout2: TLayout;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.

