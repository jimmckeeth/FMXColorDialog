unit ColorDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Colors,
  FMX.StdCtrls;

type
  TfrmColorDialog = class(TForm)
    ColorPanel1: TColorPanel;
    ColorListBox1: TColorListBox;
    Panel1: TPanel;
    Layout1: TLayout;
    edtColorHex: TEdit;
    edtColorName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtRed: TEdit;
    edtGreen: TEdit;
    edtBlue: TEdit;
    edtAlpha: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure ColorPanel1Change(Sender: TObject);
    procedure ColorListBox1Change(Sender: TObject);
    procedure edtColorNameChangeTracking(Sender: TObject);
    procedure edtColorHexChangeTracking(Sender: TObject);
    procedure edtColorNameKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure edtRedChangeTracking(Sender: TObject);
    procedure edtAlphaKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    procedure FindNextColorName;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  public
    { Public declarations }
    property Color: TAlphaColor read GetColor write SetColor;
  end;

var
  frmColorDialog: TfrmColorDialog;

implementation

{$R *.fmx}

procedure TfrmColorDialog.ColorListBox1Change(Sender: TObject);
begin
  if edtColorName.IsFocused then exit;

  if (ColorListBox1.ItemIndex > -1) then
  begin
    ColorPanel1.Color := ColorListBox1.Color;
    edtColorName.Text := ColorListBox1.Items[ColorListBox1.ItemIndex];
  end
  else
    edtColorName.Text := '';
end;

procedure TfrmColorDialog.ColorPanel1Change(Sender: TObject);
begin
  var c: TAlphaColorRec;
  c.Color := ColorPanel1.Color;
  edtColorHex.Text := Format('%x',[ColorPanel1.Color]);
  if not edtRed.IsFocused then edtRed.Text := c.R.ToHexString;
  if not edtGreen.IsFocused then edtGreen.Text := c.G.ToHexString;
  if not edtBlue.IsFocused then edtBlue.Text := c.B.ToHexString;
  if not edtAlpha.IsFocused then edtAlpha.Text := c.A.ToHexString;
  ColorListBox1.Color := ColorPanel1.Color;
  if (ColorListBox1.Color <> ColorPanel1.Color) then
  begin
    ColorListBox1.ItemIndex := -1;
  end;

end;

procedure TfrmColorDialog.edtAlphaKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  KeyChar := UpCase(KeyChar);
end;

procedure TfrmColorDialog.edtColorHexChangeTracking(Sender: TObject);
begin
  ColorPanel1.Color := StrToInt64('$' + edtColorHex.Text.TrimLeft(['#',' ']));
end;

procedure TfrmColorDialog.edtColorNameChangeTracking(Sender: TObject);
begin
  if (ColorListBox1.ItemIndex = -1) or (edtColorName.Text <> ColorListBox1.Items[ColorListBox1.ItemIndex]) then
    FindNextColorName;
end;

procedure TfrmColorDialog.edtColorNameKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case key of
    vkDown: FindNextColorName;
  end;

end;

procedure TfrmColorDialog.edtRedChangeTracking(Sender: TObject);
begin
  if (edtRed.Text = '') or (edtGreen.Text = '') or
     (edtBlue.Text = '') or (edtAlpha.Text = '') then
  exit;

  var c: TAlphaColorRec;
  c.R := StrToInt('$'+edtRed.Text);
  c.G := StrToInt('$'+edtGreen.Text);
  c.B := StrToInt('$'+edtBlue.Text);
  c.A := StrToInt('$'+edtAlpha.Text);

  ColorPanel1.Color := c.Color;
end;

procedure TfrmColorDialog.FindNextColorName;
begin
  if edtColorName.Text = '' then exit;

  var Offset := ColorListBox1.ItemIndex + 1;
  var max := Pred(ColorListBox1.Items.Count);
  for var i := 0 to max do
  begin
    if i + Offset > max then
      offset := 0 - i;

    if (I + Offset < 0) or (I + Offset > Pred(ColorListBox1.Items.Count)) then
      raise ERangeError.Create('oops');


    if ColorListBox1.Items[I + Offset].Contains(edtColorName.Text) then
    begin
      ColorListBox1.ItemIndex := I + Offset;
      Break;
    end;
  end;
end;

procedure TfrmColorDialog.FormCreate(Sender: TObject);
begin
  SetColor(TAlphaColorRec.Powderblue);
end;

procedure TfrmColorDialog.FormResize(Sender: TObject);
begin
  if Width < 450 then Width := 450;
  if Height < 250 then Height := 250;


end;

function TfrmColorDialog.GetColor: TAlphaColor;
begin
  Result := ColorPanel1.Color;
end;

procedure TfrmColorDialog.SetColor(const Value: TAlphaColor);
begin
  ColorPanel1.Color := Value;
end;

end.
