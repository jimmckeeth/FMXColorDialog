unit ColorBrowserMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, Vcl.Grids,
  Vcl.DBGrids, FireDAC.Comp.DataSet, FireDAC.Comp.Client, System.Skia,
  Vcl.Skia, System.Types, System.UITypes, System.UIConsts, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  CMYKutil,
  System.Math, Vcl.ComCtrls;

type
  TForm3 = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDQuery1idx: TFDAutoIncField;
    FDQuery1hex: TWideStringField;
    FDQuery1name: TWideStringField;
    FDQuery1red: TSmallintField;
    FDQuery1green: TSmallintField;
    FDQuery1blue: TSmallintField;
    FDQuery1family: TWideStringField;
    FDQuery1link: TWideStringField;
    FDQuery1source: TWideStringField;
    FDQuery1ID: TWideStringField;
    FDQuery1hue: TSingleField;
    FDQuery1saturation: TSingleField;
    FDQuery1luminance: TSingleField;
    FDQuery1cyan: TSingleField;
    FDQuery1magenta: TSingleField;
    FDQuery1yellow: TSingleField;
    FDQuery1key: TSingleField;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    SkPaintBox1: TSkPaintBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    ListBox1: TListBox;
    Edit3: TEdit;
    satEdit: TTrackBar;
    lumEdit: TTrackBar;
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FDQuery1CalcFields(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure FDQuery1AfterScroll(DataSet: TDataSet);
    procedure lumEditChange(Sender: TObject);
  private
    procedure GetColor(X, Y: Integer);
    procedure Filter;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  FDQuery1.DisableControls;
  try
    while not FDQuery1.eof do
    begin
      var hexColor: TAlphaColor := TAlphaColorRec.Alpha;
      try
        hexColor := StringToColor(FDQuery1hex.Value);
      except on EConvertError do
        //hexColor := TAlphaColorRec.Alpha;
      end;
      var rgb: TAlphaColorRec;
      rgb.r := FDQuery1red.Value;
      rgb.g := FDQuery1green.Value;
      rgb.b := FDQuery1blue.Value;
      var rgbColor := RGBtoBGR(RGB.Color);
      if rgbColor <> hexColor then
      begin
        FDQuery1.Edit;
        FDQuery1hex.Value := ColorToStringExt(rgbColor, csfWebHex).ToLower;
      end;

      var h,s,l: Single;
      RGBtoHSL(rgb.Color, h,s,l);

      var hue := roundto(h * 360,-2);
      if (roundto(hue,-2) <> roundto(FDQuery1hue.Value,-2)) or FDQuery1hue.IsNull or FDQuery1saturation.IsNull or FDQuery1luminance.IsNull then
      begin
        FDQuery1.Edit;
        FDQuery1hue.Value := hue;
        FDQuery1saturation.Value := roundto(s*100,-2);
        FDQuery1luminance.Value := roundto(l*100,-2);
      end;
      if FDQuery1cyan.IsNull or FDQuery1magenta.IsNull or FDQuery1yellow.IsNull or FDQuery1key.IsNull then
      begin
        var cmyk := TCMYKColor.RGBToCMYK(rgb.Color);
        FDQuery1.Edit;
        FDQuery1cyan.Value := roundto(cmyk.Cyan * 100,-2);
        FDQuery1magenta.Value := roundto(cmyk.Magenta * 100,-2);
        FDQuery1yellow.Value := roundto(cmyk.Yellow * 100,-2);
        FDQuery1key.Value := roundto(cmyk.Black * 100,-2);
      end;

      //FDQuery1.post;
      FDQuery1.Next;
      //break;
    end;
  finally
    FDQuery1.EnableControls;
  end;
end;

procedure TForm3.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  rgb: TColorRec;
begin
  var grid := sender as TDBGrid;

  if column.Index = 0 then
  begin
    rgb.r := FDQuery1red.Value;
    rgb.g := FDQuery1green.Value;
    rgb.b := FDQuery1blue.Value;
    Grid.Canvas.Brush.Style := bsClear;
    Grid.Canvas.Brush.Color := rgb.Color;
    Grid.Canvas.Brush.Style := bsSolid;
    Grid.Canvas.Pen.Color := rgb.Color;
    Grid.Canvas.Rectangle(Rect);
    Grid.Canvas.FillRect(Rect);
  end
  else
  begin
    Grid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TForm3.DBGrid1TitleClick(Column: TColumn);
begin
  // Get the column name clicked by the user
  var SortColumn := Column.FieldName;
  var SortOrder: String;

  // Determine the sort order
  if DBGrid1.Tag = Column.Index then
  begin
    // Ascending order
    SortOrder := 'ASC';
  end
  else
  begin
    // Descending order
    SortOrder := 'DESC';
  end;
  DBGrid1.Tag := Column.Index;

  var idx := FDQuery1.Indexes.Items[1];
  idx.Fields := SortColumn;
  FDQuery1.IndexName := idx.Name;
  FDQuery1.IndexesActive := True;

end;

procedure TForm3.FDQuery1AfterScroll(DataSet: TDataSet);
begin
  Edit3.Text := FDQuery1name.Value;
end;

procedure TForm3.FDQuery1CalcFields(DataSet: TDataSet);
begin
  var argb: TAlphaColorRec;
  argb.R := FDQuery1red.Value;
  argb.G := FDQuery1green.Value;
  argb.B := FDQuery1blue.Value;
  argb.A := $FF;
  var h,s,l: Single;
  RGBtoHSL(aRGB.Color, h,s,l);
  //FDQuery1calchue.Value := round(h*36000)/100;

  var rgb: TColorRec;
  rgb.R := FDQuery1red.Value;
  rgb.G := FDQuery1green.Value;
  rgb.B := FDQuery1blue.Value;
  //FDQuery1calchex.Value := ColorToStringExt(RGB.Color, csfWebHex).ToLower;
end;

procedure TForm3.Filter;
begin
  var hue := Edit2.tag / 10;
  var sat := satEdit.Position;
  var lum := lumEdit.Position;

  FDQuery1.Filter := Format('hue between %n and %n and saturation between %d and %d and luminance between %d and %d',
    [hue-10, hue+10, sat-10, sat+10, lum-10, lum+10]);

  //FDQuery1.Filter := 'hue between ' + (RoundTo(Hue*360,-2)-10 ).ToString + ' and ' + (RoundTo(Hue*360,-2)+10 ).ToString;
  FDQuery1.Filtered := True;

end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  for var itm in DBGrid1.Columns do
  begin
    var col := itm as TColumn;

    if col.Visible and (col.FieldName <> '') then
      ListBox1.Items.AddObject(col.DisplayName, col);
  end;


end;

function HueGradientArray(Saturation: Single = 1; Luminance: Single = 0.5): TArray<TAlphaColor>;
begin
  Result :=
    [HSLtoRGB(   0, Saturation, Luminance),
     HSLtoRGB( 1/7, Saturation, Luminance),
     HSLtoRGB( 2/7, Saturation, Luminance),
     HSLtoRGB( 3/7, Saturation, Luminance),
     HSLtoRGB( 4/7, Saturation, Luminance),
     HSLtoRGB( 5/7, Saturation, Luminance),
     HSLtoRGB( 6/7, Saturation, Luminance),
     HSLtoRGB(0.99, Saturation, Luminance)
    ];
end;

procedure TForm3.SkPaintBox1Draw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var LPaint: ISkPaint := TSkPaint.Create;
  LPaint.Shader :=
    TSkShader.MakeGradientLinear(
      ADest.TopLeft, ADest.BottomRight,
      HueGradientArray());
  ACanvas.DrawRect(ADest, LPaint);
end;

procedure TForm3.GetColor(X, Y: Integer);
begin
  if not PtInRect(SkPaintBox1.BoundsRect, TPoint.Create(X,Y)) then exit;

  Edit1.Text := ColorToString(Canvas.Pixels[X,Y]);
  var hue := x/SkPaintBox1.Width;
  Edit2.Text := RoundTo(Hue*360,-2).ToString;
  Edit2.Tag := Round(RoundTo(Hue*360,-2)*10);
  Filter;
end;

procedure TForm3.ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source = ListBox1 then
  begin
    var SourceIndex := ListBox1.ItemIndex;
    var TargetIndex := ListBox1.ItemAtPos(Point(X, Y), True);

    if (SourceIndex <> TargetIndex) and (TargetIndex >= 0) then
    begin
      // Swap items
      var TempString := ListBox1.Items[SourceIndex];
      var TempObj := ListBox1.Items.Objects[SourceIndex];
      ListBox1.Items.Delete(SourceIndex);
      ListBox1.Items.InsertObject(TargetIndex, TempString, TempObj);

      ListBox1.Items.Delimiter := ';';
      var idx := FDQuery1.Indexes.Items[1];
      idx.Fields := ListBox1.Items.DelimitedText;
      FDQuery1.IndexName := idx.Name;
      FDQuery1.IndexesActive := True;
    end;
  end;
end;

procedure TForm3.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
Accept := Source = ListBox1;
end;

procedure TForm3.ListBox1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  // Reset the ItemIndex after dragging
  ListBox1.ItemIndex := ListBox1.ItemAtPos(Point(X, Y), True);
end;

procedure TForm3.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ListBox1.BeginDrag(False);
  end;
end;

procedure TForm3.lumEditChange(Sender: TObject);
begin
  Filter;
end;

procedure TForm3.SkPaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then GetColor(X,Y);
end;

procedure TForm3.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (TShiftStateItem.ssLeft in Shift) then GetColor(X,Y);
end;

end.
