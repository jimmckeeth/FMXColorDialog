unit Jim.FMX.TrackAndSpin;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.Edit, FMX.SpinBox, FMX.StdCtrls, FMX.StdActns, FMX.Text, FMX.Graphics,
  FMX.EditBox, FMX.Controls.Presentation, System.Types, System.UITypes,
  System.UIConsts,

  Jim.FMX.MoreTrackbars,
  FMX.Colors;

type
  TCustomTrackSpin = class(TPresentedControl, IReadOnly, IValueRange,
    ITextActions, IVirtualKeyboardControl, ITextSettings, ICaret)
  private
    { Private declarations }
    FLabel: TLabel;
    FTrack: TCustomTrack;
    FSpin: TSpinBox;
    FOnChange: TNotifyEvent;
    procedure SetText(AText: String);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure DoSpinChange(Sender: TObject);
    procedure DoTrackChange(Sender: TObject);
    procedure SetTextWidth(const Value: single);
    function GetText: string;
    function GetTextWidth: single;
    function GetMax: Double;
    function GetMin: Double;
    function GetValue: Double;
    // IValueRange
    function GetValueRange: TCustomValueRange;
    procedure SetValueRange(const AValue: TCustomValueRange);

    function GetModel: TSpinBoxModel;
    function IsTextWidthStored: Boolean;
    function GetValueType: TNumValueType;
    procedure SetValueType(const Value: TNumValueType);
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
  protected
    { Protected declarations }
    procedure Change;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure Paint; override;
    property Model: TSpinBoxModel read GetModel;

    // IVirtualKeyboardControl
    procedure SetKeyboardType(Value: TVirtualKeyboardType);
    function GetKeyboardType: TVirtualKeyboardType;
    procedure SetReturnKeyType(Value: TReturnKeyType);
    function GetReturnKeyType: TReturnKeyType;
    function IsPassword: Boolean;

    // ITextSettings
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetResultingTextSettings: TTextSettings;
    function GetStyledSettings: TStyledSettings;
    procedure SetStyledSettings(const Value: TStyledSettings);
    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property ResultingTextSettings: TTextSettings read GetResultingTextSettings;

    // ICaret
    function GetObject: TCustomCaret;
    procedure ShowCaret;
    procedure HideCaret;

    // customization
    function CreateTrackBar: TCustomTrack; virtual;
  public

    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // ITextActions
    procedure DeleteSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure SelectWord;
    procedure ResetSelection;
    procedure GoToTextEnd;
    procedure GoToTextBegin;
    procedure Replace(const AStartPos: Integer; const ALength: Integer; const AStr: string);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property Hint;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property TextWidth: single read GetTextWidth write SetTextWidth stored IsTextWidthStored;

    property Text: string read GetText write SetText;
    property Min: Double read GetMin write SetMin stored IsMinStored;
    property Max: Double read GetMax write SetMax stored IsMaxStored;
    property Value: Double read GetValue write SetValue;

    // IReadOnly
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    // IVirtualKeyboardControl
    property KeyboardType: TVirtualKeyboardType read GetKeyboardType write SetKeyboardType;
    property ReturnKeyType: TReturnKeyType read GetReturnKeyType write SetReturnKeyType;
    property Password: Boolean read IsPassword;

    // ITextSettings
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings;

    // ICaret
    property Caret: TCustomCaret read GetObject;

    property ValueType: TNumValueType read GetValueType write SetValueType default TNumValueType.Integer;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGradientTrackSpin = class(TCustomTrackSpin)
  protected
    function CreateTrackBar: TCustomTrack; override;
  public
    function GetColorEnd: TAlphaColor;
    function GetColorBegin: TAlphaColor;
    procedure SetColorEnd(const Value: TAlphaColor);
    procedure SetColorBegin(const Value: TAlphaColor);
  published
    property ColorEnd: TAlphaColor read GetColorEnd write SetColorEnd;
    property ColorBegin: TAlphaColor read GetColorBegin write SetColorBegin;
  end;

  THueTrackSpin = class(TCustomTrackSpin)
  protected
    function CreateTrackBar: TCustomTrack; override;
  end;

  TBWTrackSpin = class(TCustomTrackSpin)
  protected
    function CreateTrackBar: TCustomTrack; override;
  end;

  TAlphaTrackSpin = class(TCustomTrackSpin)
  protected
    function CreateTrackBar: TCustomTrack; override;
  end;

  procedure Register;

implementation

const
  CDefaultTextWidth = 70;
  CDefaultMin = 0;
  CDefaultMax = 255;

procedure Register;
begin
  RegisterComponents('Colors',
    [TGradientTrackSpin, THueTrackSpin, TBWTrackSpin, TAlphaTrackSpin]);
end;

type
  TSpinBoxHack = class(TSpinBox)
   public
     property Model;
  end;
  TTrackBarHack = class(TTrackBar)
  public
    Property TheTrack: TControl read FTrack;
  end;

{ TCustomTrackSpin }

procedure TCustomTrackSpin.CopyToClipboard;
begin
  FSpin.CopyToClipboard;
end;

constructor TCustomTrackSpin.Create(AOwner: TComponent);
begin
  inherited;
  Height := 25;
  Width := 320;

  FLabel := TLabel.Create(self);
  FLabel.Stored := False;
  FLabel.Parent := Self;
  FLabel.SetDesign(False);
  FLabel.Width := CDefaultTextWidth;

  FTrack := CreateTrackBar; //TTrackBar.Create(self);
  FTrack.Stored := False;
  FTrack.Parent := Self;
  FTrack.SetDesign(False);

  FSpin := TSpinBox.Create(self);
  FSpin.Stored := False;
  FSpin.SetDesign(False);
  FSpin.Parent := Self;
  FSpin.Width := 75;

  FLabel.Align := TAlignLayout.Left;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.HitTest := False;
  FLabel.Locked := True;
  FLabel.CanFocus := False;
  FLabel.Text := Self.Name;

  FSpin.Align := TAlignLayout.Right;
  FSpin.OnChangeTracking := DoSpinChange;
  FSpin.OnChange := DoSpinChange;
  FSpin.HitTest := True;
  FSpin.Locked := True;
  FSpin.Stored := False;
  FSpin.CanFocus := True;
  FSpin.Max :=  CDefaultMax;
  FSpin.Min := CDefaultMin;

  FTrack.Align := TAlignLayout.Client;
  FTrack.OnChange := DoTrackChange;
  FTrack.HitTest := True;
  FTrack.Locked := True;
  FTrack.Stored := False;
  FTrack.CanFocus := True;
  FTrack.Margins.Right := 3;
  FTrack.Max := CDefaultMax;
  FTrack.Min := CDefaultMin;
end;

function TCustomTrackSpin.CreateTrackBar: TCustomTrack;
begin
  FTrack := TTrackBar.Create(self);
  Result := FTrack;
end;

procedure TCustomTrackSpin.CutToClipboard;
begin
  FSpin.CutToClipboard;
end;

procedure TCustomTrackSpin.DeleteSelection;
begin
  FSpin.DeleteSelection;
end;

destructor TCustomTrackSpin.Destroy;
begin

  inherited;
end;

procedure TCustomTrackSpin.Change;
begin
  Repaint;
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomTrackSpin.DoSpinChange(Sender: TObject);
begin
  FTrack.Value := FSpin.Value;
  Change;
end;

procedure TCustomTrackSpin.DoTrackChange(Sender: TObject);
begin
  FSpin.Value := FTrack.Value;
  Change;
end;

function TCustomTrackSpin.GetText: string;
begin
  Result := FLabel.Text;
end;

function TCustomTrackSpin.GetTextWidth: single;
begin
  Result := FLabel.Width;
end;

function TCustomTrackSpin.GetDefaultTextSettings: TTextSettings;
begin
  Result := FSpin.DefaultTextSettings;
end;

function TCustomTrackSpin.GetKeyboardType: TVirtualKeyboardType;
begin
  Result := FSpin.KeyboardType;
end;

function TCustomTrackSpin.GetMax: Double;
begin
  Result := FSpin.Max;
end;

function TCustomTrackSpin.GetMin: Double;
begin
  Result := FSpin.Min;
end;

function TCustomTrackSpin.GetModel: TSpinBoxModel;
begin
  Result := TSpinBoxHack(FSpin).Model;
end;

function TCustomTrackSpin.GetObject: TCustomCaret;
begin
  Result := FSpin.Caret;
end;

function TCustomTrackSpin.GetReadOnly: Boolean;
begin
  Result := FSpin.ReadOnly;
end;

function TCustomTrackSpin.GetResultingTextSettings: TTextSettings;
begin
  Result := FSpin.ResultingTextSettings;
end;

function TCustomTrackSpin.GetReturnKeyType: TReturnKeyType;
begin
  Result := FSpin.ReturnKeyType;
end;

function TCustomTrackSpin.GetStyledSettings: TStyledSettings;
begin
  Result := FSpin.StyledSettings;
end;

function TCustomTrackSpin.GetTextSettings: TTextSettings;
begin
  Result := FSpin.TextSettings;
end;

function TCustomTrackSpin.GetValue: Double;
begin
  Result := FSpin.Value;
end;

function TCustomTrackSpin.GetValueRange: TCustomValueRange;
begin
  Result := FSpin.ValueRange;
end;

function TCustomTrackSpin.GetValueType: TNumValueType;
begin
  Result := FSpin.ValueType;
end;

procedure TCustomTrackSpin.GoToTextBegin;
begin
  FSpin.GoToTextBegin;
end;

procedure TCustomTrackSpin.GoToTextEnd;
begin
  FSpin.GoToTextEnd;
end;

procedure TCustomTrackSpin.HideCaret;
begin
  FSpin.Caret.Hide;
end;

function TCustomTrackSpin.IsMaxStored: Boolean;
begin
  Result := Trunc(FSpin.Max) <> CDefaultMax
end;

function TCustomTrackSpin.IsMinStored: Boolean;
begin
  Result := Trunc(FSpin.Min) <> CDefaultMin
end;

function TCustomTrackSpin.IsPassword: Boolean;
begin
  Result := FSpin.Password;
end;

function TCustomTrackSpin.IsTextWidthStored: Boolean;
begin
  Result := trunc(FLabel.Width) = CDefaultTextWidth;
end;

procedure TCustomTrackSpin.Paint;
begin
  inherited;
  FLabel.Repaint;
  FTrack.Repaint;
  FSpin.Repaint;
end;

procedure TCustomTrackSpin.PasteFromClipboard;
begin
  FSpin.PasteFromClipboard;
end;

procedure TCustomTrackSpin.Replace(const AStartPos, ALength: Integer;
  const AStr: string);
begin
  FSpin.Replace(AStartPos, ALength, AStr);
end;

procedure TCustomTrackSpin.ResetSelection;
begin
  FSpin.ResetSelection;
end;

procedure TCustomTrackSpin.SelectAll;
begin
  FSpin.SelectAll;
end;

procedure TCustomTrackSpin.SelectWord;
begin
  FSpin.SelectWord;
end;

procedure TCustomTrackSpin.SetText(AText: String);
begin
  if (FLabel.Text <> AText) then
  begin
    FLabel.Text := AText;
    Repaint;
  end;
end;

procedure TCustomTrackSpin.SetTextWidth(const Value: single);
begin
  if FLabel.Width <> Value then
  begin
    FLabel.Width := Value;
    Repaint;
  end;
end;

procedure TCustomTrackSpin.SetKeyboardType(Value: TVirtualKeyboardType);
begin
  FSpin.KeyboardType := Value;
end;

procedure TCustomTrackSpin.SetMax(const Value: Double);
begin
  if ((FSpin.Max) <> Value) or (FTrack.Max <> Value) then
  begin
    FSpin.Max := Value;
    FTrack.Max := Value;
    Change;
  end;
end;

procedure TCustomTrackSpin.SetMin(const Value: Double);
begin
  if ((FSpin.Min) <> Value) or (FTrack.Min <> Value) then
  begin
    FSpin.Min := Value;
    FTrack.Min := Value;
    Change;
  end;
end;

procedure TCustomTrackSpin.SetReadOnly(const Value: Boolean);
begin
  if (FSpin.ReadOnly <> Value) or (not FTrack.Enabled <> Value) then
  begin
    FSpin.ReadOnly := Value;
    FTrack.Enabled := not Value;
    Change;
  end;
end;

procedure TCustomTrackSpin.SetReturnKeyType(Value: TReturnKeyType);
begin
  FSpin.ReturnKeyType := Value;
end;

procedure TCustomTrackSpin.SetStyledSettings(const Value: TStyledSettings);
begin
  FSpin.StyledSettings := Value;
end;

procedure TCustomTrackSpin.SetTextSettings(const Value: TTextSettings);
begin
  FSpin.TextSettings.Assign(Value);
end;

procedure TCustomTrackSpin.SetValue(const Value: Double);
begin
  if (FTrack.Value <> Value) or (FSpin.Value <> Value) then
  begin
    FTrack.Value := Value;
    FSpin.Value := Value;
    Change;
  end;
end;

procedure TCustomTrackSpin.SetValueRange(const AValue: TCustomValueRange);
begin
  FSpin.ValueRange.Assign(AValue);
end;

procedure TCustomTrackSpin.SetValueType(const Value: TNumValueType);
begin
  FSpin.ValueType := Value;
end;

procedure TCustomTrackSpin.ShowCaret;
begin
  FSpin.Caret.Show;
end;

{ TGradientTrackSpin }

procedure TGradientTrackSpin.SetColorBegin(const Value: TAlphaColor);
begin
  TGradientTrackBar(FTrack).BeginColor := Value;
end;

procedure TGradientTrackSpin.SetColorEnd(const Value: TAlphaColor);
begin
  TGradientTrackBar(FTrack).EndColor := Value;
end;

function TGradientTrackSpin.GetColorEnd: TAlphaColor;
begin
  Result := TGradientTrackBar(FTrack).EndColor;
end;

function TGradientTrackSpin.GetColorBegin: TAlphaColor;
begin
  Result := TGradientTrackBar(FTrack).BeginColor;
end;

function TGradientTrackSpin.CreateTrackBar: TCustomTrack;
begin
  FTrack := TGradientTrackBar.Create(self);
  Result := FTrack;
end;

{ THueTrackSpin }

function THueTrackSpin.CreateTrackBar: TCustomTrack;
begin
  FTrack := THueTrackBar.Create(self);
  Result := FTrack;
end;

{ TAlphaTrackSpin }

function TAlphaTrackSpin.CreateTrackBar: TCustomTrack;
begin
  FTrack := TAlphaTrackBar.Create(self);
  Result := FTrack;
end;

{ TBWTrackSpin }

function TBWTrackSpin.CreateTrackBar: TCustomTrack;
begin
  FTrack := TBWTrackBar.Create(self);
  Result := FTrack;
end;

initialization

  RegisterFmxClasses( [ TBWTrackSpin, TAlphaTrackSpin, THueTrackSpin, TGradientTrackSpin, TCustomTrackSpin ] );

end.
