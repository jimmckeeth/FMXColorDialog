unit Jim.FMX.MoreTrackbars;

interface

uses
  System.SysUtils,
  FMX.StdCtrls,
  FMX.Graphics,
  System.Classes,
  System.UITypes,
  System.Types,
  FMX.Types,
  FMX.Colors;

type
  TGradientTrackBar = class(TBitmapTrackbar)
  private
    FGradient: TGradient;
    procedure SetEndColor(const Value: TAlphaColor);
    procedure SetBeginColor(const Value: TAlphaColor);
    function GetEndColor: TAlphaColor;
    function GetBeginColor: TAlphaColor;
  protected
    procedure FillBitmap; override;
    procedure DoPaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BeginColor: TAlphaColor read GetBeginColor write SetBeginColor;
    property EndColor: TAlphaColor read GetEndColor write SetEndColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Colors', [TGradientTrackBar{, THueTrackBar, TAlphaTrackBar, TBWTrackBar}]);
end;

{ TGradientTrackBar }

const
  CEndIndex = 0;
  CBeginIndex = 1;

constructor TGradientTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FGradient := TGradient.Create;
  FGradient.StartPosition.X := 1;
  FGradient.StartPosition.y := 0;
  FGradient.Points[CBeginIndex].Color := TAlphaColorRec.Red;
  FGradient.Points[CEndIndex].Color := TAlphaColorRec.Blue;
end;

destructor TGradientTrackBar.Destroy;
begin
  FGradient.Free;
  inherited;
end;

procedure TGradientTrackBar.DoPaint;
begin
  inherited;
  FillBitmap;
end;

procedure TGradientTrackBar.FillBitmap;
begin
  if not assigned(FBitmap) then exit;

  FBitmap.Canvas.BeginScene();
  try
    FBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
    FBitmap.Canvas.Fill.Gradient.Assign(FGradient);
    FBitmap.Canvas.FillRect(TRectF.Create(0, 0, FBitmap.Width, FBitmap.Height), 1);
  finally
    FBitmap.Canvas.EndScene;
  end;
end;

function TGradientTrackBar.GetEndColor: TAlphaColor;
begin
  Result := FGradient.Points[CEndIndex].Color;
end;

function TGradientTrackBar.GetBeginColor: TAlphaColor;
begin
  Result := FGradient.Points[CBeginIndex].Color;
end;

procedure TGradientTrackBar.SetEndColor(const Value: TAlphaColor);
begin
  if FGradient.Points[CEndIndex].Color <> Value then
  begin
    FGradient.Points[CEndIndex].Color := Value;
    UpdateBitmap;
  end;
end;

procedure TGradientTrackBar.SetBeginColor(const Value: TAlphaColor);
begin
  if FGradient.Points[CBeginIndex].Color <> Value then
  begin
    FGradient.Points[CBeginIndex].Color := Value;
    UpdateBitmap;
  end;
end;

initialization

  RegisterFmxClasses( [ TGradientTrackBar ] );

end.
