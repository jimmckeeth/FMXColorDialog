unit SkiaHelpers;

interface

uses
  System.Skia,
  System.UITypes;

type
  TSKPaintHelper = class helper for TSKPaint
    class function Interfaced: ISKPaint; overload;
    class function Interfaced(const APaint: ISkPaint): ISKPaint; overload;
    class function Interfaced(const AStyle: TSkPaintStyle): ISKPaint; overload;
    class function Interfaced(const AColor: TAlphaColor): ISKPaint; overload;
  end;
  TSkPathBuilderHelper = class helper for TSkPathBuilder
    class function Interfaced: ISkPathBuilder; overload;
    class function Interfaced(const APathBuilder: ISkPathBuilder): ISkPathBuilder; overload;
    class function Interfaced(const AFillType: TSkPathFillType): ISkPathBuilder; overload;
  end;
  TSKFontHelper = class helper for TSKFont
    class function Interfaced(ATypeface: ISkTypeface = nil; const ASize: Single = 12; const AScaleX: Single = 1; const ASkewX: Single = 0): ISkFont; overload;
  end;

implementation

{ TSKPaintHelper }

class function TSKPaintHelper.Interfaced: ISKPaint;
begin
  Result := TSkPaint.Create;
end;

class function TSKPaintHelper.Interfaced(
  const AStyle: TSkPaintStyle): ISKPaint;
begin
  Result := TSkPaint.Create(AStyle);
end;

class function TSKPaintHelper.Interfaced(const APaint: ISkPaint): ISKPaint;
begin
  Result := TSkPaint.Create(APaint);
end;

class function TSKPaintHelper.Interfaced(
  const AColor: TAlphaColor): ISKPaint;
begin
  Result := TSkPaint.Create;
  Result.Color := AColor;
end;

{ TSkPathBuilderHelper }

class function TSkPathBuilderHelper.Interfaced: ISkPathBuilder;
begin
  Result := TSkPathBuilder.Create;
end;

class function TSkPathBuilderHelper.Interfaced(
  const AFillType: TSkPathFillType): ISkPathBuilder;
begin
  Result := TSkPathBuilder.Create(AFillType);
end;

class function TSkPathBuilderHelper.Interfaced(
  const APathBuilder: ISkPathBuilder): ISkPathBuilder;
begin
  Result := TSkPathBuilder.Create(APathBuilder);
end;

{ TSKFontHelper }

class function TSKFontHelper.Interfaced(ATypeface: ISkTypeface;
  const ASize, AScaleX, ASkewX: Single): ISkFont;
begin
  Result := TSkFont.Create(ATypeface, ASize, AScaleX, ASkewX);
end;

end.
