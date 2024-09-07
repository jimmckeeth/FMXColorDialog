object Form27: TForm27
  Left = 0
  Top = 0
  Caption = 'Triangle Color Wheel'
  ClientHeight = 667
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object SkPaintBox1: TSkPaintBox
    Left = 0
    Top = 0
    Width = 663
    Height = 667
    Align = alClient
    OnDraw = SkPaintBox1Draw
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
end
