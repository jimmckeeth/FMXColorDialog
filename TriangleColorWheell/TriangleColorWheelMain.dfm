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
    Top = 41
    Width = 663
    Height = 626
    Align = alClient
    OnDraw = SkPaintBox1Draw
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 663
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Export'
      TabOrder = 0
      OnClick = Button1Click
    end
    object TrackBar1: TTrackBar
      Left = 97
      Top = 0
      Width = 566
      Height = 45
      Max = 120
      Min = 3
      Position = 72
      TabOrder = 1
      OnChange = TrackBar1Change
    end
  end
end
