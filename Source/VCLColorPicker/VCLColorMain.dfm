object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'VCL Color Picker'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object Label1: TLabel
    Left = 14
    Top = 14
    Width = 20
    Height = 15
    Alignment = taRightJustify
    Caption = 'Red'
  end
  object Label2: TLabel
    Left = 3
    Top = 51
    Width = 31
    Height = 15
    Alignment = taRightJustify
    Caption = 'Green'
  end
  object Label3: TLabel
    Left = 11
    Top = 88
    Width = 23
    Height = 15
    Alignment = taRightJustify
    Caption = 'Blue'
  end
  object Label4: TLabel
    Left = 18
    Top = 173
    Width = 16
    Height = 15
    Alignment = taRightJustify
    Caption = 'Sat'
  end
  object Label5: TLabel
    Left = 10
    Top = 211
    Width = 24
    Height = 15
    Alignment = taRightJustify
    Caption = 'Lum'
  end
  object Label6: TLabel
    Left = 12
    Top = 141
    Width = 22
    Height = 15
    Alignment = taRightJustify
    Caption = 'Hue'
  end
  object lblRed: TLabel
    Left = 302
    Top = 8
    Width = 18
    Height = 15
    Caption = '000'
  end
  object lblGreen: TLabel
    Left = 302
    Top = 45
    Width = 18
    Height = 15
    Caption = '000'
  end
  object lblBlue: TLabel
    Left = 302
    Top = 82
    Width = 18
    Height = 15
    Caption = '000'
  end
  object lblSat: TLabel
    Left = 302
    Top = 167
    Width = 18
    Height = 15
    Caption = '000'
  end
  object lblLum: TLabel
    Left = 302
    Top = 205
    Width = 18
    Height = 15
    Caption = '000'
  end
  object lblHue: TLabel
    Left = 302
    Top = 135
    Width = 18
    Height = 15
    Caption = '000'
  end
  object tbRed: TTrackBar
    Left = 40
    Top = 8
    Width = 256
    Height = 34
    Max = 255
    Frequency = 10
    TabOrder = 0
    OnChange = tbRedChange
  end
  object tbGreen: TTrackBar
    Left = 40
    Top = 46
    Width = 256
    Height = 30
    Max = 255
    Frequency = 10
    TabOrder = 1
    OnChange = tbRedChange
  end
  object tbBlue: TTrackBar
    Left = 40
    Top = 84
    Width = 256
    Height = 30
    Max = 255
    Frequency = 10
    TabOrder = 2
    OnChange = tbRedChange
  end
  object tbSat: TTrackBar
    Left = 40
    Top = 172
    Width = 256
    Height = 34
    Max = 100
    Frequency = 5
    Position = 100
    TabOrder = 3
    OnChange = tbSatChange
  end
  object tbLum: TTrackBar
    Left = 40
    Top = 208
    Width = 256
    Height = 30
    Max = 100
    Frequency = 5
    Position = 50
    TabOrder = 4
    OnChange = tbSatChange
  end
  object tbHue: TTrackBar
    Left = 40
    Top = 137
    Width = 256
    Height = 30
    Max = 360
    Frequency = 18
    TabOrder = 5
    OnChange = tbSatChange
  end
  object pnlColor: TPanel
    Left = 368
    Top = 8
    Width = 167
    Height = 159
    Caption = ' '
    Color = clRed
    ParentBackground = False
    TabOrder = 6
    object paintPalette: TPaintBox
      Left = 1
      Top = 133
      Width = 165
      Height = 25
      Align = alBottom
      OnPaint = paintPalettePaint
    end
    object paintShade: TPaintBox
      Left = 1
      Top = 1
      Width = 165
      Height = 25
      Align = alTop
      OnPaint = paintShadePaint
    end
    object paintTone: TPaintBox
      Left = 141
      Top = 26
      Width = 25
      Height = 107
      Align = alRight
    end
    object paintTint: TPaintBox
      Left = 1
      Top = 26
      Width = 25
      Height = 107
      Align = alLeft
    end
  end
  object rdPalette: TRadioGroup
    Left = 368
    Top = 205
    Width = 166
    Height = 191
    Caption = 'Palette'
    ItemIndex = 3
    Items.Strings = (
      'Analogous'
      'Triad'
      'Split Complementary'
      'Quad'
      'Rectangle'
      'Complementary'
      'All')
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button1: TButton
    Left = 252
    Top = 259
    Width = 68
    Height = 46
    Caption = 'Generate'
    TabOrder = 8
    OnClick = Button1Click
  end
end