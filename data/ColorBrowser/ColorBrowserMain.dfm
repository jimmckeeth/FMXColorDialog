object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 468
  ClientWidth = 934
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object SkPaintBox1: TSkPaintBox
    Left = 0
    Top = 0
    Width = 934
    Height = 91
    Align = alTop
    OnMouseDown = SkPaintBox1MouseDown
    OnMouseMove = SkPaintBox1MouseMove
    OnDraw = SkPaintBox1Draw
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 132
    Width = 789
    Height = 336
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnDrawColumnCell = DBGrid1DrawColumnCell
    OnTitleClick = DBGrid1TitleClick
    Columns = <
      item
        Expanded = False
        Width = 20
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'idx'
        Visible = False
      end
      item
        Expanded = False
        FieldName = 'hex'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'name'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'red'
        Title.Alignment = taRightJustify
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'green'
        Title.Alignment = taRightJustify
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'blue'
        Title.Alignment = taRightJustify
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'hue'
        Title.Alignment = taRightJustify
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'saturation'
        Title.Alignment = taRightJustify
        Title.Caption = 'sat'
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'luminance'
        Title.Alignment = taRightJustify
        Title.Caption = 'lum'
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'cyan'
        Title.Alignment = taRightJustify
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'magenta'
        Title.Alignment = taRightJustify
        Width = 53
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'yellow'
        Title.Alignment = taRightJustify
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'key'
        Title.Alignment = taRightJustify
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'family'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'source'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'link'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 91
    Width = 934
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Edit2: TEdit
      Left = 230
      Top = 6
      Width = 41
      Height = 23
      TabOrder = 0
      Text = 'Edit2'
    end
    object Edit1: TEdit
      Left = 40
      Top = 6
      Width = 57
      Height = 23
      TabOrder = 1
      Text = 'Edit1'
    end
    object Button1: TButton
      Left = 859
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Edit3: TEdit
      Left = 103
      Top = 6
      Width = 121
      Height = 23
      TabOrder = 3
      Text = 'Edit3'
    end
    object satEdit: TTrackBar
      Left = 295
      Top = 0
      Width = 222
      Height = 35
      Max = 100
      Frequency = 10
      Position = 50
      TabOrder = 4
      OnChange = lumEditChange
    end
    object lumEdit: TTrackBar
      Left = 554
      Top = 0
      Width = 222
      Height = 41
      Max = 100
      Frequency = 10
      Position = 50
      TabOrder = 5
      OnChange = lumEditChange
    end
  end
  object Panel2: TPanel
    Left = 789
    Top = 132
    Width = 145
    Height = 336
    Align = alRight
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object ListBox1: TListBox
      Left = 16
      Top = 6
      Width = 121
      Height = 499
      ItemHeight = 15
      TabOrder = 0
      OnDragDrop = ListBox1DragDrop
      OnDragOver = ListBox1DragOver
      OnEndDrag = ListBox1EndDrag
      OnMouseDown = ListBox1MouseDown
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\jim\Documents\Git\FMXColorDialog\data\colors.d' +
        'b'
      'LockingMode=Normal'
      'JournalMode=WAL'
      'StringFormat=Unicode'
      'DriverID=sQLite')
    Connected = True
    LoginPrompt = False
    Left = 512
    Top = 280
  end
  object FDQuery1: TFDQuery
    Active = True
    AfterScroll = FDQuery1AfterScroll
    OnCalcFields = FDQuery1CalcFields
    Indexes = <
      item
        Active = True
        Name = 'pk'
        Fields = 'idx'
        Distinct = True
      end
      item
        Active = True
        Name = 'dynamic'
        Fields = 'red;green'
      end>
    IndexesActive = False
    Connection = FDConnection1
    SQL.Strings = (
      'select * from colors')
    Left = 512
    Top = 336
    object FDQuery1idx: TFDAutoIncField
      DisplayWidth = 5
      FieldName = 'idx'
      Origin = 'idx'
      ProviderFlags = [pfInWhere, pfInKey]
      Required = True
    end
    object FDQuery1hex: TWideStringField
      DisplayWidth = 10
      FieldName = 'hex'
      Origin = 'hex'
      Size = 32767
    end
    object FDQuery1name: TWideStringField
      DisplayWidth = 20
      FieldName = 'name'
      Origin = 'name'
      Size = 32767
    end
    object FDQuery1red: TSmallintField
      DisplayWidth = 5
      FieldName = 'red'
      Origin = 'red'
    end
    object FDQuery1green: TSmallintField
      DisplayWidth = 5
      FieldName = 'green'
      Origin = 'green'
    end
    object FDQuery1blue: TSmallintField
      DisplayWidth = 5
      FieldName = 'blue'
      Origin = 'blue'
    end
    object FDQuery1family: TWideStringField
      DisplayWidth = 10
      FieldName = 'family'
      Origin = 'family'
      Size = 32767
    end
    object FDQuery1source: TWideStringField
      DisplayWidth = 10
      FieldName = 'source'
      Origin = 'source'
      Size = 32767
    end
    object FDQuery1ID: TWideStringField
      DisplayWidth = 10
      FieldName = 'ID'
      Origin = 'ID'
      Size = 32767
    end
    object FDQuery1hue: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'hue'
      Origin = 'hue'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1saturation: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'saturation'
      Origin = 'saturation'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1luminance: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'luminance'
      Origin = 'luminance'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1cyan: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'cyan'
      Origin = 'cyan'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1magenta: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'magenta'
      Origin = 'magenta'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1yellow: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'yellow'
      Origin = 'yellow'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1key: TSingleField
      DefaultExpression = '0'
      DisplayWidth = 5
      FieldName = 'key'
      Origin = '"key"'
      DisplayFormat = '##0.##'
      Precision = 2
    end
    object FDQuery1link: TWideStringField
      DisplayWidth = 20
      FieldName = 'link'
      Origin = 'link'
      Size = 32767
    end
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 512
    Top = 392
  end
end
