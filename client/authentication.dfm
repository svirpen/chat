object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 218
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 144
  TextHeight = 19
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 618
    Height = 218
    Align = alClient
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Label2
        Row = 0
      end
      item
        Column = 1
        ColumnSpan = 3
        Control = Edit1
        Row = 0
      end
      item
        Column = 0
        Control = Label1
        Row = 1
      end
      item
        Column = 1
        ColumnSpan = 3
        Control = Edit2
        Row = 1
      end
      item
        Column = 2
        Control = Button1
        Row = 2
      end
      item
        Column = 3
        Control = Button2
        Row = 2
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 30.000000000000000000
      end>
    TabOrder = 0
    ExplicitLeft = 48
    ExplicitTop = 312
    ExplicitWidth = 569
    ExplicitHeight = 169
    DesignSize = (
      618
      218)
    object Label2: TLabel
      Left = 1
      Top = 75
      Width = 208
      Height = 19
      Align = alBottom
      Alignment = taRightJustify
      Caption = 'Label2'
      ExplicitLeft = 60
      ExplicitTop = 25
      ExplicitWidth = 46
    end
    object Edit1: TEdit
      Left = 209
      Top = 65
      Width = 408
      Height = 29
      Align = alBottom
      TabOrder = 0
      Text = 'Edit1'
      ExplicitLeft = 184
      ExplicitTop = 16
      ExplicitWidth = 383
    end
    object Label1: TLabel
      Left = 1
      Top = 94
      Width = 208
      Height = 19
      Align = alTop
      Alignment = taRightJustify
      Caption = 'Label1'
      ExplicitLeft = -5
      ExplicitTop = 75
      ExplicitWidth = 183
    end
    object Edit2: TEdit
      Left = 209
      Top = 94
      Width = 408
      Height = 27
      Align = alTop
      TabOrder = 1
      Text = 'Edit2'
      ExplicitLeft = 215
      ExplicitTop = 89
      ExplicitWidth = 121
    end
    object Button1: TButton
      Left = 429
      Top = 189
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Button1'
      TabOrder = 2
      ExplicitLeft = 55
      ExplicitTop = 139
    end
    object Button2: TButton
      Left = 529
      Top = 189
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Button2'
      TabOrder = 3
      ExplicitLeft = 55
      ExplicitTop = 139
    end
  end
end
