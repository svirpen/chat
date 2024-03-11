object FormLogin: TFormLogin
  Left = 0
  Top = 0
  AutoSize = True
  Caption = #1042#1093#1086#1076
  ClientHeight = 321
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 144
  TextHeight = 19
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 476
    Height = 321
    Align = alClient
    ColumnCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 100.000000000000000000
      end
      item
        Value = 100.000000000000000000
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
        Column = 2
        Control = btnOk
        Row = 3
      end
      item
        Column = 3
        Control = btnCancel
        Row = 3
      end
      item
        Column = 0
        ColumnSpan = 4
        Control = GroupBoxUser
        Row = 2
      end
      item
        Column = 0
        ColumnSpan = 4
        Control = GroupBoxConnect
        Row = 0
      end
      item
        Column = 0
        ColumnSpan = 4
        Control = GroupBoxCert
        Row = 1
      end>
    FullRepaint = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    RowCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 70.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 70.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 130.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 0
    object btnOk: TButton
      Left = 275
      Top = 285
      Width = 90
      Height = 25
      Align = alBottom
      Caption = 'OK'
      TabOrder = 3
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 375
      Top = 286
      Width = 90
      Height = 24
      Align = alBottom
      Caption = #1054#1090#1084#1077#1085#1072
      TabOrder = 4
      OnClick = btnCancelClick
    end
    object GroupBoxUser: TGroupBox
      Left = 11
      Top = 151
      Width = 454
      Height = 120
      Align = alClient
      Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
      Padding.Left = 3
      Padding.Top = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 2
      object GridPanel2: TGridPanel
        Left = 5
        Top = 24
        Width = 444
        Height = 91
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 90.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = LabelLogin
            Row = 0
          end
          item
            Column = 1
            Control = EditUsername
            Row = 0
          end
          item
            Column = 0
            Control = LabelPassword
            Row = 1
          end
          item
            Column = 1
            Control = EditPassword
            Row = 1
          end>
        FullRepaint = False
        Padding.Left = 3
        Padding.Top = 3
        Padding.Right = 3
        Padding.Bottom = 3
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end
          item
            SizeStyle = ssAuto
          end>
        TabOrder = 0
        object LabelLogin: TLabel
          AlignWithMargins = True
          Left = 42
          Top = 9
          Width = 45
          Height = 19
          Align = alTop
          Alignment = taRightJustify
          Caption = #1051#1086#1075#1080#1085
        end
        object EditUsername: TEdit
          Left = 96
          Top = 6
          Width = 342
          Height = 27
          Align = alTop
          TabOrder = 0
          Text = 'admin'
        end
        object LabelPassword: TLabel
          AlignWithMargins = True
          Left = 33
          Top = 51
          Width = 54
          Height = 19
          Align = alTop
          Alignment = taRightJustify
          Caption = #1055#1072#1088#1086#1083#1100
        end
        object EditPassword: TEdit
          Left = 96
          Top = 48
          Width = 342
          Height = 27
          Align = alTop
          PasswordChar = '*'
          TabOrder = 1
          Text = 'passwd'
          OnKeyDown = EditPasswordKeyDown
        end
      end
    end
    object GroupBoxConnect: TGroupBox
      Left = 11
      Top = 11
      Width = 454
      Height = 60
      Align = alClient
      Caption = #1055#1086#1076#1082#1083#1102#1095#1077#1085#1080#1077
      Padding.Left = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 0
      object GridPanel3: TGridPanel
        Left = 5
        Top = 21
        Width = 444
        Height = 34
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 90.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 60.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = LabelServer
            Row = 0
          end
          item
            Column = 1
            Control = EditServer
            Row = 0
          end
          item
            Column = 2
            Control = LabelPort
            Row = 0
          end
          item
            Column = 3
            Control = EditPort
            Row = 0
          end>
        FullRepaint = False
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        object LabelServer: TLabel
          AlignWithMargins = True
          Left = 35
          Top = 3
          Width = 52
          Height = 19
          Align = alTop
          Alignment = taRightJustify
          Caption = #1057#1077#1088#1074#1077#1088
        end
        object EditServer: TEdit
          Left = 90
          Top = 0
          Width = 147
          Height = 27
          Align = alTop
          TabOrder = 0
          Text = '127.0.0.1'
        end
        object LabelPort: TLabel
          AlignWithMargins = True
          Left = 257
          Top = 3
          Width = 37
          Height = 19
          Align = alTop
          Alignment = taRightJustify
          Caption = #1055#1086#1088#1090
        end
        object EditPort: TEdit
          Left = 297
          Top = 0
          Width = 147
          Height = 27
          Align = alTop
          TabOrder = 1
          Text = '2006'
        end
      end
    end
    object GroupBoxCert: TGroupBox
      Left = 11
      Top = 81
      Width = 454
      Height = 60
      Align = alClient
      Caption = #1054#1090#1087#1077#1095#1072#1090#1086#1082' SHA-256 '#1089#1077#1088#1090#1080#1092#1080#1082#1072#1090#1072' '#1089#1077#1088#1074#1077#1088#1072' ('#1086#1087#1094#1080#1086#1085#1072#1083#1100#1085#1086')'
      Padding.Left = 3
      Padding.Right = 3
      Padding.Bottom = 3
      TabOrder = 1
      object EditHashCert: TEdit
        Left = 5
        Top = 21
        Width = 444
        Height = 27
        Align = alTop
        TabOrder = 0
      end
    end
  end
  object TimerWaitResponse: TTimer
    Enabled = False
    Interval = 50000
    OnTimer = TimerWaitResponseTimer
    Left = 72
    Top = 128
  end
end
