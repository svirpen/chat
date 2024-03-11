object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 397
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 19
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 616
    Height = 397
    Align = alClient
    ColumnCollection = <
      item
        Value = 67.344631385624810000
      end
      item
        Value = 32.655368614375200000
      end>
    ControlCollection = <
      item
        Column = 0
        ColumnSpan = 2
        Control = GridPanel2
        Row = 1
      end
      item
        Column = 0
        ColumnSpan = 2
        Control = Panel1
        Row = 0
      end>
    FullRepaint = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 10
    Padding.Bottom = 10
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 45.000000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 0
    object GridPanel2: TGridPanel
      Left = 11
      Top = 346
      Width = 584
      Height = 30
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvNone
      Caption = 'GridPanel2'
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 120.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = EditMessage
          Row = 0
        end
        item
          Column = 1
          Control = btnSend
          Row = 0
        end>
      FullRepaint = False
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      DesignSize = (
        584
        30)
      object EditMessage: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 458
        Height = 24
        Align = alClient
        TabOrder = 0
        OnKeyDown = EditMessageKeyDown
        ExplicitHeight = 27
      end
      object btnSend: TButton
        Left = 489
        Top = 0
        Width = 95
        Height = 30
        Anchors = [akRight, akBottom]
        Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100
        TabOrder = 1
        OnClick = btnSendClick
      end
    end
    object Panel1: TPanel
      Left = 11
      Top = 11
      Width = 584
      Height = 320
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 1
      object Splitter1: TSplitter
        Left = 389
        Top = 1
        Height = 318
        Align = alRight
        ExplicitLeft = 312
        ExplicitTop = 16
        ExplicitHeight = 100
      end
      object GroupBoxMessages: TGroupBox
        Left = 1
        Top = 1
        Width = 388
        Height = 318
        Align = alClient
        Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1103
        TabOrder = 0
        object MemoHistory: TMemo
          Left = 2
          Top = 21
          Width = 384
          Height = 295
          Align = alClient
          TabOrder = 0
        end
      end
      object ListViewParticipants: TListView
        Left = 392
        Top = 1
        Width = 191
        Height = 318
        Align = alRight
        Columns = <
          item
            AutoSize = True
            Caption = #1059#1095#1072#1089#1090#1085#1080#1082#1080
            MaxWidth = 300
            MinWidth = 300
          end>
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
  end
  object TCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 48
    Top = 24
  end
  object FAdoConnection1: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security In' +
      'fo=False;Initial Catalog=chatDb;Data Source=IEWIN7\SQLEXPRESS'
    Provider = 'SQLOLEDB.1'
    Left = 112
    Top = 32
  end
end
