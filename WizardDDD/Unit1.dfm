object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GenerateDDD'
  ClientHeight = 636
  ClientWidth = 949
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 237
    Height = 636
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      237
      636)
    object Button2: TButton
      Left = 13
      Top = 575
      Width = 67
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Genarate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = Button2Click
    end
    object ValueListEditor1: TValueListEditor
      Left = 1
      Top = 177
      Width = 235
      Height = 365
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      TabOrder = 1
      TitleCaptions.Strings = (
        'Fields'
        'Types')
      ColWidths = (
        130
        99)
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 235
      Height = 176
      Align = alTop
      TabOrder = 2
      object Label1: TLabel
        Left = 4
        Top = 44
        Width = 28
        Height = 13
        Caption = 'Entity'
      end
      object Label2: TLabel
        Left = 4
        Top = 68
        Width = 22
        Height = 13
        Caption = 'Field'
      end
      object Label3: TLabel
        Left = 4
        Top = 96
        Width = 24
        Height = 13
        Caption = 'Type'
      end
      object Label4: TLabel
        Left = 4
        Top = 20
        Width = 34
        Height = 13
        Caption = 'Project'
      end
      object SpeedButton1: TSpeedButton
        Left = 44
        Top = 10
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = SpeedButton1Click
      end
      object Label5: TLabel
        Left = 4
        Top = 125
        Width = 33
        Height = 13
        Caption = 'Length'
      end
      object edtEntity: TEdit
        Left = 66
        Top = 38
        Width = 153
        Height = 21
        TabOrder = 0
      end
      object edtField: TEdit
        Left = 66
        Top = 65
        Width = 153
        Height = 21
        TabOrder = 1
      end
      object cboType: TComboBox
        Left = 66
        Top = 92
        Width = 153
        Height = 21
        TabOrder = 2
        Items.Strings = (
          'TString'
          'TInteger'
          'TFloat'
          'TLinqDatetime')
      end
      object Button1: TButton
        Left = 66
        Top = 145
        Width = 63
        Height = 25
        Caption = 'Add Field'
        TabOrder = 3
        OnClick = Button1Click
      end
      object edtProject: TEdit
        Left = 66
        Top = 11
        Width = 153
        Height = 21
        TabOrder = 4
      end
      object edtLength: TEdit
        Left = 66
        Top = 120
        Width = 49
        Height = 21
        NumbersOnly = True
        TabOrder = 5
      end
      object chkNotNull: TCheckBox
        Left = 133
        Top = 122
        Width = 97
        Height = 17
        Caption = 'not null'
        TabOrder = 6
      end
    end
    object GroupBox1: TGroupBox
      Left = 5
      Top = 520
      Width = 226
      Height = 49
      TabOrder = 3
      object chkEntity: TCheckBox
        Left = 8
        Top = 5
        Width = 97
        Height = 17
        Caption = 'Entity'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkRepository: TCheckBox
        Left = 8
        Top = 26
        Width = 97
        Height = 17
        Caption = 'Repository'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chkService: TCheckBox
        Left = 126
        Top = 3
        Width = 97
        Height = 17
        Caption = 'Service'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object chkController: TCheckBox
        Left = 126
        Top = 26
        Width = 97
        Height = 17
        Caption = 'Controller'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object chkSave: TCheckBox
      Left = 86
      Top = 579
      Width = 123
      Height = 17
      Caption = 'save  on generate'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
    end
    object Button3: TButton
      Left = 13
      Top = 602
      Width = 67
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = Button3Click
    end
  end
  object PageControl1: TPageControl
    Left = 237
    Top = 0
    Width = 712
    Height = 636
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Entity'
      object MemoEntity: TMemo
        Left = 0
        Top = 0
        Width = 704
        Height = 608
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Repository'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoRepository: TMemo
        Left = 0
        Top = 257
        Width = 704
        Height = 282
        Align = alTop
        TabOrder = 0
      end
      object MemoInterfaceRepository: TMemo
        Left = 0
        Top = 0
        Width = 704
        Height = 257
        Align = alTop
        TabOrder = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Service'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoService: TMemo
        Left = 0
        Top = 265
        Width = 704
        Height = 265
        Align = alTop
        TabOrder = 0
      end
      object MemoInterfaceService: TMemo
        Left = 0
        Top = 0
        Width = 704
        Height = 265
        Align = alTop
        TabOrder = 1
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Controller'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoController: TMemo
        Left = 0
        Top = 0
        Width = 704
        Height = 608
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.pas'
    Filter = '*.pas|*.pas'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 145
    Top = 361
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Title = 'Project Delphi'
    Left = 65
    Top = 209
  end
  object SQLConnection1: TSQLConnection
    Left = 617
    Top = 144
  end
  object FDConnection1: TFDConnection
    Left = 585
    Top = 200
  end
end
