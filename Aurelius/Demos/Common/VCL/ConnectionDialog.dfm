object frmConnectionDialog: TfrmConnectionDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Connection Configuration'
  ClientHeight = 323
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Database Connection'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 282
    Width = 478
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      478
      41)
    object btOk: TButton
      Left = 301
      Top = 7
      Width = 79
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Ok'
      Default = True
      TabOrder = 0
      OnClick = btOkClick
    end
    object btCancel: TButton
      Left = 390
      Top = 7
      Width = 78
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btCancelClick
    end
  end
  object MainPanel: TPanel
    Left = 0
    Top = 71
    Width = 478
    Height = 211
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 10
      Top = 10
      Width = 458
      Height = 191
      ActivePage = tsSQLite
      Align = alClient
      TabOrder = 0
      object tsdbExpress: TTabSheet
        Caption = 'dbExpress Settings'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lbConnectionName: TLabel
          Left = 10
          Top = 14
          Width = 88
          Height = 13
          Caption = 'Connection Name:'
        end
        object cbConnectionName: TComboBox
          Left = 114
          Top = 11
          Width = 295
          Height = 21
          Style = csDropDownList
          Sorted = True
          TabOrder = 0
        end
        object pnInfo: TPanel
          Left = 0
          Top = 110
          Width = 450
          Height = 53
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 1
          object lbInfo: TLabel
            Left = 5
            Top = 5
            Width = 424
            Height = 39
            Align = alClient
            Caption = 
              'Choose an existing dbExpress connection that is properly configu' +
              'red to connect to an existing database. User name and password m' +
              'ust be already set in the connection. IT'#39'S RECOMMENDED that you ' +
              'don'#39't use a production database.'
            WordWrap = True
          end
        end
      end
      object tsdbGo: TTabSheet
        Caption = 'dbGo (ADO) Settings'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          450
          163)
        object Label2: TLabel
          Left = 10
          Top = 14
          Width = 89
          Height = 13
          Caption = 'Connection String:'
        end
        object edConnectionString: TEdit
          Left = 114
          Top = 11
          Width = 295
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btEditConnectionString: TButton
          Left = 414
          Top = 11
          Width = 23
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = btEditConnectionStringClick
        end
        object Panel2: TPanel
          Left = 0
          Top = 131
          Width = 450
          Height = 32
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 2
          object Label3: TLabel
            Left = 5
            Top = 5
            Width = 392
            Height = 13
            Align = alClient
            Caption = 
              'Note: Only MS SQL Server and DB2 databases are supported in dbGo' +
              ' connections'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 78
          Width = 450
          Height = 53
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 3
          object Label4: TLabel
            Left = 5
            Top = 5
            Width = 439
            Height = 39
            Align = alClient
            Caption = 
              'Provide a connection string to connect to an existing database. ' +
              'User name and password must be already set in the connection. IT' +
              #39'S RECOMMENDED that you don'#39't use a production database. '
            WordWrap = True
          end
        end
      end
      object tsSQLite: TTabSheet
        Caption = 'SQLite (Native) Settings'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          450
          163)
        object Label5: TLabel
          Left = 10
          Top = 14
          Width = 67
          Height = 13
          Caption = 'Database file:'
        end
        object edSQLiteFile: TEdit
          Left = 104
          Top = 11
          Width = 305
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object Panel5: TPanel
          Left = 0
          Top = 128
          Width = 450
          Height = 35
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 1
          object Label6: TLabel
            Left = 5
            Top = 5
            Width = 364
            Height = 13
            Align = alClient
            Caption = 
              'Specify the SQLite database file. If the file does not exist, it' +
              ' will be created.'
            WordWrap = True
          end
        end
        object btOpenDialog: TButton
          Left = 414
          Top = 11
          Width = 23
          Height = 21
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 2
          OnClick = btOpenDialogClick
        end
      end
      object tsNoSettings: TTabSheet
        Caption = 'No Settings'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 450
          Height = 163
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          object Memo1: TMemo
            Left = 5
            Top = 5
            Width = 440
            Height = 153
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Lines.Strings = (
              
                'Library not available. Either the library is not supported in cu' +
                'rrent environment, or you '
              'must manually activate it.'
              
                'To activate, open DBConnection.pas and define one of the followi' +
                'ng directives depending '
              'on the library you want to activate:'
              ''
              '{$DEFINE DBEXPRESS} // To use dbExpress'
              '{$DEFINE DBGO} // To use dbGo/ADO')
            ReadOnly = True
            TabOrder = 0
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 478
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      Left = 10
      Top = 8
      Width = 125
      Height = 13
      Caption = 'Component library to use:'
    end
    object cbLibrary: TComboBox
      Left = 163
      Top = 6
      Width = 142
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbLibraryChange
      Items.Strings = (
        'SQLite (native)'
        'dbGo (ADO)'
        'dbExpress')
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.sqlite'
    Filter = 'SQLite (*.sqlite)|*.sqlite|All Files (*.*)|*.*'
    FilterIndex = 0
    Left = 232
    Top = 168
  end
end
