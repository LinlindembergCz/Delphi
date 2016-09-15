object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TMS Aurelius - Getting Started'
  ClientHeight = 439
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 43
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      573
      43)
    object lbConnection: TLabel
      Left = 167
      Top = 14
      Width = 396
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Step 1 - Configure a connection to the database'
      WordWrap = True
      ExplicitWidth = 418
    end
    object btConnection: TButton
      Left = 7
      Top = 9
      Width = 147
      Height = 25
      Caption = 'Configure Connection...'
      TabOrder = 0
      OnClick = btConnectionClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 43
    Width = 573
    Height = 396
    Align = alClient
    BorderWidth = 5
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 6
      Top = 6
      Width = 561
      Height = 384
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet2: TTabSheet
        Caption = 'Map Classes'
        ImageIndex = 1
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          object Label2: TLabel
            Left = 5
            Top = 5
            Width = 543
            Height = 35
            Align = alClient
            AutoSize = False
            Caption = 
              'Step 2 - Map existing (or new classes) to the database by adding' +
              ' attributes to it. This must be done in source code, below is a ' +
              'snippet of the class used in this demo.'
            WordWrap = True
            ExplicitWidth = 524
            ExplicitHeight = 38
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 45
          Width = 553
          Height = 311
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Memo1: TMemo
            Left = 0
            Top = 0
            Width = 553
            Height = 311
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            Lines.Strings = (
              'type'
              '  [Entity]'
              '  [Automapping]'
              '  TPerson = class'
              '  private'
              '    FId: integer;'
              '    FLastName: string;'
              '    FFirstName: string;'
              '    FEmail: string;'
              '  public'
              '    property Id: integer read FId;'
              '    property LastName: string read FLastName write FLastName;'
              '    property FirstName: string read FFirstName write FFirstName;'
              '    property Email: string read FEmail write FEmail;'
              '  end;')
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            WordWrap = False
          end
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Database Schema'
        object btCreateSchema: TButton
          Left = 9
          Top = 44
          Width = 147
          Height = 25
          Caption = 'Create Schema'
          TabOrder = 0
          OnClick = btCreateSchemaClick
        end
        object brDestroySchema: TButton
          Left = 9
          Top = 70
          Width = 147
          Height = 25
          Caption = 'Destroy Schema'
          TabOrder = 1
          OnClick = brDestroySchemaClick
        end
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 2
          object Label3: TLabel
            Left = 5
            Top = 5
            Width = 543
            Height = 35
            Align = alClient
            AutoSize = False
            Caption = 
              'Step 3 - Create database schema (tables and fields). You can lat' +
              'er destroy schema if you want to. (You just need to create schem' +
              'a once for the database)'
            WordWrap = True
            ExplicitWidth = 524
            ExplicitHeight = 38
          end
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Save Objects'
        ImageIndex = 2
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          object Label1: TLabel
            Left = 5
            Top = 5
            Width = 543
            Height = 18
            Align = alClient
            AutoSize = False
            Caption = 'Save (insert) new objects to the database. '
            WordWrap = True
            ExplicitLeft = 0
            ExplicitTop = 4
            ExplicitHeight = 28
          end
        end
        object Panel7: TPanel
          Left = 0
          Top = 28
          Width = 553
          Height = 328
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object GroupBox1: TGroupBox
            Left = 0
            Top = 0
            Width = 553
            Height = 101
            Align = alTop
            Caption = 'Person Data'
            TabOrder = 0
            object Label4: TLabel
              Left = 24
              Top = 24
              Width = 55
              Height = 13
              Caption = 'First Name:'
            end
            object Label5: TLabel
              Left = 24
              Top = 48
              Width = 54
              Height = 13
              Caption = 'Last Name:'
            end
            object Label6: TLabel
              Left = 24
              Top = 72
              Width = 32
              Height = 13
              Caption = 'E-mail:'
            end
            object Label7: TLabel
              Left = 300
              Top = 52
              Width = 149
              Height = 13
              Caption = 'Identifier of last saved person:'
            end
            object edFirstName: TEdit
              Left = 92
              Top = 18
              Width = 121
              Height = 21
              TabOrder = 0
            end
            object edLastName: TEdit
              Left = 92
              Top = 45
              Width = 121
              Height = 21
              TabOrder = 1
            end
            object edEmail: TEdit
              Left = 92
              Top = 69
              Width = 189
              Height = 21
              TabOrder = 2
            end
            object btSavePerson: TButton
              Left = 296
              Top = 18
              Width = 89
              Height = 25
              Caption = 'Save Person'
              TabOrder = 3
              OnClick = btSavePersonClick
            end
            object edLastId: TEdit
              Left = 463
              Top = 48
              Width = 50
              Height = 21
              TabOrder = 4
            end
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Find and Update Objects'
        ImageIndex = 3
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          object Label8: TLabel
            Left = 5
            Top = 5
            Width = 543
            Height = 18
            Align = alClient
            AutoSize = False
            Caption = 'Find existing objects and update them in the database'
            WordWrap = True
            ExplicitLeft = 0
            ExplicitTop = 4
            ExplicitHeight = 28
          end
        end
        object Panel9: TPanel
          Left = 0
          Top = 28
          Width = 553
          Height = 328
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object GroupBox2: TGroupBox
            Left = 0
            Top = 0
            Width = 553
            Height = 153
            Align = alTop
            Caption = 'Find and Update Person'
            TabOrder = 0
            object lbFoundPerson: TLabel
              Left = 24
              Top = 74
              Width = 87
              Height = 13
              Caption = '(no person found)'
            end
            object Label11: TLabel
              Left = 24
              Top = 96
              Width = 32
              Height = 13
              Caption = 'E-mail:'
            end
            object Label12: TLabel
              Left = 24
              Top = 26
              Width = 50
              Height = 13
              Caption = 'Person Id:'
            end
            object edEmailToUpdate: TEdit
              Left = 92
              Top = 93
              Width = 189
              Height = 21
              TabOrder = 0
            end
            object btUpdateEmail: TButton
              Left = 289
              Top = 92
              Width = 89
              Height = 25
              Caption = 'Update E-mail'
              Enabled = False
              TabOrder = 1
              OnClick = btUpdateEmailClick
            end
            object edIdToFind: TEdit
              Left = 92
              Top = 23
              Width = 50
              Height = 21
              TabOrder = 2
              Text = '1'
            end
            object btFindPerson: TButton
              Left = 148
              Top = 22
              Width = 64
              Height = 23
              Caption = 'Find'
              TabOrder = 3
              OnClick = btFindPersonClick
            end
          end
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Querying for Objects'
        ImageIndex = 4
        object Panel10: TPanel
          Left = 0
          Top = 0
          Width = 553
          Height = 28
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 5
          TabOrder = 0
          object Label9: TLabel
            Left = 5
            Top = 5
            Width = 543
            Height = 18
            Align = alClient
            AutoSize = False
            Caption = 'Query for objects using complex criteria'
            WordWrap = True
            ExplicitLeft = 0
            ExplicitTop = 4
            ExplicitHeight = 28
          end
        end
        object Panel11: TPanel
          Left = 0
          Top = 28
          Width = 553
          Height = 328
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object GroupBox3: TGroupBox
            Left = 0
            Top = 0
            Width = 553
            Height = 328
            Align = alClient
            Caption = 'Person Search'
            TabOrder = 0
            DesignSize = (
              553
              328)
            object Label14: TLabel
              Left = 24
              Top = 26
              Width = 195
              Height = 13
              Caption = 'Search for people whose name contains:'
            end
            object edNameToFind: TEdit
              Left = 231
              Top = 23
              Width = 90
              Height = 21
              TabOrder = 0
            end
            object btListPeople: TButton
              Left = 327
              Top = 24
              Width = 64
              Height = 23
              Caption = 'List'
              TabOrder = 1
              OnClick = btListPeopleClick
            end
            object lbResults: TListBox
              Left = 12
              Top = 53
              Width = 525
              Height = 260
              Anchors = [akLeft, akTop, akRight, akBottom]
              ItemHeight = 13
              TabOrder = 2
            end
          end
        end
      end
    end
  end
end
