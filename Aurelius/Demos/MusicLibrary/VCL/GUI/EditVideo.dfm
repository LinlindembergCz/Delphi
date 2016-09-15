object frmEditVideo: TfrmEditVideo
  Left = 0
  Top = 0
  Caption = 'Music Videos'
  ClientHeight = 301
  ClientWidth = 459
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
  object BevelTop: TBevel
    Left = 0
    Top = 41
    Width = 459
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitWidth = 542
  end
  object BevelBottom: TBevel
    Left = 0
    Top = 258
    Width = 459
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 49
    ExplicitWidth = 542
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 459
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Music Videos'
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
  object MainPanel: TPanel
    Left = 0
    Top = 43
    Width = 459
    Height = 215
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      459
      215)
    object lbName: TLabel
      Left = 16
      Top = 13
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object lbFileLocation: TLabel
      Left = 16
      Top = 44
      Width = 63
      Height = 13
      Caption = 'File Location:'
    end
    object lbFormat: TLabel
      Left = 16
      Top = 76
      Width = 38
      Height = 13
      Caption = 'Format:'
    end
    object lbArtist: TLabel
      Left = 16
      Top = 108
      Width = 30
      Height = 13
      Caption = 'Artist:'
    end
    object LbAlbum: TLabel
      Left = 16
      Top = 140
      Width = 33
      Height = 13
      Caption = 'Album:'
    end
    object Label1: TLabel
      Left = 16
      Top = 172
      Width = 45
      Height = 13
      Caption = 'Duration:'
    end
    object btNewFormat: TSpeedButton
      Left = 424
      Top = 73
      Width = 23
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = btNewFormatClick
    end
    object btNewArtist: TSpeedButton
      Left = 424
      Top = 105
      Width = 23
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = btNewArtistClick
    end
    object btNewAlbum: TSpeedButton
      Left = 423
      Top = 137
      Width = 23
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = btNewAlbumClick
    end
    object btFile: TSpeedButton
      Left = 424
      Top = 41
      Width = 23
      Height = 21
      Anchors = [akTop, akRight]
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00EAFFFF0085D8EC0010A5CA00007BAD0000759D006DB3CC00E0F9FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0030C2
        E200009BC8000095C2000093C2000093C2000073A900006197000085BE000095
        C4000093C4000091C20000A3CE007DD6EC00FF00FF00FF00FF00FF00FF0000B1
        D80014BEDE0002B1D60000A7D000009DCA000073A90000619B000085C4000097
        D2000097D2000097D2000097D00000A3CE00FF00FF00FF00FF00FF00FF0000B7
        DE0026CCE60014BEDE0002B1D60000A7D0000073A90000659D000085C4000097
        D2000097D2000097D200BB5F00000093C400FF00FF00FF00FF00FF00FF0000BE
        E40038D8EE0026CCE60014BEDE0002B1D6000075A90000679D000089C600009B
        D4000097D2000097D200D08700000095C600FF00FF00FF00FF00FF00FF0006C6
        E8004AE4F50038D8EE0026CCE60014BEDE000079AD00006DA100008DC80000A3
        D800009FD600009BD400EEEEEE000097C800FF00FF00FF00FF00FF00FF000ECC
        EE005AEEFD004AE4F50038D8EE0026CCE600007DB1000073A5000093CA0000AF
        DE0000A9DC0000A3D800EEEEEE000099CA00FF00FF00FF00FF00FF00FF0016D2
        F20069F7FF005AEEFD004AE4F50038D8EE000083B5000079A7000099CE0000BB
        E60000B5E20000AFDE00EEEEEE00009BCC00FF00FF00FF00FF00FF00FF001CD6
        F50075FFFF0069F7FF005AEEFD00A1F7FF000089B9000081AB0000A1D20018CA
        EE0008C4EA0000BBE60000B5E200009FCE00FF00FF00FF00FF00FF00FF0024DC
        F9007DFFFF0075FFFF0069F7FF00E6FFFF00008FBE000087AF0000A7D60034DA
        F50026D2F20018CAEE0000ADDA005FCEEA00FF00FF00FF00FF00FF00FF0028E0
        FB007DFFFF007DFFFF0075FFFF00FDFFFF000095C400008DB10001ADD80050E8
        FB0042E0F90034DAF50000A7D400FF00FF00FF00FF00FF00FF00FF00FF002EE2
        FF007DFFFF007DFFFF007DFFFF00FF00FF00009DCA000091B5000EB3DC006DF3
        FF005FEEFF0050E8FB0000A9D800FF00FF00FF00FF00FF00FF00FF00FF002EE4
        FF007DFFFF007DFFFF007DFFFF00FF00FF0000A5D0000495B70016B9E00083FD
        FF0077F9FF006DF3FF0000ADDA00FF00FF00FF00FF00FF00FF00FF00FF002EE4
        FF007DFFFF007DFFFF00FF00FF00FF00FF0000ABD60012A1C00071F2FF0093FF
        FF008BFFFF0083FDFF0000AFDE00FF00FF00FF00FF00FF00FF00FF00FF002EE4
        FF0077FFFF00E8FFFF0046EAFD0022D4F00006AFCE008DFFFF0099FFFF0099FF
        FF0099FFFF0081FDFF0022C0E600FF00FF00FF00FF00FF00FF00FF00FF0071EE
        FF0030E4FF002EDEFD0026D6F90024D2F70026D0F70020CCF3001AC8F20014C4
        EE000EC0EA002AC8EC0093E6F900FF00FF00FF00FF00FF00FF00}
      OnClick = btFileClick
    end
    object edName: TEdit
      Left = 116
      Top = 10
      Width = 331
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object cbFormat: TComboBox
      Left = 116
      Top = 73
      Width = 301
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object cbArtist: TComboBox
      Left = 116
      Top = 105
      Width = 301
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object CbAlbum: TComboBox
      Left = 116
      Top = 137
      Width = 301
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object edDuration: TMaskEdit
      Left = 116
      Top = 169
      Width = 78
      Height = 21
      EditMask = '!90:00;1;_'
      MaxLength = 5
      TabOrder = 5
      Text = '  :  '
    end
    object edFileLocation: TEdit
      Left = 116
      Top = 41
      Width = 301
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 260
    Width = 459
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      459
      41)
    object btSave: TButton
      Left = 265
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Save'
      Default = True
      TabOrder = 0
      OnClick = btSaveClick
    end
    object btCancel: TButton
      Left = 361
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btCancelClick
    end
  end
end
