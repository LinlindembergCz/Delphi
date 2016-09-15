object frmListMediaFiles: TfrmListMediaFiles
  Left = 0
  Top = 0
  Caption = 'Media Files'
  ClientHeight = 354
  ClientWidth = 825
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
    Width = 825
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitWidth = 542
  end
  object BevelBottom: TBevel
    Left = 0
    Top = 311
    Width = 825
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 49
    ExplicitWidth = 542
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 825
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Media Files'
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
    Width = 825
    Height = 268
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      825
      268)
    object Grid: TStringGrid
      Left = 8
      Top = 6
      Width = 810
      Height = 256
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 6
      DefaultRowHeight = 20
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      ScrollBars = ssVertical
      TabOrder = 0
      OnDblClick = GridDblClick
      ColWidths = (
        49
        158
        140
        128
        59
        251)
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 313
    Width = 825
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      825
      41)
    object btNewSong: TButton
      Left = 535
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'New &Song'
      TabOrder = 2
      OnClick = btNewSongClick
    end
    object btExit: TButton
      Left = 727
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Close'
      ModalResult = 2
      TabOrder = 4
    end
    object btNewVideo: TButton
      Left = 631
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'New &Video'
      TabOrder = 3
      OnClick = btNewVideoClick
    end
    object btEdit: TButton
      Left = 343
      Top = 6
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit'
      Default = True
      TabOrder = 0
      OnClick = btEditClick
    end
    object btDelete: TButton
      Left = 439
      Top = 6
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 1
      OnClick = btDeleteClick
    end
  end
end
