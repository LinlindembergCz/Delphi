object frmListArtists: TfrmListArtists
  Left = 0
  Top = 0
  Caption = 'Artists'
  ClientHeight = 355
  ClientWidth = 413
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
    Width = 413
    Height = 2
    Align = alTop
    Shape = bsTopLine
    ExplicitLeft = -76
    ExplicitWidth = 825
  end
  object BevelBottom: TBevel
    Left = 0
    Top = 312
    Width = 413
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = -76
    ExplicitTop = 311
    ExplicitWidth = 825
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 413
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Artists'
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
    Width = 413
    Height = 269
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      413
      269)
    object Grid: TStringGrid
      Left = 7
      Top = 8
      Width = 398
      Height = 257
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 2
      DefaultRowHeight = 20
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      ScrollBars = ssVertical
      TabOrder = 0
      OnDblClick = GridDblClick
      ColWidths = (
        188
        186)
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 314
    Width = 413
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      413
      41)
    object btNew: TButton
      Left = 219
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&New'
      TabOrder = 2
      OnClick = btNewClick
    end
    object btExit: TButton
      Left = 315
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Close'
      ModalResult = 2
      TabOrder = 3
    end
    object btEdit: TButton
      Left = 27
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit'
      Default = True
      TabOrder = 0
      OnClick = btEditClick
    end
    object btDelete: TButton
      Left = 123
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 1
      OnClick = btDeleteClick
    end
  end
end
