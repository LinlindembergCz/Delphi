object FrmSqlMonitor: TFrmSqlMonitor
  Left = 0
  Top = 0
  Caption = 'SQL Monitor'
  ClientHeight = 268
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object BevelBottom: TBevel
    Left = 0
    Top = 225
    Width = 473
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = -76
    ExplicitTop = 311
    ExplicitWidth = 825
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 473
    Height = 225
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 227
    Width = 473
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 231
    DesignSize = (
      473
      41)
    object btClear: TButton
      Left = 279
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'C&lear'
      TabOrder = 0
      OnClick = btClearClick
    end
    object btExit: TButton
      Left = 375
      Top = 7
      Width = 90
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Close'
      TabOrder = 1
      OnClick = btExitClick
    end
  end
end
