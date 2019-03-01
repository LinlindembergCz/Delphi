object Form37: TForm37
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TotaisNFE'
  ClientHeight = 284
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Itens: TLabel
    Left = 10
    Top = 48
    Width = 25
    Height = 13
    Caption = 'Itens'
  end
  object Label1: TLabel
    Left = 8
    Top = 162
    Width = 29
    Height = 13
    Caption = 'Totais'
  end
  object btn1: TButton
    Left = 88
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Importar'
    TabOrder = 0
    OnClick = btn1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 67
    Width = 241
    Height = 89
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 9
    Top = 181
    Width = 241
    Height = 89
    TabOrder = 2
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = '*.xml'
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 200
    Top = 8
  end
end
