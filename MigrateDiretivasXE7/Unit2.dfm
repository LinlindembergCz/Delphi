object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 580
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 280
    Top = 8
    Width = 23
    Height = 22
    Caption = 'Go'
  end
  object lbcount: TLabel
    Left = 326
    Top = 413
    Width = 35
    Height = 13
    Caption = 'lbcount'
  end
  object lbReplaces: TLabel
    Left = 379
    Top = 413
    Width = 35
    Height = 13
    Caption = 'lbcount'
  end
  object Memo1: TMemo
    Left = 329
    Top = 8
    Width = 476
    Height = 169
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 329
    Top = 214
    Width = 476
    Height = 193
    TabOrder = 1
  end
  object Button1: TButton
    Left = 326
    Top = 183
    Width = 88
    Height = 25
    Caption = 'Ajustar Units'
    TabOrder = 2
    OnClick = Button1Click
  end
  object chkSave: TCheckBox
    Left = 8
    Top = 551
    Width = 97
    Height = 17
    Caption = 'save on replace'
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 265
    Height = 21
    TabOrder = 4
  end
  object Memo3: TMemo
    Left = 329
    Top = 432
    Width = 476
    Height = 113
    TabOrder = 5
  end
  object Button4: TButton
    Left = 207
    Top = 546
    Width = 113
    Height = 25
    Caption = 'Par'
    TabOrder = 6
  end
  object Button5: TButton
    Left = 420
    Top = 183
    Width = 90
    Height = 25
    Caption = 'To FireDAC'
    TabOrder = 7
    OnClick = Button5Click
  end
  object FileListBox100: TFileListBox
    Left = 8
    Top = 35
    Width = 312
    Height = 505
    ItemHeight = 13
    Mask = '*.pas'
    TabOrder = 8
    OnClick = FileListBox1Click
  end
  object FDQuery1: TFDQuery
    Left = 608
    Top = 200
  end
end
