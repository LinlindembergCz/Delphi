object Form35: TForm35
  Left = 0
  Top = 0
  Caption = 'Form35'
  ClientHeight = 518
  ClientWidth = 841
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 561
    Height = 19
    Caption = 
      'Cole aqui a lista de Chave de Acesso para calculo do digito veri' +
      'fiador'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 33
    Width = 825
    Height = 184
    TabOrder = 0
  end
  object Button1: TButton
    Left = 384
    Top = 233
    Width = 34
    Height = 33
    Caption = '>>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 272
    Width = 825
    Height = 238
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 233
    Width = 97
    Height = 17
    Caption = 'Gerar update'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object Button2: TButton
    Left = 120
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Limpar'
    TabOrder = 4
    OnClick = Button2Click
  end
end
