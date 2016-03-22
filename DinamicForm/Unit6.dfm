object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 424
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      810000009619E0BD0100000018000000040000000000030000008100044E6F6D
      65010049000000010005574944544802000200320008456E64657265636F0100
      4900000001000557494454480200020032000653746174757301004900000001
      00055749445448020002000A000D446174614E6163696D656E746F0400060000
      0000000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 432
    Top = 16
    object ClientDataSet1Nome: TStringField
      FieldName = 'Nome'
      Size = 50
    end
    object ClientDataSet1Endereco: TStringField
      FieldName = 'Endereco'
      Size = 50
    end
    object ClientDataSet1Status: TStringField
      FieldName = 'Status'
      Size = 10
    end
    object ClientDataSet1DataNacimento: TDateField
      FieldName = 'DataNacimento'
    end
  end
end
