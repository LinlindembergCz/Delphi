object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 235
  ClientWidth = 485
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
    Top = 32
    Width = 29
    Height = 13
    Caption = 'NOme'
  end
  object Label2: TLabel
    Left = 8
    Top = 75
    Width = 28
    Height = 13
    Caption = 'Idade'
  end
  object Label3: TLabel
    Left = 8
    Top = 123
    Width = 24
    Height = 13
    Caption = 'email'
  end
  object Edit1: TEdit
    Left = 96
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 96
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object Edit3: TEdit
    Left = 96
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object BindNavigator1: TBindNavigator
    Left = 24
    Top = 176
    Width = 240
    Height = 25
    DataSource = AdapterBindSource1
    Orientation = orHorizontal
    TabOrder = 3
  end
  object Button1: TButton
    Left = 312
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button1Click
  end
  object DataGeneratorAdapter1: TDataGeneratorAdapter
    FieldDefs = <
      item
        Name = 'Nome'
        ReadOnly = False
      end
      item
        Name = 'Idade'
        FieldType = ftInteger
        ReadOnly = False
      end
      item
        Name = 'Email'
        ReadOnly = False
      end>
    Active = True
    AutoPost = True
    Options = [loptAllowInsert, loptAllowDelete, loptAllowModify]
    Left = 328
    Top = 32
  end
  object AdapterBindSource1: TAdapterBindSource
    AutoActivate = True
    OnCreateAdapter = AdapterBindSource1CreateAdapter
    Adapter = DataGeneratorAdapter1
    ScopeMappings = <>
    Left = 328
    Top = 88
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 5
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = AdapterBindSource1
      FieldName = 'Nome'
      Control = Edit1
      Track = True
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = AdapterBindSource1
      FieldName = 'Idade'
      Control = Edit2
      Track = True
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = AdapterBindSource1
      FieldName = 'Email'
      Control = Edit3
      Track = True
    end
  end
end
