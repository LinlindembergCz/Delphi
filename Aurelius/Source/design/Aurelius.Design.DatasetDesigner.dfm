object fmFieldLoader: TfmFieldLoader
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'TAureliusDataset Field Loader'
  ClientHeight = 248
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    480
    248)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 12
    Width = 106
    Height = 13
    Caption = 'Load fields from class:'
  end
  object btOk: TButton
    Left = 306
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 0
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 394
    Top = 215
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btCancelClick
  end
  object cbClasses: TComboBox
    Left = 140
    Top = 8
    Width = 241
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 35
    Width = 460
    Height = 174
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Additional Packages'
    TabOrder = 3
    DesignSize = (
      460
      174)
    object btAddPackage: TSpeedButton
      Left = 429
      Top = 18
      Width = 25
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '+'
      OnClick = btAddPackageClick
      ExplicitLeft = 425
    end
    object btRemovePackage: TSpeedButton
      Left = 429
      Top = 45
      Width = 25
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '-'
      OnClick = btRemovePackageClick
      ExplicitLeft = 425
    end
    object lbPackages: TCheckListBox
      Left = 9
      Top = 19
      Width = 417
      Height = 113
      OnClickCheck = lbPackagesClickCheck
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbPackagesClick
    end
    object edPath: TEdit
      Left = 9
      Top = 138
      Width = 417
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      ReadOnly = True
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.bpl'
    Filter = 'Package Library (*.bpl)|*.bpl'
    Title = 'Add Library'
    Left = 200
    Top = 40
  end
end
