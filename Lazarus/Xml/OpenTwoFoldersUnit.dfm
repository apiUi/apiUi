object OpenTwoFoldersForm: TOpenTwoFoldersForm
  Left = 0
  Top = 0
  Caption = 'reqExplorer - Start project'
  ClientHeight = 216
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object FolderName1Edit: TLabeledEdit
    Left = 32
    Top = 36
    Width = 537
    Height = 21
    TabStop = False
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Folder 1'
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
    OnChange = enableOK
  end
  object FolderName2Edit: TLabeledEdit
    Left = 32
    Top = 85
    Width = 537
    Height = 21
    TabStop = False
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Folder 2'
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
    OnChange = enableOK
  end
  object BrowseFolder1Button: TButton
    Left = 576
    Top = 33
    Width = 75
    Height = 25
    Caption = 'B&rowse'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseFolder1ButtonClick
  end
  object BrowseFolder2Button: TButton
    Left = 576
    Top = 83
    Width = 75
    Height = 25
    Caption = 'Bro&wse'
    TabOrder = 3
    TabStop = False
    OnClick = BrowseFolder2ButtonClick
  end
  object OKButton: TButton
    Left = 32
    Top = 131
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 120
    Top = 131
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
