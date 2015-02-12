object ShowRtfForm: TShowRtfForm
  Left = 271
  Top = 162
  Width = 544
  Height = 352
  BorderStyle = bsSizeToolWin
  Caption = 'Information Window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 287
    Align = alClient
    BorderWidth = 3
    TabOrder = 0
    object Memo: TRichEdit
      Left = 4
      Top = 4
      Width = 528
      Height = 279
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      OnKeyDown = MemoKeyDown
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 287
    Width = 536
    Height = 41
    Align = alBottom
    TabOrder = 1
    object OKButton: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object WrapTextCheckBox: TCheckBox
      Left = 127
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Wrap text'
      TabOrder = 1
      OnClick = WrapTextCheckBoxClick
    end
  end
end
