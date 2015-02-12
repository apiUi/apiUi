object Form5: TForm5
  Left = 0
  Top = 0
  Width = 601
  Height = 462
  Caption = 'Delphi Stomp Client - CHAT DEMO - www.danieleteti.it'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    593
    435)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 77
    Top = 14
    Width = 28
    Height = 13
    Caption = 'PutQ:'
  end
  object Label2: TLabel
    Left = 205
    Top = 14
    Width = 29
    Height = 13
    Caption = 'GetQ:'
  end
  object Edit1: TEdit
    Left = 9
    Top = 10
    Width = 62
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object PutQEdit: TEdit
    Left = 107
    Top = 10
    Width = 62
    Height = 21
    TabOrder = 1
    Text = 'Q1'
  end
  object EnterButton: TButton
    Left = 404
    Top = 8
    Width = 66
    Height = 25
    Caption = 'Connect'
    TabOrder = 4
    OnClick = EnterButtonClick
  end
  object SenderEdit: TEdit
    Left = 304
    Top = 10
    Width = 94
    Height = 21
    TabOrder = 3
    Text = 'Jan_Bouwman'
  end
  object ReceivedMemo: TMemo
    Left = 9
    Top = 39
    Width = 576
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clMenuBar
    Font.Charset = ANSI_CHARSET
    Font.Color = clHotLight
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object SendMemo: TMemo
    Left = 8
    Top = 359
    Width = 471
    Height = 68
    Anchors = [akLeft, akRight, akBottom]
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnKeyUp = SendMemoKeyUp
  end
  object SendButton: TButton
    Left = 485
    Top = 359
    Width = 100
    Height = 68
    Anchors = [akRight, akBottom]
    Caption = 'Send'
    Enabled = False
    TabOrder = 7
    OnClick = SendButtonClick
  end
  object GetQEdit: TEdit
    Left = 235
    Top = 11
    Width = 49
    Height = 21
    TabOrder = 2
    Text = 'Q2'
  end
  object DisconnectButton: TButton
    Left = 479
    Top = 8
    Width = 66
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 8
    OnClick = DisconnectButtonClick
  end
  object ExchangeButton: TButton
    Left = 177
    Top = 13
    Width = 24
    Height = 17
    Caption = '<>'
    TabOrder = 9
    OnClick = ExchangeButtonClick
  end
  object ClearButton: TButton
    Left = 552
    Top = 8
    Width = 35
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = ClearButtonClick
  end
end
