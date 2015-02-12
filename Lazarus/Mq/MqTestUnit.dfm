object Form1: TForm1
  Left = 284
  Top = 202
  Width = 453
  Height = 265
  Caption = 'MQ Interface Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 156
    Top = 22
    Width = 45
    Height = 13
    Caption = '&Manager:'
    FocusControl = MQManagerEdit
  end
  object MQPutQueueLable: TLabel
    Left = 146
    Top = 47
    Width = 54
    Height = 13
    Caption = '&Put Queue:'
    FocusControl = MQPutEdit
  end
  object MQReplyToLabel: TLabel
    Left = 122
    Top = 73
    Width = 78
    Height = 13
    Caption = '&ReplyTo Queue:'
    FocusControl = MQReplyToEdit
  end
  object MQGetLabel: TLabel
    Left = 145
    Top = 98
    Width = 55
    Height = 13
    Caption = '&Get Queue:'
    FocusControl = MQGetEdit
  end
  object MqTimeOutLabel: TLabel
    Left = 156
    Top = 123
    Width = 43
    Height = 13
    Caption = '&TimeOut:'
    FocusControl = MQManagerEdit
  end
  object MqTimeOutUnitsLabel: TLabel
    Left = 252
    Top = 123
    Width = 48
    Height = 13
    Caption = '(Seconds)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -5
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 158
    Top = 149
    Width = 43
    Height = 13
    Caption = 'Request:'
    FocusControl = RequestEdit
  end
  object Label3: TLabel
    Left = 152
    Top = 176
    Width = 49
    Height = 13
    Caption = 'Received:'
    FocusControl = ReplyEdit
  end
  object FireAndForgetButton: TButton
    Left = 234
    Top = 207
    Width = 89
    Height = 25
    Caption = 'Fire And Forget'
    TabOrder = 8
    OnClick = FireAndForgetButtonClick
  end
  object MqUseRadioGroup: TRadioGroup
    Left = 8
    Top = 16
    Width = 81
    Height = 73
    Caption = 'Client/Server'
    ItemIndex = 1
    Items.Strings = (
      'Server'
      'Client')
    TabOrder = 10
    OnClick = MqUseRadioGroupClick
  end
  object RequestReplyButton: TButton
    Left = 138
    Top = 207
    Width = 89
    Height = 25
    Caption = 'Request / Reply'
    TabOrder = 7
    OnClick = RequestReplyButtonClick
  end
  object GetOnlyButton: TButton
    Left = 330
    Top = 207
    Width = 89
    Height = 25
    Caption = 'Get Only'
    TabOrder = 9
    OnClick = GetOnlyButtonClick
  end
  object MQManagerEdit: TEdit
    Left = 202
    Top = 18
    Width = 216
    Height = 21
    TabOrder = 0
    Text = 'QM_test'
  end
  object MQPutEdit: TEdit
    Left = 202
    Top = 43
    Width = 216
    Height = 21
    TabOrder = 1
    Text = 'Q_in'
  end
  object MQReplyToEdit: TEdit
    Left = 202
    Top = 69
    Width = 216
    Height = 21
    TabOrder = 2
    Text = 'Q_reply'
  end
  object MQGetEdit: TEdit
    Left = 202
    Top = 94
    Width = 216
    Height = 21
    TabOrder = 3
    Text = 'Q_reply'
  end
  object MQTimeOutEdit: TEdit
    Left = 202
    Top = 119
    Width = 45
    Height = 21
    Hint = 'TimeOut in seconds'
    TabOrder = 4
    Text = '60'
  end
  object RequestEdit: TEdit
    Left = 202
    Top = 145
    Width = 216
    Height = 21
    TabOrder = 5
    Text = 'This is a request'
  end
  object ReplyEdit: TEdit
    Left = 202
    Top = 172
    Width = 216
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
  object MqInterface: TMqInterface
    Qmanager = 'MQ_Test'
    GetQueue = 'MQ_Get'
    PutQueue = 'MQ_Put'
    ReplyToQueue = 'MQ_Get'
    TimeOut = '60'
    Use = mquClient
    Left = 24
    Top = 112
  end
end
