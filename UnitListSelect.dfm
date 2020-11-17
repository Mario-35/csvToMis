object FormListSelect: TFormListSelect
  Left = 845
  Top = 231
  ActiveControl = ListBoxItems
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'FormListSelect'
  ClientHeight = 447
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBas: TPanel
    Left = 0
    Top = 406
    Width = 356
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtnOk: TBitBtn
      Left = 192
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtnCancel: TBitBtn
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object ListBoxItems: TListBox
    Left = 0
    Top = 0
    Width = 356
    Height = 406
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 1
    OnDblClick = ListBoxItemsDblClick
  end
end
