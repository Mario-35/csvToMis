object FormSelectFiles: TFormSelectFiles
  Left = 910
  Top = 238
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'FormSelectFiles'
  ClientHeight = 458
  ClientWidth = 372
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
    Top = 417
    Width = 372
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtnOk: TBitBtn
      Left = 192
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = BitBtnOkClick
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
  object CheckListBoxLog: TCheckListBox
    Left = 0
    Top = 0
    Width = 372
    Height = 417
    OnClickCheck = CheckListBoxLogClickCheck
    Align = alClient
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 1
  end
end
