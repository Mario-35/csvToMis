object FormResultGrid: TFormResultGrid
  Left = 591
  Top = 146
  Width = 632
  Height = 675
  BorderStyle = bsSizeToolWin
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
    Top = 595
    Width = 616
    Height = 41
    Align = alBottom
    Locked = True
    TabOrder = 0
    object BitBtnClose: TBitBtn
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
  end
  object DBGridResult: TDBGrid
    Left = 0
    Top = 0
    Width = 616
    Height = 595
    Align = alClient
    DataSource = DataSource1
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDrawColumnCell = DBGridResultDrawColumnCell
  end
  object DataSource1: TDataSource
    DataSet = DataModuleDatas.ReadOnlySql
    Left = 568
    Top = 16
  end
end
