object FormConfiguration: TFormConfiguration
  Left = 1153
  Top = 257
  BorderStyle = bsToolWindow
  Caption = 'Configuration'
  ClientHeight = 514
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBas: TPanel
    Left = 0
    Top = 473
    Width = 855
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BitBtnOk: TBitBtn
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = SaveConfig
      Kind = bkOK
    end
    object BitBtnCancel: TBitBtn
      Left = 296
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = CancelConfig
      Kind = bkCancel
    end
    object BitBtn1: TBitBtn
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 2
      OnClick = BitBtn1Click
    end
  end
  object PageControlConfiguration: TPageControl
    Left = 0
    Top = 0
    Width = 855
    Height = 473
    ActivePage = TabSheetCsv
    Align = alClient
    TabOrder = 1
    OnChange = PageControlConfigurationChange
    object TabSheetImportCsv: TTabSheet
      Caption = 'Survellance et Importation'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 0
        Top = 0
        Height = 445
      end
      object PanelParamImport: TPanel
        Left = 10
        Top = 131
        Width = 400
        Height = 145
        TabOrder = 0
        object LabelParamImports: TLabel
          Left = 1
          Top = 1
          Width = 255
          Height = 24
          Align = alTop
          Alignment = taCenter
          Caption = 'Param'#233'trages d'#39'importation'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelTampon: TLabel
          Left = 10
          Top = 40
          Width = 60
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '&Tampon'
        end
        object SpinEditTampon: TSpinEdit
          Left = 80
          Top = 35
          Width = 81
          Height = 22
          Hint = 'csvBuffer'
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 30000
          OnExit = EditExit
        end
        object CheckBoxLaurhAtStartup: TCheckBox
          Left = 32
          Top = 64
          Width = 201
          Height = 17
          Hint = 'launchWindowsStartup'
          Caption = 'Lancer au demmarage de windows'
          TabOrder = 1
          OnExit = EditExit
        end
        object CheckBoxlaunchMonitoringStartup: TCheckBox
          Left = 32
          Top = 87
          Width = 273
          Height = 17
          Hint = 'launchMonitoringStartup'
          Caption = 'Lancer la surveillance au d'#233'marrage de l'#39'application'
          TabOrder = 2
          OnExit = EditExit
        end
        object CheckBoxLaunchImportAtStartup: TCheckBox
          Left = 32
          Top = 111
          Width = 273
          Height = 17
          Hint = 'launchImportAtStartup'
          Caption = 'Lancer l'#39'importation au d'#233'marrage de l'#39'application'
          TabOrder = 3
          OnExit = EditExit
        end
      end
      object PageControl1: TPageControl
        Left = 10
        Top = 8
        Width = 400
        Height = 113
        Hint = 'ArchiveFolder'
        ActivePage = TabSheet3
        TabOrder = 1
        object TabSheet1: TTabSheet
          Caption = 'Source Csv'
          object PanelCsv: TPanel
            Left = 0
            Top = 0
            Width = 392
            Height = 85
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object SpeedButton1: TSpeedButton
              Left = 365
              Top = 23
              Width = 23
              Height = 24
              Hint = 'CsvFolder'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              Glyph.Data = {
                E6040000424DE604000000000000360000002800000014000000140000000100
                180000000000B0040000232E0000232E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6F6F6ECECECECEC
                ECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECEC
                ECECECECECEFEFEFFCFCFCFFFFFFFFFFFFFFFFFFB2B6BA646C73666D74666D74
                666D74666D74666D74666D74666D74666D74666D74666D74666D74666D74666D
                74787B7EE0E0E0FFFFFFFFFFFFFFFFFF00ADE51DC9F71EC8F71EC9F71DC9F81D
                C8F71AC8F718C8F712C7F70FC7F708C6F703C5F700C5F700C4F700C5F8437FA1
                D8D8D8FFFFFFFFFFFFFFFFFF00B6EA30D5FE32D5FE32D5FE31D4FE30D5FF2DD4
                FE2AD3FF26D3FE22D2FE1DD1FE19D0FE14CEFF0FCEFE0BCFFF467E9FD8D8D8FF
                FFFFFFFFFFFFFFFF00B8EA38D7FF3AD6FF3BD7FE3AD7FF37D7FF35D6FE31D5FF
                2DD4FE28D4FE23D2FF1ED1FF18D0FE13CFFE0ED0FF467E9FD8D8D8FFFFFFFFFF
                FFFFFFFF00BCE948D9FE42D8FE43D8FF42D9FE40D8FF3CD7FE38D6FF34D6FE2E
                D4FE28D4FE22D2FE1CD1FE17D0FF11D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF
                2FBFEA73E2FF4FDBFE4ADBFE4ADAFE48D9FF43D9FF3FD7FE39D7FE34D5FE2ED5
                FF28D3FF20D2FE1AD0FE15D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF47C1E988
                E7FF7CE4FF57DCFE51DCFF4EDBFF4BDAFF46D9FF3FD8FF39D7FE33D5FF2CD4FF
                25D2FE1DD2FF17D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF58C4EA99EAFF98EA
                FE8CE7FF64DFFE54DCFF50DBFF4BDAFF45D9FE3ED7FF36D6FE2ED4FF28D3FF20
                D2FE19D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF68C6EAA9EEFFA8EDFFA3ECFF
                9CEBFF7EE4FF59DCFE4EDBFE49DAFE41D9FE39D6FF31D6FF2AD3FF22D3FE1CD2
                FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF65C3E8BCF2FFB5F0FFB1EFFFA9EDFFA0
                ECFF92E9FF7FE4FF61DFFF44D9FF32D6FE2FD5FE2AD4FF23D2FF1DD2FF467E9F
                D8D8D8FFFFFFFFFFFFFFFFFF51B6DE7BD5F2C1F3FFBEF2FFB4EFFFACEEFFA6ED
                FFA5EDFEA4ECFFA3ECFE88E7FE6BE0FE4FDAFE37D6FF28D4FF467D9FD8D8D8FF
                FFFFFFFFFFFFFFFFBED9ED41CBF851BEF081D0EEBAF2FFB8F3FFBBF4FFBCF5FF
                BEF5FFC1F6FFC3F6FFC0F5FFB2F3FF9BEFFF7DEAFF4C81A4DEDEDEFFFFFFFFFF
                FFFFFFFFBAD8ED81E5FF65DAFF40C6FF00ADF300A9EB00A5E100A3E000A1E000
                A0E0009FE000A0E000A1E000A3E0009CD697AFC1FBFBFBFFFFFFFFFFFFFFFFFF
                B8D7EDACF1FF9FEBFF91E9FF7CE3FF4AC5F000A6D400AEDB00AEDB00AFDC00B0
                DC00B0DC00B0DC00B1DD00AEDAEEEBE9FFFFFFFFFFFFFFFFFFFFFFFFDCEDF71D
                C1E874CCED65CAED4FC7ED7FBFD6F3F0EDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF}
              ParentFont = False
              Transparent = False
              OnClick = SpeedButton1Click
            end
            object LabelCsv: TLabel
              Left = 0
              Top = 0
              Width = 266
              Height = 24
              Align = alTop
              Alignment = taCenter
              Caption = 'Repertoire des sources CSV'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object EditFolderCsv: TEdit
              Left = 5
              Top = 27
              Width = 360
              Height = 21
              Hint = 'CsvFolder'
              TabOrder = 0
              OnExit = EditExit
            end
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Destination Mis'
          ImageIndex = 1
          object PanelMis: TPanel
            Left = 0
            Top = 0
            Width = 392
            Height = 85
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object Label1: TLabel
              Left = 0
              Top = 0
              Width = 324
              Height = 24
              Align = alTop
              Alignment = taCenter
              Caption = 'Repertoire des fichiers Hydras MIS'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object SpeedButton2: TSpeedButton
              Left = 365
              Top = 23
              Width = 23
              Height = 24
              Hint = 'MisFolder'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              Glyph.Data = {
                E6040000424DE604000000000000360000002800000014000000140000000100
                180000000000B0040000232E0000232E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6F6F6ECECECECEC
                ECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECEC
                ECECECECECEFEFEFFCFCFCFFFFFFFFFFFFFFFFFFB2B6BA646C73666D74666D74
                666D74666D74666D74666D74666D74666D74666D74666D74666D74666D74666D
                74787B7EE0E0E0FFFFFFFFFFFFFFFFFF00ADE51DC9F71EC8F71EC9F71DC9F81D
                C8F71AC8F718C8F712C7F70FC7F708C6F703C5F700C5F700C4F700C5F8437FA1
                D8D8D8FFFFFFFFFFFFFFFFFF00B6EA30D5FE32D5FE32D5FE31D4FE30D5FF2DD4
                FE2AD3FF26D3FE22D2FE1DD1FE19D0FE14CEFF0FCEFE0BCFFF467E9FD8D8D8FF
                FFFFFFFFFFFFFFFF00B8EA38D7FF3AD6FF3BD7FE3AD7FF37D7FF35D6FE31D5FF
                2DD4FE28D4FE23D2FF1ED1FF18D0FE13CFFE0ED0FF467E9FD8D8D8FFFFFFFFFF
                FFFFFFFF00BCE948D9FE42D8FE43D8FF42D9FE40D8FF3CD7FE38D6FF34D6FE2E
                D4FE28D4FE22D2FE1CD1FE17D0FF11D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF
                2FBFEA73E2FF4FDBFE4ADBFE4ADAFE48D9FF43D9FF3FD7FE39D7FE34D5FE2ED5
                FF28D3FF20D2FE1AD0FE15D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF47C1E988
                E7FF7CE4FF57DCFE51DCFF4EDBFF4BDAFF46D9FF3FD8FF39D7FE33D5FF2CD4FF
                25D2FE1DD2FF17D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF58C4EA99EAFF98EA
                FE8CE7FF64DFFE54DCFF50DBFF4BDAFF45D9FE3ED7FF36D6FE2ED4FF28D3FF20
                D2FE19D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF68C6EAA9EEFFA8EDFFA3ECFF
                9CEBFF7EE4FF59DCFE4EDBFE49DAFE41D9FE39D6FF31D6FF2AD3FF22D3FE1CD2
                FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF65C3E8BCF2FFB5F0FFB1EFFFA9EDFFA0
                ECFF92E9FF7FE4FF61DFFF44D9FF32D6FE2FD5FE2AD4FF23D2FF1DD2FF467E9F
                D8D8D8FFFFFFFFFFFFFFFFFF51B6DE7BD5F2C1F3FFBEF2FFB4EFFFACEEFFA6ED
                FFA5EDFEA4ECFFA3ECFE88E7FE6BE0FE4FDAFE37D6FF28D4FF467D9FD8D8D8FF
                FFFFFFFFFFFFFFFFBED9ED41CBF851BEF081D0EEBAF2FFB8F3FFBBF4FFBCF5FF
                BEF5FFC1F6FFC3F6FFC0F5FFB2F3FF9BEFFF7DEAFF4C81A4DEDEDEFFFFFFFFFF
                FFFFFFFFBAD8ED81E5FF65DAFF40C6FF00ADF300A9EB00A5E100A3E000A1E000
                A0E0009FE000A0E000A1E000A3E0009CD697AFC1FBFBFBFFFFFFFFFFFFFFFFFF
                B8D7EDACF1FF9FEBFF91E9FF7CE3FF4AC5F000A6D400AEDB00AEDB00AFDC00B0
                DC00B0DC00B0DC00B1DD00AEDAEEEBE9FFFFFFFFFFFFFFFFFFFFFFFFDCEDF71D
                C1E874CCED65CAED4FC7ED7FBFD6F3F0EDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF}
              ParentFont = False
              Transparent = False
              OnClick = SpeedButton2Click
            end
            object EditFolderMis: TEdit
              Left = 5
              Top = 27
              Width = 360
              Height = 21
              Hint = 'MisFolder'
              TabOrder = 0
              OnExit = EditExit
            end
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Archivage'
          ImageIndex = 2
          object PanelArchive: TPanel
            Left = 0
            Top = 0
            Width = 392
            Height = 85
            Align = alClient
            TabOrder = 0
            object SpeedButton3: TSpeedButton
              Left = 365
              Top = 23
              Width = 23
              Height = 24
              Hint = 'ArchiveFolder'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              Glyph.Data = {
                E6040000424DE604000000000000360000002800000014000000140000000100
                180000000000B0040000232E0000232E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6F6F6ECECECECEC
                ECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECECEC
                ECECECECECEFEFEFFCFCFCFFFFFFFFFFFFFFFFFFB2B6BA646C73666D74666D74
                666D74666D74666D74666D74666D74666D74666D74666D74666D74666D74666D
                74787B7EE0E0E0FFFFFFFFFFFFFFFFFF00ADE51DC9F71EC8F71EC9F71DC9F81D
                C8F71AC8F718C8F712C7F70FC7F708C6F703C5F700C5F700C4F700C5F8437FA1
                D8D8D8FFFFFFFFFFFFFFFFFF00B6EA30D5FE32D5FE32D5FE31D4FE30D5FF2DD4
                FE2AD3FF26D3FE22D2FE1DD1FE19D0FE14CEFF0FCEFE0BCFFF467E9FD8D8D8FF
                FFFFFFFFFFFFFFFF00B8EA38D7FF3AD6FF3BD7FE3AD7FF37D7FF35D6FE31D5FF
                2DD4FE28D4FE23D2FF1ED1FF18D0FE13CFFE0ED0FF467E9FD8D8D8FFFFFFFFFF
                FFFFFFFF00BCE948D9FE42D8FE43D8FF42D9FE40D8FF3CD7FE38D6FF34D6FE2E
                D4FE28D4FE22D2FE1CD1FE17D0FF11D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF
                2FBFEA73E2FF4FDBFE4ADBFE4ADAFE48D9FF43D9FF3FD7FE39D7FE34D5FE2ED5
                FF28D3FF20D2FE1AD0FE15D1FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF47C1E988
                E7FF7CE4FF57DCFE51DCFF4EDBFF4BDAFF46D9FF3FD8FF39D7FE33D5FF2CD4FF
                25D2FE1DD2FF17D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF58C4EA99EAFF98EA
                FE8CE7FF64DFFE54DCFF50DBFF4BDAFF45D9FE3ED7FF36D6FE2ED4FF28D3FF20
                D2FE19D2FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF68C6EAA9EEFFA8EDFFA3ECFF
                9CEBFF7EE4FF59DCFE4EDBFE49DAFE41D9FE39D6FF31D6FF2AD3FF22D3FE1CD2
                FF467E9FD8D8D8FFFFFFFFFFFFFFFFFF65C3E8BCF2FFB5F0FFB1EFFFA9EDFFA0
                ECFF92E9FF7FE4FF61DFFF44D9FF32D6FE2FD5FE2AD4FF23D2FF1DD2FF467E9F
                D8D8D8FFFFFFFFFFFFFFFFFF51B6DE7BD5F2C1F3FFBEF2FFB4EFFFACEEFFA6ED
                FFA5EDFEA4ECFFA3ECFE88E7FE6BE0FE4FDAFE37D6FF28D4FF467D9FD8D8D8FF
                FFFFFFFFFFFFFFFFBED9ED41CBF851BEF081D0EEBAF2FFB8F3FFBBF4FFBCF5FF
                BEF5FFC1F6FFC3F6FFC0F5FFB2F3FF9BEFFF7DEAFF4C81A4DEDEDEFFFFFFFFFF
                FFFFFFFFBAD8ED81E5FF65DAFF40C6FF00ADF300A9EB00A5E100A3E000A1E000
                A0E0009FE000A0E000A1E000A3E0009CD697AFC1FBFBFBFFFFFFFFFFFFFFFFFF
                B8D7EDACF1FF9FEBFF91E9FF7CE3FF4AC5F000A6D400AEDB00AEDB00AFDC00B0
                DC00B0DC00B0DC00B1DD00AEDAEEEBE9FFFFFFFFFFFFFFFFFFFFFFFFDCEDF71D
                C1E874CCED65CAED4FC7ED7FBFD6F3F0EDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF}
              ParentFont = False
              Transparent = False
              OnClick = SpeedButton3Click
            end
            object Label2: TLabel
              Left = 1
              Top = 1
              Width = 326
              Height = 24
              Align = alTop
              Alignment = taCenter
              Caption = 'Repertoire d'#39'archivage des fichiers'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object EditFolderArchive: TEdit
              Left = 5
              Top = 27
              Width = 360
              Height = 21
              Hint = 'ArchiveFolder'
              TabOrder = 0
              OnExit = EditExit
            end
            object CheckBoxZipAfterImport: TCheckBox
              Left = 5
              Top = 50
              Width = 273
              Height = 20
              Hint = 'ZipAfterImport'
              Caption = 'Zipper les fichiers apr'#232's importation r'#233'ussie'
              TabOrder = 1
              OnExit = EditExit
            end
          end
        end
      end
    end
    object TabSheetIniFile: TTabSheet
      Caption = 'Fichier de configuration'
      ImageIndex = 1
      object ValueListEditorTemp: TValueListEditor
        Left = 0
        Top = 0
        Width = 847
        Height = 445
        Align = alClient
        TabOrder = 0
        ColWidths = (
          150
          691)
      end
    end
    object TabSheetCsv: TTabSheet
      Caption = 'TabSheetCsv'
      ImageIndex = 2
      object StringGridCsv: TStringGrid
        Left = 0
        Top = 0
        Width = 847
        Height = 445
        Align = alClient
        FixedCols = 0
        TabOrder = 0
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 4
    Top = 488
  end
end
