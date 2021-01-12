// ********************************************
// *  Unité Main export                       *
// *  @Inrae 2020                             *
// *  by mario Adam mario.adam@inrae.fr       *
// ********************************************

unit UnitExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ValEdit, StrUtils, ZipForge,
  Buttons, ZAbstractConnection, ZConnection, Menus, ComCtrls, DBCtrls, UnitVariables,  ShellAPI,
  CheckLst, FolderMon, ImgList, ToolWin;

Const
  wm_GetTraitement = WM_USER + 100;
  WM_ICONTRAY =  WM_USER + 1;


type
  TExport = class(TForm)
    StatusBarMain: TStatusBar;
    Mnu: TPopupMenu;
    HideForm: TMenuItem;
    ShowForm: TMenuItem;
    Quitter: TMenuItem;
    TimerStart: TTimer;
    BitBtnStop: TBitBtn;
    ImageOnOff: TImageList;
    PopupMenuLogs: TPopupMenu;
    RichEditLog: TRichEdit;
    PanelUp: TPanel;
    ToolBarExport: TToolBar;
    ToolButtonHome: TToolButton;
    ToolButtonConfig: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonStartMaual: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    LabelMonitoring: TLabel;
    RichEditLogs: TRichEdit;
    ListBoxFilesQueue: TListBox;
    Actuel1: TMenuItem;

    procedure RefreshStatus(newState: TProcessType);
    procedure HideFormClick(Sender: TObject);
    procedure ShowFormClick(Sender: TObject);

    procedure Exit1MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure ShowFormDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure HideFormDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitterClick(Sender: TObject);
    procedure QuitterDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure BitBtnStopClick(Sender: TObject);

    procedure StartMonitoring;
    procedure ToolButtonConfigClick(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure ToolButtonStartMaualClick(Sender: TObject);

    procedure Lancer;
    function exportToMis(Filename: String): boolean;
    function ZipFiles(lst: TStrings) : Boolean;
    procedure TimerStartTimer(Sender: TObject);

  private
    TrayIconData: TNotifyIconData;
    ligne: string;
    procedure HandlePopupItem(Sender: TObject);

// ********************************************************************************************************************
// *                                                      Folder Monitoring                                           *
// ********************************************************************************************************************
    procedure HandleFolderChange(ASender: TFolderMon; AFolderItem: TFolderItemInfo);
    procedure HandleFolderMonActivated(ASender: TObject);
    procedure HandleFolderMonDeactivated(ASender: TObject);
    procedure DirectoryWatch1Change(Sender: TObject);

    procedure CleanQueueFiles;
  public
    procedure AddInQueueFiles(FileName : String);
    procedure WmICONTRAY(var Msg: TMessage); message WM_ICONTRAY;
    Procedure MinimizeClick(Sender:TObject);
    procedure DrawBar(ACanvas: TCanvas);
    procedure LogLine(Indent: Integer; AMessage: string);
    procedure UpdateQueueFiles;

  end;

var
  Export: TExport;

implementation

uses UnitConfiguration, Registry, IniFiles, UnitSplashScreen,
  UnitFormSelectFiles, UnitListSelect;

{$R *.dfm}



procedure TExport.FormCreate(Sender: TObject);
    var
      i : integer;
      Item: TMenuItem;
      LogFolder: string;

    Begin
       // DEBUG := True;

        ToolBarExport.Top := 0;
        ToolBarExport.Left := 0;
        ToolBarExport.Height := 35;
        ToolBarExport.Width := 230;
        LabelMonitoring.Left := ToolBarExport.Width;
        LabelMonitoring.Top := 0;
        LabelMonitoring.Width := Export.Width - ToolBarExport.Width;
        LabelMonitoring.Height := ToolBarExport.Height;
        Mnu.OwnerDraw:=True;
        With TrayIconData
          Do Begin
            cbSize := SizeOf(TrayIconData);
            Wnd := Handle;
            uID := 0;
            uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
            uCallbackMessage := WM_ICONTRAY;
            hIcon := Application.Icon.Handle;
            StrPCopy(szTip, Application.Title);
          End;
        Shell_NotifyIcon(NIM_ADD, @TrayIconData);
        Application.OnMinimize:= MinimizeClick;

        StatusBarMain.Panels[0].Text := 'ver : ' + APP_VERSION;

        LogFolder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\';
        If not directoryExists(LogFolder) Then CreateDir(LogFolder);
        FILE_LOGFILE := IncludeTrailingPathDelimiter(LogFolder + '[' + Application.Title  + '] ' + formatdatetime('dd-mm-yyyy', now) +  '.log';
        LogLine(0, 'Demmarrage version : ' + APP_VERSION + _CR);
        If DEBUG
          Then LogLine(1, 'mode debug');

        RichEditLog.Align := alClient;
        RichEditLogs.Align := alClient;

        BitBtnStop.Left := Round(Export.Width / 2) - Round(BitBtnStop.Width / 2);
        BitBtnStop.top := Export.Height - Round(Export.Height / 4);

        FindFilePattern(TEMP_LIST, IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)), '.log', True);
        For i := 0 To TEMP_LIST.Count - 1
          Do Begin
            Item := TMenuItem.Create(PopupMenuLogs);
            Item.Caption := extractFileName(TEMP_LIST[i]);
            Item.OnClick := HandlePopupItem;
            PopupMenuLogs.Items.Add(Item);
          End;
        Self.PopupMenu := PopupMenuLogs;

    End;

procedure TExport.QuitterClick(Sender: TObject);
    Begin
        Application.Terminate;
    End;

procedure TExport.FormDestroy(Sender: TObject);
  Begin
    LogLine(0, 'Sortie de programme');
    Shell_NotifyIcon(NIM_DELETE, @TrayIconData);
  End;

procedure TExport.LogLine(Indent: Integer; AMessage: string);
    Var
        F: TextFile;

    Begin
        AssignFile(F, FILE_LOGFILE);
        If FileExists(FILE_LOGFILE)
          Then Append(F)
          Else Rewrite(F);
        Try
            ligne := Format('[%s] %s %s', [TimeToStr(Now), StringOfChar(_TAB, abs(indent)), AMessage]);
            WriteLn(F, ligne);
        finally
            CloseFile(F);
            Application.ProcessMessages;
        end;

        With RichEditLog
          Do Begin
            SelAttributes.Color := clBlue;
            // RichEdit1.SelAttributes.Style := [fsBold];
            SelText := '['+ TimeToStr(Now) + ']' + StringOfChar(_TAB, abs(indent));
            If indent = 0
              Then SelAttributes.Color := clBlack
              Else If indent > 0
                Then SelAttributes.Color := clGreen
                Else SelAttributes.Color := clRed;
            SelText := #32 + AMessage + #13;

            //SetFocus;
            SelStart := GetTextLen;
            Perform(EM_SCROLLCARET, 0, 0);
        End;
    end;


// ********************************************************************************************************************
// *                                                         Systray                                                  *
// ********************************************************************************************************************

procedure TExport.DrawBar(ACanvas: TCanvas);
  // Dessine dans la notification
  var
    lf : TLogFont;
    tf : TFont;
    Begin
        With ACanvas do
        Begin
            Brush.Color := clGray;
            FillRect(Rect(0,0,20,92));
            Font.Name := 'Tahoma';
            Font.Size := 7;
            Font.Style := Font.Style - [fsBold];
            Font.Color := clWhite;
            tf := TFont.Create;
            Try
                tf.Assign(Font);
                GetObject(tf.Handle, sizeof(lf), @lf);
                lf.lfEscapement := 900;
                lf.lfHeight := Font.Height - 2;
                tf.Handle := CreateFontIndirect(lf);
                Font.Assign(tf);
            Finally
                tf.Free;
            End;
                TextOut(2, 58, 'HydrasExport');
        End;
    End;



procedure TExport.Exit1MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  begin
    Width := 190;
  end;

Procedure TExport.MinimizeClick(Sender:TObject);
begin
  Self.Hide;
end;

procedure TExport.HideFormClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TExport.ShowFormClick(Sender: TObject);
begin
  Self.Show;
end;

procedure TExport.WmICONTRAY(var Msg: TMessage);
    Var
     p : TPoint;

    Begin
        Case Msg.lParam Of
            WM_LBUTTONDOWN:
                Begin
                    self.Show;
                end;
            WM_RBUTTONDOWN:
                begin
                    SetForegroundWindow(Handle);
                    GetCursorPos(p);
                    Mnu.Popup(p.x, p.y);
                    PostMessage(Handle, WM_NULL, 0, 0);
                end;
        end;
    end;

procedure TExport.ShowFormDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    Begin
        If Selected Then
            ACanvas.Brush.Color := clHighlight
        Else
            ACanvas.Brush.Color := clMenu;

        ARect.Left := 25;
        ACanvas.FillRect(ARect);
        DrawText(ACanvas.Handle, PChar('Show Form'), -1, ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
    End;

procedure TExport.HideFormDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    Begin
        If Selected Then
            ACanvas.Brush.Color := clHighlight
        Else
            ACanvas.Brush.Color := clMenu;

        ARect.Left := 25;
        ACanvas.FillRect(ARect);

        DrawText(ACanvas.Handle, PChar('Hide Form'), -1, ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
    End;

procedure TExport.QuitterDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    Begin
        If Selected Then
            ACanvas.Brush.Color := clHighlight
        Else
            ACanvas.Brush.Color := clMenu;

        ARect.Left := 25;
        ACanvas.FillRect(ARect);
        DrawText(ACanvas.Handle, PChar('Exit'), -1, ARect, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);
        DrawBar(ACanvas);
    End;


  procedure TExport.RefreshStatus(newState: TProcessType);
    Begin
      PROCESS := newState;
      BitBtnStop.Visible := False;
      Case PROCESS of
        tpInit : Begin
          StatusBarMain.Panels[1].Text := 'Initialisation';
          BitBtnStop.Visible := True;
          BitBtnStop.SetFocus;
        End;
        tpStart : Begin
          StatusBarMain.Panels[1].Text := 'Démarrage';
          BitBtnStop.Visible := True;
          BitBtnStop.SetFocus;
        End;
        tpRun : Begin
          StatusBarMain.Panels[1].Text := 'Execution ...';
          BitBtnStop.Visible := PROCESS = tpRun;
          If BitBtnStop.Visible Then BitBtnStop.SetFocus;
        End;
        tpWait : StatusBarMain.Panels[1].Text := 'Traitement en Attente';
        tpPause : StatusBarMain.Panels[1].Text := 'Traitement en pause';
        tpStop : Begin
          StatusBarMain.Panels[1].Text := 'Traitement stoppé';
        End;
        tpError : StatusBarMain.Panels[1].Text := 'Erreur';
        tpDone :  StatusBarMain.Panels[1].Text := 'Terminé';
        tpZip :  StatusBarMain.Panels[1].Text := 'Zip fichier(s)';
        tpMonitor : Begin
          if FOLDER_MONITORING.Folder = ''
            Then StartMonitoring
            Else FOLDER_MONITORING.Activate;
          StatusBarMain.Panels[1].Text := 'Ecoute en cours';
        End;
    End;

    Application.ProcessMessages;
  End;

procedure TExport.BitBtnStopClick(Sender: TObject);
begin
    If messagedlg('Annuler le traitement ',mtConfirmation , mbOKCancel, 0) = mrOk
      Then Begin
        LogLine(0, 'Arret de traitement');
        RefreshStatus(tpStop);
      End Else LogLine(0, 'Annulation do l''arret de traitement');
end;
// ********************************************************************************************************************
// *                                                      Folder Monitoring                                           *
// ********************************************************************************************************************

procedure TExport.UpdateQueueFiles;
  Begin
      ListBoxFilesQueue.Visible := (ListBoxFilesQueue.Items.Count > 0);
      ListBoxFilesQueue.Height :=   ListBoxFilesQueue.Items.Count * 13;
      If ListBoxFilesQueue.Height > (RichEditLog.Height div 2 ) Then ListBoxFilesQueue.Height := (RichEditLog.Height div 2 );
      ListBoxFilesQueue.Top := (StatusBarMain.top - ListBoxFilesQueue.Height) - 2;
      ListBoxFilesQueue.left := (RichEditLog.Width - ListBoxFilesQueue.Width) - 2;
  End;

procedure TExport.AddInQueueFiles(FileName : String);
  var
    tmp : string;
     i : integer;

  Begin
      If (ExtractFileExt(FileName) = '.csv')
      Then Begin
        If AnsiPos(FileName, ListBoxFilesQueue.Items.Text) = 0
          Then Begin
            ListBoxFilesQueue.Items.Add(FileName);
            LogLine(1 , 'Ajout du fichier dans la queue : ' + tmp);
          End;
      End;
      CleanQueueFiles;
      UpdateQueueFiles;
  End;


procedure TExport.CleanQueueFiles;
  var
    tmp : string;
     i : integer;

  Begin
    For i := 0 To ListBoxFilesQueue.Items.count - 1
      Do If Not FileExists(ListBoxFilesQueue.Items[i])
        Then ListBoxFilesQueue.Items[i] := '';
    Supprime_Ligne_Blanche(ListBoxFilesQueue.Items);
  End;

procedure TExport.HandleFolderChange(ASender: TFolderMon; AFolderItem: TFolderItemInfo);
  var
    tmp : string;
     i : integer;

  Begin
      tmp := IncludeTrailingPathDelimiter(ASender.Folder) + AFolderItem.Name;
      LogLine(0,FOLDER_ACTION_NAMES[AFolderItem.Action] + ' Fichier : ' + tmp);
      If FOLDER_MONITORING.IsActive
        Then Begin
          TimerStart.Enabled := False;
          AddInQueueFiles(tmp);
          TimerStart.Enabled := (ListBoxFilesQueue.Items.count > 0) AND (FOLDER_MONITORING.IsActive) AND (PROCESS in [tpNone, tpWait, tpDone, tpMonitor]);
        End;
  End;

procedure TExport.HandleFolderMonActivated(ASender: TObject);
  Begin
    LogLine(1, format(_MONITORING_DIR_START, [FOLDER_MONITORING.Folder]));
    LabelMonitoring.Caption := format(_MONITORING_DIR_START, [FOLDER_MONITORING.Folder]);
  End;

procedure TExport.HandleFolderMonDeactivated(ASender: TObject);
  Begin
    LogLine(1, format(_MONITORING_DIR_STOP, [FOLDER_MONITORING.Folder]));
     LabelMonitoring.Caption := format(_MONITORING_DIR_STOP, [FOLDER_MONITORING.Folder]);
     TimerStart.Enabled := False;
  End;

procedure TExport.DirectoryWatch1Change(Sender: TObject);
  Begin
    LogLine(0, 'Fichier ' + Sender.ClassName);
  End;

procedure TExport.StartMonitoring;
  var
    vMonitoredChanges: TChangeTypes;

  Begin
    If FOLDER_MONITORING.IsActive Then
      FOLDER_MONITORING.Deactivate;

    FOLDER_MONITORING.OnActivated := HandleFolderMonActivated;
    FOLDER_MONITORING.OnDeactivated := HandleFolderMonDeactivated;
    FOLDER_MONITORING.OnFolderChange := HandleFolderChange;

      FOLDER_MONITORING.Folder := CONFIG.Values['CsvFolder'];
      vMonitoredChanges := [];
      Include(vMonitoredChanges, ctFileName);
      Include(vMonitoredChanges, ctSize);
      Include(vMonitoredChanges, ctLastWriteTime);
      Include(vMonitoredChanges, ctCreationTime);
      Include(vMonitoredChanges, ctSecurityAttr);

      FOLDER_MONITORING.MonitoredChanges := vMonitoredChanges;
      FOLDER_MONITORING.MonitorSubFolders := TestBool(CONFIG.Values['MonitorSubfolders']);
      FOLDER_MONITORING.Activate;
  End;

procedure TExport.ToolButtonConfigClick(Sender: TObject);
begin
  FormConfiguration.ShowModal;
end;

procedure TExport.ToolButtonExitClick(Sender: TObject);
begin
  Close;
end;

procedure TExport.ToolButtonAboutClick(Sender: TObject);
begin
  Application.CreateForm(TSplashScreen, SplashScreen);
  SplashScreen.ButtonOk.Visible := True;
  SplashScreen.Showmodal;
  SplashScreen.Free;
end;

procedure TExport.HandlePopupItem(Sender: TObject);
  Var
    S : string;

  begin
    S := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs') + TMenuItem(Sender).Caption;
      If FileExists(S)
          Then Begin
            RichEditLogs.Lines.LoadFromFile(S);
            ACTUAL_LOG := False;
          End Else ACTUAL_LOG := True;
    RichEditLogs.Visible := Not ACTUAL_LOG;
    RichEditLog.Visible := ACTUAL_LOG;
end;

procedure TExport.ToolButtonStartMaualClick(Sender: TObject);
  var
      i : Integer;
      ZipListFiles : TStringList;

  Begin
      If CONFIG.Values['CsvFolder'] = '' Then exit;
        Try
          Application.CreateForm(TFormSelectFiles, FormSelectFiles);
          FormSelectFiles.Caption := _FILES_SELECT;
          FindFilePattern(FormSelectFiles.CheckListBoxLog.Items, CONFIG.Values['CsvFolder'], '.CSV', True);
          If (FormSelectFiles.CheckListBoxLog.Items.Count > 0) Then
          Begin
            Try
              ZipListFiles := TStringList.Create;
;
              For i := 0 To FormSelectFiles.CheckListBoxLog.Items .Count - 1
                Do FormSelectFiles.CheckListBoxLog.Checked[i] := true;
              PROCESS := tpWait;
              If FormSelectFiles.ShowModal = mrOk
                Then Begin
                  For i := 0 To FormSelectFiles.CheckListBoxLog.Items .Count - 1
                    Do If FormSelectFiles.CheckListBoxLog.Checked[i]
                      Then If exportToMis(FormSelectFiles.CheckListBoxLog.Items[i])
                        Then ZipListFiles.Add(FormSelectFiles.CheckListBoxLog.Items[i]);
                End;
              If directoryExists(CONFIG.Values['ArchiveFolder'])
                Then ZipFiles(ZipListFiles);
            Finally

              ZipListFiles.Free;
              RefreshStatus(tpDone);
            End;
          End Else messagedlg(_FILES_NOT_EXIST, mtInformation , [mbOK] , 0);
        Finally
          FormSelectFiles.Free;
        End;
  End;

  procedure TExport.Lancer;
    Var
       ZipListFiles : TStringList;

    Begin
      ZipListFiles := TStringList.Create;
      Try
        While ListBoxFilesQueue.Items.Count > 0
          Do Begin
            If PROCESS <> tpStop
              Then If exportToMis(export.ListBoxFilesQueue.Items[0])
                Then Begin
                  ZipListFiles.Add(export.ListBoxFilesQueue.Items[0]);
                  export.ListBoxFilesQueue.Items.Delete(0);
                  export.UpdateQueueFiles;
                End;
          End;
      Finally
        If (PROCESS <> tpStop) AND (directoryExists(CONFIG.Values['ArchiveFolder']))
          Then ZipFiles(ZipListFiles);
        ZipListFiles.Free;
        RefreshStatus(tpDone);
      End;
    End;

function TExport.exportToMis(Filename: String): boolean;
var
    myFile : TextFile;
    _FILEIN, _FILEOUT, _TEMP, _ENTETE : TStringList;
    i,j : integer;
    ligne, station : String;

const
    _OUTLINE = '%s;%s;%s';
    //_OUTLINE = '%s;%s;%n';
    _OUTENTETE = '<STATION>%s</STATION><SENSOR>%s</SENSOR><DATEFORMAT>YYYYMMDD</DATEFORMAT>';

Begin








  Result := False;
  Filename := trim(Filename);
  _FILEIN := TStringList.Create;
  _FILEOUT := TStringList.Create;
  _ENTETE := TStringList.Create;
  _TEMP := TStringList.Create;

  LogLine(1,  format(_FILE_CSV_PROCESS, [Filename]));

  If Not FileExists(Filename) Then
  Begin
    LogLine(2,  format(_FILE_NOT_EXIST, [Filename]));
    exit;
  End;

  _ENTETE.Text := AnsiReplaceText(extractFileName(Filename), '_', #13+#10);
  station := _ENTETE[0];
  _FILEIN.LoadFromFile(Filename);
  _FILEIN[0] := AnsiLowerCase(_FILEIN[0]);

  If Not AnsiStartsStr('date;heure;', _FILEIN[0]) Then
  Begin
    LogLine(2 , 'Fichier csv non correct');
    exit;
  End;

  _ENTETE.Text := AnsiReplaceText(_FILEIN[0], ';', #13+#10);

  For i := 2 to _ENTETE.Count - 1 Do
  Begin
    LogLine(2 , 'station : ' + _ENTETE[i]);
    _FILEOUT.Add(format(_OUTENTETE, [station, _ENTETE[i]]));
    For j := 1 to _FILEIN.Count - 1 Do
    Begin
      _TEMP.Text := AnsiReplaceText(_FILEIN[j], ';', #13+#10);
      _FILEOUT.Add(format(_OUTLINE, [AnsiReplaceText(_TEMP[0],'/',''), AnsiReplaceText(_TEMP[1],':',''),AnsiReplaceText(_TEMP[i],',','.')]));
    End;
  End;

  _FILEOUT.SaveToFile(IncludeTrailingPathDelimiter(CONFIG.Values['MisFolder'])+AnsiReplaceText(ExtractFileName(Filename),'.csv','.mis'));
  _FILEIN.Free;
  _FILEOUT.Free;
  _TEMP.Free;
  _ENTETE.Free;

  LogLine(1 , _PROCESS_FILE_END_OK);
  Result := True;
End;


function TExport.ZipFiles(lst: TStrings) : Boolean;
  Var
    archiver : TZipForge;
    i : Integer;

  Begin
    Result := False;
    If lst.Count >= 0
      Then Begin
        RefreshStatus(tpZip);
        archiver := TZipForge.Create(nil);
        Try
          With archiver
            Do Begin
              FileName := IncludeTrailingPathDelimiter(CONFIG.Values['ArchiveFolder']) +  'import [' + AnsiReplaceText(AnsiReplaceText(AnsiReplaceText(DateTimeToStr(now), '/' , '-'), ':' , '-'), ' ' , '-')  + '].zip';
              LogLine(1, format(_ZIP_CREATE_ARCHIVE, [FileName]));
              OpenArchive(fmCreate);
              BaseDir := IncludeTrailingPathDelimiter(FOLDER_MONITORING.Folder);
              LogLine(1, Format(_ZIP_CREATE_BASEDIR, [BaseDir]));
              For i := 0 To lst.Count - 1
                Do Begin
                  LogLine(2, format(_FILE_MOVE, [lst[i]]));
                  MoveFiles(lst[i]);
                  Result := True;
                End;
              CloseArchive();
              LogLine(1, _ZIP_CLOSE_ARCHIVE);
            End;
        Except
          On E: Exception
            Do Begin
              LogLine(2 , 'Exception: ' + E.Message);
              Result := False;
            End;
        End;
      End Else LogLine(-1, _ZIP_NO_FILE);
  End;


procedure TExport.TimerStartTimer(Sender: TObject);

    Begin
      CleanQueueFiles;
      If (CANEXECUTE = true) AND(FOLDER_MONITORING.IsActive) AND (PROCESS in [tpNone, tpWait, tpDone, tpMonitor]) AND (ListBoxFilesQueue.Items.Count > 0)
        Then Begin
          LogLine(0, _PROCESS_START_TIMER);
          LogLine(1, _PROCESS_MODE_AUTO);
          Lancer;
        End;
end;

end.
