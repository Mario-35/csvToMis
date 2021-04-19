// ********************************************
// *  Unit? Main export                       *
// *  @Inrae 2020                             *
// *  by mario Adam mario.adam@inrae.fr       *
// ********************************************

unit UnitExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ValEdit, StrUtils, ZipForge,
  Buttons, Menus, ComCtrls, DBCtrls, UnitVariables,  ShellAPI,
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
    BitBtnStop: TBitBtn;
    ImageOnOff: TImageList;
    PopupMenuLogs: TPopupMenu;
    RichEditLog: TRichEdit;
    PanelUp: TPanel;
    ToolBarExport: TToolBar;
    ToolButtonHome: TToolButton;
    ToolButtonConfig: TToolButton;
    ToolButton3: TToolButton;
    ToolButtonStartManual: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    LabelMonitoring: TLabel;
    RichEditLogs: TRichEdit;
    Actuel1: TMenuItem;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    procedure RefreshStatus(newState: TProcessType);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitterClick(Sender: TObject);
    procedure BitBtnStopClick(Sender: TObject);

    procedure ToolButtonConfigClick(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure ToolButtonStartManualClick(Sender: TObject);

    function exportToMis(Filename: String): boolean;
    function ZipFiles(lst: TStrings) : Boolean;

    procedure Actuel1Click(Sender: TObject);
  private
    ligne: string;
  public
    procedure LogLine(Indent: Integer; AMessage: string);

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
        Application.UpdateFormatSettings := false;
        DecimalSeparator := '.';

        ToolBarExport.Top := 0;
        ToolBarExport.Left := 0;
        ToolBarExport.Height := 35;
        ToolBarExport.Width := 230;
        LabelMonitoring.Left := ToolBarExport.Width;
        LabelMonitoring.Top := 0;
        LabelMonitoring.Width := Export.Width - ToolBarExport.Width;
        LabelMonitoring.Height := ToolBarExport.Height;
        Mnu.OwnerDraw:=True;


        StatusBarMain.Panels[0].Text := 'ver : ' + APP_VERSION;

        LogFolder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\';
        If not directoryExists(LogFolder) Then CreateDir(LogFolder);
        FILE_LOGFILE := LogFolder + '[' + Application.Title  + '] ' + formatdatetime('dd-mm-yyyy', now) +  '.log';
        LogLine(0, 'Demmarrage version : ' + APP_VERSION + _CR);
        If DEBUG
          Then LogLine(1, 'mode debug');

        RichEditLog.Align := alClient;
        RichEditLogs.Align := alClient;

        BitBtnStop.Left := Round(Export.Width / 2) - Round(BitBtnStop.Width / 2);
        BitBtnStop.top := Export.Height - Round(Export.Height / 4);

        FindFilePattern(TEMP_LIST, IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\', '.log', True);
        For i := 0 To TEMP_LIST.Count - 1
          Do Begin
            Item := TMenuItem.Create(PopupMenuLogs);
            Item.Caption := extractFileName(TEMP_LIST[i]);
            Item.OnClick := Actuel1Click;
            PopupMenuLogs.Items.Add(Item);
          End;
        Self.PopupMenu := PopupMenuLogs;

        DragAcceptFiles(Self.Handle, True);

    End;





procedure TExport.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name

  DropPoint: TPoint;          // point where files dropped

begin
  inherited;
  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    // Get count of files dropped
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    if (DroppedFileCount = 1) Then
    begin
      // get length of file name
      FileNameLength := DragQueryFile(DropH, 0, nil, 0);
      // create string large enough to store file
      SetLength(FileName, FileNameLength);
      // get the file name
      DragQueryFile(DropH, 0, PChar(FileName), FileNameLength + 1);
      // process file name (application specific)
      exportToMis(PChar(FileName));
    end;
    // Optional: Get point at which files were dropped
    DragQueryPoint(DropH, DropPoint);
    // ... do something with drop point here
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
end;

procedure TExport.QuitterClick(Sender: TObject);
    Begin
        Application.Terminate;
    End;

procedure TExport.FormDestroy(Sender: TObject);
  Begin
    LogLine(0, 'Sortie de programme');
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
          StatusBarMain.Panels[1].Text := 'D?marrage';
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
          StatusBarMain.Panels[1].Text := 'Traitement stopp?';
        End;
        tpError : StatusBarMain.Panels[1].Text := 'Erreur';
        tpDone :  StatusBarMain.Panels[1].Text := 'Termin?';
        tpZip :  StatusBarMain.Panels[1].Text := 'Zip fichier(s)';
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


procedure TExport.ToolButtonStartManualClick(Sender: TObject);
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
                  RichEditLog.Visible := true;
                  RichEditLogs.Visible := false;
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

function TExport.exportToMis(Filename: String): boolean;
  var
      myFile : TextFile;
      calcul, resultat : double;
      operation : char;
      _FILEIN, _FILEOUT, _TEMP, _ENTETE : TStringList;
      i,j, k, trouve, position : integer;
      ligne, station, test, _DATE, _HEURE, _VALEUR, _SEPARATEUR : String;

  const
      _OUTLINE = '%s;%s;%s';
      //_OUTLINE = '%s;%s;%n';
      _TEST ='date/time';
      _OUTENTETE = '<STATION>%s</STATION><SENSOR>%s</SENSOR><DATEFORMAT>YYYYMMDD</DATEFORMAT>';

  Begin
    Result := False;
    Filename := trim(Filename);
    _FILEIN := TStringList.Create;
    _FILEOUT := TStringList.Create;
    _ENTETE := TStringList.Create;
    _TEMP := TStringList.Create;

    // Demmarage fichier
    LogLine(1,  format(_FILE_CSV_PROCESS, [Filename]));

    // si fichier existe pas on sort
    If Not FileExists(Filename) Then
    Begin
      LogLine(2,  format(_FILE_NOT_EXIST, [Filename]));
      exit;
    End;

    // recupere le nom de la station dans le nom du fichier
    _ENTETE.Text := AnsiReplaceText(extractFileName(Filename), '_', #13+#10);
    station := _ENTETE[0];
    // chargement du fichier
    _FILEIN.LoadFromFile(Filename);

    // supprime entete indesirable
    While Not AnsiStartsStr(_TEST, AnsiLowerCase(_FILEIN[0]))
      Do If _FILEIN.Count -1 >= 1
        Then _FILEIN.Delete(0)
        Else Begin
          LogLine(2 , 'Fichier csv non correct');
          exit;
        End;

    // verifie (si fin de fichier ...)
    If Not AnsiStartsStr(_TEST, AnsiLowerCase(_FILEIN[0])) Then
    Begin
      LogLine(2 , 'Fichier csv non correct');
      exit;
    End;

    // creation d'un tableau avec l'entete
    if  ansiPos(';',_FILEIN[0]) > 1
     Then _SEPARATEUR := ';'
      Else If  ansiPos(',',_FILEIN[0]) > 1
      Then _SEPARATEUR := ','
        Else Begin
          LogLine(2 , 'Seperateur non correct non correct (; ou ,)');
          exit;
        End;

    _ENTETE.Text := AnsiReplaceText(_FILEIN[0], _SEPARATEUR, #13+#10);

    // boucle sur les colonnes sauf la date de la premiere colone

    For i := 1 to _ENTETE.Count - 1 Do
    Begin
      LogLine(2 , 'Recherche Station : ' + _ENTETE[i]);
      test := trim(AnsiUpperCase(FormConfiguration.testFormat(_ENTETE[i])));
      // cherche la colonne de correspondance
      trouve := -1;
      k := 0;
      while trouve = -1 Do
      begin
        if AnsiUpperCase(FormConfiguration.StringGridCsv.Cells[1,k]) = AnsiUpperCase(station) Then
          if test = FormConfiguration.StringGridCsv.Cells[2,k] Then
            begin
              trouve := k;
              LogLine(3 , '=========================== Trouve ===========================');
              LogLine(3 , 'station: ' + FormConfiguration.StringGridCsv.Cells[3,trouve]);
              LogLine(3 , 'sensor: ' + FormConfiguration.StringGridCsv.Cells[4,trouve]);
            end;
        inc(k);
         if k > FormConfiguration.StringGridCsv.RowCount Then trouve := -2;
      end;
      if trouve > 0 Then
      Begin
        if Length(FormConfiguration.StringGridCsv.Cells[6,trouve]) > 0 Then
        Begin
            test := copy(FormConfiguration.StringGridCsv.Cells[6,trouve],0,1);
              if test = '+' Then operation := '+';
              if test = '-' Then operation := '-';
              if test = '*' Then operation := '*';
              if test = '/' Then operation := '/';
              calcul :=  strtofloat(copy(FormConfiguration.StringGridCsv.Cells[6,trouve], 2, 20));
        End else  operation := '!';
      End Else begin
        LogLine(2 , 'Erreur : Pas de correspondance pour station :  ' + station);
        LogLine(2 , '                                 et sensor  :  ' + test);
        exit;
      End;
        
      _FILEOUT.Add(format(_OUTENTETE, [FormConfiguration.StringGridCsv.Cells[3,trouve], FormConfiguration.StringGridCsv.Cells[4,trouve]]));
        
      For j := 1 to _FILEIN.Count - 1 Do
      Begin
        _TEMP.Text := AnsiReplaceText(_FILEIN[j], _SEPARATEUR, #13+#10);
        if (_TEMP.count - 1 >= i)  Then
        Begin
          position := ansiPos(' ',_TEMP[0]);
          if position > 1 Then
          Begin
            // DD/MM/YYYY
            if  copy(_TEMP[0],0,position)[3] = '/'
              Then _DATE := trim(AnsiReplaceText(copy(_TEMP[0],0,position),'/',''))
              // YYYY/MM/DD
            else if copy(_TEMP[0],0,position)[5] = '/' Then Begin
            _DATE := copy(_TEMP[0],9,2) + copy(_TEMP[0],6,2) + copy(_TEMP[0],0,4);
            End;
            _HEURE := trim(AnsiReplaceText(copy(_TEMP[0], position, 10),':',''));

            if (trim(_TEMP[i]) <> '') Then
            begin
              _VALEUR := AnsiReplaceText(_TEMP[i],',','.');
              if  operation = '!' Then _VALEUR := _VALEUR
              Else Begin
                Case operation of
                  '+' : resultat := StrToFloat(_VALEUR) + calcul;
                  '-' : resultat := StrToFloat(_VALEUR) - calcul;
                  '*' : resultat := StrToFloat(_VALEUR) * calcul;
                  '/' : resultat := StrToFloat(_VALEUR) / calcul;
                end;
                _VALEUR := floatToStr(resultat);
              End;
            End else _VALEUR := 'null';
          End;
          _FILEOUT.Add(format(_OUTLINE, [_DATE, _HEURE ,AnsiReplaceText(_VALEUR,',','.')]))
        end Else if AnsiStartsStr(_FILEIN[j], 'END OF DATA FILE') Then break;
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




procedure TExport.Actuel1Click(Sender: TObject);
  Var
    S : string;

  begin
    S := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Logs\' + TMenuItem(Sender).Caption;
    If FileExists(S)
        Then Begin
          RichEditLogs.Lines.LoadFromFile(S);
          RichEditLog.Visible := false;
          RichEditLogs.Visible := true;
        End Else Begin
          RichEditLog.Visible := true;
          RichEditLogs.Visible := false;
        end;
  end;

end.

