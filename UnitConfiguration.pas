// ********************************************
// *  Unité configuration                     *
// *  @Inrae 2020                             *
// *  by mario Adam mario.adam@inrae.fr       *
// ********************************************

unit UnitConfiguration;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Grids, ValEdit, ComCtrls, strUtils,
  Menus, Spin, Mask, Types, ZipForge, UnitVariables, CheckLst, SHLOBJ,
   DB, DBGrids, DBTables, FileCtrl, ShellAPI;

type
  TFormConfiguration = class(TForm)
    PanelBas: TPanel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    PageControlConfiguration: TPageControl;
    TabSheetIniFile: TTabSheet;
    TabSheetImportCsv: TTabSheet;
    PanelParamImport: TPanel;
    LabelParamImports: TLabel;
    LabelTampon: TLabel;
    SpinEditTampon: TSpinEdit;
    CheckBoxLaurhAtStartup: TCheckBox;
    CheckBoxlaunchMonitoringStartup: TCheckBox;
    OpenDialog1: TOpenDialog;
    ValueListEditorTemp: TValueListEditor;
    CheckBoxLaunchImportAtStartup: TCheckBox;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    PanelMis: TPanel;
    Label1: TLabel;
    SpeedButton2: TSpeedButton;
    EditFolderMis: TEdit;
    PanelCsv: TPanel;
    SpeedButton1: TSpeedButton;
    LabelCsv: TLabel;
    EditFolderCsv: TEdit;
    PanelArchive: TPanel;
    SpeedButton3: TSpeedButton;
    Label2: TLabel;
    EditFolderArchive: TEdit;
    CheckBoxZipAfterImport: TCheckBox;
    TabSheetCsv: TTabSheet;
    StringGridCsv: TStringGrid;
    BitBtn1: TBitBtn;



    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure loadFile(fileName: string);

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure PageControlConfigurationChange(Sender: TObject);

// ********************************************************************************************************************
// *                                              Fichier de config et sauvegarde                                     *
// ********************************************************************************************************************
    procedure ReadConfig;
    procedure RefreshConfig;
    procedure SaveConfig(Sender: TObject);
    procedure CancelConfig(Sender: TObject);
    procedure AddToStartup;
    function  ExistInRegisTry(Nom, Valeur : String) : Boolean;

// ********************************************************************************************************************
// *                                              Editions  et fichiers                                               *
// ********************************************************************************************************************
    function  testColor(Test: Boolean): Tcolor;
    function  boolToString(Entry: boolean): string;
    procedure EditUpdate(Sender: TObject);
    procedure EditExit(Sender: TObject);
    function SelectDirectory(var Foldr: string; Title: string): Boolean;
    procedure UpdateValueList(Sender: TObject);

    procedure LoadCSV(Filename: string);
    procedure SaveStringGrid;
    procedure LoadStringGrid;
    procedure resizeCsv;
    function testFormat(input: string): string;

// ********************************************************************************************************************
// *                                            Actions et traitements                                                *
// ********************************************************************************************************************
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    procedure LogLine(Indent: Integer; AMessage: string);
    function readEnTry(Entry: string; DefaultValue: string): string; Overload;
    function readEnTry(Entry: string; DefaultValue: integer): integer; Overload;
  public
    { Public declarations }
  End;

Const
    wm_GetTraitement = WM_USER + 100;
    WM_ICONTRAY =  WM_USER + 1;

var
_COL, _ROW : Integer;
  FormConfiguration: TFormConfiguration;

implementation

uses UnitExport, UnitListSelect, RegisTry, IniFiles;

{$R *.dfm}





procedure TFormConfiguration.WMDropFiles(var Msg: TWMDropFiles);
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
      loadFile(PChar(FileName));
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

procedure TFormConfiguration.loadFile(fileName: string);
Begin
   if AnsiUpperCase(ExtractFileExt(fileName)) = '.CSV' Then
      LoadCSV(fileName);

   if AnsiUpperCase(ExtractFileExt(fileName)) = '.INI' Then
   begin
      CONFIG.LoadFromFile(fileName);
      FormCreate(Self);
   end;
      ShowMessage('Ok');
End ;

procedure TFormConfiguration.FormCreate(Sender: TObject);
  Begin

     DragAcceptFiles(Self.Handle, True);

    PageControlConfiguration.ActivePageIndex := 0;
    ReadConfig;

    FormConfiguration.CheckBoxLaurhAtStartup.Checked := FormConfiguration.existInRegisTry(_CONFIG_REGISTRY_REGKEYNAME , application.exename);

    If directoryExists(CONFIG.Values['CsvFolder'])
      Then EditFolderCsv.Text := CONFIG.Values['CsvFolder'];

    If directoryExists(CONFIG.Values['MisFolder'])
      Then EditFolderMis.Text := CONFIG.Values['MisFolder'];

    If directoryExists(CONFIG.Values['ArchiveFolder'])
      Then EditFolderArchive.Text := CONFIG.Values['ArchiveFolder'];

    LoadStringGrid();

    RefreshConfig;

  End;

procedure TFormConfiguration.FormShow(Sender: TObject);
  Begin
    If FOLDER_MONITORING.IsActive
      Then FOLDER_MONITORING.Deactivate;
  End;

procedure TFormConfiguration.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    RefreshConfig;
    DragAcceptFiles(Self.Handle, False);
  end;


procedure TFormConfiguration.FormResize(Sender: TObject);
  Begin
    BitBtnCancel.Left := PanelBas.Width -(BitBtnCancel.Width + 10);
    BitBtnOk.Left := BitBtnCancel.Left -(BitBtnOk.Width + 10);
  End;

procedure TFormConfiguration.PageControlConfigurationChange(Sender: TObject);
  Begin
    If PageControlConfiguration.ActivePage = TabSheetIniFile
      Then ValueListEditorTemp.Strings := CONFIG;
  End;

// ********************************************************************************************************************
// *                                              Fichier de config et sauvegarde                                     *
// ********************************************************************************************************************
  { ============================================= }
  {       Chargement de la configuration          }
  { ============================================= }

procedure TFormConfiguration.ReadConfig;
  Begin
    LogLine(0, _CONFIG_READ);

    readEnTry ('CsvFolder', 'C:\export');
    readEnTry ('AppDataFolder', getPathAppData);
    readEnTry ('csvBuffer', '50000');

    EditUpdate(SpinEditTampon);
    EditUpdate(CheckBoxLaurhAtStartup);
    EditUpdate(CheckBoxlaunchMonitoringStartup);
    EditUpdate(CheckBoxLaunchImportAtStartup);
    EditUpdate(CheckBoxZipAfterImport);

    EditUpdate(EditFolderCsv);
    EditUpdate(EditFolderMis);
    EditUpdate(EditFolderArchive);
  End;

  procedure TFormConfiguration.SaveConfig(Sender: TObject);
  Begin
    LogLine(0, _CONFIG_SAVE);
    SaveStringGrid();
    CONFIG.SaveToFile(FILE_CONFIG);
  End;

  procedure TFormConfiguration.CancelConfig(Sender: TObject);
  Begin
    ReadConfig;
  End;

  procedure TFormConfiguration.AddToStartup;
  var
    Registre: TRegisTry;
    nomdelakey: string;
    programme:string;

  Begin
    // on définie le nom de la clé qui sera dans le registre
    nomdelakey := 'HYDRASCSVTOMIS';
    // on définie le chemin de destination du programme
    programme := applicaTion.exename;
    // on crée la clé dans la registre
    Registre := TRegisTry.Create;
    Registre.RootKey := HKEY_LOCAL_MACHINE;
    Registre.OpenKey(_CONFIG_REGISTRY_PATH, True);
    If CheckBoxLaurhAtStartup.Checked
      Then Registre.WriteString(nomdelakey,programme)
      Else Registre.DeleteValue(nomdelakey);
    Registre.CloseKey;
    Registre.Free;
  End;

  function TFormConfiguration.ExistInRegisTry(Nom, Valeur : String) : Boolean;
  var
    Reg: TRegisTry;

  Begin
    Result := False;
    If DEBUG
      Then LogLine(1, Format(_CONFIG_REGISTRY_TEST, [Nom, Valeur ]));
    Reg := TRegisTry.Create;
    With Reg
    Do Begin
      Try
        RootKey := HKEY_LOCAL_MACHINE;
        If OpenKey(_CONFIG_REGISTRY_PATH, True)
          Then Result := (ReadString(Nom) = Valeur)
          Else If DEBUG
            Then LogLine(-1, _CONFIG_REGISTRY_FOUND);
        CloseKey;
      Finally
        Free;
      End;
    End;
    If DEBUG
      Then LogLine(1, format(_Result, [boolToString(Result)]));
  End;

  { ==================================================== }
  {       Raffraichissement de la configuration          }
  { ==================================================== }

procedure TFormConfiguration.RefreshConfig;

  function  verifDir(Dir:  string): boolean;
    Begin
      Result := (directoryexists(Dir) AND (IncludeTrailingPathDelimiter(Dir) <> ExtractFilePath(Application.ExeName)))
    End;


  Begin
    LogLine(0, _CONFIG_REFRESH);
    CSVDIR_OK := verifDir(EditFolderCsv.Text);
    MISDIR_OK := verifDir(EditFolderMis.Text);
    ARCHIVEDIR_OK := verifDir(EditFolderArchive.Text);
    CANEXECUTE := CSVDIR_OK AND MISDIR_OK AND ARCHIVEDIR_OK;
    // Attention le sens el l'ordre des des tests est tres important
    PanelCsv.Color := testColor(CSVDIR_OK);
    PanelMis.Color := testColor(MISDIR_OK);
    PanelArchive.Color := testColor(ARCHIVEDIR_OK);
    PanelParamImport.Visible := CSVDIR_OK;
    export.ToolButtonStartManual.enabled := CSVDIR_OK;

    If (CONFIG.Values['launchMonitoringStartup'] = 'True') AND (CANEXECUTE = True)
      Then export.RefreshStatus(tpMonitor);

  End;

// ********************************************************************************************************************
// *                                              Editions  et fichiers                                               *
// ********************************************************************************************************************


function  TFormConfiguration.testColor(Test: Boolean): Tcolor;
  Begin
      If Test
        Then Result := _OKCOLOR
        Else Result := _ERRCOLOR;
  End;

function TFormConfiguration.boolToString(Entry: boolean): string;
  Begin
    If Entry
      Then Result := 'True'
      Else Result := 'False';
  End;

  { ==================================================== }
  {       Fonction d'édition d'un composant              }
  { ==================================================== }


procedure TFormConfiguration.EditUpdate(Sender: TObject);
  var
    temp: string;

  Begin
      If DEBUG Then
        Logline(1, 'EditUpdate : ' + Sender.ClassName + '   (' + TEdit(Sender).Hint + ')');

      If Sender.ClassName = 'TEdit'
        Then TEdit(Sender).Text := readEnTry(TEdit(Sender).Hint, TEdit(Sender).Text);

      If Sender.ClassName = 'TMaskEdit'
        Then TMaskEdit(Sender).Text := readEnTry(TMaskEdit(Sender).Hint, TMaskEdit(Sender).Text);

      If Sender.ClassName = 'TDirectoryListBox'
      Then Begin
        temp := readEnTry(TDirectoryListBox(Sender).Hint, TDirectoryListBox(Sender).Directory);
        If directoryexists(temp)
          Then TDirectoryListBox(Sender).Directory := temp
          Else TDirectoryListBox(Sender).Directory := ExtractFilePath(Application.Exename);
      End;

      If Sender.ClassName = 'TSpinEdit'
        Then TSpinEdit(Sender).Value := readEnTry(TSpinEdit(Sender).Hint, TSpinEdit(Sender).Value);

      If Sender.ClassName = 'TCheckBox'
        Then TCheckBox(Sender).Checked := (readEnTry(TCheckBox(Sender).Hint, 'True') = 'True');
  End;

  { ============================================================ }
  {      Lecture d'une entrée dans la configuration (string)     }
  { ============================================================ }

function TFormConfiguration.readEntry(entry: string; DefaultValue: string): string;
  Begin
    Result := CONFIG.Values[Entry];
    If Result = ''
      Then Result := DefaultValue;

    If DEBUG
      Then Begin
        logline(1, format(_READ_ENTRY, [Entry]));
        logline(2, format(_DEFAULT_VALUE, [DefaultValue]));
        logline(2, format(_Result, [Result]));
      End;
  End;

  { ============================================================ }
  {      Lecture d'une entrée dans la configuration (Integer)    }
  { ============================================================ }

function TFormConfiguration.readEnTry(Entry: string; DefaultValue: integer): integer;
  Var
    tmp : string;

  Begin
    tmp := CONFIG.Values[Entry];
    If tmp = ''
      Then Result := DefaultValue
      Else Result := StrToInt(Renvoi_Chiffre(tmp));

    If Result = 0
      Then Result := DefaultValue;
  End;

  { ========================================================== }
  {       Fonction de mise a jour a la sortie d'un composant   }
  { ========================================================== }

procedure TFormConfiguration.EditExit(Sender: TObject);
  function EncodePWDEx(Data, SecurityString: string; MinV: Integer = 0; MaxV: Integer = 5): string;
    Var
        i, x: integer;
        s1, s2, ss: string;

    function MakeRNDString(Chars: string; Count: Integer): string;
      Var
        i, x: integer;

      Begin
        Result := '';
        For i := 0 to Count - 1 Do
        Begin
            x := Length(chars) - Random(Length(chars));
            Result := Result + chars[x];
            chars := Copy(chars, 1,x - 1) + Copy(chars, x + 1,Length(chars));
        End;
      End;


    Begin
        If minV > MaxV Then
        Begin
            i := minv;
            minv := maxv;
            maxv := i;
        End;

        If MinV < 0 Then MinV := 0;
        If MaxV > 100 Then MaxV := 100;
        Result := '';
        If Length(SecurityString) < 16 Then
            Exit;
        For i := 1 To Length(SecurityString) Do
        Begin
            s1 := Copy(SecurityString, i + 1,Length(securitystring));
            If Pos(SecurityString[i], s1) > 0 Then
                Exit;
            If Pos(SecurityString[i], _CODE64) <= 0 Then
                Exit;
        End;
        s1 := _CODE64;
        s2 := '';
        For i := 1 to Length(SecurityString) Do
        Begin
            x := Pos(SecurityString[i], s1);
            If x > 0 Then
                s1 := Copy(s1, 1,x - 1) + Copy(s1, x + 1,Length(s1));
        End;
        ss := securitystring;
        For i := 1 To Length(Data) Do
        Begin
            s2 := s2 + ss[Ord(Data[i]) mod 16 + 1];
            ss := Copy(ss, Length(ss), 1) + Copy(ss, 1,Length(ss) - 1);
            s2 := s2 + ss[Ord(Data[i]) div 16 + 1];
            ss := Copy(ss, Length(ss), 1) + Copy(ss, 1,Length(ss) - 1);
        End;
        Result := MakeRNDString(s1, Random(MaxV - MinV) + minV + 1);
        For i := 1 To Length(s2) Do
            Result := Result + s2[i] + MakeRNDString(s1, Random(MaxV - MinV) + minV);
    End;

Begin
     If Sender.ClassName = 'TEdit' Then
      CONFIG.Values[TEdit(Sender).Hint] := TEdit(Sender).Text;

     If Sender.ClassName = 'TDirectoryListBox' Then
          CONFIG.Values[TDirectoryListBox(Sender).Hint] := TDirectoryListBox(Sender).Directory;

     if Sender.ClassName = 'TSpinEdit' Then
        CONFIG.Values[TEdit(Sender).Hint] := IntToStr(TSpinEdit(Sender).value);

     if Sender.ClassName = 'TCheckBox' Then
        CONFIG.Values[TCheckBox(Sender).Hint] := boolToString(TCheckBox(Sender).Checked);
End;

function TFormConfiguration.SelectDirectory(var Foldr: string; Title: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  DisplayName: array[0..MAX_PATH] of Char;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName[0];
    lpszTitle := PChar(Title);
    ulFlags := BIF_RETURNONLYFSDIRS;
  end;
  ItemIDList := SHBrowseForFolder(BrowseInfo);
  if Assigned(ItemIDList) then
    if SHGetPathFromIDList(ItemIDList, DisplayName) then begin
      Foldr := DisplayName;
      Result := True;
    end;
end;

procedure TFormConfiguration.UpdateValueList(Sender: TObject);
  Begin
    CONFIG.Values[TValueListEditor(Sender).Hint] := AnsiReplaceText(TValueListEditor(Sender).Strings.Text, #13+#10, ';');
  End;

procedure TFormConfiguration.LogLine(Indent: Integer; AMessage: string);
    Begin
        Export.LogLine(Indent, AMessage);
    End;

// ********************************************************************************************************************
// *                                            Actions et traitements                                                *
// ********************************************************************************************************************

procedure TFormConfiguration.SpeedButton1Click(Sender: TObject);
var
  Foldr: string;

begin
  if Not SelectDirectory(Foldr, 'Répértoire des sources csv') then exit;
  EditFolderCsv.Text := Foldr;
  CONFIG.Values[TSpeedButton(Sender).Hint] := Foldr;
  RefreshConfig;
end;

procedure TFormConfiguration.SpeedButton2Click(Sender: TObject);
var
  Foldr: string;
begin
  if Not SelectDirectory(Foldr, 'Répértoire des fichiers MIS') then exit;
  EditFolderMis.Text := Foldr;
  CONFIG.Values[TSpeedButton(Sender).Hint] := Foldr;
  RefreshConfig;
end;

procedure TFormConfiguration.SpeedButton3Click(Sender: TObject);
var
  Foldr: string;
begin
  if Not SelectDirectory(Foldr, 'Répértoire des achives') then exit;
  EditFolderArchive.Text := Foldr;
  CONFIG.Values[TSpeedButton(Sender).Hint] := Foldr;
  RefreshConfig;
end;





procedure TFormConfiguration.LoadCSV(Filename: string);
var
   i, j, Position, count, edt1 : integer;
   temp, entete, tempField : string;
   FieldDel: char;
   Data, Ligne: TStringList;
   strArray  : Array of String;
   strA      : String;
begin
  Data := TStringList.Create;
  Ligne := TStringList.create;
  FieldDel := ';';
  Data.LoadFromFile(Filename);
  StringGridCsv.RowCount := Data.Count;
  Ligne.text := StringReplace(Data[0], FieldDel, #13#10, [rfReplaceAll]);
  StringGridCsv.ColCount := Ligne.Count;
  for i := 0 to Ligne.Count - 1 do StringGridCsv.Cells[i,0] :=  Ligne[i];

  for j := 1 to Data.Count - 1 do
  Begin
    Ligne.text := StringReplace(Data[j], FieldDel, #13#10, [rfReplaceAll]);
    for i := 0 to Ligne.Count - 1 do
        StringGridCsv.Cells[i,j] :=  Ligne[i];
  End;

  if StringGridCsv.Cells[StringGridCsv.ColCount - 1 ,0] <> 'Calcul' Then
  Begin
    StringGridCsv.ColCount := StringGridCsv.ColCount + 2;
    StringGridCsv.Cells[StringGridCsv.ColCount - 1,0] := 'Calcul';
    StringGridCsv.Cells[StringGridCsv.ColCount - 2,0] := 'Test';
  End;


        for i := 0 to StringGridCsv.RowCount - 1 do Begin
        StringGridCsv.Cells[2, i] := testFormat(StringGridCsv.Cells[2, i]);
      End;

  resizeCsv();



    Data.Free;
    Ligne.Free;
    SaveStringGrid();

end;

 function TFormConfiguration.testFormat(input: string): string;
const
  CHARS = ['0'..'9', 'A'..'Z'];
var
  i: Integer;
begin
result := '';
  input := trim(uppercase(input));
  for i := 1 to Length(input) do
    If (input[i] in CHARS) then
      result := result + input[i];
end;

procedure TFormConfiguration.resizeCsv;
var
   i, j, k, W, WMax: integer;

begin

  with StringGridCsv do
  Begin
      i := ColCount - 2;
      if StringGridCsv.Cells[i,0] = 'Test' Then
      begin
        for k := 1 to RowCount - 1 do
         If trim(Cells[i, k]) = ''
          Then Cells[i, k] := testFormat(Cells[3, k]) ;
      end;

    WMax := 0;
    for j := 0 to ColCount - 1 do
    begin
      for i := 0 to (StringGridCsv.RowCount - 1) do begin
        W := StringGridCsv.Canvas.TextWidth(StringGridCsv.Cells[j, i]);
        if W > WMax then
          WMax := W;
        end;
      ColWidths[j] := WMax + 5;
    end;
  End;






end;

procedure TFormConfiguration.SaveStringGrid();
var
  i, k: Integer;
  Data: TStringList;

begin
  Data := TStringList.Create;
  with StringGridCsv do
  begin
    // Write number of Columns/Rows
    Data.Add(intToStr(ColCount));
    Data.Add(intToStr(RowCount));
    // loop through cells
    for i := 0 to ColCount - 1 do
      for k := 0 to RowCount - 1 do
        Data.Add(Cells[i, k]);
  end;
  CONFIG.Values['csv'] := StringReplace(Data.Text, #13#10, ';', [rfReplaceAll]);
  Data.Free;
end;

procedure TFormConfiguration.LoadStringGrid();
var
  i, k, index: Integer;
  Data: TStringList;

begin
  if CONFIG.Values['csv'] = '' then exit;
  Data := TStringList.Create;
  data.Text := StringReplace(CONFIG.Values['csv'], ';', #13#10, [rfReplaceAll]);
  with StringGridCsv do
  begin
    ColCount := strToInt(data[0]);
    RowCount := strToInt(data[1]);
    // Write number of Columns/Rows
    Data.Add(intToStr(ColCount));
    Data.Add(intToStr(RowCount));
    // loop through cells
    index := 2;
    for i := 0 to ColCount - 1 do
      for k := 0 to RowCount - 1 do
      Begin
        Cells[i, k] := data[index];
        inc(index);
      End;



  end;
  Data.Free;


  resizeCsv();



end;




procedure TFormConfiguration.BitBtn1Click(Sender: TObject);
var i,k : integer;
begin
  with StringGridCsv do
  Begin
      i := ColCount - 2;
      if StringGridCsv.Cells[i,0] = 'Test' Then
      begin
        for k := 1 to RowCount - 1 do
        Begin
          Cells[i, k] := testFormat(Cells[3, k]) ;
        End;
      end;
  End;
end;





End.


