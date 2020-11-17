// ********************************************
// *  Unité Variables                         *
// *  @Inrae 2020                             *
// *  by mario Adam mario.adam@inrae.fr       *
// ********************************************

unit UnitVariables;

interface
  uses
    Classes, SysUtils, Forms, FolderMon, strUtils;

  Type
    TProcessType = (tpNone, tpInit, tpStart, tpRun, tpWait, tpPause, tpStop, tpError, tpDone, tpMonitor, tpZip);

  function changeEndValues(enTry : string; EndLine : String): string;
  function GetFileSize(fileName : wideString) : Int64;
  function TestBool(entry : string) : boolean;
  function DeleteFiles(Directory: string; Extension: string): Boolean;
  function Renvoi_Chiffre(Chaine: string): string;
  procedure Supprime_Ligne_Blanche(lst: TStrings);
  procedure FindFilePattern(lst: TStrings; root:String; pattern:String ; Recursive: Boolean);
  function getPathAppData: string;


  const
    _CRLF = #13+#10;
    _LF    = #10;
    _TAB   = #9;
    _CR    = #13;
    _BLANK = #32;
    _CONFIG_REFRESH = 'Rafraichissement de la configuration';
    _CONFIG_READ = 'lecture de la configuration';
    _CONFIG_SAVE  = 'sauvegarde de la configuration';
    _PWDEx = 'np2kB508CGxXm/SV';

    _CONFIG_REGISTRY_TEST               = 'Test dans la base de registre de  : %s et de la valeur %s';
    _CONFIG_REGISTRY_PATH               = '\Software\Microsoft\Windows\CurrentVersion\Run\';
    _CONFIG_REGISTRY_FOUND              = 'Cle non trouve';
    _CONFIG_REGISTRY_REGKEYNAME         = 'HYDRASCSVTOMIS';

    _CODE64                             = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
    _EXCEPTION                          = 'Exception : ';
    _NOTFOUND                           = 'non trouvé';
    _OKCOLOR                            = $0080FF80;
    _ERRCOLOR                           = $008080FF;

    _PROCESS_STOP                       = 'Arret en cours...';
    _PROCESS_ACTIVE                     = 'Traitement en cours ...';
    _PROCESS_MANUEL                     = 'Lancement du traitement manuel';
    _PROCESS_AUTO                       = 'Lancement du traitement automatique';
    _PROCESS_END_OK                     = 'Traitement fini avec succes';
    _PROCESS_FILE_END_OK                = 'Traitement du fichier fini';
    _PROCESS_FILE_START                 = 'Traitement du fichier';
    _PROCESS_START_TIMER                = 'Démarrage du timer [AUTO]';
    _PROCESS_MODE_AUTO                  = 'Mode Automatique';
    _PROCESS_MODE_MANUEL                = 'Mode manuel';
    _PROCESS_EXIT                       = 'Sortie de l''execution';

    _ZIP_CREATE_ARCHIVE                 = 'Creation du fichier ZIP : %s';
    _ZIP_CLOSE_ARCHIVE                  = 'Fermeture du fichier ZIP';
    _ZIP_CREATE_BASEDIR                 = 'Archive basedir %s';
    _ZIP_NO_FILE                        = 'Aucun fichier a archiver';

    _FILE_ADD                           = 'Ajout du fichier %s';
    _FILE_DEL                           = 'Suppression du fichier %s';
    _FILE_MOVE                          = 'Deplacement du fichier %s';
    _FILE_CSV_PROCESS                   = 'Traitement du fichier csv : %s';
    _FILE_EXIST                         = 'Fichier %s dejà existant';
    _FILE_NOT_EXIST                     = 'Fichier %s non trouvé';
    _FILES_NOT_EXIST                    = 'Aucun fichier csv trouvé';
    _FILES_SELECT                       = 'Séléctionnez le(s) fichier(s) à traiter';


    _TEST_DOMAIN = 'Test existance domaine';
    _TEST_DOMAINS = 'Test des domaines';
    _TEST_DOMAIN_OK = 'Domaine %s  -> Ok';
    _TEST_DOMAIN_ERR = 'Domaine %s  -> incorrect';
    _TEST_DOMAINS_NOT_FOUND_FILE = 'Aucun domaine pour ce fichier';
    _RESULT = 'Resultat : %s';
    _READ_ENTRY = 'Lecture de la cle';
    _DEFAULT_VALUE = 'Valeur par default';

    _MONITORING_DIR_START = 'Activation surveillance de : %s';
    _MONITORING_DIR_STOP = 'Arret de la surveillance de : %s';

  var
    PATH_APPDATAS_HYDRAS                : string;


    FILE_APP_PATH : string;
    FILE_CONFIG: string;
    FILE_LOGFILE : string;
    FILE_SQL : string;

    APP_VERSION : string;

    DEBUG : boolean;

    PROCESS : TProcessType;
    POSTGRES_OK : boolean;
    POSTGRES_ADMIN_OK : boolean;
    HYDRAS_OK : boolean;
    HYDRASRX_OK : boolean;
    CSVDIR_OK : boolean;
    MISDIR_OK : boolean;
    ARCHIVEDIR_OK : boolean;
    CANEXECUTE : boolean;

    ACTUAL_LOG : boolean;

    LIST_FILES2: TStringList;
    FOLDER_MONITORING: TFolderMon;

    CONFIG : TStringList;
    TEMP_LIST : TStringList;


    TEMP_SQL : string;
    TEMP_STRING : string;
    TEMP_INTEGER : Integer;

implementation


function getPathAppData: string;
begin
  Result := SysUtils.GetEnvironmentVariable('APPDATA');
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(extractFilePath(Result)) + 'Local\VirtualStore\Program Files (x86)\OTT\HYDRAS3\';

end;

procedure FindFilePattern(lst: TStrings; root:String; pattern:String ; Recursive: Boolean);
  var
    SR:TSearchRec;

  Begin
    root:=IncludeTrailingPathDelimiter(root);
      If FindFirst(root+'*.*',faAnyFile,SR) = 0
        Then begin
          Repeat
            Application.ProcessMessages;
            If ((SR.Attr and faDirectory) = SR.Attr ) and (pos('.',SR.Name)=0)
              Then Begin
                If Recursive
                  Then FindFilePattern(lst, root+SR.Name, pattern, Recursive)
              End Else If pos(pattern, SR.Name) > 0
                    Then lst.Add(Root+SR.Name);
          Until FindNext(SR) <> 0;
        End;
  End;



  function changeEndValues(enTry : string; EndLine : String): string;
    Begin
      result := Copy(enTry, 1 , length(enTry)-1) + EndLine;
    End;

function Renvoi_Chiffre(Chaine: string): string;
  { ======================================================= }
  { fonction renvoyant uniquement les chiffres d'une chaine }
  { ======================================================= }
  Var
    i : Integer;

  Begin
    Result := '0';
    If Trim(Chaine) <> ''
      Then For i := 0 To Length(Chaine)
        Do If Chaine[i] In ['1', '2', '3', '4', '5', '6', '7', '8', '8', '9', '0']
          Then Result := Result + Chaine[i];
  End;

  function GetFileSize(fileName : wideString) : Int64;
    Var
      sr : TSearchRec;

    Begin
      If FindFirst(fileName, faAnyFile, sr ) = 0
        Then result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
        Else result := -1;
      FindClose(sr);
    End;

  function TestBool(entry : string) : boolean;

    Begin
      If UpperCase(entry) = 'TRUE'
        Then result := True
        Else Result := False;
    End;

function DeleteFiles(Directory: string; Extension: string): Boolean;
    Var
        searchResult: TSearchRec;

    Begin
        If FindFirst(IncludeTrailingPathDelimiter(Directory)+'*', faAnyFile, searchResult)=0
          Then Begin
            Try
                Repeat
                  If (searchResult.Attr and faDirectory)= 0
                    Then If SameText(UpperCase(ExtractFileExt(searchResult.Name)), UpperCase(Extension))
                      Then SysUtils.DeleteFile(IncludeTrailingPathDelimiter(Directory)+searchResult.Name);
                Until FindNext(searchResult)<>0
            Finally
                FindClose(searchResult);
            End;
          End;
    End;


  procedure Supprime_Ligne_Blanche(lst: TStrings);
  { =================================================== }
  { Supprime les doublons et lignes blanche d'une Liste }
  { =================================================== }
  var
    iEncours: Integer;

  begin
    StringReplace(lst.Text,Char(13)+Char(10),Char(13)+Char(10),[rfReplaceAll]);
    If lst.count < 1
      Then exit;
    iEncours := lst.count - 1;
    while iEncours >= 0 do
    begin
      If trim(lst.Strings[iEncours]) = ''
        Then Begin
          lst.delete(iEncours);
          dec(iEncours);
        End;
      dec(iEncours);
    End;
  End;

  initialization
    PATH_APPDATAS_HYDRAS                := getPathAppData;
    ACTUAL_LOG                          := True;
    LIST_FILES2 := TStringList.Create;
    CONFIG := TStringList.Create;
    TEMP_LIST := TStringList.Create;
    FOLDER_MONITORING := TFolderMon.Create;

    FILE_APP_PATH := IncludeTrailingPathDelimiter(ExtractFilePath(Application.exename));
    FILE_CONFIG := FILE_APP_PATH + 'config.ini';
    If FileExists(FILE_CONFIG)
      Then CONFIG.LoadFromFile(FILE_CONFIG);

  finalization
    LIST_FILES2.Free;
    CONFIG.Free;
    TEMP_LIST.Free;

end.
