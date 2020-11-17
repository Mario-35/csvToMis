// ********************************************
// *  Unité SplashScreen                      *
// *  @Inrae 2020                             *
// *  by mario Adam mario.adam@inrae.fr       *
// ********************************************

unit UnitSplashScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, jpeg, UnitVariables;

type
  TSplashScreen = class(TForm)
    PanelSplashScreen: TPanel;
    Image1: TImage;
    LabelName: TLabel;
    LabelVersion: TLabel;
    Image2: TImage;
    ButtonOk: TButton;
    procedure FormCreate(Sender: TObject);
    procedure getVersion;
    procedure ButtonOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SplashScreen: TSplashScreen;

implementation

{$R *.dfm}



procedure TSplashScreen.getVersion;
  var
    V1,       // Major Version
    V2,       // Minor Version
    V3,       // Release
    V4: Word; // Build Number

    procedure getBuildInfo(var V1, V2, V3, V4: Word);
      Var
        VerInfoSize, VerValueSize, Dummy : DWORD;
        VerInfo : Pointer;
        VerValue : PVSFixedFileInfo;

      Begin
        VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
        GetMem(VerInfo, VerInfoSize);
        GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        With VerValue^
          Do Begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          End;
        FreeMem(VerInfo, VerInfoSize);
      End;

  Begin
    getBuildInfo(V1, V2, V3, V4);
    APP_VERSION := IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + '.' + IntToStr(V4);
  End;

procedure TSplashScreen.FormCreate(Sender: TObject);
  Begin
    getVersion;
    LabelVersion.Caption := APP_VERSION;
  End;

procedure TSplashScreen.ButtonOkClick(Sender: TObject);
begin
    close;
end;

end.
