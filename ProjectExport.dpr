program ProjectExport;

uses
  Forms,
  UnitVariables in 'UnitVariables.pas',
  UnitExport in 'UnitExport.pas' {Export},
  UnitListSelect in 'UnitListSelect.pas' {FormListSelect},
  UnitSplashScreen in 'UnitSplashScreen.pas' {SplashScreen},
  UnitFormSelectFiles in 'UnitFormSelectFiles.pas' {FormSelectFiles},
  UnitConfiguration in 'UnitConfiguration.pas' {FormConfiguration};

{$R *.res}

begin
  Application.Initialize;
  SplashScreen := TSplashScreen.Create(nil) ;
  SplashScreen.Show;
  SplashScreen.Update;
  Application.Title := 'CsvToHydrasMis';
  Application.CreateForm(TExport, Export);
  Application.CreateForm(TFormConfiguration, FormConfiguration);
  SplashScreen.Hide;
  SplashScreen.Free;
  Application.Run;
End.
