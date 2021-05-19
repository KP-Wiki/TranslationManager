program TranslationManager;
{$I ..\..\KM_CompilerDirectives.inc}
uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  Forms,
  UnitTranslationManager in 'UnitTranslationManager.pas' {Form1},
  KM_ResLocales in '..\..\src\res\KM_ResLocales.pas',
  KM_TextManager in 'KM_TextManager.pas',
  Unit_PathManager in 'Unit_PathManager.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
