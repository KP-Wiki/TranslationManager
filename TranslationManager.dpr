program TranslationManager;
uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  Forms,

  UnitTranslationManager in 'UnitTranslationManager.pas' {Form1},

  KromStringUtils in '3rdparty\KromStringUtils.pas',
  KromUtils in '3rdparty\KromUtils.pas',
  KM_CommonTypes in 'common\KM_CommonTypes.pas',
  KM_IoXml in 'common\KM_IoXml.pas',
  Xml.VerySimple in 'common\Xml.VerySimple.pas',
  KM_ResLocales in 'res\KM_ResLocales.pas',

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
