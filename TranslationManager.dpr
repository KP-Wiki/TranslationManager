program TranslationManager;
uses
  Vcl.Forms,

  UnitTranslationManager in 'UnitTranslationManager.pas' {fmTranslationManager},

  KromNestedLibrary in '3rdparty\KromNestedLibrary.pas',
  KromStringUtils in '3rdparty\KromStringUtils.pas',
  KromUtils in '3rdparty\KromUtils.pas',
  SciZipFile in '3rdparty\SciZipFile.pas',
  Zippit in '3rdparty\Zippit.pas',
  KM_IoXml in 'common\KM_IoXml.pas',
  Xml.VerySimple in 'common\Xml.VerySimple.pas',

  KM_LibxFinder in 'KM_LibxFinder.pas',
  KM_TextLines in 'KM_TextLines.pas',
  KM_TextManager in 'KM_TextManager.pas',

  KM_ResLocales in 'res\KM_ResLocales.pas',
  KM_ResLocales_KMR in 'res\KM_ResLocales_KMR.pas',
  KM_ResLocales_KP in 'res\KM_ResLocales_KP.pas';

{$R *.res}

var
  fmTranslationManager: TfmTranslationManager;

begin
  Application.Initialize;
  Application.CreateForm(TfmTranslationManager, fmTranslationManager);
  if not fmTranslationManager.Start then
    Exit;

  Application.Run;
end.
