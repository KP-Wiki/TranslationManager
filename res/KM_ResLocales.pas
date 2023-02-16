unit KM_ResLocales;
{$I KM_CompilerDirectives.inc}
interface
uses
  SysUtils,
  KM_ResLocales_KMR,
  KM_ResLocales_KP,
  KM_IoXML;


type
  TKMLocaleSpec = record
  public
    Code: string;             // 3-letter code: 'eng', 'rus'
    Title: string;            // Full name: 'English', 'Russian'
  end;

// Minimal interface required by TranslationManager
  TKMResLocales = class
  private
    fCount: Integer;
    fLocaleList: array of TKMLocaleSpec;
    function GetLocaleByIndex(aIndex: Integer): TKMLocaleSpec;
  public const
    DEFAULT_LOCALE: string = 'eng';

    constructor Create(const aPath, aTag: string);
    property Count: Integer read fCount;
    property Locales[aIndex: Integer]: TKMLocaleSpec read GetLocaleByIndex; default;
    function IndexByCode(const aLocaleCode: string): Integer;
  end;


implementation


{ TKMResLocales }
constructor TKMResLocales.Create(const aPath, aTag: string);
var
  resLocalesKMR: KM_ResLocales_KMR.TKMResLocales;
  resLocalesKP: KM_ResLocales_KP.TKMResLocales;
  I: Integer;
begin
  inherited Create;

  if aTag = 'KMR' then
  begin
    resLocalesKMR := KM_ResLocales_KMR.TKMResLocales.Create(aPath, DEFAULT_LOCALE);

    fCount := resLocalesKMR.Count;
    SetLength(fLocaleList, fCount);
    for I := 0 to fCount - 1 do
    begin
      fLocaleList[I].Code := resLocalesKMR[I].Code;
      fLocaleList[I].Title := resLocalesKMR[I].Title;
    end;

    resLocalesKMR.Free;
  end else
  if aTag = 'KP' then
  begin
    // KP.TKMResLocales needs just a path. Filename "locales.xml" is hardcoded inside
    resLocalesKP := KM_ResLocales_KP.TKMResLocales.Create(ExtractFilePath(aPath));

    fCount := resLocalesKP.Count;
    SetLength(fLocaleList, fCount);
    for I := 0 to fCount - 1 do
    begin
      fLocaleList[I].Code := resLocalesKP[I].Code;
      fLocaleList[I].Title := resLocalesKP[I].Title;
    end;

    resLocalesKP.Free;
  end else
    raise Exception.Create('Bad tag');
end;


function TKMResLocales.GetLocaleByIndex(aIndex: Integer): TKMLocaleSpec;
begin
  Result := fLocaleList[aIndex];
end;


function TKMResLocales.IndexByCode(const aLocaleCode: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if fLocaleList[I].Code = aLocaleCode then
      Exit(I);
end;


end.
