unit KM_ResLocales;
{$I KM_CompilerDirectives.inc}
interface
uses
  Math, SysUtils, StrUtils,
  KM_IoXML;


type
  TKMLocaleSpec = record
  public
    Code: string;             // 3-letter code: 'eng', 'rus'
    Title: string;            // Full name: 'English', 'Russian'
    FlagSpriteID: Integer;
    FallbackLocale: string;   // Locale to use if this one is empty. English is universal 2nd fallback locale
    TranslatorCredit: string; // Who did the translation
    procedure LoadFromXml(aNode: TXMLNode);
  end;

  TKMResLocales = class
  private
    fCount: Integer;
    fLocaleList: array of TKMLocaleSpec;
    procedure LoadLocales(const aFilename: string);
    function GetLocaleByIndex(aIndex: Integer): TKMLocaleSpec;
  public const
    DEFAULT_LOCALE: string = 'eng';

    constructor Create(const aPath: string);
    property Count: Integer read fCount;
    property Locales[aIndex: Integer]: TKMLocaleSpec read GetLocaleByIndex; default;
    function IndexByCode(const aLocaleCode: string): Integer;
    function LocaleByCode(const aCode: string): TKMLocaleSpec;
    function GetTranslatorCredits: string;

    function GetFallback(const aLocaleCode: string): string;
  end;

var
  gResLocales: TKMResLocales;

implementation


{ TKMLocaleSpec }
procedure TKMLocaleSpec.LoadFromXml(aNode: TXMLNode);
begin
  Code := aNode.Attributes['Code'].AsString;
  Title := aNode.Attributes['Title'].AsString;
  FlagSpriteID := aNode.Attributes['FlagSpriteID'].AsInteger(0);
  FallbackLocale := aNode.Attributes['FallbackLocale'].AsString;
  TranslatorCredit := aNode.Attributes['TranslatorCredit'].AsString;
end;


{ TKMResLocales }
// aPath - Path to locales info file, usually \data\text\locales.xml
constructor TKMResLocales.Create(const aPath: string);
begin
  inherited Create;

  LoadLocales(aPath);
end;


procedure TKMResLocales.LoadLocales(const aFilename: string);
var
  newXML: TKMXMLDocument;
  nRoot, nLocale: TXMLNode;
  I: Integer;
begin
  newXML := TKMXMLDocument.Create;
  newXML.LoadFromFile(aFilename);

  nRoot := newXML.Root;

  fCount := nRoot.ChildNodes.Count;

  SetLength(fLocaleList, fCount);
  for I := 0 to nRoot.ChildNodes.Count - 1 do
  begin
    nLocale := nRoot.ChildNodes[I];

    fLocaleList[I].LoadFromXml(nLocale);
  end;

  newXML.Free;
end;


// Fetch fallback locale. Let caller handle if it's missing
function TKMResLocales.GetFallback(const aLocaleCode: string): string;
var
  I: Integer;
begin
  Result := '';
  I := IndexByCode(aLocaleCode);
  if I <> -1 then
    Result := Locales[I].FallbackLocale;
end;


function TKMResLocales.GetLocaleByIndex(aIndex: Integer): TKMLocaleSpec;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fLocaleList[aIndex];
end;


function TKMResLocales.LocaleByCode(const aCode: string): TKMLocaleSpec;
var
  I: Integer;
begin
  Result := default(TKMLocaleSpec);

  for I := 0 to fCount - 1 do
    if fLocaleList[I].Code = aCode then
      Exit(fLocaleList[I]);

  raise Exception.Create(aCode + ' is not a valid Locale');
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


function TKMResLocales.GetTranslatorCredits: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if (fLocaleList[I].TranslatorCredit <> '') and (fLocaleList[I].Code <> DEFAULT_LOCALE) then
      Result := Result + IfThen(Result <> '', '|') + fLocaleList[I].Title + ' - ' + fLocaleList[I].TranslatorCredit;
end;


end.
