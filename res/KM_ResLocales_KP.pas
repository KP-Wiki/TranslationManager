unit KM_ResLocales_KP;
// Mirror copy from KP
interface
uses
  System.Math, System.SysUtils, System.StrUtils,
  KM_IoXML;


type
  TKMLocaleSpec = record
  public
    LocaleEnabled: Boolean;
    Code: string;             // 3-letter code: 'eng', 'rus'
    Title: string;            // Full name: 'English', 'Russian'
    FlagSpriteID: Integer;
    FallbackLocale: string;   // Locale to use if this one is empty. English is universal 2nd fallback locale
    TranslatorCredit: string; // Who did the translation
    procedure LoadFromXml(aNode: TXMLNode);
  end;

  TKMResLocales = class
  private
    fPath: string;
    fCount: Integer;
    fLocaleList: array of TKMLocaleSpec;
    function GetLocaleByIndex(aIndex: Integer): TKMLocaleSpec;
    procedure Init;
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
  LocaleEnabled := aNode.Attributes['LocaleEnabled'].AsBoolean(True);
  Code := aNode.Attributes['Code'].AsString;
  Title := aNode.Attributes['Title'].AsString;
  FlagSpriteID := aNode.Attributes['FlagSpriteID'].AsInteger(0);
  FallbackLocale := aNode.Attributes['FallbackLocale'].AsString;
  TranslatorCredit := aNode.Attributes['TranslatorCredit'].AsString;
end;


{ TKMResLocales }
constructor TKMResLocales.Create(const aPath: string);
begin
  inherited Create;

  fPath := aPath;

  Init;
end;


procedure TKMResLocales.Init;
var
  newXML: TKMXMLDocument;
  nRoot, nLocale: TXMLNode;
  I: Integer;
begin
  newXML := TKMXMLDocument.Create;
  newXML.LoadFromFile(fPath + 'locales.xml');

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
