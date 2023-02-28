unit KM_TextLines;
interface
uses
  Generics.Collections,
  StrUtils, SysUtils;


type
  TByteSet = set of Byte;

  // Maybe slower, but it's much simpler to manage classes (when there are strings in them)
  TKMLine = class
  private const
    ID_SPACER = -1;
  public class var
    LOCALE_COUNT: Integer;
    LOCALE_DEFAULT: Integer;
  public
    Id: Integer; // Unique
    Tag: string; // Unique
    Strings: array {locale} of string;
    LastChanged: array {locale} of TDateTime;
    Description: string;
    constructor CreateSpacer;
    constructor Create(aId: Integer); overload;
    constructor Create(aId: Integer; aTag: string); overload;
    procedure Autoname(const aPath: string);
    function IsSpacer: Boolean;
    function GetLineForConst: string;
    function GetLineForLibx(aLoc: Integer): string;
    function GetLineForDict(aLoc: Integer): string;
    function GetLastChanged(aLoc: Integer): string;
    procedure SetLastChanged(aLoc: Integer; aLastChanged: string);
    function CheckMatchingForCharCount(const aSub: array of string; aLocales: TByteSet): Boolean;

    function HasDuplicates(aSelectedLocales: TByteSet): Boolean;
    function HasEmptyTexts(aSelectedLocales: TByteSet): Boolean;
    function HasTextFilter(const aSub: string; aSelectedLocales: TByteSet): Boolean;
    function HasTagIdFilter(const aFilter: string): Boolean;
    function HasTagNameFilter(const aSub: string): Boolean;
  end;

  TKMLines = class(TList<TKMLine>)
  private
    fTagToIdLookup: TDictionary<string,Integer>;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOfId(aId: Integer): Integer;
    function IndexOfTag(const aTag: string): Integer;
    function AddLine(aLine: TKMLine): Integer;
    procedure AddOrAppendString(aId: Integer; aLocale: Integer; aString: string);
    procedure Clear; reintroduce;
    procedure TagsAutoName(const aPath: string);
  end;


implementation
uses
  Classes, DateUtils, Math;


// Custom S -> DT -> S functions that read/write into reliable format quickly
function KMDateTimeToString(const aDateTime: TDateTime): string;
var
  y, m, d, h, n, s, ms: Word;
begin
  DecodeDateTime(aDateTime, y, m, d, h, n, s, ms);

  // Simplest readable format is: yyyy-mm-dd hh-nn-ss
  // Loosely based on ISO8601 and RFC3339

  Result := IntToStr(y) + '-' + IfThen(m < 10, '0') + IntToStr(m) + '-' + IfThen(d < 10, '0') + IntToStr(d) + ' ' +
            IfThen(h < 10, '0') + IntToStr(h) + '-' + IfThen(n < 10, '0') + IntToStr(n) + '-' + IfThen(s < 10, '0') + IntToStr(s);
end;


function KMStringToDateTime(const aString: string): TDateTime;
var
  y, m, d, h, n, s: Word;
begin
  // Format is set by KMDateTimeToString to be: yyyy-mm-dd hh-nn-ss

  y := StrToInt(Copy(aString, 1, 4));
  m := StrToInt(Copy(aString, 6, 2));
  d := StrToInt(Copy(aString, 9, 2));
  h := StrToInt(Copy(aString, 12, 2));
  n := StrToInt(Copy(aString, 15, 2));
  s := StrToInt(Copy(aString, 18, 2));

  Result := EncodeDateTime(y, m, d, h, n, s, 0);
end;


function ArrayContains(aValue: Integer; const aArray: TArray<Integer>): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(aArray) to High(aArray) do
    if aValue = aArray[I] then
      Exit(True);
end;


function ParseRange(const aString: string; out aFrom, aTo: Integer): Boolean;
var
  p: Integer;
  sFrom, sTo: String;
begin
  Result := False;
  aFrom := -1;
  aTo := -1;
  p := Pos('-', aString);
  if p > 0 then
  begin
    sFrom := Copy(aString, 1, p - 1);
    sTo := Copy(aString, p + 1, Length(aString) - p);
    if Trim(sFrom) = '' then // allow '-100' range: all IDs <= 100
      sFrom := '0';

    if Trim(sTo) = '' then // allow '100-' range: all IDs >= 100
      sTo := IntToStr(MaxInt);

    if TryStrToInt(Trim(sFrom), aFrom)
    and TryStrToInt(Trim(sTo), aTo) then
      Result := True;
  end;
end;


function ParseList(const aString: string; out aIdArray: TArray<Integer>): Boolean;
var
  I, newValue: Integer;
  sl: TStringList;
begin
  Result := False;
  sl := TStringList.Create;
  try
    // Treat all possible separators
    ExtractStrings([';', ',', ' '], [], PWideChar(WideString(aString)), sl);
    for I := 0 to sl.Count - 1 do
    if TryStrToInt(Trim(sl[I]), newValue) then
    begin
      SetLength(aIdArray, Length(aIdArray) + 1);
      aIdArray[Length(aIdArray) - 1] := newValue;
      Result := True;
    end;
  finally
    sl.Free;
  end;
end;


{ TKMLine }
constructor TKMLine.Create(aId: Integer; aTag: string);
begin
  inherited Create;

  Id := aId;
  Tag := aTag;
  SetLength(Strings, LOCALE_COUNT);
  SetLength(LastChanged, LOCALE_COUNT);
end;


constructor TKMLine.Create(aId: Integer);
begin
  inherited Create;

  Id := aId;
  SetLength(Strings, LOCALE_COUNT);
  SetLength(LastChanged, LOCALE_COUNT);
end;


constructor TKMLine.CreateSpacer;
begin
  inherited Create;

  Id := ID_SPACER;
end;


procedure TKMLine.Autoname(const aPath: string);
var
  txt: string;
begin
  if LOCALE_DEFAULT <= High(Strings) then
    txt := Strings[LOCALE_DEFAULT]
  else
    txt := Trim(aPath);

  txt := StringReplace(txt, '.%s.libx', '', [rfReplaceAll]);
  txt := StringReplace(txt, '\', '_', [rfReplaceAll]);
  txt := StringReplace(txt, '.', '_', [rfReplaceAll]);
  txt := StringReplace(txt, ' ', '_', [rfReplaceAll]);

  // All tags must be unique, thats why we need to add ID
  Tag := 'TX_' + IntToStr(Id) + '_' + UpperCase(LeftStr(txt, 32));
end;


function TKMLine.IsSpacer: Boolean;
begin
  Result := Id = ID_SPACER;
end;


function TKMLine.CheckMatchingForCharCount(const aSub: array of string; aLocales: TByteSet): Boolean;
var
  I, localizationsCount: Integer;
  charCount: array {locale} of Integer;
  K: Integer;
begin
  Result := True;

  for K := Low(aSub) to High(aSub) do
  begin
    SetLength(charCount, Length(Strings));

    localizationsCount := 0;
    for I := 0 to High(Strings) do
    if (aLocales = []) or (I in aLocales) then
    if Length(Strings[I]) > 0 then
    begin
      charCount[localizationsCount] := Length(Strings[I]) - Length(StringReplace(Strings[I], aSub[K], '', [rfReplaceAll]));

      // Some localizations could be missing, so we count each line separately
      Inc(localizationsCount);
    end;

    // We can compare adjucent pairs - if there's a mismatch it will show up
    for I := 0 to localizationsCount - 2 do
    if charCount[I] <> charCount[I+1] then
      Exit(False);
  end;
end;


function TKMLine.GetLastChanged(aLoc: Integer): string;
begin
  Result := KMDateTimeToString(LastChanged[aLoc]);
end;


procedure TKMLine.SetLastChanged(aLoc: Integer; aLastChanged: string);
begin
  LastChanged[aLoc] := KMStringToDateTime(aLastChanged);
end;


function TKMLine.GetLineForConst: string;
begin
  if IsSpacer then Exit('');

  Result := Strings[LOCALE_DEFAULT];
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '\n', [rfReplaceAll, rfIgnoreCase]);

  // Append english text for easier lookup from code
  Result := Tag + ' = ' + IntToStr(Id) + '; //' + Result;
end;


function TKMLine.GetLineForLibx(aLoc: Integer): string;
begin
  Result := IntToStr(Id) + ':' + Strings[aLoc];
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '\n', [rfReplaceAll, rfIgnoreCase]);
end;


function TKMLine.GetLineForDict(aLoc: Integer): string;
begin
  Result := Tag + ':' + Strings[aLoc];
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '\n', [rfReplaceAll, rfIgnoreCase]);
end;


function TKMLine.HasDuplicates(aSelectedLocales: TByteSet): Boolean;
var
  I, K: Integer;
begin
  Result := False;
  if not IsSpacer then
    for I := 0 to LOCALE_COUNT - 1 do
      if (I in aSelectedLocales) and (Strings[I] <> '') then // Empty strings are not interesting to see in terms of duplicates
        for K := 0 to LOCALE_COUNT - 1 do
          if (K <> I) and (K in aSelectedLocales) and (Strings[K] <> '') then
            if Strings[I] = Strings[K] then
              Exit(True);
end;


function TKMLine.HasEmptyTexts(aSelectedLocales: TByteSet): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not IsSpacer then
    for I := 0 to LOCALE_COUNT - 1 do
    if I in aSelectedLocales then
      if Strings[I] = '' then
        Exit(True);
end;


function TKMLine.HasTextFilter(const aSub: string; aSelectedLocales: TByteSet): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not IsSpacer then
    for I := 0 to LOCALE_COUNT - 1 do
    if I in aSelectedLocales then
      if (Pos(UpperCase(aSub), UpperCase(Strings[I])) <> 0) then
        Exit(True);
end;


function TKMLine.HasTagIdFilter(const aFilter: string): Boolean;
var
  idOne: Integer;
  idFrom, idTo: Integer;
  idArray: TArray<Integer>;
begin
  if IsSpacer then Exit(False);

  Result := (TryStrToInt(aFilter, idOne) and (Id = idOne))
            or (ParseRange(aFilter, idFrom, idTo) and InRange(Id, idFrom, idTo))
            or (ParseList(aFilter, idArray) and ArrayContains(Id, idArray));
end;


// Cutting corners here, we check wildcard only on first/last place
function TKMLine.HasTagNameFilter(const aSub: string): Boolean;
var
  subTag: string;
  wildcardCount: Integer;
begin
  if aSub = '' then Exit(True);

  subTag := UpperCase(aSub);
  wildcardCount := Length(subTag) - Length(ReplaceStr(subTag, '*', ''));

  if (wildcardCount = 1) and StartsText('*', subTag) then
    Result := EndsText(ReplaceStr(subTag, '*', ''), UpperCase(Tag))
  else
  if (wildcardCount = 1) and EndsText('*', subTag) then
    Result := StartsText(ReplaceStr(subTag, '*', ''), UpperCase(Tag))
  else
    // No wildcards or more than 1 wildcard or wildcard in the middle - do the normal matching
    Result := Pos(subTag, UpperCase(Tag)) <> 0;
end;


{ TKMLines }
procedure TKMLines.Clear;
begin
  inherited Clear;

  fTagToIdLookup.Clear;
end;


constructor TKMLines.Create;
begin
  inherited;

  fTagToIdLookup := TDictionary<string,Integer>.Create;
end;


destructor TKMLines.Destroy;
begin
  fTagToIdLookup.Free;

  inherited;
end;


function TKMLines.AddLine(aLine: TKMLine): Integer;
begin
  Result := Add(aLine);

  Assert(not fTagToIdLookup.ContainsKey(aLine.Tag), Format('Lookup dictionary already contains key "%s"', [aLine.Tag]));

  // Save to dictionary for faster lookup
  fTagToIdLookup.Add(aLine.Tag, Result);
end;


procedure TKMLines.TagsAutoName(const aPath: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  if not Items[I].IsSpacer then
  begin
    Items[I].Autoname(aPath);

    Assert(not fTagToIdLookup.ContainsKey(Items[I].Tag), Format('Lookup dictionary already contains key "%s"', [Items[I].Tag]));

    // Save to dictionary for faster lookup
    fTagToIdLookup.Add(Items[I].Tag, I);
  end;
end;


procedure TKMLines.AddOrAppendString(aId, aLocale: Integer; aString: string);
var
  I: Integer;
begin
  I := IndexOfId(aId);

  if I = -1 then
    I := Add(TKMLine.Create(aId));

  Items[I].Strings[aLocale] := aString;
end;


function TKMLines.IndexOfId(aId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Id = aId then
      Exit(I);
end;


function TKMLines.IndexOfTag(const aTag: string): Integer;
begin
  if fTagToIdLookup.ContainsKey(aTag) then
    Result := fTagToIdLookup[aTag]
  else
    Result := -1;
end;


end.
