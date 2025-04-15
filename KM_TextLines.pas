unit KM_TextLines;
{$I KM_CompilerDirectives.inc}
interface
uses
  Vcl.Dialogs, System.Generics.Collections,
  System.StrUtils, System.SysUtils;


type
  TByteSet = set of Byte;

  // Maybe slower, but it's much simpler to manage Classes (when there are strings in them)
  TKMLine = class
  private const
    ID_SPACER = -1;
  public class var
    LOCALE_COUNT: Integer;
    LOCALE_DEFAULT: Integer;
  public
    Id: Integer; // Unique                           // Used by mission libx files
    Tag: string; // Unique                           // Used by game consts
    Strings: array {locale} of string;               // All localization strings
    LastChanged: array {locale} of TDateTime;
    Description: string;
    constructor CreateSpacer;
    constructor Create(aId: Integer); overload;
    constructor Create(aId: Integer; aTag: string); overload;

    procedure Autoname(const aPath: string);
    function IsSpacer: Boolean;
    function GetLineForGameConst: string;
    function GetLineForLibx(aLoc: Integer): string;
    function GetLastChanged(aLoc: Integer): string;
    procedure SetLastChanged(aLoc: Integer; aLastChanged: string);
    function CheckMatchingForCharCount(const aSub: array of string; aLocales: TByteSet): Boolean;

    function HasDuplicates(aSelectedLocales: TByteSet): Boolean;
    function HasEmptyTexts(aSelectedLocales: TByteSet): Boolean;
    function HasTextFilter(const aSub: string; aSelectedLocales: TByteSet): Boolean;
    function HasTagIdFilter(const aFilter: string): Boolean;
    function HasTagNameFilter(const aSub: string): Boolean;
  end;

  // For the sake of SRP, this is only a list of lines. No locales, no libType, etc
  TKMLines = class(TList<TKMLine>)
  private const
    MAX_ALLOWED_ID = 4096;
    SAME_TEXT = '<==';
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
    procedure LoadGameConsts(const aFilename: string);
    procedure SaveGameConsts(const aFilename: string);

    procedure LoadLibx(const aFilename: string; aLocaleId: Integer);
    procedure LoadLibxKP(const aFilename: string; aLocaleId: Integer);
    procedure SaveLibx(const aFilename: string; aLocaleId: Integer; aForceSort, aSortById: Boolean);
    procedure SaveLibxKP(const aFilename: string; aLocaleId: Integer; aAddComment: Boolean);
  end;


implementation
uses
  System.Classes, System.DateUtils, System.Math,
  KromNestedLibrary;


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


function TKMLine.GetLineForGameConst: string;
var
  txt: string;
begin
  if IsSpacer then Exit('');

  // Output format:
  // CONST_TAG = 123; //English line

  // Escape the EOL same as we do for the libx for consistency
  txt := Strings[LOCALE_DEFAULT];
  txt := StringReplace(txt, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  txt := StringReplace(txt, sLineBreak, '\n', [rfReplaceAll, rfIgnoreCase]);

  // Append english text for easier lookup from code
  Result := Tag + ' = ' + IntToStr(Id) + '; //' + txt;
end;


function TKMLine.GetLineForLibx(aLoc: Integer): string;
begin
  // Output format:
  // 123:Localized line

  Result := IntToStr(Id) + ':' + Strings[aLoc];
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


procedure TKMLines.SaveGameConsts(const aFilename: string);
var
  myFile: TextFile;
  I: Integer;
begin
  AssignFile(myFile, aFilename);
  Rewrite(myFile);

  for I := 0 to Count - 1 do
    WriteLn(myFile, Items[I].GetLineForGameConst);

  CloseFile(myFile);
end;


procedure TKMLines.LoadGameConsts(const aFilename: string);
var
  sl: TStringList;
  newLine: string;
  I, K, delimiterPos, commentPos: Integer;
  id: Integer;
  tagName: string;
  prevIndex: Integer;
begin
  if not FileExists(aFilename) then
    Exit;

  sl := TStringList.Create;
  sl.LoadFromFile(aFilename);

  prevIndex := -1;
  for I := 0 to sl.Count - 1 do
  begin
    newLine := Trim(sl[I]);

    delimiterPos := Pos(' = ', newLine);
    // Separator (newLine without ' = ')
    if delimiterPos = 0 then
    begin
      if prevIndex <> -1 then
        Insert(prevIndex+1, TKMLine.CreateSpacer);
    end
    else
    begin
      commentPos := Pos('; //', newLine);
      if commentPos = 0 then
        id := StrToInt(Copy(newLine, delimiterPos + 3, Length(newLine) - delimiterPos - 3))
      else
        id := StrToInt(Copy(newLine, delimiterPos + 3, commentPos - delimiterPos - 3));
      tagName := Copy(newLine, 1, delimiterPos - 1);

      prevIndex := AddLine(TKMLine.Create(id, tagName));
    end;
  end;

  // Ensure there are no duplicates, because that's a very bad situation
  for I := 0 to Count - 1 do
    for K := I+1 to Count - 1 do
      if not Items[I].IsSpacer then
      begin
        if Items[I].Id = Items[K].Id then
          ShowMessage('Error: Two lines have the same Id!' + sLineBreak + Items[I].Tag + ' & ' + Items[K].Tag + ' = ' + IntToStr(Items[I].Id));
        if Items[I].Tag = Items[K].Tag then
          ShowMessage('Error: Two lines have the same Tag!' + sLineBreak + Items[I].Tag);
      end;

  sl.Free;
end;


procedure TKMLines.LoadLibx(const aFilename: string; aLocaleId: Integer);
var
  sl: TStringList;
  delimiterPos: Integer;
  I: Integer;
  id: Integer;
  newLine: string;
begin
  if not FileExists(aFilename) then Exit;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(aFilename);

    for I := 0 to sl.Count - 1 do
    begin
      newLine := Trim(sl[I]);

      delimiterPos := Pos(':', newLine);

      // If there's no delimiter we can not extract anything anyway (but we could insert spacer or comment?)
      if delimiterPos = 0 then Continue;

      if not TryStrToInt(TrimLeft(LeftStr(newLine, delimiterPos-1)), id) then Continue;

      newLine := RightStr(newLine, Length(newLine) - delimiterPos);
      // Required characters that can't be stored in plain text
      newLine := StringReplace(newLine, '\n', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      newLine := StringReplace(newLine, '\\', '\', [rfReplaceAll, rfIgnoreCase]);

      Assert(id <= MAX_ALLOWED_ID, Format('Dont allow ids higher than %d for no reason', [MAX_ALLOWED_ID]));

      AddOrAppendString(id, aLocaleId, newLine);
    end;
  finally
    sl.Free;
  end;
end;


procedure TKMLines.LoadLibxKP(const aFilename: string; aLocaleId: Integer);
var
  sa: TArray<TKMPair>;
begin
  if not FileExists(aFilename) then Exit;

  var nl := TKMNestedLibrary.Create;
  try
    var sl := TStringList.Create;
    try
      sl.LoadFromFile(aFilename);
      nl.LoadFromString(sl.Text);
    finally
      FreeAndNil(sl);
    end;

    sa := nl.ToArray;
  finally
    nl.Free;
  end;

  for var I := 0 to High(sa) do
  begin
    var id: Integer;
    if fTagToIdLookup.ContainsKey(sa[I].Name) then
      id := fTagToIdLookup[sa[I].Name]
    else
      id := AddLine(TKMLine.Create(I, sa[I].Name));

    AddOrAppendString(id, aLocaleId, sa[I].Value);
  end;
end;


procedure TKMLines.SaveLibx(const aFilename: string; aLocaleId: Integer; aForceSort, aSortById: Boolean);
var
  sl: TStringList;
  I: Integer;
  localeHasStrings: Boolean;
  sortedLines: array of Integer;
begin
  // We want to sort lines by Id only for the save, We do not want to change their order in TM
  SetLength(sortedLines, MAX_ALLOWED_ID+1);
  FillChar(sortedLines[0], Length(sortedLines) * SizeOf(sortedLines[0]), -1);
  for I := 0 to Count - 1 do
  begin
    if (not Items[I].IsSpacer and (Items[I].Strings[aLocaleId] <> ''))
    or aForceSort then
      if aSortById then
        sortedLines[Items[I].Id] := I
      else
        sortedLines[I] := I;
  end;

  sl := TStringList.Create;
  try
    sl.DefaultEncoding := TEncoding.UTF8;

    localeHasStrings := False;
    for I := 0 to High(sortedLines) do
    if sortedLines[I] <> -1 then
    begin
      sl.Append(Items[sortedLines[I]].GetLineForLibx(aLocaleId));

      if Items[sortedLines[I]].Strings[aLocaleId] <> '' then
        localeHasStrings := True;
    end;

    if localeHasStrings then
      sl.SaveToFile(aFilename);
  finally
    sl.Free;
  end;
end;


procedure TKMLines.SaveLibxKP(const aFilename: string; aLocaleId: Integer; aAddComment: Boolean);
begin
  var nl := TKMNestedLibrary.Create;

  if aAddComment then
    nl.Root.Comment := 'Knights Province localization file' + sLineBreak +
                       'Padding is purely visual thing. Format does not care about it';

  try
    var localeHasStrings := False;

    for var I := 0 to Count - 1 do
      if (not Items[I].IsSpacer and (Items[I].Tag <> '')) then
      begin
        nl.Root.AddNode(Items[I].Tag, Items[I].Strings[aLocaleId]);

        if (Items[I].Strings[aLocaleId] <> '') and (Items[I].Strings[aLocaleId] <> SAME_TEXT) then
          localeHasStrings := True;
      end;

    if not localeHasStrings then
      Exit;

    var sl := TStringList.Create;
    try
      sl.DefaultEncoding := TEncoding.UTF8;
      sl.Text := nl.SaveToString;
      sl.SaveToFile(aFilename);
    finally
      sl.Free;
    end;
  finally
    nl.Free;
  end;
end;


end.
