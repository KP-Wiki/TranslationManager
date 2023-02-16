unit KM_TextManager;
interface
uses
  Classes, Dialogs, Math, Vcl.Clipbrd, Generics.Defaults,
  StrUtils, SysUtils, Types,
  KM_ResLocales,
  KM_TextLines;


type
  TKMLibraryType = (ltGame, ltMissions);
  TKMClipboardExport = (
    ceSimple,     // Just columns with locales
    ceLastChanged // Include LastChanged for each locale
  );

  TKMTextManager = class
  private const
    COL_NAME_TAG = 'Tag';
    COL_NAME_NEEDS_UPDATE = 'Needs update';
    COL_NAME_DESC = 'Description';
  private class var
    LOCALE_DEFAULT: Integer;
  private
    fLocales: TKMResLocales;
    fWorkDir: string;

    fTextPath: string;
    fTagsPath: string; // We use consts only for ingame library, others don't need them
    fMetaPath: string; // We use meta only for ingame library, others don't need it

    fLines: TKMLines;
    fLibType: TKMLibraryType;

    // For copy/paste
    fBuffer: array of string;

    fHasChanges: Boolean;

    function GetCount: Integer;
    function GetItem(aIndex: Integer): TKMLine;
    procedure LoadTags(const aFilename: string);
    procedure LoadLibx(const aFilename: string; aLocaleId: Integer);
    procedure LoadMeta(const aFilename: string);
    procedure SaveTags(const aFilename: string);
    procedure SaveLibx(const aFilename: string; aLocaleId: Integer);
    procedure SaveDict(const aFilename: string; aLocaleId: Integer);
    procedure SaveMeta(const aFilename: string);
    procedure TagsAutoName(const aPath: string);
    function ToClipboardHeader(aLocales: TByteSet; aExport: TKMClipboardExport): string;
    function ToClipboardBody(aLocales: TByteSet; aExport: TKMClipboardExport): string;
    procedure ClipboardColumnsToLocIndex(aLine: string; out aLocIndex: TArray<Integer>);
  public const
    // Path matches for KMR and KP
    GAME_TEXT_PATH = 'data\text\text.%s.libx';

    constructor Create(aLocales: TKMResLocales; const aWorkDir: string);
    destructor Destroy; override;

    procedure Load4(const aTextPath, aTagsPath, aMetaPath: string; aLocales: TByteSet);
    procedure Save;

    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TKMLine read GetItem; default;
    property HasChanges: Boolean read fHasChanges write fHasChanges;

    procedure TextCopy(aIndex: Integer);
    procedure TextPaste(aIndex: Integer);
    procedure EraseAllStringsButEng(aIndex: Integer);

    procedure Delete(aIndex: Integer);
    procedure Insert(aIndex: Integer);
    procedure InsertSeparator(aIndex: Integer);
    procedure MoveUp(aIndex: Integer);
    procedure MoveDown(aIndex: Integer);
    procedure SortByIndex;
    procedure SortByTag;
    procedure CompactIndexes;

    procedure ToClipboard(aLocales: TByteSet; aExport: TKMClipboardExport);
    procedure ToClipboardAll(aList: TStringList; aLocales: TByteSet);
    procedure FromClipboard;
    procedure FromClipboardAll;

    procedure ListMismatchingAll(aFolders: TStringList; aList: TStringList);
  end;


const
  TAGS_PATH = 'data\text\text_IDs.inc';
  META_PATH = 'data\text\text_meta.xml';


implementation
uses
  KromStringUtils,
  KM_IoXml;


{ TKMTextManager }
constructor TKMTextManager.Create(aLocales: TKMResLocales; const aWorkDir: string);
begin
  inherited Create;

  fLines := TKMLines.Create;
  fLocales := aLocales;
  fWorkDir := aWorkDir;

  TKMLine.LOCALE_COUNT := fLocales.Count;
  LOCALE_DEFAULT := fLocales.IndexByCode(gResLocales.DEFAULT_LOCALE);
  TKMLine.LOCALE_DEFAULT := LOCALE_DEFAULT;
end;


destructor TKMTextManager.Destroy;
begin
  FreeAndNil(fLines);

  inherited;
end;


procedure TKMTextManager.Load4(const aTextPath, aTagsPath, aMetaPath: string; aLocales: TByteSet);
var
  I: Integer;
begin
  fTextPath := fWorkDir + aTextPath;
  fTagsPath := fWorkDir + aTagsPath;
  fMetaPath := fWorkDir + aMetaPath;

  fLines.Clear;

  // See if this is a game libx or a mission
  if Pos(GAME_TEXT_PATH, fTextPath) <> 0 then
    fLibType := ltGame
  else
    fLibType := ltMissions;

  // If we have consts - good, use them
  LoadTags(fTagsPath);

  for I := 0 to fLocales.Count - 1 do
    LoadLibx(Format(fTextPath, [fLocales[I].Code]), I);

  LoadMeta(fMetaPath);

  if fLibType = ltMissions then
    TagsAutoName(aTextPath); // Name tags just for UI, they wont be saved

  fHasChanges := False;
end;


procedure TKMTextManager.Save;
var
  I: Integer;
  fname: string;
begin
  for I := 0 to fLocales.Count - 1 do
  begin
    fname := Format(fTextPath, [fLocales[I].Code]);
    SaveLibx(fname, I);
    // Did not work out  SaveDict(ChangeFileExt(fname, '.dict'), I);
  end;

  if fLibType = ltGame then
  begin
    SaveTags(fTagsPath);
    SaveMeta(fMetaPath);
  end;

  fHasChanges := False;
end;


procedure TKMTextManager.SaveTags(const aFilename: string);
var
  myFile: TextFile;
  I: Integer;
begin
  AssignFile(myFile, aFilename);
  Rewrite(myFile);

  for I := 0 to fLines.Count - 1 do
    WriteLn(myFile, fLines[I].GetLineForConst);

  CloseFile(myFile);
end;


procedure TKMTextManager.LoadTags(const aFilename: string);
var
  sl: TStringList;
  Line: string;
  I, K, CenterPos, CommentPos: Integer;
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
    Line := Trim(sl[I]);

    CenterPos := Pos(' = ', Line);
    // Separator (line without ' = ')
    if CenterPos = 0 then
    begin
      if prevIndex <> -1 then
        fLines.Insert(prevIndex+1, TKMLine.CreateSpacer);
    end
    else
    begin
      CommentPos := Pos('; //', Line);
      if CommentPos = 0 then
        id := StrToInt(Copy(Line, CenterPos + 3, Length(Line) - CenterPos - 3))
      else
        id := StrToInt(Copy(Line, CenterPos + 3, CommentPos - CenterPos - 3));
      tagName := Copy(Line, 1, CenterPos - 1);

      prevIndex := fLines.AddLine(TKMLine.Create(id, tagName));
    end;
  end;

  // Ensure there are no duplicates, because that's a very bad situation
  for I := 0 to fLines.Count - 1 do
    for K := I+1 to fLines.Count - 1 do
      if not fLines[I].IsSpacer then
      begin
        if fLines[I].Id = fLines[K].Id then
          ShowMessage('Error: Two constants have the same ID!' + sLineBreak + fLines[I].Tag + ' & ' + fLines[K].Tag + ' = ' + IntToStr(fLines[I].Id));
        if fLines[I].Tag = fLines[K].Tag then
          ShowMessage('Error: Two constants have the same name!' + sLineBreak + fLines[I].Tag);
      end;

  sl.Free;
end;


procedure TKMTextManager.LoadLibx(const aFilename: string; aLocaleId: Integer);
var
  SL: TStringList;
  firstDelimiter: Integer;
  I: Integer;
  id: Integer;
  Line: string;
begin
  if not FileExists(aFilename) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(aFilename);

    for I := 0 to SL.Count - 1 do
    begin
      Line := Trim(SL[I]);

      firstDelimiter := Pos(':', Line);
      if firstDelimiter = 0 then Continue;
      if not TryStrToInt(TrimLeft(LeftStr(Line, firstDelimiter-1)), id) then Continue;

      Line := RightStr(Line, Length(Line) - firstDelimiter);
      // Required characters that can't be stored in plain text
      Line := StringReplace(Line, '\n', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      Line := StringReplace(Line, '\\', '\', [rfReplaceAll, rfIgnoreCase]);

      Assert(id <= 4096, 'Dont allow too many strings for no reason');

      fLines.AddOrAppendString(id, aLocaleId, Line);
    end;
  finally
    SL.Free;
  end;
end;


procedure TKMTextManager.LoadMeta(const aFilename: string);
var
  I, K, L: Integer;
  xml: TKMXMLDocument;
  nTags, nLine, nLastChanged, nDesc, nLastEng: TXmlNode;
  tagName: string;
  lastEng: string;
  tagExists: Boolean;
begin
  if not FileExists(aFilename) then Exit;

  xml := TKMXMLDocument.Create;
  try
    xml.LoadFromFile(aFilename);

    nTags := xml.Root.Find('Tags');

    for I := 0 to nTags.ChildNodes.Count - 1 do
    begin
      nLine := nTags.ChildNodes[I];
      tagName := nLine.Name;
      nLastChanged := nLine.Find('LastChanged');

      L := fLines.IndexOfTag(tagName);

      tagExists := L <> -1;
      if tagExists then
      begin
        if nLastChanged <> nil then
          for K := 0 to fLocales.Count - 1 do
            if nLastChanged.HasAttribute(fLocales[K].Code) then
              fLines[L].SetLastChanged(K, nLastChanged.Attributes[fLocales[K].Code].AsString);

        nDesc := nLine.Find('Description');
        fLines[L].Description := nDesc.Attributes['Value'].AsString;

        nLastEng := nLine.Find('LastEng');
        lastEng := nLastEng.Attributes['Value'].AsString;
        if lastEng <> fLines[L].Strings[LOCALE_DEFAULT] then
          fLines[L].LastChanged[LOCALE_DEFAULT] := Now;
      end;
    end;
  finally
    xml.Free;
  end;
end;


procedure TKMTextManager.SaveMeta(const aFilename: string);
var
  I, K: Integer;
  xml: TKMXMLDocument;
  nTags, nLine, nLastChanged, nDesc, nLastEng: TXmlNode;
begin
  xml := TKMXMLDocument.Create;
  try
    nTags := xml.Root.AddChild('Tags');

    for I := 0 to fLines.Count - 1 do
    if not fLines[I].IsSpacer then
    begin
      nLine := nTags.AddChild(fLines[I].Tag);

      nLastChanged := nLine.AddChild('LastChanged');
      for K := 0 to fLocales.Count - 1 do
        nLastChanged.Attributes[fLocales[K].Code] := fLines[I].GetLastChanged(K);

      nDesc := nLine.AddChild('Description');
      nDesc.Attributes['Value'] := fLines[I].Description;

      nLastEng := nLine.AddChild('LastEng');
      nLastEng.Attributes['Value'] := fLines[I].Strings[LOCALE_DEFAULT];
    end;

    xml.SaveToFile(aFilename);
  finally
    xml.Free;
  end;
end;


procedure TKMTextManager.SaveLibx(const aFilename: string; aLocaleId: Integer);
var
  sl: TStringList;
  I: Integer;
  localeHasStrings: Boolean;
begin
  sl := TStringList.Create;
  try
    sl.DefaultEncoding := TEncoding.UTF8;

    localeHasStrings := False;
    for I := 0 to fLines.Count - 1 do
    if (not fLines[I].IsSpacer and (fLines[I].Strings[aLocaleId] <> ''))
    or (fLibType = ltMissions) then
    begin
      sl.Append(fLines[I].GetLineForLibx(aLocaleId));

      if fLines[I].Strings[aLocaleId] <> '' then
        localeHasStrings := True;
    end;

    if localeHasStrings then
      sl.SaveToFile(aFilename);
  finally
    sl.Free;
  end;
end;


// Not sure if this is a better alternative, especially since with Sheets we dont rely on IDs anyway
procedure TKMTextManager.SaveDict(const aFilename: string; aLocaleId: Integer);
var
  sl: TStringList;
  I: Integer;
begin
  Assert(fLibType = ltGame);

  sl := TStringList.Create;
  try
    sl.DefaultEncoding := TEncoding.UTF8;

    for I := 0 to fLines.Count - 1 do
    if (not fLines[I].IsSpacer and (fLines[I].Strings[aLocaleId] <> '')) or (fLibType = ltMissions) then
      sl.Append(fLines[I].GetLineForDict(aLocaleId));

    sl.SaveToFile(aFilename);
  finally
    sl.Free;
  end;
end;


procedure TKMTextManager.SortByIndex;
begin
  fLines.Sort(TComparer<TKMLine>.Construct(
    function (const aLeft, aRight: TKMLine): Integer
    begin
      Result := CompareValue(aLeft.Id, aRight.Id);
    end));

  fHasChanges := True;
end;


procedure TKMTextManager.SortByTag;
begin
  fLines.Sort(TComparer<TKMLine>.Construct(
    function (const aLeft, aRight: TKMLine): Integer
    begin
      Result := CompareStr(aLeft.Tag, aRight.Tag);
    end));

  fHasChanges := True;
end;


// Our goal is to keep everything in place but update the Ids
procedure TKMTextManager.CompactIndexes;
const
  SPACING = 5; // Handy for adding new strings
var
  I, K: Integer;
begin
  if fLibType = ltMissions then Exit;

  K := 0;
  for I := 0 to fLines.Count - 1 do
  if not fLines[I].IsSpacer then
  begin
    fLines[I].Id := K;
    Inc(K);
  end else
    Inc(K, SPACING);

  fHasChanges := True;
end;


function TKMTextManager.GetCount: Integer;
begin
  Result := fLines.Count;
end;


function TKMTextManager.GetItem(aIndex: Integer): TKMLine;
begin
  Result := fLines[aIndex];
end;


procedure TKMTextManager.TagsAutoName(const aPath: string);
begin
  fLines.TagsAutoName(aPath);

  fHasChanges := True;
end;


procedure TKMTextManager.TextCopy(aIndex: Integer);
var
  I: Integer;
begin
  SetLength(fBuffer, fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fBuffer[I] := fLines[aIndex].Strings[I];
end;


procedure TKMTextManager.TextPaste(aIndex: Integer);
var
  I: Integer;
begin
  Assert(Length(fBuffer) = fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
    fLines[aIndex].Strings[I] := fBuffer[I];

  fHasChanges := True;
end;


procedure TKMTextManager.EraseAllStringsButEng(aIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to fLocales.Count - 1 do
  if I <> LOCALE_DEFAULT then
    fLines[aIndex].Strings[I] := '';

  fHasChanges := True;
end;


procedure TKMTextManager.Insert(aIndex: Integer);
begin
  fLines.Insert(aIndex, TKMLine.Create(fLines.Last.Id + 1, 'TX_NEW' + IntToStr(fLines.Last.Id + 1)));
end;


procedure TKMTextManager.Delete(aIndex: Integer);
begin
  fLines.Delete(aIndex);

  fHasChanges := True;
end;


procedure TKMTextManager.InsertSeparator(aIndex: Integer);
begin
  fLines.Insert(aIndex, TKMLine.CreateSpacer);

  fHasChanges := True;
end;


procedure TKMTextManager.MoveUp(aIndex: Integer);
begin
  if aIndex > 0 then
    // Can't move the top item up
    fLines.Move(aIndex, aIndex - 1);

  fHasChanges := True;
end;


procedure TKMTextManager.MoveDown(aIndex: Integer);
begin
  if aIndex < fLines.Count - 1 then
    // Can't move the bottom item down
    fLines.Move(aIndex, aIndex + 1);

  fHasChanges := True;
end;


function TKMTextManager.ToClipboardHeader(aLocales: TByteSet; aExport: TKMClipboardExport): string;
var
  I: Integer;
begin
  // First line, list lang names
  Result := COL_NAME_TAG;

  for I := 0 to fLocales.Count - 1 do
  if (aLocales = []) or (I in aLocales) then
    Result := Result + #9 + fLocales[I].Code;

  if aExport = ceLastChanged then
    Result := Result + #9 + COL_NAME_NEEDS_UPDATE;

  Result := Result + #9 + COL_NAME_DESC;

  Result := Result + sLineBreak;
end;


function TKMTextManager.ToClipboardBody(aLocales: TByteSet; aExport: TKMClipboardExport): string;
var
  I, K: Integer;
  s: string;
begin
  Result := '';

  // Do the export
  for I := 0 to fLines.Count - 1 do
  begin
    if not fLines[I].IsSpacer then
    begin
      Result := Result + fLines[I].Tag;

      for K := 0 to fLocales.Count - 1 do
      if (aLocales = []) or (K in aLocales) then
      begin
        s := fLines[I].Strings[K];

        s := EscapeTextForGoogleSheets(s);

        Result := Result + #9 + s;

        if (fLibType = ltGame) and (aExport = ceLastChanged) and (K <> LOCALE_DEFAULT) then
        begin
          if (fLines[I].LastChanged[LOCALE_DEFAULT] = 0) then
            Result := Result + #9 + '' // New line (not saved in Meta)
          else
          if (fLines[I].LastChanged[K] = 0) then
            Result := Result + #9 + '' // New localization (not saved in Meta)
          else
          if (fLines[I].LastChanged[K] < fLines[I].LastChanged[LOCALE_DEFAULT]) then
            Result := Result + #9 + 'NEEDS UPDATE'
          else
            Result := Result + #9 + '';
        end;
      end;

      Result := Result + #9 + fLines[I].Description;
    end
    else
      Result := Result + '-';

    Result := Result + sLineBreak;
  end;
end;


// Copy to clipboard to be pasted into Excel/Sheets
// Tag Text Text Text
procedure TKMTextManager.ToClipboard(aLocales: TByteSet; aExport: TKMClipboardExport);
begin
  Clipboard.AsText := ToClipboardHeader(aLocales, aExport) + ToClipboardBody(aLocales, aExport);
end;


// Copy to clipboard to be pasted into Excel/Sheets
// Tag Text Text Text
procedure TKMTextManager.ToClipboardAll(aList: TStringList; aLocales: TByteSet);
  function TranslationPercent(aLineFrom, aLineTo: Integer): string;
  var
    I, K: Integer;
    fromStr, toStr: string;
  begin
    Result := '-';
    K := 0;
    fromStr := IntToStr(aLineFrom);
    toStr := IntToStr(aLineTo);
    for I := 0 to fLocales.Count - 1 do
    if (aLocales = []) or (I in aLocales) then
    begin
      Result := Result + #9 + '=ROUND(COUNTA('+Chr(Ord('B')+K)+fromStr+':'+Chr(Ord('B')+K)+toStr+')/COUNTA(B'+fromStr+':B'+toStr+')*100,2)&" %"';
      Inc(K);
    end;
  end;
var
  localesCount: Integer;
  I: Integer;
  s: string;
  lineCount: Integer;
  exportMode: TKMClipboardExport;
begin
  // How many locales are we exporting (except Eng)
  localesCount := 0;
  for I := 0 to fLocales.Count - 1 do
  if (I <> LOCALE_DEFAULT) and ((aLocales = []) or (I in aLocales)) then
    Inc(localesCount);

  if localesCount = 1 then
    exportMode := ceLastChanged
  else
    exportMode := ceSimple;

  s := '';
  lineCount := 1;
  for I := 0 to aList.Count - 1 do
  begin
    s := s + '> ' + aList[I] + sLineBreak;
    Inc(lineCount);

    Load4(aList[I], TAGS_PATH, META_PATH, aLocales);

    s := s + ToClipboardBody(aLocales, exportMode);
    Inc(lineCount, fLines.Count);
  end;

  Clipboard.AsText := ToClipboardHeader(aLocales, exportMode) + TranslationPercent(3, lineCount+1) + sLineBreak + s;
end;


procedure TKMTextManager.ClipboardColumnsToLocIndex(aLine: string; out aLocIndex: TArray<Integer>);
var
  ss: TStringDynArray;
  I: Integer;
begin
  ss := SplitString(aLine, #9);

  Assert(Length(ss) > 1, 'Column names line needs to have at least 2 columns (Tag and one Locale)');

  // Always skip first column (Tag)
  SetLength(aLocIndex, Length(ss) - 1);

  for I := 1 to High(ss) do
  if (ss[I] <> '')
  and (ss[I] <> TKMTextManager.COL_NAME_NEEDS_UPDATE)
  and (ss[I] <> TKMTextManager.COL_NAME_DESC) then
    aLocIndex[I - 1] := fLocales.IndexByCode(ss[I])
  else
  begin
    SetLength(aLocIndex, I-1);
    Break;
  end;
end;


// Update translations
procedure TKMTextManager.FromClipboard;
var
  sl: TStringList;
  locIndex: TArray<Integer>;
  I, K: Integer;
  tag: string;
  txt: array of string;
  idLine, idLoc: Integer;
  changesCount: Integer;
  s: string;
begin
  sl := TStringList.Create;
  sl.Text := Clipboard.AsText;

  // First line, get lang names
  ClipboardColumnsToLocIndex(sl[0], locIndex);

  SetLength(txt, fLocales.Count);

  // Other lines, get the strings
  changesCount := 0;
  for I := 1 to sl.Count - 1 do
  if StringFromString(sl[I], #9, 0) <> '-' then
  begin
    // Source data
    tag := StringFromString(sl[I], #9, 0);
    for K := 0 to High(locIndex) do
    begin
      s := StringFromString(sl[I], #9, K+1);
      if StartsStr(#39'=', s) then
        s := LeftStr(s, Length(s) - 1);

      txt[K] := s;
    end;

    // Now we have all data to update

    idLine := fLines.IndexOfTag(tag);

    for K := 0 to High(locIndex) do
    begin
      idLoc := locIndex[K];
      if fLines[idLine].Strings[idLoc] <> txt[K] then
      begin
        fLines[idLine].Strings[idLoc] := txt[K];
        Inc(changesCount);
      end;
    end;
  end;

  ShowMessage(Format('%d lines updated', [changesCount]));

  sl.Free;

  fHasChanges := True;
end;


procedure TKMTextManager.FromClipboardAll;
var
  sl: TStringList;
  locIndex: TArray<Integer>;
  I, K: Integer;
  tag: string;
  txt: array of string;
  idLine, idLoc: Integer;
  changesCount: Integer;
  s: string;
  fname: string;
  filesCount: Integer;
  dt: TDateTime;
begin
  dt := Now; // Query once, so we dont get on a "second change" moment

  sl := TStringList.Create;
  sl.Text := Clipboard.AsText;

  // First line, get lang names
  ClipboardColumnsToLocIndex(sl[0], locIndex);

  SetLength(txt, fLocales.Count);

  // Other lines, get the strings
  filesCount := 0;
  changesCount := 0;

  // Line 0 - Column names
  // Line 1 - Translation percentage
  for I := 2 to sl.Count - 1 do
  if StartsStr('>', sl[I]) then
  begin
    // Save previous
    if I > 2 then
      Save;

    // Take 1st cell and trim the header
    fname := StringFromString(sl[I], #9, 0);
    fname := StringReplace(fname, '> ', '', [rfReplaceAll]);

    Load4(fname, TAGS_PATH, META_PATH, []);

    Inc(filesCount);
  end else
  if StringFromString(sl[I], #9, 0) <> '-' then
  begin
    // Source data
    tag := StringFromString(sl[I], #9, 0);

    if tag = '' then Continue;

    for K := 0 to High(locIndex) do
    begin
      s := StringFromString(sl[I], #9, K+1);
      if StartsStr(#39'=', s) then
        s := LeftStr(s, Length(s) - 1);

      txt[K] := s;
    end;

    // Now we have all data to update

    idLine := fLines.IndexOfTag(tag);

    if idLine = -1 then
      Assert(False, Format('Line %d. Tag "%s" can not be found', [I, tag]));

    for K := 0 to High(locIndex) do
    begin
      idLoc := locIndex[K];
      if fLines[idLine].Strings[idLoc] <> txt[K] then
      begin
        fLines[idLine].Strings[idLoc] := txt[K];
        fLines[idLine].LastChanged[idLoc] := dt;
        Inc(changesCount);
      end;
    end;
  end;

  Save;

  ShowMessage(Format('%d files updated. %d lines updated', [filesCount, changesCount]));

  sl.Free;
end;


procedure TKMTextManager.ListMismatchingAll(aFolders: TStringList; aList: TStringList);
var
  I, K: Integer;
  newFile: Boolean;

  procedure Append(aString: string);
  begin
    if newFile then
    begin
      if aList.Count > 0 then
        aList.Append('');

      aList.Append(aFolders[I] + ': ');
      newFile := False;
    end;

    aList.Append(aString);
  end;
begin
  for I := 0 to aFolders.Count - 1 do
  begin
    Load4(aFolders[I], '', '', []);

    newFile := True;

    // Linebreaks
    for K := 0 to fLines.Count - 1 do
      if not fLines[K].CheckMatchingForCharCount(['|'], []) then
        Append('| in ' + fLines[K].Tag);

    // Formats
    for K := 0 to fLines.Count - 1 do
      if not fLines[K].CheckMatchingForCharCount(['%'], []) then
        Append('% in ' + fLines[K].Tag);

    // Colors
    for K := 0 to fLines.Count - 1 do
      if not fLines[K].CheckMatchingForCharCount(['[$'], []) then
        Append('[$ in ' + fLines[K].Tag);
  end;
end;


end.
