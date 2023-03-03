unit UnitTranslationManager;
{$I KM_CompilerDirectives.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus,
  StdCtrls, StrUtils, Windows, SysUtils, CheckLst,
  KM_ResLocales, KM_TextLines, KM_TextManager, Unit_PathManager;

type
  TKMTargetGame = (tgUnknown, tgKaMRemake, tgKnightsProvince);

  TKMUsageMode = (umDeveloper, umUser);

  TKMLibxDomain = (ldGame, ldTutorials, ldCampaigns, ldMaps, ldMapsMP, ldMapsUnofficial, ldMapdDev);

const
  TARGET_GAME: array [TKMTargetGame] of string = ('Unknown', 'KaM Remake', 'Knights Province');
  USAGE_MODE: array [TKMUsageMode] of string = ('Developer mode', 'User mode');
  LIBX_DOMAIN: array [TKMLibxDomain] of string = ('data\', 'tutorials\', 'campaigns\', 'maps\', 'mapsmp\', 'maps_unofficial\', 'mapsdev\');

type
  TForm1 = class(TForm)
    lbTagName: TLabel;
    ScrollBox1: TScrollBox;
    lbLibs: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
    clbShowLang: TCheckListBox;
    btnEraseAllButEng: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btnSortByIndex: TButton;
    btnSortByTag: TButton;
    btnCompactIndexes: TButton;
    btnListUnusedTags: TButton;
    btnCopyToClipboard: TButton;
    btnPasteFromClipboard: TButton;
    lbTags: TListBox;
    btnSave: TButton;
    btnInsert: TButton;
    btnInsertSeparator: TButton;
    btnDelete: TButton;
    btnRename: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    cbFilterDuplicateTexts: TCheckBox;
    cbFilterEmptyTexts: TCheckBox;
    edFilterText: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    btnPasteFromClipboardAll: TButton;
    btnCopyToClipboardAll: TButton;
    btnListMismatching: TButton;
    Label3: TLabel;
    edFilterTagName: TEdit;
    cbFilterMismatching: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    cbLibxDomains: TCheckListBox;
    edFilterTagId: TEdit;
    Label9: TLabel;
    btnSaveToZip: TButton;
    btnSaveAllToZip: TButton;
    sdExportZIP: TSaveDialog;
    procedure lbTagsClick(Sender: TObject);
    procedure btnSortByIndexClick(Sender: TObject);
    procedure btnSortByTagClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertSeparatorClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnCompactIndexesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbLibsClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure edFilterTextChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnListUnusedTagsClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure clbShowLangClickCheck(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbFilterEmptyTextsClick(Sender: TObject);
    procedure btnEraseAllButEngClick(Sender: TObject);
    procedure lbTagsKeyPress(Sender: TObject; var Key: Char);
    procedure btnCopyToCBClick(Sender: TObject);
    procedure btnPasteFromCBClick(Sender: TObject);
    procedure btnCopyToClipboardAllClick(Sender: TObject);
    procedure btnPasteFromClipboardAllClick(Sender: TObject);
    procedure btnListMismatchingClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbLibxDomainsClick(Sender: TObject);
    procedure btnSaveToZipClick(Sender: TObject);
    procedure btnSaveAllToZipClick(Sender: TObject);
  private
    // Automated things
    fMode: TKMUsageMode;
    fSettingsPath: string;
    fTargetGame: TKMTargetGame;
    fWorkDir: string;

    // Components
    fPathManager: TPathManager;
    fTextManager: TKMTextManager;
    fLocales: TKMResLocales;

    // User settings
    fAltWorkDir: string;
    fSelectedFolders: string;
    fSelectedLocales: string;

    // Runtime things
    fTransLabels: array of TLabel;
    fTransMemos: array of TMemo;

    ListBoxLookup: array of Integer;
    fUpdating: Boolean;
    fPreviousFolder: Integer;
    procedure MemoChange(Sender: TObject);

    procedure InitFormControls;
    procedure InitLibxDomainsList;
    procedure InitLocalesList;
    procedure RefreshFolders;
    procedure RefreshControls;
    procedure UpdateVisibleLocales;
    procedure RefreshList;
    procedure LoadSettings(const aPath: string);
    procedure SaveSettings(const aPath: string);
    procedure UpdateMenuItemVisibility;
    procedure SaveToZip(const aZipName: string; aLocales: TByteSet);
  public
    function Start: Boolean;
  end;


implementation
uses
  KromUtils, Zippit,
  KM_IoXML;

{$R *.dfm}


// todo: Feature requests by Rey:
// + 1. add libx filter, same as in KMR TM (game / tutorial (?) / maps / mapsMP / campaigns
// + 2. add filter for label name contains
// + 2. add filter for label ID or ID range
// + 3. Save via Ctrl + S hotkey
// + 4. Export all languages to ZIP
// + 5. Export selected languages to ZIP
// + 6. use nicer form style / fonts. KMR TM window looks way nicer IMHO
// 3. 4. 5. could be added as a menu

const
  LOCALES_PATH: array [TKMTargetGame] of string = ('', 'data\locales.txt', 'data\text\locales.xml');
  TAGS_PATH: array [TKMTargetGame] of string = ('', 'KM_TextIDs.inc', 'data\text\text_IDs.inc');
  META_PATH: array [TKMTargetGame] of string = ('', 'KM_TextMeta.xml', 'data\text\text_meta.xml');

procedure DetectGameAndPath(aAltWorkDir: string; out aTargetGame: TKMTargetGame; out aWorkDir: string);
var
  exeDir: string;
begin
  aTargetGame := tgUnknown;
  aWorkDir := '';

  exeDir := ExtractFilePath(ParamStr(0));

  if FileExists(aAltWorkDir + LOCALES_PATH[tgKaMRemake]) then
  begin
    // aAltWorkDir in a completely arbitrary location
    aTargetGame := tgKaMRemake;
    aWorkDir := aAltWorkDir;
  end else
  if FileExists(exeDir + '..\..\' + LOCALES_PATH[tgKaMRemake]) then
  begin
    // KaM Remake\Utils\TMshared\TM.exe
    aTargetGame := tgKaMRemake;
    aWorkDir := ExpandFileName(exeDir + '..\..\');
  end else
  if FileExists(exeDir + '..\' + LOCALES_PATH[tgKaMRemake]) then
  begin
    // KaM Remake\Utils\TM.exe
    aTargetGame := tgKaMRemake;
    aWorkDir := ExpandFileName(exeDir + '..\');
  end else
  if FileExists(exeDir + LOCALES_PATH[tgKaMRemake]) then
  begin
    // KaM Remake\TM.exe
    aTargetGame := tgKaMRemake;
    aWorkDir := exeDir;
  end else
  if FileExists(exeDir + '..\..\' + LOCALES_PATH[tgKnightsProvince]) then
  begin
    // Knights Province\Utils\TM\TM.exe
    aTargetGame := tgKnightsProvince;
    aWorkDir := ExpandFileName(exeDir + '..\..\');
  end else
  if FileExists(exeDir + LOCALES_PATH[tgKnightsProvince]) then
  begin
    // Knights Province\TM.exe
    aTargetGame := tgKnightsProvince;
    aWorkDir := exeDir;
  end;
end;


{ TForm1 }
function TForm1.Start: Boolean;
const
  MODE: array [Boolean] of TKMUsageMode = (umUser, umDeveloper);
begin
  // Load settings first, including fAltWorkDir
  fSettingsPath := ChangeFileExt(ParamStr(0), '.xml');
  LoadSettings(fSettingsPath);

  // Detect the game
  DetectGameAndPath(fAltWorkDir, fTargetGame, fWorkDir);

  case fTargetGame of
    tgUnknown:          begin
                          MessageBox(
                            Handle,
                            'Can not find locales.txt\locales.xml file.' + sLineBreak +
                            'Please make sure to run the Translation Manager from the games folder',
                            'Error',
                            MB_ICONERROR);
                          Exit(False);
                        end;
    tgKaMRemake:        fLocales := TKMResLocales.Create(fWorkDir + LOCALES_PATH[tgKaMRemake], 'KMR');
    tgKnightsProvince:  fLocales := TKMResLocales.Create(fWorkDir + LOCALES_PATH[tgKnightsProvince], 'KP');
  end;

  // Detect the run from IDE
  fMode := MODE[DebugHook <> 0];

  Caption := Format('Translation Manager (%s) [%s] [%s]', [DateTimeToStr(GetExeBuildTime), TARGET_GAME[fTargetGame], USAGE_MODE[fMode]]);

  InitLibxDomainsList;
  InitLocalesList;

  fPathManager := TPathManager.Create;
  RefreshFolders;

  fTextManager := TKMTextManager.Create(fLocales, fWorkDir, TAGS_PATH[fTargetGame], META_PATH[fTargetGame]);

  UpdateMenuItemVisibility;

  InitFormControls;
  RefreshControls;

  Result := True;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  if fTargetGame <> tgUnknown then
    SaveSettings(fSettingsPath);

  fTextManager.Free;
  fLocales.Free;
  //gIoPack.Free;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('S')) then
    fTextManager.Save;
end;


procedure TForm1.FormResize(Sender: TObject);
var
  I,K: Integer;
  selCount, secHeight: Word;
begin
  selCount := 0;
  for I := 0 to fLocales.Count - 1 do
  if I+1 < clbShowLang.Count then
  if clbShowLang.Checked[I+1] then
    Inc(selCount);

  if selCount = 0 then
    Exit;

  secHeight := ScrollBox1.ClientHeight div selCount;

  K := 0;
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
  begin
    fTransLabels[I].SetBounds(8, 2 + K * secHeight, 160, 20);
    fTransMemos[I].SetBounds(8, 22 + K * secHeight, ScrollBox1.Width - 20, secHeight - 24);
    Inc(K);
  end;
end;


procedure TForm1.UpdateMenuItemVisibility;
begin
  // Hide entries that Users should not access

  // Top
  btnSortByIndex.Visible := (fMode = umDeveloper);
  btnSortByTag.Visible := (fMode = umDeveloper);
  btnCompactIndexes.Visible := (fMode = umDeveloper);
  btnListUnusedTags.Visible := (fMode = umDeveloper);

  // Consts
  btnInsert.Visible := (fMode = umDeveloper);
  btnInsertSeparator.Visible := (fMode = umDeveloper);
  btnRename.Visible := (fMode = umDeveloper);
  btnDelete.Visible := (fMode = umDeveloper);
  btnMoveUp.Visible := (fMode = umDeveloper);
  btnMoveDown.Visible := (fMode = umDeveloper);

  // Texts
  btnCopy.Visible := (fMode = umDeveloper);
  btnPaste.Visible := (fMode = umDeveloper);
  btnEraseAllButEng.Visible := (fMode = umDeveloper);
end;


procedure TForm1.RefreshFolders;
var
  I: Integer;
begin
  lbLibs.Clear;
  fPathManager.Clear;

  // Whitelist of paths to scan (so we dont include build folders and such)
  for I := 0 to cbLibxDomains.Count - 1 do
  if cbLibxDomains.Checked[I] then
    fPathManager.AddPath(fWorkDir, cbLibxDomains.Items[I]);

  for I := 0 to fPathManager.Count - 1 do
    lbLibs.Items.Add(fPathManager[I]);
end;


procedure TForm1.lbLibsClick(Sender: TObject);
const
  MSG_WARNING: string = 'You have unsaved changes that will be lost, load new libx anyway?';
var
  id: Integer;
begin
  // Let the user abort and save edited translations
  if fTextManager.HasChanges
  and (MessageDlg(MSG_WARNING, mtWarning, mbOKCancel, 0) <> mrOk) then
  begin
    lbLibs.ItemIndex := fPreviousFolder;
    Exit;
  end;

  fPreviousFolder := lbLibs.ItemIndex;
  id := lbLibs.ItemIndex;
  if id = -1 then Exit;

  fTextManager.Load4(lbLibs.Items[id], []);

  RefreshControls;
  RefreshList;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  fTextManager.Save;
end;


procedure TForm1.btnSaveToZipClick(Sender: TObject);
var
  I: Integer;
  localesToZip: TByteSet;
begin
  sdExportZIP.InitialDir := fWorkDir;
  if not sdExportZIP.Execute(Handle) then Exit;

  localesToZip := [];
  for I := 1 to clbShowLang.Count - 1 do
    if clbShowLang.Checked[I] then
      localesToZip := localesToZip + [I-1];

  SaveToZip(sdExportZIP.FileName, localesToZip);
end;


procedure TForm1.btnSaveAllToZipClick(Sender: TObject);
begin
  sdExportZIP.InitialDir := fWorkDir;
  if not sdExportZIP.Execute(Handle) then Exit;

  SaveToZip(sdExportZIP.FileName, []);
end;


procedure TForm1.SaveToZip(const aZipName: string; aLocales: TByteSet);
var
  newZip: TZippit;
  I, K: Integer;
  libxName: string;
begin
  newZip := TZippit.Create;
  try
    for I := 0 to fPathManager.Count - 1 do
      for K := 0 to fLocales.Count - 1 do
        if (aLocales = []) or (K in aLocales) then
        begin
          libxName := Format(fPathManager[I], [fLocales[K].Code]);
          if FileExists(fWorkDir + libxName) then
            newZip.AddFile(fWorkDir + libxName, ExtractFilePath(libxName));
        end;

    newZip.SaveToFile(aZipName);
  finally
    newZip.Free;
  end;
end;


procedure TForm1.RefreshList;
var
  selectedLocales: TByteSet;

  function ShowTag(aLine: TKMLine): Boolean;
  begin
    Result := True;

    if cbFilterEmptyTexts.Checked then
      Result := aLine.HasEmptyTexts(selectedLocales);

    if Result and cbFilterDuplicateTexts.Checked then
      Result := aLine.HasDuplicates(selectedLocales);

    if Result and cbFilterMismatching.Checked then
      Result := not aLine.CheckMatchingForCharCount([{'|',} '%', '[$'], selectedLocales);

    if Result and (edFilterTagName.Text <> '') then
      Result := aLine.HasTagNameFilter(edFilterTagName.Text);

    if Result and (edFilterText.Text <> '') then
      Result := aLine.HasTextFilter(edFilterText.Text, selectedLocales);

    if Result and (edFilterTagId.Text <> '') then
      Result := aLine.HasTagIdFilter(Trim(edFilterTagId.Text));
  end;
var
  I, TopIdx, ItemIdx: Integer;
  s: string;
begin
  lbTags.Items.BeginUpdate;
  ItemIdx := lbTags.ItemIndex;
  TopIdx := lbTags.TopIndex;
  try
    lbTags.Clear;

    SetLength(ListBoxLookup, 0);
    SetLength(ListBoxLookup, fTextManager.Count);

    selectedLocales := [];
    for I := 0 to fLocales.Count - 1 do
      if clbShowLang.Checked[I+1] then
        selectedLocales := selectedLocales + [I];

    for I := 0 to fTextManager.Count - 1 do
    if ShowTag(fTextManager[I]) then
    begin
      ListBoxLookup[lbTags.Items.Count] := I;
      if fTextManager[I].IsSpacer then
        s := ''
      else
        s := IntToStr(fTextManager[I].Id) + ': ' + fTextManager[I].Tag;

      lbTags.Items.Add(s);
    end;
  finally
    lbTags.Items.EndUpdate;
    lbTags.ItemIndex := EnsureRange(ItemIdx, 0, lbTags.Count - 1);
    lbTags.TopIndex := TopIdx;
  end;

  lbTagsClick(lbTags);

  Label5.Caption := 'Tags (' + IntToStr(lbTags.Count) + ')';
end;


procedure TForm1.InitLibxDomainsList;
var
  LD: TKMLibxDomain;
begin
  cbLibxDomains.Clear;

  for LD := Low(TKMLibxDomain) to High(TKMLibxDomain) do
  if DirectoryExists(fWorkDir + LIBX_DOMAIN[LD]) then
  begin
    cbLibxDomains.AddItem(LIBX_DOMAIN[LD], TObject(LD));
    cbLibxDomains.Checked[cbLibxDomains.Count-1] := Pos(IntToStr(Ord(LD)), fSelectedFolders) <> 0;
  end;
end;


procedure TForm1.InitLocalesList;
  function GetCharset(const aLang: string): TFontCharset;
  begin
    // Using slower but more compact comparisons
    if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
      Result := RUSSIAN_CHARSET
    else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
      Result := EASTEUROPE_CHARSET
    else if Pos(aLang, 'tur') <> 0 then
      Result := TURKISH_CHARSET
    else if Pos(aLang, 'lit,lat') <> 0 then
      Result := BALTIC_CHARSET
    else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
      Result := ANSI_CHARSET
    else
      Result := DEFAULT_CHARSET;
  end;
var
  I: Integer;
begin
  clbShowLang.Clear;

  clbShowLang.Items.Add('All');

  for I := 0 to fLocales.Count - 1 do
    clbShowLang.Items.Add(fLocales[I].Code);

  SetLength(fTransLabels, fLocales.Count);
  SetLength(fTransMemos, fLocales.Count);
  for I := 0 to fLocales.Count - 1 do
  begin
    fTransLabels[I] := TLabel.Create(Self);
    fTransLabels[I].Parent := ScrollBox1;
    fTransLabels[I].Caption := fLocales[I].Title + ' (' + fLocales[I].Code + ')';
    fTransLabels[I].Hide;

    fTransMemos[I] := TMemo.Create(Self);
    fTransMemos[I].Parent := ScrollBox1;
    fTransMemos[I].Anchors := [akLeft, akRight, akTop];
    fTransMemos[I].Font.Name := 'Arial Unicode MS'; //If not found, parent font is used
    fTransMemos[I].Font.Charset := GetCharset(fLocales[I].Code);
    fTransMemos[I].OnChange := MemoChange;
    fTransMemos[I].Tag := I;
    fTransMemos[I].Hide;
  end;
end;


procedure TForm1.UpdateVisibleLocales;
var
  I: Integer;
begin
  for I := 0 to fLocales.Count - 1 do
  begin
    fTransMemos[I].Visible := clbShowLang.Checked[I+1];
    fTransLabels[I].Visible := clbShowLang.Checked[I+1];
  end;

  FormResize(Self);
end;


procedure TForm1.InitFormControls;
var
  I: Integer;
begin
  // If there are any items "All" should be greyed
  if fSelectedLocales <> '' then
    clbShowLang.State[0] := cbGrayed;

  for I := 0 to fLocales.Count - 1 do
  if Pos(fLocales[I].Code, fSelectedLocales) <> 0 then
    clbShowLang.Checked[I+1] := True;

  UpdateVisibleLocales;
end;


procedure TForm1.LoadSettings(const aPath: string);
var
  xml: TKMXMLDocument;
  isMaximized: Boolean;
begin
  xml := TKMXMLDocument.Create;
  try
    xml.LoadFromFile(aPath);

    fSelectedFolders := xml.Root.Attributes['SelectedFolders'].AsString('1,2,3,4,5,6,7,8,9');
    fSelectedLocales := xml.Root.Attributes['Selected_Locales'].AsString(TKMResLocales.DEFAULT_LOCALE);
    isMaximized := xml.Root.Attributes['Maximized'].AsBoolean(True);
    fAltWorkDir := xml.Root.Attributes['AltWorkDir'].AsString('');
  finally
    xml.Free;
  end;

  if isMaximized then
    WindowState := wsMaximized
  else
    WindowState := wsNormal;
end;


procedure TForm1.SaveSettings(const aPath: string);
var
  selectedLocales: string;
  selectedFolders: string;
  I: Integer;
  xml: TKMXMLDocument;
begin
  selectedFolders := '';
  for I := 0 to cbLibxDomains.Count - 1 do
  if cbLibxDomains.Checked[I] then
    selectedFolders := selectedFolders + IfThen(selectedFolders <> '', ',') + IntToStr(Integer(cbLibxDomains.Items.Objects[I]));

  selectedLocales := '';
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
    selectedLocales := selectedLocales + IfThen(selectedLocales <> '', ',') + fLocales[I].Code;

  xml := TKMXMLDocument.Create;
  try
    xml.Root.Attributes['SelectedFolders'] := selectedFolders;
    xml.Root.Attributes['Selected_Locales'] := selectedLocales;
    xml.Root.Attributes['Maximized'] := (WindowState = wsMaximized);
    xml.Root.Attributes['AltWorkDir'] := fAltWorkDir;

    xml.SaveToFile(aPath);
  finally
    xml.Free;
  end;
end;


procedure TForm1.lbTagsClick(Sender: TObject);
var
  I,ID: Integer;
begin
  if lbTags.ItemIndex = -1 then Exit;

  fUpdating := True;

  ID := ListBoxLookup[lbTags.ItemIndex];

  btnRename.Enabled := not fTextManager[ID].IsSpacer;

  lbTagName.Caption := fTextManager[ID].Tag;

  for I := 0 to fLocales.Count - 1 do
    if not fTextManager[ID].IsSpacer then
      fTransMemos[I].Text := {$IFDEF FPC}AnsiToUTF8{$ENDIF}(fTextManager[ID].Strings[I])
    else
      fTransMemos[I].Text := '';

  fUpdating := False;
end;


procedure TForm1.lbTagsKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_DELETE then
    btnDeleteClick(Self);
end;


procedure TForm1.btnSortByIndexClick(Sender: TObject);
begin
  fTextManager.SortByIndex;
  RefreshList;
end;


procedure TForm1.btnSortByTagClick(Sender: TObject);
begin
  fTextManager.SortByTag;
  RefreshList;
end;


procedure TForm1.btnCompactIndexesClick(Sender: TObject);
begin
  fTextManager.CompactIndexes;
  RefreshList;
end;


procedure TForm1.edFilterTextChange(Sender: TObject);
begin
  RefreshControls;
  RefreshList;
end;


procedure TForm1.btnExitClick(Sender: TObject);
begin
  Close;
end;


procedure TForm1.MemoChange(Sender: TObject);
var
  idx,T: Integer;
begin
  if fUpdating then Exit;

  idx := ListBoxLookup[lbTags.ItemIndex];
  if fTextManager[idx].IsSpacer then Exit;

  T := TMemo(Sender).Tag;
  fTextManager[idx].Strings[T] := {$IFDEF FPC}Utf8ToAnsi{$ENDIF}(TMemo(Sender).Text);

  fTextManager.HasChanges := True;
end;


procedure TForm1.btnInsertClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex; // Item place we are adding
  if idx = -1 then Exit;

  fTextManager.Insert(idx);
  RefreshList;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex; //Item place we are adding
  if idx = -1 then Exit;

  fTextManager.InsertSeparator(idx);
  RefreshList;
end;


procedure TForm1.btnDeleteClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex; //Item place we are deleting
  if idx = -1 then Exit;

  fTextManager.Delete(idx);
  RefreshList;
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex;

  fTextManager.MoveUp(idx);
  RefreshList;
  lbTags.ItemIndex := Max(idx - 1, 0);
  lbTagsClick(nil); //Reselect the item to update the translation boxes
end;


procedure TForm1.btnCopyClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex;
  if idx = -1 then Exit;

  fTextManager.TextCopy(ListBoxLookup[idx]);

  btnPaste.Enabled := True;
end;


procedure TForm1.btnPasteClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex;
  if idx = -1 then Exit;

  fTextManager.TextPaste(ListBoxLookup[idx]);

  lbTagsClick(nil);
end;


procedure TForm1.btnEraseAllButEngClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex;
  if idx = -1 then Exit;

  fTextManager.EraseAllStringsButEng(ListBoxLookup[idx]);

  lbTagsClick(nil);
end;


procedure TForm1.btnRenameClick(Sender: TObject);
var
  newName: string;
  idx: Integer;
begin
  if lbTags.ItemIndex = -1 then Exit;

  idx := ListBoxLookup[lbTags.ItemIndex];
  if fTextManager[idx].IsSpacer then Exit;

  newName := UpperCase(InputBox('', 'New name:', fTextManager[idx].Tag));

  fTextManager[idx].Tag := newName;

  fTextManager.HasChanges := True;

  RefreshList;
end;


procedure TForm1.btnMoveDownClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := lbTags.ItemIndex;
  if idx = -1 then Exit;

  fTextManager.MoveDown(idx);
  RefreshList;
  lbTags.ItemIndex := Min(idx + 1, lbTags.Count - 1);
  lbTagsClick(nil); //Reselect the item to update the translation boxes
end;


procedure TForm1.cbFilterEmptyTextsClick(Sender: TObject);
begin
  RefreshControls;
  RefreshList;

  //Select the first item
  lbTags.ItemIndex := 0;
  lbTagsClick(lbTags);
end;


procedure TForm1.cbLibxDomainsClick(Sender: TObject);
begin
  RefreshFolders;
end;


procedure TForm1.RefreshControls;
var
  id: Integer;
  isItemSelected, isMainFile, isFiltered: Boolean;
begin
  id := lbLibs.ItemIndex;

  isItemSelected := id <> -1;
  isMainFile := isItemSelected and SameText(lbLibs.Items[id], TKMTextManager.GAME_TEXT_PATH);
  isFiltered := (edFilterText.Text <> '') or (edFilterTagName.Text <> '') or
                cbFilterEmptyTexts.Checked or cbFilterDuplicateTexts.Checked or cbFilterMismatching.Checked;

  btnCopyToClipboard.Enabled := isItemSelected;
  btnPasteFromClipboard.Enabled := isItemSelected;

  cbFilterDuplicateTexts.Enabled := isItemSelected;
  cbFilterEmptyTexts.Enabled := isItemSelected;
  cbFilterMismatching.Enabled := isItemSelected;

  btnSortByIndex.Enabled := isItemSelected and isMainFile and not isFiltered;
  btnSortByTag.Enabled := isItemSelected and isMainFile and not isFiltered;
  btnCompactIndexes.Enabled := isItemSelected and isMainFile and not isFiltered;
  btnListUnusedTags.Enabled := isItemSelected and isMainFile and not isFiltered;

  btnInsert.Enabled := isItemSelected and not isFiltered;
  btnInsertSeparator.Enabled := isItemSelected and isMainFile and not isFiltered;
  btnRename.Enabled := isItemSelected and isMainFile;
  btnDelete.Enabled := isItemSelected and not isFiltered;
  btnMoveUp.Enabled := isItemSelected and isMainFile and not isFiltered;
  btnMoveDown.Enabled := isItemSelected and isMainFile and not isFiltered;
end;


procedure TForm1.clbShowLangClickCheck(Sender: TObject);
var
  I,K: Integer;
begin
  if clbShowLang.Selected[0] then
    case clbShowLang.State[0] of
      cbChecked:    for I := 1 to clbShowLang.Count - 1 do
                      clbShowLang.Checked[I] := True;
      cbUnchecked:  for I := 1 to clbShowLang.Count - 1 do
                      clbShowLang.Checked[I] := False;
    end;

  K := 0;
  for I := 1 to clbShowLang.Count - 1 do
  if clbShowLang.Checked[I] then
    Inc(K);

  if K = 0 then
    clbShowLang.State[0] := cbUnchecked
  else
  if K = clbShowLang.Count - 1 then
    clbShowLang.State[0] := cbChecked
  else
    clbShowLang.State[0] := cbGrayed;

  RefreshList;
  UpdateVisibleLocales;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not fTextManager.HasChanges or (MessageDlg('Exit without saving?', mtWarning, [mbYes, mbNo], 0) = mrYes);
end;


procedure TForm1.btnListMismatchingClick(Sender: TObject);
var
  slMiss: TStringList;
begin
  slMiss := TStringList.Create;
  try
    fTextManager.ListMismatchingAll(fPathManager.GetPaths, slMiss);

    slMiss.SaveToFile(fWorkDir + 'TM_Mismatching.txt');
    ShowMessage(slMiss.Text);
  finally
    slMiss.Free;
  end;
end;


procedure TForm1.btnListUnusedTagsClick(Sender: TObject);
var
  slTags: TStringList;

  procedure CheckPasFiles(const aPath: string);
  var
    slPaths: TStringList;
    slPas: TStringList;
    I, K: Integer;
    SearchRec: TSearchRec;
  begin
    if not SysUtils.DirectoryExists(aPath) then Exit;

    slPas := TStringList.Create;
    slPaths := TStringList.Create;
    slPaths.Add('');
    I := 0;

    while I < slPaths.Count do
    begin
      if FindFirst(aPath + slPaths[I] + '*', faAnyFile, SearchRec) = 0 then
      begin
        repeat
          if (SearchRec.Attr and faDirectory) <> 0 then
          begin
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
              slPaths.Append(slPaths[I] + SearchRec.Name + '\');
          end else
          if (ExtractFileExt(SearchRec.Name) = '.pas')
          and not StartsStr('_', SearchRec.Name) then
          begin
            slPas.LoadFromFile(aPath + slPaths[I] + SearchRec.Name);
            for K := slTags.Count - 1 downto 0 do
            if Pos(slTags[K], slPas.Text) <> 0 then
              slTags.Delete(K);
          end;
        until (FindNext(SearchRec) <> 0);

        Inc(I);

        FindClose(SearchRec);
      end;
    end;

    slPas.Free;
    slPaths.Free;
  end;
var
  I: Integer;
begin
  Cursor := crHourGlass;
  try
    slTags := TStringList.Create;
    try
      // Prepare list of all constants we will be looking for (skip enums, marked with __)
      for I := 0 to fTextManager.Count - 1 do
      if Pos('__', fTextManager[I].Tag) = 0 then
        slTags.Append(fTextManager[I].Tag);

      // Check all *.pas files
      CheckPasFiles(fWorkDir + 'src\');

      // Remove duplicate EOLs (keep section separators)
      for I := slTags.Count - 2 downto 0 do
      if (slTags[I] = '') and (slTags[I+1] = '') then
        slTags.Delete(I);

      slTags.SaveToFile(fWorkDir + 'TM_unused.txt');
      ShowMessage(slTags.Text);
    finally
      slTags.Free;
    end;
  finally
    Cursor := crDefault;
  end;
end;


procedure TForm1.btnCopyToCBClick(Sender: TObject);
var
  I: Integer;
  localesToCopy: TByteSet;
begin
  localesToCopy := [];
  for I := 1 to clbShowLang.Count - 1 do
    if clbShowLang.Checked[I] then
      localesToCopy := localesToCopy + [I-1];

  fTextManager.ToClipboard(localesToCopy, ceSimple);
end;


procedure TForm1.btnCopyToClipboardAllClick(Sender: TObject);
var
  I: Integer;
  localesToCopy: TByteSet;
begin
  localesToCopy := [];
  for I := 1 to clbShowLang.Count - 1 do
    if clbShowLang.Checked[I] then
      localesToCopy := localesToCopy + [I-1];

  fTextManager.ToClipboardAll(fPathManager.GetPaths, localesToCopy);
end;


procedure TForm1.btnPasteFromCBClick(Sender: TObject);
begin
  fTextManager.FromClipboard;
  RefreshList;
end;


procedure TForm1.btnPasteFromClipboardAllClick(Sender: TObject);
begin
  fTextManager.FromClipboardAll;
end;


end.
