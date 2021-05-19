unit UnitTranslationManager;
{$I KM_CompilerDirectives.inc}
interface
uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Menus,
  {$IFDEF MSWINDOWS} ComCtrls, FileCtrl, {$ENDIF}
  StdCtrls, StrUtils, Windows, SysUtils, CheckLst,
  KM_ResLocales, KM_TextManager, Unit_PathManager;

type
  TKMTargetGame = (
    tgUnknown, // Game could not be detected
    tgKaMRemake,
    tgKnightsProvince
  );

  TKMUsageMode = (umDeveloper, umUser);

type
  TForm1 = class(TForm)
    lbTagName: TLabel;
    ScrollBox1: TScrollBox;
    lbLibs: TListBox;
    btnCopy: TButton;
    btnPaste: TButton;
    clbShowLang: TCheckListBox;
    StatusBar1: TStatusBar;
    btnEraseAllButEng: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btnSortByIndex: TButton;
    btnSortByTag: TButton;
    btnCompactIndexes: TButton;
    btnListUnusedTags: TButton;
    btnCopyToClipboard: TButton;
    btnPasteFromClipboard: TButton;
    pnBody: TPanel;
    pnTop: TPanel;
    lbTags: TListBox;
    btnSave: TButton;
    btnExit: TButton;
    btnInsert: TButton;
    btnInsertSeparator: TButton;
    btnDelete: TButton;
    btnRename: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    cbFilterDuplicate: TCheckBox;
    cbFilterMissing: TCheckBox;
    edFilterText: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    cbPreferDict: TCheckBox;
    btnPasteFromClipboardAll: TButton;
    btnCopyToClipboardAll: TButton;
    btnListMismatching: TButton;
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
    procedure cbFilterMissingClick(Sender: TObject);
    procedure btnEraseAllButEngClick(Sender: TObject);
    procedure lbTagsKeyPress(Sender: TObject; var Key: Char);
    procedure btnCopyToCBClick(Sender: TObject);
    procedure btnPasteFromCBClick(Sender: TObject);
    procedure btnCopyToClipboardAllClick(Sender: TObject);
    procedure btnPasteFromClipboardAllClick(Sender: TObject);
    procedure btnListMismatchingClick(Sender: TObject);
  private
    fTargetGame: TKMTargetGame;
    fWorkDir: string;
    fSettingsPath: string;

    fPathManager: TPathManager;
    fTextManager: TKMTextManager;
    fLocales: TKMResLocales;

    fMode: TKMUsageMode;

    fTransLabels: array of TLabel;
    fTransMemos: array of TMemo;

    ListBoxLookup: array of Integer;
    fUpdating: Boolean;
    fPreviousFolder: Integer;
    procedure MemoChange(Sender: TObject);

    procedure InitLocalesList;
    procedure RefreshFolders;
    procedure RefreshFilter;
    procedure UpdateVisibleLocales;
    procedure RefreshList;
    procedure LoadSettings(const aPath: string);
    procedure SaveSettings(const aPath: string);
    procedure UpdateMenuItemVisibility;
  public
    function Start: Boolean;
  end;


var
  Form1: TForm1;


implementation
uses
  KromUtils, KM_IoXML;


{$R *.dfm}


procedure DetectGameAndPath(out aTargetGame: TKMTargetGame; out aWorkDir: string);
const
  KMR_LOCALES_PATH = 'data\locales.txt';
  KP_LOCALES_PATH = 'data\text\locales.xml';
var
  exeDir: string;
begin
  aTargetGame := tgUnknown;
  aWorkDir := '';

  exeDir := ExtractFilePath(ParamStr(0));

  if FileExists(exeDir + '..\' + KMR_LOCALES_PATH) then
  begin
    aTargetGame := tgKaMRemake;
    aWorkDir := exeDir + '..\';
  end else
  if FileExists(exeDir + KMR_LOCALES_PATH) then
  begin
    aTargetGame := tgKaMRemake;
    aWorkDir := exeDir;
  end else
  if FileExists(exeDir + KP_LOCALES_PATH) then
  begin
    aTargetGame := tgKnightsProvince;
    aWorkDir := exeDir;
  end;
end;


{ TForm1 }
function TForm1.Start: Boolean;
begin
  // Detect the game
  DetectGameAndPath(fTargetGame, fWorkDir);

  Caption := 'Translation Manager (' + DateTimeToStr(GetExeBuildTime) + ')';

  case fTargetGame of
    tgUnknown:          begin
                          MessageBox(
                            Form1.Handle,
                            'Can not find locales.txt\locales.xml file.' + sLineBreak +
                            'Please make sure to run the Translation Manager from the games folder',
                            'Error',
                            MB_ICONERROR);
                          Exit(False);
                        end;
    tgKaMRemake:        fLocales := TKMResLocales.Create(fWorkDir + 'data\locales.txt');
    tgKnightsProvince:  fLocales := TKMResLocales.Create(fWorkDir + 'data\text\locales.xml');
  end;

  fMode := umDeveloper;
  //fMode := umUser;

  InitLocalesList;

  fPathManager := TPathManager.Create;
  RefreshFolders;

  fTextManager := TKMTextManager.Create(fLocales);

  UpdateMenuItemVisibility;

  fSettingsPath := ChangeFileExt(ParamStr(0), '.xml');
  LoadSettings(fSettingsPath);

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


procedure TForm1.FormResize(Sender: TObject);
var
  I,K: Integer;
  SelCount, SecHeight: Word;
begin
  SelCount := 0;
  for I := 0 to fLocales.Count - 1 do
  if (I+1 < clbShowLang.Count) then
  if clbShowLang.Checked[I+1] then
    Inc(SelCount);

  if SelCount = 0 then
    Exit;

  SecHeight := ScrollBox1.ClientHeight div SelCount;

  K := 0;
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
  begin
    fTransLabels[I].SetBounds(8, 2 + K * SecHeight, 100, 20);
    fTransMemos[I].SetBounds(8, 22 + K * SecHeight, ScrollBox1.Width - 20, SecHeight - 24);
    Inc(K);
  end;
end;


procedure TForm1.UpdateMenuItemVisibility;
begin
  // Hide entries that Users should not access

  // Top
  cbPreferDict.Visible := (fMode = umDeveloper);
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
  fPathManager.AddPath(fWorkDir);

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

  // Special case for ingame text library
  if SameText(lbLibs.Items[id], TEXT_PATH) then
    fTextManager.Load(fWorkDir, lbLibs.Items[id], TAGS_PATH, META_PATH, [])
  else
    fTextManager.Load(fWorkDir, lbLibs.Items[id], '', '', []);

  RefreshFilter;
  RefreshList;
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  fTextManager.Save(cbPreferDict.Checked);
end;


procedure TForm1.RefreshList;
  function ShowTag(aLine: TKMLine): Boolean;
  var
    I,K, defLoc: Integer;
  begin
    Result := True;
    defLoc := fLocales.IndexByCode(gResLocales.DEFAULT_LOCALE);

    // Hide lines that have text
    if cbFilterMissing.Checked then
    begin
      Result := False;
      if not aLine.IsSpacer then
        for I := 0 to fLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
            Result := Result or (aLine.Strings[I] = '');
    end;

    // Show lines that are the same in selected locales
    if Result and cbFilterDuplicate.Checked then
    begin
      Result := False;
      if not aLine.IsSpacer then
        for I := 0 to fLocales.Count - 1 do
          if clbShowLang.Checked[I+1] then
          for K := 0 to fLocales.Count - 1 do
            if (K <> I) and clbShowLang.Checked[K+1] then
              Result := Result or (aLine.Strings[I] = aLine.Strings[K]);
    end;

    if Result and (edFilterText.Text <> '') then
      Result := not aLine.IsSpacer and (Pos(UpperCase(edFilterText.Text), UpperCase(aLine.Strings[defLoc])) <> 0);
  end;
var
  I, TopIdx, ItemIdx: Integer;
  s: string;
begin
  lbTags.Items.BeginUpdate;
  ItemIdx := lbTags.ItemIndex;
  TopIdx := lbTags.TopIndex;
  lbTags.Clear;

  SetLength(ListBoxLookup, 0);
  SetLength(ListBoxLookup, fTextManager.Count);

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

  lbTags.Items.EndUpdate;
  lbTags.ItemIndex := EnsureRange(ItemIdx, 0, lbTags.Count - 1);
  lbTags.TopIndex := TopIdx;
  lbTagsClick(lbTags);

  StatusBar1.Panels[0].Text := 'Count ' + IntToStr(lbTags.Count);
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
    fTransLabels[I] := TLabel.Create(Form1);
    fTransLabels[I].Parent := ScrollBox1;
    fTransLabels[I].Caption := fLocales[I].Title + ' (' + fLocales[I].Code + ')';
    fTransLabels[I].Hide;

    fTransMemos[I] := TMemo.Create(Form1);
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


procedure TForm1.LoadSettings(const aPath: string);
var
  I: Integer;
  xml: TKMXMLDocument;
  locs: string;
  isMaximized: Boolean;
begin
  xml := TKMXMLDocument.Create;
  try
    xml.LoadFromFile(aPath);
    locs := xml.Root.Attributes['Selected_Locales'].AsString(TKMResLocales.DEFAULT_LOCALE);
    isMaximized := xml.Root.Attributes['Maximized'].AsBoolean(True);
  finally
    xml.Free;
  end;

  //If there are any items "All" should be greyed
  if locs <> '' then
    clbShowLang.State[0] := cbGrayed;

  for I := 0 to fLocales.Count - 1 do
  if Pos(fLocales[I].Code, locs) <> 0 then
    clbShowLang.Checked[I+1] := True;

  if isMaximized then
    WindowState := wsMaximized
  else
    WindowState := wsNormal;

  UpdateVisibleLocales;
end;


procedure TForm1.SaveSettings(const aPath: string);
var
  locs: string;
  I: Integer;
  xml: TKMXMLDocument;
begin
  locs := '';
  for I := 0 to fLocales.Count - 1 do
  if clbShowLang.Checked[I+1] then
    locs := locs + IfThen(locs <> '', ',') + fLocales[I].Code;

  xml := TKMXMLDocument.Create;
  try
    xml.Root.Attributes['Selected_Locales'] := locs;
    xml.Root.Attributes['Maximized'] := (WindowState = wsMaximized);
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
  RefreshFilter;
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
  idx: integer;
begin
  idx := lbTags.ItemIndex; // Item place we are adding
  if idx = -1 then Exit;

  fTextManager.Insert(idx);
  RefreshList;
end;


procedure TForm1.btnInsertSeparatorClick(Sender: TObject);
var
  idx: integer;
begin
  idx := lbTags.ItemIndex; //Item place we are adding
  if idx = -1 then Exit;

  fTextManager.InsertSeparator(idx);
  RefreshList;
end;


procedure TForm1.btnDeleteClick(Sender: TObject);
var
  idx: integer;
begin
  idx := lbTags.ItemIndex; //Item place we are deleting
  if idx = -1 then Exit;

  fTextManager.Delete(idx);
  RefreshList;
end;


procedure TForm1.btnMoveUpClick(Sender: TObject);
var
  idx: integer;
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
  idx: integer;
begin
  idx := lbTags.ItemIndex;
  if idx = -1 then Exit;

  fTextManager.MoveDown(idx);
  RefreshList;
  lbTags.ItemIndex := Min(idx + 1, lbTags.Count - 1);
  lbTagsClick(nil); //Reselect the item to update the translation boxes
end;


procedure TForm1.cbFilterMissingClick(Sender: TObject);
begin
  RefreshFilter;
  RefreshList;

  //Select the first item
  lbTags.ItemIndex := 0;
  lbTagsClick(lbTags);
end;


procedure TForm1.RefreshFilter;
var
  id: Integer;
  isFiltered, isMainFile: Boolean;
begin
  id := lbLibs.ItemIndex;
  if id = -1 then Exit;

  isMainFile := SameText(lbLibs.Items[id], TEXT_PATH);
  isFiltered := cbFilterMissing.Checked or cbFilterDuplicate.Checked or (edFilterText.Text <> '');

  // Disable buttons
  btnCopyToClipboard.Enabled := True;
  btnPasteFromClipboard.Enabled := True;

  btnSortByIndex.Enabled := isMainFile and not isFiltered;
  btnSortByTag.Enabled := isMainFile and not isFiltered;
  btnCompactIndexes.Enabled := isMainFile and not isFiltered;
  btnListUnusedTags.Enabled := isMainFile and not isFiltered;

  btnInsert.Enabled := not isFiltered;
  btnInsertSeparator.Enabled := isMainFile and not isFiltered;
  btnRename.Enabled := isMainFile;
  btnDelete.Enabled := not isFiltered;
  btnMoveUp.Enabled := isMainFile and not isFiltered;
  btnMoveDown.Enabled := isMainFile and not isFiltered;
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
    fTextManager.ListMismatchingAll(fWorkDir, fPathManager.GetPaths, slMiss);

    slMiss.SaveToFile(fWorkDir + 'TM_Mismatching.txt');
    ShowMessage(slMiss.Text);
  finally
    slMiss.Free;
  end;
end;


procedure TForm1.btnListUnusedTagsClick(Sender: TObject);
var
  slTags: TStringList;

  procedure CheckPasFiles(const aDir: string);
  begin
    //todo: gIoPack.ListFiles(
    {
      aDir,
      True,
      procedure (const aFilename: string)
      var
        I: Integer;
        pasString: AnsiString;
      begin
        if (ExtractFileExt(aFilename) = '.pas')
        and not StartsStr('_', aFilename) then
        begin
          pasString := ReadTextA(aDir + aFilename);
          for I := slTags.Count - 1 downto 0 do
          if Pos(slTags[I], PasString) <> 0 then
            slTags.Delete(I);
        end;
      end,
      dlFolder);}
  end;
var
  I: Integer;
begin
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

  fTextManager.ToClipboardAll(fWorkDir, fPathManager.GetPaths, localesToCopy);
end;


procedure TForm1.btnPasteFromCBClick(Sender: TObject);
begin
  fTextManager.FromClipboard;
  RefreshList;
end;


procedure TForm1.btnPasteFromClipboardAllClick(Sender: TObject);
begin
  fTextManager.FromClipboardAll(fWorkDir);
end;


end.
