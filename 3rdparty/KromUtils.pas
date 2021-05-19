unit KromUtils;
{$I ..\KM_CompilerDirectives.inc}
interface
uses
  Classes, Math, StrUtils,
  {$IFDEF ANDROID}

  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$IFDEF WDC}
    Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, DateUtils, ImageHlp,
    {$IF CompilerVersion >= 29.0}
    // in XE8 (2015) we got the System.Hash unit, which brought the MD5, SHA-1, and Bob Jenkins hashes.
    // Then in 10.0 Seattle (2015) it was expanded with SHA-2 support.
    System.Hash,
    {$IFEND}
    ActiveX, ComObj, Variants, ShellApi,
    {$ENDIF}
    Windows, MMSystem, psAPI,
  {$ENDIF}
  SysUtils;


  procedure FreeThenNil(var Obj);

  function Int2fix(aValue, aLength: Integer): string;

  function Min(const A, B, C: Integer): Integer; overload;
  function Min(const A, B, C: Single): Single; overload;
  function Min(const A, B, C, D: Byte): Byte; overload;
  function Min(const A, B, C, D: Single): Single; overload;
  function Min(const A, B, C, D, E: Single): Single; overload;
  function Min(aValues: array of Single): Single; overload;
  function MinIndex(aValues: array of Single): Byte;

  function Max(const A, B, C: Integer): Integer; overload;
  function Max(const A, B, C: Single): Single; overload;
  function Max(const A, B, C, D: Integer): Integer; overload;
  function Max(const A, B, C, D: Single): Single; overload;
  function Max(const A, B, C, D, E: Single): Single; overload;

  function GetLengthF(A, B: Single): Single;
  function GetLengthSqr(aX, aY: Integer): Integer;

  function Lerp(A,B: Single; aMixValue: Single): Single;

  procedure SwapStr(var A, B: string);
  procedure SwapInt(var A, B: Byte); overload;
  procedure SwapInt(var A, B: ShortInt); overload;
  procedure SwapInt(var A, B: SmallInt); overload;
  procedure SwapInt(var A, B: Word); overload;
  procedure SwapInt(var A, B: Integer); overload;
  procedure SwapInt(var A, B: Cardinal); overload;
  {$IFDEF WDC}
  procedure SwapInt(var A, B: NativeUInt); overload;
  {$ENDIF}
  procedure SwapFloat(var A, B: Single);
  function Equals(A, B: Single; const Epsilon: Single = 0.001): Boolean;

  function MakePOT(num: Integer): Integer;
  function Adler32CRC(aPointer: Pointer; aLength: Cardinal): Cardinal; overload;
  function Adler32CRC(const aText: string): Cardinal; overload;
  function Adler32CRC(S: TMemoryStream): Cardinal; overload;

  function BobJenkins32CRC(const aData; aLength: Cardinal): Cardinal; overload;
  function BobJenkins32CRC(const aText: string): Cardinal; overload;
  function BobJenkins32CRC(S: TMemoryStream): Cardinal; overload;

  function KMSameContents(const aFilenameA, aFilenameB: string): Boolean;

{$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  procedure DoClientAreaResize(aForm: TForm);
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  procedure BrowseFolder(const aPath: string);
  function BrowseURL(const URL: string) : Boolean;
  procedure MailTo(const aAddress, aSubject, aBody: string);

  {$IFDEF DESKTOP}
  function RunOpenDialog(Sender: TOpenDialog; Name,Path,Filter: string): Boolean;
  function RunSaveDialog(Sender: TSaveDialog; aFilename, aFilePath, aFilter: string; const aFileExt: string = ''): Boolean;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  function GetMotherBoardSerial: string;
  function GetSystemUILanguage: Word;
  function GetLanguageName(aId: LANGID): string;

  function GetExeBuildTime: TDateTime;

  function CreateProcessSimple(aFilename: string; aShowWindow, aWait: Boolean): NativeUInt;
  procedure TerminateProcessSimple(aProcessHandle: NativeUInt);
  function GetMemUsed: NativeUInt;
  function GetCommittedStackSize: NativeUInt;
  {$ENDIF}
{$ENDIF}

implementation


function Min(const A, B, C: Integer): Integer;
begin
  if A < B then if A < C then Result := A else Result := C
           else if B < C then Result := B else Result := C;
end;

function Min(const A, B, C: Single): Single;
begin
  if A < B then if A < C then Result := A else Result := C
           else if B < C then Result := B else Result := C;
end;


function Min(const A, B, C, D: Byte): Byte;
begin
  Result := Math.Min(Math.Min(A, B), Math.Min(C, D));
end;


function Min(const A, B, C, D: Single): Single;
begin
  Result := Math.Min(Math.Min(A, B), Math.Min(C, D));
end;


function Min(const A, B, C, D, E: Single): Single;
begin
  Result := Math.Min(Math.Min(A, B), Math.Min(Math.Min(C, D), E));
end;


function Min(aValues: array of Single): Single;
var
  I: Integer;
begin
  Assert(Length(aValues) > 0);
  Result := aValues[0];
  for I := 1 to High(aValues) do
    if aValues[I] < Result then
      Result := aValues[I];
end;


// Pick minimum value and return its index
// if two or more values are equal then first in list is returned
function MinIndex(aValues: array of Single): Byte;
var
  I: Integer;
begin
  Assert(Length(aValues) > 0);
  Result := 0;
  for I := 1 to High(aValues) do
    if aValues[I] < aValues[Result] then
      Result := I;
end;


function Max(const A, B, C: Integer): Integer; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;


function Max(const A, B, C: Single): Single; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;


function Max(const A, B, C, D: Integer): Integer;
begin
  Result := Max(Max(A, B), Max(C, D));
end;


function Max(const A, B, C, D: Single): Single;
begin
  Result := Math.Max(Math.Max(A, B), Math.Max(C, D));
end;


function Max(const A, B, C, D, E: Single): Single;
begin
  Result := Math.Max(Math.Max(A, B), Math.Max(Math.Max(C, D), E));
end;


procedure FreeThenNil(var Obj);
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;


function Int2fix(aValue, aLength: Integer): string;
begin
  Result := IntToStr(aValue);
  Result := DupeString('0', aLength - Length(Result)) + Result;

  if Length(Result) > aLength then
    Result := DupeString('*', aLength);
end;


// Return closest bigger PowerOfTwo number
function MakePOT(num: Integer): Integer;
begin
  num := num - 1;
  num := num OR (num SHR 1);
  num := num OR (num SHR 2);
  num := num OR (num SHR 4);
  num := num OR (num SHR 8);
  num := num OR (num SHR 16); // 32bit needs no more
  Result := num + 1;
end;


function GetLengthSqr(aX, aY: Integer): Integer;
begin
  Result := Sqr(aX) + Sqr(aY);
end;


function GetLengthF(A, B: Single): Single;
begin
  Result := Sqrt(Sqr(A) + Sqr(B));
end;


function Lerp(A,B: Single; aMixValue: Single): Single; inline;
begin
  Result := A + (B - A) * aMixValue;
end;


procedure SwapStr(var A, B: string);
var
  S: string;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A, B: Byte);
var
  S: Byte;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A, B: ShortInt);
var
  S: ShortInt;
begin
  S := A; A := B; B := S;
end;


procedure SwapInt(var A, B: SmallInt);
var s: SmallInt;
begin
  s:=A; A:=B; B:=s;
end;


procedure SwapInt(var A, B: Word);
var s: Word;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A, B: Integer);
var s: Integer;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A, B: Cardinal);
var s: Cardinal;
begin
  s:=A; A:=B; B:=s;
end;

{$IFDEF WDC}
procedure SwapInt(var A, B: NativeUInt);
var s:NativeUInt;
begin
  s:=A; A:=B; B:=s;
end;
{$ENDIF}

procedure SwapFloat(var A, B: Single);
var s: Single;
begin
  s:=A; A:=B; B:=s;
end;


function Equals(A, B: Single; const Epsilon: Single = 0.001): Boolean;
begin
  Result := Abs(A-B) <= Epsilon;
end;


function Adler32CRC(aPointer: Pointer; aLength: Cardinal): Cardinal;
const
  MAX_PRIME_16BIT = 65521; // 65521 is the largest prime number smaller than 2^16
var
  I, A, B: Cardinal;
begin
  A := 1;
  B := 0; // A is initialized to 1, B to 0

  if aLength <> 0 then // Check to avoid CardinalOverflow on -1
  for I := 0 to aLength - 1 do
  begin
    Inc(A, pByte(Cardinal(aPointer) + I)^);
    // We need to MOD B within cos it may overflow in files larger than 65kb, A overflows with files larger than 16mb
    B := (B + A) mod MAX_PRIME_16BIT;
  end;

  A := A mod MAX_PRIME_16BIT;
  Result := B + A shl 16; // Reverse order for smaller numbers
end;


function Adler32CRC(const aText: string): Cardinal;
begin
  Result := Adler32CRC(PChar(aText), Length(aText) * SizeOf(Char));
end;


function Adler32CRC(S: TMemoryStream): Cardinal;
begin
  Result := Adler32CRC(S.Memory, S.Size);
end;


// BobJenkins32 is faster than Adler32 and has fewer collisions
// We still use it as CRC since it has the same compact 4byte size
function BobJenkins32CRC(const aData; aLength: Cardinal): Cardinal; overload;
{$IF CompilerVersion >= 29.0}
var
  h: THashBobJenkins;
{$IFEND}
begin
  {$IF CompilerVersion >= 29.0}
  h := THashBobJenkins.Create;
  h.Update(aData, aLength);
  Result := Cardinal(h.HashAsInteger);
  {$IFEND}
end;


function BobJenkins32CRC(const aText: string): Cardinal;
begin
  Result := BobJenkins32CRC(PChar(aText)^, Length(aText) * SizeOf(Char));
end;


function BobJenkins32CRC(S: TMemoryStream): Cardinal;
begin
  Result := BobJenkins32CRC(PByte(S.Memory)^, S.Size);
end;


function KMSameContents(const aFilenameA, aFilenameB: string): Boolean;
var
  fsa: TFileStream;
  fsb: TFileStream;
  msa: TMemoryStream;
  msb: TMemoryStream;
begin
  fsa := TFileStream.Create(aFilenameA, fmOpenRead or fmShareDenyNone);
  fsb := TFileStream.Create(aFilenameB, fmOpenRead or fmShareDenyNone);
  msa := TMemoryStream.Create;
  msb := TMemoryStream.Create;

  msa.LoadFromStream(fsa);
  msb.LoadFromStream(fsb);

  Result := (msa.Size = msb.Size) and CompareMem(msa.Memory, msb.Memory, msa.Size);

  msa.Free;
  msb.Free;
  fsa.Free;
  fsb.Free;
end;


{$IFNDEF FPC}
{$IFDEF MSWindows}
procedure DoClientAreaResize(aForm: TForm);
const
  DesignHeight = 18;
var
  HeightDif: Integer;
  I: Integer;
begin
  HeightDif := GetSystemMetrics(SM_CYCAPTION) - DesignHeight;

  for I:=0 to aForm.ControlCount-1 do
    if (akBottom in aForm.Controls[I].Anchors) and
       (akTop in aForm.Controls[I].Anchors) then
      aForm.Controls[I].Height := aForm.Controls[I].Height - HeightDif
    else
    if (akBottom in aForm.Controls[I].Anchors) then
      aForm.Controls[I].Top := aForm.Controls[I].Top - HeightDif;

  aForm.ClientHeight := aForm.ClientHeight + HeightDif;
end;
{$ENDIF}


// Browse to folder and select a file
procedure BrowseFolder(const aPath: string);
begin
  ShellExecute(Application.Handle, 'open', 'explorer.exe', PChar('/select,"' + aPath + '"'), nil, SW_NORMAL);
end;


function BrowseURL(const URL: string): Boolean;
{$IFDEF FPC}
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: LongInt;
  BrowserProcess: TProcessUTF8;
{$ENDIF}
begin
  //We need some result incase it's neither WDC nor FPC
  Result := False;

  {$IFDEF MSWindows}
    {$IFDEF WDC}
      //ShellExecute returns a value greater than 32 if successful, or an error value that is less than or equal to 32 otherwise
      if ShellExecute(Application.Handle, 'open', PChar(URL),nil,nil, SW_SHOWNORMAL) > 32 then
        Result := True;
    {$ENDIF}

    {$IFDEF FPC}
    v := THTMLBrowserHelpViewer.Create(nil);
    try
      v.FindDefaultBrowser(BrowserPath, BrowserParams);

      p := System.Pos('%s', BrowserParams);
      System.Delete(BrowserParams, p, 2);
      System.Insert(URL, BrowserParams, p);

      // start browser
      BrowserProcess := TProcessUTF8.Create(nil);
      try
        BrowserProcess.CommandLine := BrowserPath + ' ' + BrowserParams;
        BrowserProcess.Execute;
        Result := True;
      finally
        BrowserProcess.Free;
      end;
    finally
      v.Free;
    end;
    {$ENDIF}
  {$ENDIF}
end;


procedure MailTo(const aAddress, aSubject, aBody: string);
begin
  BrowseURL('mailto:' + aAddress + '?subject=' + aSubject + '&body=' + aBody);
end;


{$IFDEF DESKTOP}
function RunOpenDialog(Sender: TOpenDialog; Name, Path, Filter: string): Boolean;
begin
  Sender.FileName := Name;
  Sender.InitialDir := Path;
  Sender.Filter := Filter;
  Result := Sender.Execute; // Returns "false" if user pressed "Cancel"
  // Result :=Result and FileExists(Sender.FileName); //Already should be enabled in OpenDialog options
end;


function RunSaveDialog(Sender: TSaveDialog; aFilename, aFilePath, aFilter: string; const aFileExt: string = ''): Boolean;
begin
  Sender.FileName := aFilename;
  Sender.InitialDir := ExpandFileName(aFilePath);
  Sender.Filter := aFilter;
  Result := Sender.Execute; // Returns "false" if user pressed "Cancel"

  if not Result then
    Exit;

  if UpperCase(ExtractFileExt(Sender.FileName)) <> aFileExt then
    Sender.FileName := Sender.FileName + aFileExt;
end;
{$ENDIF}


function GetExeBuildTime: TDateTime;
var
  {$IFDEF MSWINDOWS}
  LI: TLoadedImage;
  {$IF CompilerVersion >= 26.0}
  m: TMarshaller;
  {$ENDIF}
  timeStamp: Cardinal;
  utcTime: TDateTime;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$IF CompilerVersion >= 26.0}
  // XE7 requires TMarshaller to convert to PAnsiChar
  // Function fail if the path has Unicode chars. Hence we trim the path
  Win32Check(MapAndLoad(PAnsiChar(m.AsAnsi(ExtractFileName(ParamStr(0)))), nil, @LI, False, True));
  {$ELSE}
  Win32Check(MapAndLoad(PAnsiChar(AnsiString(ExtractFileName(ParamStr(0)))), nil, @LI, False, True));
  {$IFEND}
  timeStamp := LI.FileHeader.FileHeader.TimeDateStamp;
  UnMapAndLoad(@LI);

  utcTime := UnixToDateTime(timeStamp);
  Result := TTimeZone.Local.ToLocalTime(utcTime);
  {$ENDIF}
  {$IFNDEF MSWINDOWS}
  Result := EncodeDate(1900, 0, 0);
  {$ENDIF}
end;


{$IFDEF MSWINDOWS}
function GetMotherBoardSerial: string;
var
  objWMIService: OLEVariant;
  colItems: OLEVariant;
  colItem: OLEVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;

  function GetWMIObject(const objectName: string): IDispatch;
  var
    chEaten: Integer;
    BindCtx: IBindCtx;
    Moniker: IMoniker;
  begin
    OleCheck(CreateBindCtx(0, BindCtx));
    OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
    OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  end;

begin
  Result := '';
  try
    objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
    colItems := objWMIService.ExecQuery('SELECT SerialNumber FROM Win32_BaseBoard', 'WQL', 0);
    oEnum := IUnknown(colItems._NewEnum) as IEnumvariant;
    if oEnum.Next(1, colItem, iValue) = 0 then
      Result := VarToStr(colItem.SerialNumber);
  except
    // We don't have much choice if this function fails (e.g. blocked by antivirus)
    Result := 'GetMotherBoardSerial.Failed';
  end;
end;


function GetUserDefaultUILanguage: LANGID; external kernel32 name 'GetUserDefaultUILanguage';


function GetSystemUILanguage: Word;
begin
  // The return value is the input locale identifier for the thread.
  // The low word contains a lang Identifier for the input lang
  // and the high word contains a device handle to the physical layout of the keyboard.
  //Result := IntToHex(GetKeyboardLayout(0), 8);

  Result := GetUserDefaultUILanguage;
end;


function GetLanguageName(aId: LANGID): string;
var
  lang: array [0 .. 100] of Char;
begin
  VerLanguageName(aId, lang, 100);
  Result := lang;
end;


function CreateProcessSimple(aFilename: string; aShowWindow, aWait: Boolean): NativeUInt;
var
  appName: array [0..512] of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  res: Cardinal;
begin
  StrPCopy(appName, aFilename);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := IfThen(aShowWindow, SW_SHOWDEFAULT, SW_HIDE);

  CreateProcess(
    nil,
    appName,
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS,
    nil,
    nil,
    StartupInfo,
    ProcessInfo);

  Result := ProcessInfo.hProcess;

  if aWait then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, res);
    Result := 0;
  end;
end;


procedure TerminateProcessSimple(aProcessHandle: NativeUInt);
begin
  TerminateProcess(aProcessHandle, 0);
end;


function GetMemUsed: NativeUInt;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Integer;
begin
  cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
  GetMem(pmc, cb);
  pmc^.cb := cb;
  if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
    Result := pmc^.WorkingSetSize
  else
    Result := 0;

  FreeMem(pmc);
end;


function GetCommittedStackSize: NativeUInt;
//NB: Win32 uses FS, Win64 uses GS as base for Thread Information Block.
asm
 {$IFDEF WIN32}
  mov eax, [fs:04h] // TIB: base of the stack
  mov edx, [fs:08h] // TIB: lowest committed stack page
  sub eax, edx      // compute difference in EAX (=Result)
 {$ENDIF}
 {$IFDEF WIN64}
  mov rax, abs [gs:08h] // TIB: base of the stack
  mov rdx, abs [gs:10h] // TIB: lowest committed stack page
  sub rax, rdx          // compute difference in RAX (=Result)
 {$ENDIF}
end;
{$ENDIF} // IFDEF MSWindows
{$ENDIF} // IFNDEF FPC


end.
