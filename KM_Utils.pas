unit KM_Utils;
{$I KM_CompilerDirectives.inc}
interface
uses
  Classes;

  // Game-specific utils
  function ApplyBrightnessToRGB(aColor: Cardinal; aBrightness: Byte): Cardinal;
  function KMGetPingColor(aPing: Word): Cardinal;
  function KMGetFPSColor(aFPS: Word): Cardinal;
  function KMIntToString(aValue: Integer): string;
  function KMTimeToString(aTime: TDateTime): string;
  function KMWrapColor(const aText: string; aColor: Cardinal): string;
  function StripColor(const aText: string): string;
  function LastColorCodeInString(const aText: string): string;

  // Custom S -> DT -> S functions that read/write into reliable format quickly
  function KMDateTimeToString(const aDateTime: TDateTime): string;
  function KMStringToDateTime(const aString: string): TDateTime;

  procedure ParseDelimited(const Value, Delimiter: UnicodeString; SL: TStringList);

  function TimeGet: Cardinal;
  function TimeGetSince(aTime: Cardinal): Cardinal;
  function TimeGetUsec: Int64;
  function TimeGetUsecSince(aTime: Int64): Int64;

  procedure KMSwapInt(var A,B: Integer); overload;
  procedure KMSwapInt(var A,B: Cardinal); overload;

  function KMGetMultiplicator(aShift: TShiftState): Word;
  function RangesOverlap(aA1, aA2, aB1, aB2: Integer): Boolean;
  function GetNextResizedName(const aPath: string): string;

  function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';


implementation
uses
  DateUtils, Math, SysUtils, StrUtils,
  {$IFDEF MSWINDOWS}
  MMSystem, Windows, // Required for TimeGet which is defined locally because this unit must NOT know about KromUtils as it is not Linux compatible (and this unit is used in Linux dedicated servers)
  {$ENDIF}
  KromStringUtils;


procedure KMSwapInt(var A,B: Integer);
var s: Integer;
begin
  s:=A; A:=B; B:=s;
end;

procedure KMSwapInt(var A,B: Cardinal);
var s: Cardinal;
begin
  s:=A; A:=B; B:=s;
end;


// Return current time in milliseconds
// This unit must not know about KromUtils because it is used by the Linux Dedicated servers
// and KromUtils is not Linux compatible. Therefore this function is copied directly from KromUtils.
// Do not remove and add KromUtils to uses, that would cause the Linux build to fail
function TimeGet: Cardinal;
begin
  {$IFDEF ANDROID}
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000) mod High(Cardinal));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := TimeGetTime; // Return milliseconds with ~1ms precision
  {$ENDIF}
  {$IFDEF UNIX}
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000) mod High(Cardinal));
  {$ENDIF}
end;


function TimeGetSince(aTime: Cardinal): Cardinal;
begin
  // TimeGet will loop back to zero after ~49 days since system start
  Result := (Int64(TimeGet) - Int64(aTime) + Int64($FFFFFFFF)) mod Int64($FFFFFFFF);
end;


// Returns time in micro-seconds (usec)
function TimeGetUsec: Int64;
var
  freq: Int64;
  newTime: Int64;
  factor: Double;
begin
  {$IFDEF ANDROID}
  // Stub for now
  Result := Int64(Trunc(Now * 24 * 60 * 60 * 1000000) mod High(Int64));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(newTime);

  factor := 1000000 / freq; // Separate calculation to avoid "big Int64 * 1 000 000" overflow

  Result := Round(newTime * factor);
  {$ENDIF}
end;


// Returns time delta in micro-seconds, accounting for loop back to zero
function TimeGetUsecSince(aTime: Int64): Int64;
var
  newTime: Int64;
begin
  newTime := TimeGetUsec;
  Result := (newTime - aTime);
end;


const
  icGreen  = $FF00C000;
  icYellow = $FF07FFFF;
  icOrange = $FF0099FF;
  icRed    = $FF0707FF;


function KMGetPingColor(aPing: Word): Cardinal;
begin
  case aPing of
    0..299  : Result := icGreen;
    300..599: Result := icYellow;
    600..999: Result := icOrange;
    else      Result := icRed;
  end;
end;


function KMGetFPSColor(aFPS: Word): Cardinal;
begin
  case aFPS of
    0..9  : Result := icRed;
    10..12: Result := icOrange;
    13..15: Result := icYellow;
    else    Result := icGreen;
  end;
end;


function ApplyBrightnessToRGB(aColor: Cardinal; aBrightness: Byte): Cardinal;
begin
  Result := Round((aColor and $FF) / 255 * aBrightness)
            or
            Round((aColor shr 8 and $FF) / 255 * aBrightness) shl 8
            or
            Round((aColor shr 16 and $FF) / 255 * aBrightness) shl 16
            or
            (aColor and $FF000000);
end;


function KMIntToString(aValue: Integer): string;
begin
  if aValue = 0 then
    Result := '-'
  else
  if aValue >= 1000 then
    Result := IntToStr(aValue div 1000) + 'k'
  else
    Result := IntToStr(aValue);
end;


// Convert DateTime to string xx:xx:xx where hours have at least 2 digits
// F.e. we can have 72:12:34 for 3 days long game
function KMTimeToString(aTime: TDateTime): string;
begin
  // We can't use simple Trunc(aTime * 24 * 60 * 60) maths because it is prone to rounding errors
  // e.g. 3599 equals to 59:58 and 3600 equals to 59:59
  // That is why we resort to DateUtils routines which are slower but much more correct
  // : Displays the time separator character given by the TimeSeparator global variable
  Result :=  Format('%.2d', [HoursBetween(aTime, 0)]) + FormatDateTime(':nn:ss', aTime);
end;


// Make a string wrapped into color code
function KMWrapColor(const aText: string; aColor: Cardinal): string;
begin
  Result := '[$' + IntToHex(aColor and $00FFFFFF, 6) + ']' + aText + '[]';
end;


function StripColor(const aText: string): string;
var
  I: Integer;
  skippingMarkup: Boolean;
begin
  Result := '';
  skippingMarkup := False;

  for I := StringLow(aText) to StringHigh(aText) do
  begin
    if (I+1 <= StringHigh(aText))
    and ((aText[I] + aText[I+1] = '[$') or (aText[I] + aText[I+1] = '[]')) then
      skippingMarkup := True;

    if not skippingMarkup then
      Result := Result + aText[I];

    if skippingMarkup and (aText[I] = ']') then
      skippingMarkup := False;
  end;
end;


function LastColorCodeInString(const aText: string): string;
var
  I: Integer;
  openBracket, closeBracket: Integer;
begin
  Result := '';
  openBracket := -1;
  closeBracket := -1;

  for I := StringHigh(aText) downto StringLow(aText) do
  begin
    if (aText[I] = ']') then
      closeBracket := I;

    if (I+1 <= StringHigh(aText))
    and ((aText[I] + aText[I+1] = '[$') or (aText[I] + aText[I+1] = '[]')) then
      openBracket := I;

    if (openBracket <> -1) and (closeBracket <> -1) and (openBracket < closeBracket) then
      Exit(Copy(aText, openBracket, closeBracket - openBracket + 1));
  end;
end;


//Taken from: http://delphi.about.com/od/adptips2005/qt/parsedelimited.htm
procedure ParseDelimited(const Value, Delimiter: UnicodeString; SL: TStringList);
var
  dx: Integer;
  ns: UnicodeString;
  txt: UnicodeString;
  Delta: Integer;
begin
  Delta := Length(Delimiter);
  txt := Value + Delimiter;
  SL.BeginUpdate;
  SL.Clear;
  try
    while Length(txt) > 0 do
    begin
      dx := Pos(Delimiter, txt);
      ns := Copy(txt, 0, dx-1);
      SL.Add(ns);
      txt := Copy(txt, dx+Delta, MaxInt);
    end;
  finally
    SL.EndUpdate;
  end;
end;


function KMGetMultiplicator(aShift: TShiftState): Word;
begin
  if (ssLeft in aShift) and (ssRight in aShift) then
    Result := 100
  else
  if ssRight in aShift then
    Result := 10
  else
  if ssLeft in aShift then
    Result := 1
  else
    Result := 0;
end;


// Test if 2 ranges overlap
//     A1----A2
// B1----B2
function RangesOverlap(aA1, aA2, aB1, aB2: Integer): Boolean;
begin
  Result := (aA1 < aB2) and (aB1 < aA2);
end;


// MapName (Resized 999)
function GetNextResizedName(const aPath: string): string;
var
  s1, s2: Integer;
  n: Integer;
begin
  s1 := Pos(' (Resized', aPath) + Length(' (Resized');
  s2 := PosEx(')', aPath, s1);
  if (s1 <> 0) and (s2 <> 0) and (s2-s1 <= 4) then
  begin
     n := StrToIntDef(Copy(aPath, s1, s2-s1), 1);

    Result := Copy(aPath, 1, s1-1) + ' ' + IntToStr(n+1) + ')';
  end else
    Result := aPath + ' (Resized)';
end;


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


end.
