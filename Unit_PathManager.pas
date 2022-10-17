unit Unit_PathManager;
{$I KM_CompilerDirectives.inc}
interface
uses
  Classes, StrUtils, SysUtils;


type
  // Scans folder and subfolders in search of a .libx files
  // Provides list of found files as "fullpath\filename.%s.libx"
  TPathManager = class
  private
    fPaths: TStringList;
    function GetPath(aIndex: Integer): string;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Paths[aIndex: Integer]: string read GetPath; default;
    property GetPaths: TStringList read fPaths;
    procedure Clear;
    procedure AddPath(aPath: string);
  end;


implementation


{ TPathManager }
constructor TPathManager.Create;
begin
  inherited;

  fPaths := TStringList.Create;
end;


destructor TPathManager.Destroy;
begin
  fPaths.Free;

  inherited;
end;


function TPathManager.GetCount: Integer;
begin
  Result := fPaths.Count;
end;


function TPathManager.GetPath(aIndex: Integer): string;
begin
  Result := fPaths[aIndex];
end;


procedure TPathManager.Clear;
begin
  fPaths.Clear;
end;


procedure TPathManager.AddPath(aPath: string);
var
  I: Integer;
  fileMask: string;
  searchRec: TSearchRec;
  subFolders: TStringList;
  pathAdded: Boolean;
begin
  subFolders := TStringList.Create;
  subFolders.Add(aPath);

  I := 0;
  repeat
    if FindFirst(subFolders[I] + '*', faAnyFile, searchRec) = 0 then
    begin
      pathAdded := False;
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          if (searchRec.Attr and faDirectory = faDirectory) then
            // Always add sub-folders
            subFolders.Add(subFolders[I] + searchRec.Name + '\')
          else
            if not pathAdded and SameText(ExtractFileExt(searchRec.Name), '.libx') then
            begin
              // When we see a libx, add its path exactly once
              fileMask := LeftStr(searchRec.Name, Length(searchRec.Name) - 8) + '%s.libx';
              fPaths.Add(ExtractRelativePath(aPath, subFolders[I]) + fileMask);
              pathAdded := True;
            end;
      until (FindNext(searchRec) <> 0);
      FindClose(searchRec);
    end;
    Inc(I);
  until (I >= subFolders.Count);

  subFolders.Free;
end;


end.
