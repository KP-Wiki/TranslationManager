unit KromNestedLibrary;
{$I KM_CompilerDirectives.inc}
interface
uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults;

// NestedLibrary is backwards compatible with LIBX
// Features description - https://github.com/Kromster80/knights_province/wiki/Text-localizations-(libx-files)

type
  TNodeType = (ntBranch, ntValue);

  TKMPair = record
  public
    Name, Value: string;
    class function New(const aName, aValue: string): TKMPair; static;
  end;

  TKMNestedNode = class
  private const
    ALLOW_DUPLICATES = False;
    ALLOW_LINEBREAKS_IN_VALUES = False;
    FULL_BRANCH_NAMES = True; // Easier to parse, padding becomes irrelevant

    CH_COMMENT = '#';
    CH_NODE_DELIMITER = '.';
    CH_NODE_BRACKET_OPEN = '[';
    CH_NODE_BRACKET_CLOSE = ']';
    CH_KEY_VALUE_DELIMITER = ':';

    ALLOWED_NODE_CHARS = ['0'..'9', 'a'..'z', 'A'..'Z', '_', CH_NODE_DELIMITER];
    ALLOWED_KEY_CHARS  = ['0'..'9', 'a'..'z', 'A'..'Z', '_', CH_NODE_DELIMITER];
  private
    fNodeType: TNodeType;
    fParent: TKMNestedNode;
    fChildren: TObjectList<TKMNestedNode>;
    function ValidateNode(const aNode: string): Boolean;
    function ValidateKey(const aKey: string): Boolean;
  public
    Comment: string;
    Name: string;
    Value: string;

    constructor Create(aParent: TKMNestedNode; aNodeType: TNodeType; const aName: string);
    destructor Destroy; override;

    procedure AddNode(const aName, aValue: string);
    procedure Clear;
    function FindNode(const aName: string): TKMNestedNode;
    function IsRoot: Boolean;
    function NameWithParent: string;
    procedure Sort;
    procedure LoadFromString(const aString: string);
    function SaveToString(const aPadding: string; aPadHierarchy: Boolean): string;
    function ToArray: TArray<TKMPair>;
  end;

  // INI-like library for localization libraries with nested (tree-like) Key-Value nodes
  TKMNestedLibrary = class
  private
    fRoot: TKMNestedNode;
  public
    PadHierarchy: Boolean;

    constructor Create;
    destructor Destroy; override;

    property Root: TKMNestedNode read fRoot;
    function ToArray: TArray<TKMPair>;

    procedure Clear;
    procedure LoadFromString(const aString: string);
    function SaveToString: string;
  end;


implementation
uses
  System.SysUtils, System.StrUtils,
  KromStringUtils;


{ TKMPair }
class function TKMPair.New(const aName, aValue: string): TKMPair;
begin
  Result.Name := aName;
  Result.Value := aValue;
end;


{ TKMNestedNode }
constructor TKMNestedNode.Create(aParent: TKMNestedNode; aNodeType: TNodeType; const aName: string);
begin
  inherited Create;

  fNodeType := aNodeType;
  fParent := aParent;
  fChildren := TObjectList<TKMNestedNode>.Create(
    TComparer<TKMNestedNode>.Construct(
    function(const A, B: TKMNestedNode): Integer
    begin
      // Values come first
      Result := Ord(B.fNodeType) - Ord(A.fNodeType);

      if Result = 0 then
        // Alphabetic
        Result := CompareStr(A.Name, B.Name);
    end));
  Name := aName;
end;


destructor TKMNestedNode.Destroy;
begin
  FreeAndNil(fChildren);

  inherited;
end;


procedure TKMNestedNode.Clear;
begin
  fChildren.Clear;
end;


function TKMNestedNode.FindNode(const aName: string): TKMNestedNode;
var
  nodeName, remainder: string;
  delimPos: Integer;
begin
  if aName = '' then
    Exit(Self);

  delimPos := Pos(CH_NODE_DELIMITER, aName);
  if delimPos > 0 then
  begin
    nodeName := LeftStr(aName, delimPos - 1);
    remainder := RightStr(aName, Length(aName) - delimPos);
  end else
  begin
    nodeName := aName;
    remainder := '';
  end;

  Result := nil;
  for var I in fChildren do
  if I.Name = nodeName then
    Result := FindNode(remainder);
end;


function TKMNestedNode.IsRoot: Boolean;
begin
  Result := (fParent = nil);
end;


function TKMNestedNode.NameWithParent: string;
begin
  if not fParent.IsRoot then
    Result := fParent.NameWithParent + '.' + Name
  else
    Result := Name;
end;


function TKMNestedNode.ValidateKey(const aKey: string): Boolean;
begin
  Result := True;
  for var I in aKey do
  if not CharInSet(I, ALLOWED_KEY_CHARS) then
    Exit(False);
end;


function TKMNestedNode.ValidateNode(const aNode: string): Boolean;
begin
  Result := True;
  for var I in aNode do
  if not CharInSet(I, ALLOWED_NODE_CHARS) then
    Exit(False);
end;


procedure TKMNestedNode.LoadFromString(const aString: string);
var
  sl: TStringList;
  thisLine: string;
  lastNode: string;
begin
  Clear;

  sl := TStringList.Create;
  sl.Text := aString;

  lastNode := '';
  for var I in sl do
  begin
    thisLine := Trim(I);

    // Empty line, safe to skip
    if thisLine = '' then
      Continue;

    // Comment, safe to skip
    if LeftStr(thisLine, 1) = CH_COMMENT then
      Continue;

    // Node
    if (LeftStr(thisLine, 1) = CH_NODE_BRACKET_OPEN) and (RightStr(thisLine, 1) = CH_NODE_BRACKET_CLOSE) then
    begin
      var newNode := Copy(thisLine, 2, Length(thisLine) - 2);
      if not ValidateNode(newNode) then
        raise Exception.Create(Format('Node "%s" does not comply with libx requirements', [newNode]));

      lastNode := newNode;

      Continue;
    end;

    // Key:Value
    var delimPos := Pos(CH_KEY_VALUE_DELIMITER, thisLine);

    // No key:value delimiter, odd line, but safe to skip
    if delimPos = 0 then
      Continue;

    var thisKey := LeftStr(thisLine, delimPos - 1);
    var thisValue := RightStr(thisLine, Length(thisLine) - delimPos);

    if not ValidateKey(thisKey) then
      raise Exception.Create(Format('Key "%s" does not comply with libx requirements', [thisKey]));

    AddNode(lastNode + IfThen(lastNode <> '', '.') + thisKey, thisValue);
  end;

  sl.Free;
end;


function TKMNestedNode.SaveToString(const aPadding: string; aPadHierarchy: Boolean): string;
begin
  Result := '';

  if Comment <> '' then
    Result := aPadding + CH_COMMENT + StringReplace(Comment, sLineBreak, sLineBreak + aPadding + CH_COMMENT, [rfReplaceAll]) + sLineBreak;

  case fNodeType of
    ntBranch: begin
                if not IsRoot then
                if FULL_BRANCH_NAMES then
                  Result := aPadding + CH_NODE_BRACKET_OPEN + NameWithParent + CH_NODE_BRACKET_CLOSE + sLineBreak
                else
                  Result := aPadding + CH_NODE_BRACKET_OPEN + Name + CH_NODE_BRACKET_CLOSE + sLineBreak;

                for var I in fChildren do
                  Result := Result + I.SaveToString(aPadding + IfThen(aPadHierarchy and not IsRoot, '    '), aPadHierarchy);
              end;
    ntValue:  Result := aPadding + Name + CH_KEY_VALUE_DELIMITER + Value + sLineBreak;
  end;
end;


procedure TKMNestedNode.Sort;
begin
  // Sort our nodes
  fChildren.Sort;

  // Sort all nodes in children too (recursively)
  for var I in fChildren do
    I.Sort;
end;


function TKMNestedNode.ToArray: TArray<TKMPair>;
begin
  Result := [];

  case fNodeType of
    ntBranch: for var I in fChildren do
                Result := Result + I.ToArray;
    ntValue:  Result := [TKMPair.New(NameWithParent, Value)];
  end;
end;


procedure TKMNestedNode.AddNode(const aName, aValue: string);
var
  nodeName, remainder: string;
  delimPos: Integer;
  n: TKMNestedNode;
  nodeFound: Boolean;
begin
  if HasLineBreakChars(aName) then
    raise Exception.Create('Unexpected linebreak in Name - ' + aName);
  if not ALLOW_LINEBREAKS_IN_VALUES and HasLineBreakChars(aValue) then
    raise Exception.Create('Unexpected linebreak in Value - ' + aValue);

  delimPos := Pos(CH_NODE_DELIMITER, aName);
  if delimPos > 0 then
  begin
    nodeName := LeftStr(aName, delimPos - 1);
    remainder := RightStr(aName, Length(aName) - delimPos);

    nodeFound := False;
    for var I in fChildren do
    if I.Name = nodeName then
    begin
      nodeFound := True;
      I.AddNode(remainder, aValue);
    end;

    if not nodeFound then
    begin
      fChildren.Add(TKMNestedNode.Create(Self, ntBranch, nodeName));
      fChildren.Last.AddNode(remainder, aValue);
    end;
  end else
  begin
    if not ALLOW_DUPLICATES and (FindNode(aName) <> nil) then
      raise Exception.Create('Duplicate node for the Name - ' + aName);

    n := TKMNestedNode.Create(Self, ntValue, aName);
    n.Value := aValue;
    fChildren.Add(n);
  end;
end;


{ TKMNestedLibrary }
constructor TKMNestedLibrary.Create;
begin
  inherited;

  // Default options
  PadHierarchy := True;

  fRoot := TKMNestedNode.Create(nil, ntBranch, '');
end;


destructor TKMNestedLibrary.Destroy;
begin
  FreeAndNil(fRoot);

  inherited;
end;


procedure TKMNestedLibrary.Clear;
begin
  fRoot.Clear;
end;


procedure TKMNestedLibrary.LoadFromString(const aString: string);
begin
  fRoot.LoadFromString(aString);
end;


function TKMNestedLibrary.SaveToString: string;
begin
  Result := fRoot.SaveToString('', PadHierarchy);
end;


function TKMNestedLibrary.ToArray: TArray<TKMPair>;
begin
  Result := fRoot.ToArray;
end;


end.
