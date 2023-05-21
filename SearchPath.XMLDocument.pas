unit SearchPath.XMLDocument;

interface

uses
  System.Classes, System.SysUtils,
  Semin64.Xml, Semin64.Memory;

type
  PsanXMLObjElementEx2 = ^TsanXMLObjElementEx2;

  TsanXMLObjElementEx2 = record
    PName: PChar;  // TsanXMLObjElement.pName
    PValue: PChar; // TsanXMLObjElement.pValue
    PNext: PsanXMLObjElementEx2;
    PFirstChild: PsanXMLObjElementEx2;
    PArrayChilds: ^PsanXMLObjElementEx2;
    ChildCount: Integer;
    PArrayAttributes: ^PsanXMLObjAttribute;
    AttrCount: Integer;
    // New attr
    LineNo: Integer;
    CharNoInLine: Integer;
  end;

  TXMLDocumentObject = class(TObject)
  private
    FParser: TsanXMLParser;
    FRootElement: PsanXMLObjElementEx2;
    FPrologue: PsanXMLObjElementEx2;
    FMemoryManager: TsanStackMemoryManager;
    procedure ErrorIndexOutOfBound;
    procedure DoParserEvent(Sender: TObject; EventType: TsanXMLEventType; XMLStack: TsanXMLStack);
    procedure DoInit;
    procedure DoPrologue(XMLStack: TsanXMLStack);
    procedure DoOpenElement(XMLStack: TsanXMLStack; Parser: TsanXMLParser);
    procedure DoCloseElement(XMLStack: TsanXMLStack);
    procedure LoadAttributes(PElement: PsanXMLObjElementEx2; XMLStack: TsanXMLStack);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    function RootElement: PsanXMLObjElement;
    function PrologueElement: PsanXMLObjElement;
    function ChildCount(PElement: PsanXMLObjElement): Integer;
    function GetElement(PParent: PsanXMLObjElement; Index: Integer): PsanXMLObjElement; overload;
    function GetElement(PParent: PsanXMLObjElement; ElementName: string): PsanXMLObjElement; overload;
    function AttributeCount(PElement: PsanXMLObjElement): Integer;
    function GetAttribute(PElement: PsanXMLObjElement; AttrIndex: Integer): PsanXMLObjAttribute; overload;
    function GetAttribute(PElement: PsanXMLObjElement; AttrName: string): PsanXMLObjAttribute; overload;
    function GetAttributeValue(PElement: PsanXMLObjElement; AttrName: string): string;
    function MemoryUsed: Cardinal;
  end;

implementation

resourcestring
  RSXmlIndexOutOfBound = 'Index out of bound';

  { TsanXMLObject }

function TXMLDocumentObject.AttributeCount(PElement: PsanXMLObjElement): Integer;
begin
  Result := PsanXMLObjElementEx(PElement)^.AttrCount;
end;

function TXMLDocumentObject.ChildCount(PElement: PsanXMLObjElement): Integer;
begin
  Result := PsanXMLObjElementEx(PElement)^.ChildCount;
end;

constructor TXMLDocumentObject.Create;
begin
  FRootElement := nil;
  FPrologue := nil;
  FMemoryManager := TsanStackMemoryManager.Create;
  FParser := TsanXMLParser.Create;
  FParser.OnParse := DoParserEvent;
end;

destructor TXMLDocumentObject.Destroy;
begin
  FMemoryManager.Clear;
  FParser.Free;
  FMemoryManager.Free;
  inherited;
end;

procedure TXMLDocumentObject.DoCloseElement(XMLStack: TsanXMLStack);
var
  PElement: PsanXMLObjElementEx;
  PChildElement: PsanXMLObjElementEx;
  PArrayCell: ^PsanXMLObjElementEx;
  N: Integer;
begin
  PElement := PsanXMLObjElementEx(XMLStack.GetElementUserData);
  PElement^.PValue := FMemoryManager.GetMem(XMLStack.GetElementValue);
  if PElement^.ChildCount > 0 then begin
    PElement^.PArrayChilds := FMemoryManager.GetMem(PElement^.ChildCount * SizeOf(Pointer));
    PChildElement := PElement^.PFirstChild;
    N := PElement^.ChildCount;
    while Assigned(PChildElement) do begin
      PArrayCell := Pointer(Int64(PElement^.PArrayChilds) + (N - 1) * SizeOf(Pointer));
      PArrayCell^ := PChildElement;
      PChildElement := PChildElement^.PNext;
      Dec(N);
    end;
  end;
end;

procedure TXMLDocumentObject.DoInit;
begin
  FRootElement := nil;
  FMemoryManager.Clear;
end;

procedure TXMLDocumentObject.DoOpenElement(XMLStack: TsanXMLStack; Parser: TsanXMLParser);
var
  PElement: PsanXMLObjElementEx2;
  PParentElement: PsanXMLObjElementEx2;
begin
  PElement := FMemoryManager.GetMem(SizeOf(TsanXMLObjElementEx2));

  XMLStack.SetElementUserData(PElement);

  PElement^.PName := FMemoryManager.GetMem(XMLStack.GetElementName);
  PElement^.PValue := nil;

  PElement^.PNext := nil;
  PElement^.PFirstChild := nil;
  PElement^.PArrayChilds := nil;
  PElement^.ChildCount := 0;
  PElement^.LineNo := Parser.ParsePositionInfo.LineNo;
  PElement^.CharNoInLine := Parser.ParsePositionInfo.CharNoInLine;

  LoadAttributes(PElement, XMLStack);

  if not Assigned(FRootElement) then
    FRootElement := PElement;

  if XMLStack.StackSize > 1 then begin
    PParentElement := PsanXMLObjElementEx2(XMLStack.GetElementUserData(1));
    PElement^.PNext := PParentElement^.PFirstChild;
    PParentElement^.PFirstChild := PElement;
    Inc(PParentElement^.ChildCount);
  end;
end;

procedure TXMLDocumentObject.DoParserEvent(Sender: TObject; EventType: TsanXMLEventType; XMLStack: TsanXMLStack);
begin
  case EventType of
    EtStart:
      DoInit;
    EtHeaderOpen:
      DoPrologue(XMLStack);
    EtElementOpen:
      DoOpenElement(XMLStack, TsanXMLParser(Sender));
    EtElementClose:
      DoCloseElement(XMLStack);
  end;
end;

procedure TXMLDocumentObject.DoPrologue(XMLStack: TsanXMLStack);
begin

  FPrologue := FMemoryManager.GetMem(SizeOf(TsanXMLObjElementEx2));

  FPrologue^.PName := nil;
  FPrologue^.PValue := nil;
  FPrologue^.PNext := nil;
  FPrologue^.PFirstChild := nil;
  FPrologue^.PArrayChilds := nil;
  FPrologue^.ChildCount := 0;

  LoadAttributes(FPrologue, XMLStack);

end;

procedure TXMLDocumentObject.ErrorIndexOutOfBound;
begin
  raise Exception.Create(RSXmlIndexOutOfBound);
end;

function TXMLDocumentObject.GetElement(PParent: PsanXMLObjElement; ElementName: string): PsanXMLObjElement;
var
  I: Integer;
  P: PsanXMLObjElement;
begin

  Result := nil;

  for I := 1 to PsanXMLObjElementEx(PParent)^.ChildCount do begin
    P := GetElement(PParent, I - 1);
    if CompareStr(P^.PName, ElementName) = 0 then begin
      Result := P;
      Break;
    end;
  end;

end;

function TXMLDocumentObject.GetAttribute(PElement: PsanXMLObjElement; AttrIndex: Integer): PsanXMLObjAttribute;
var
  NAdr: Int64;
  PAdr: ^PsanXMLObjAttribute;
begin

  if AttrIndex >= PsanXMLObjElementEx(PElement)^.AttrCount then
    ErrorIndexOutOfBound;

  NAdr := Int64(PsanXMLObjElementEx(PElement)^.PArrayAttributes) + AttrIndex * SizeOf(Pointer);
  PAdr := Pointer(NAdr);
  Result := PAdr^;

end;

function TXMLDocumentObject.GetAttribute(PElement: PsanXMLObjElement; AttrName: string): PsanXMLObjAttribute;
var
  I: Integer;
  P: PsanXMLObjAttribute;
begin

  Result := nil;

  for I := 1 to PsanXMLObjElementEx(PElement)^.AttrCount do begin
    P := GetAttribute(PElement, I - 1);
    if CompareStr(P^.PName, AttrName) = 0 then begin
      Result := P;
      Break;
    end;
  end;

end;

function TXMLDocumentObject.GetAttributeValue(PElement: PsanXMLObjElement; AttrName: string): string;
var
  P: PsanXMLObjAttribute;
begin
  P := GetAttribute(PElement, AttrName);

  if Assigned(P) then begin
    Result := P^.PValue;
  end else begin
    Result := '';
  end;
end;

function TXMLDocumentObject.GetElement(PParent: PsanXMLObjElement; Index: Integer): PsanXMLObjElement;
var
  NAdr: Int64;
  PAdr: ^PsanXMLObjElement;
begin
  if index >= PsanXMLObjElementEx(PParent)^.ChildCount then
    ErrorIndexOutOfBound;
  NAdr := Int64(PsanXMLObjElementEx(PParent)^.PArrayChilds) + index * SizeOf(Pointer);
  PAdr := Pointer(NAdr);
  Result := PAdr^;
end;

procedure TXMLDocumentObject.LoadAttributes(PElement: PsanXMLObjElementEx2; XMLStack: TsanXMLStack);
var
  I: Integer;
  PAttr: PsanXMLObjAttribute;
  PArrayCell: ^PsanXMLObjAttribute;
begin

  PElement^.AttrCount := XMLStack.GetAttributeCount;
  PElement^.PArrayAttributes := nil;

  if PElement^.AttrCount > 0 then begin
    PElement^.PArrayAttributes := FMemoryManager.GetMem(SizeOf(Pointer) * PElement^.AttrCount);
  end;

  for I := 1 to PElement^.AttrCount do begin
    PAttr := FMemoryManager.GetMem(SizeOf(TsanXMLObjAttribute));
    PAttr^.PName := FMemoryManager.GetMem(XMLStack.GetAttributeName(I - 1));
    PAttr^.PValue := FMemoryManager.GetMem(XMLStack.GetAttributeValue(I - 1));
    PArrayCell := Pointer(Uint64(PElement^.PArrayAttributes) + (I - 1) * SizeOf(Pointer));
    PArrayCell^ := PAttr;
  end;

end;

procedure TXMLDocumentObject.LoadFromFile(FileName: string);
begin
  FParser.ParseFromFile(FileName);
end;

procedure TXMLDocumentObject.LoadFromStream(Stream: TStream);
begin
  FParser.ParseFromStream(Stream);
end;

function TXMLDocumentObject.MemoryUsed: Cardinal;
begin
  Result := FMemoryManager.TotalMemory;
end;

function TXMLDocumentObject.PrologueElement: PsanXMLObjElement;
begin
  Result := PsanXMLObjElement(FPrologue);
end;

function TXMLDocumentObject.RootElement: PsanXMLObjElement;
begin
  Result := PsanXMLObjElement(FRootElement);
end;

end.
