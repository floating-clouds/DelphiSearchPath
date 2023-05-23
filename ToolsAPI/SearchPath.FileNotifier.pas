unit SearchPath.FileNotifier;

interface

uses
  System.Classes, System.SysUtils,
  SearchPath.Common;

  procedure RegisterFileNotifier(ASearchPathManager: ISearchPathManager);

implementation

uses ToolsAPI, System.TypInfo, System.Generics.Collections;

type
  TModuleNotifier = class(TModuleNotifierObject, IOTAModuleNotifier, IOTAModuleNotifier80, IOTAModuleNotifier90)
    procedure BeforeRename(const OldFileName, NewFileName: string);
    procedure AfterRename(const OldFileName, NewFileName: string);
  private
    FSearchPathManager: ISearchPathManager;
    FMapOfModuleNotifierIndex: TDictionary<string, Integer>;
    function GetModuleNotifierIndex(FileName: string): Integer;
    procedure SetModuleNotifierIndex(FileName: string; const Value: Integer);
  public
    constructor Create(ASearchPathManager: ISearchPathManager);
    destructor Destroy; override;
    procedure RemoveModuleNotifier(const FileName: string);

    property ModuleNotifierIndex[FileName: string]: Integer read GetModuleNotifierIndex write SetModuleNotifierIndex;
  end;

  TFileNotifier = class(TNotifierObject{, IOTANotifier}, IOTAIDENotifier)
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  private
    FModuleNotifier: IOTAModuleNotifier;
    FSearchPathManager: ISearchPathManager;
  public
    constructor Create(ASearchPathManager: ISearchPathManager);
    destructor Destroy; override;
  end;

{ TFileNotifier }

procedure TFileNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TFileNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

constructor TFileNotifier.Create(ASearchPathManager: ISearchPathManager);
begin
  FSearchPathManager := ASearchPathManager;
  FModuleNotifier := TModuleNotifier.Create(ASearchPathManager);
end;

destructor TFileNotifier.Destroy;
begin
  FModuleNotifier := nil;
  inherited;
end;

procedure TFileNotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);

//  function CodeToString: string;
//  begin
//    Result := GetEnumName(TypeInfo(TOTAFileNotification), Ord(NotifyCode));
//  end;

var
  I: Integer;
begin
  if (NotifyCode = ofnFileOpened) and (not Cancel) then begin
    if (BorlandIDEServices as IOTAServices).IsProject(FileName) then begin
      I := (BorlandIDEServices as IOTAModuleServices).OpenModule(FileName).AddNotifier(FModuleNotifier);
      TModuleNotifier(FModuleNotifier).ModuleNotifierIndex[FileName] := I;
      //FSearchPathManager.OutputMessage(Format('Open %s-%d', [FileName, I]));
    end else
    if (BorlandIDEServices as IOTAServices).IsProjectGroup(FileName) then begin
      I := (BorlandIDEServices as IOTAModuleServices).OpenModule(FileName).AddNotifier(FModuleNotifier);
      TModuleNotifier(FModuleNotifier).ModuleNotifierIndex[FileName] := I;
      //FSearchPathManager.OutputMessage(Format('Open %s-%d', [FileName, I]));
    end;;
  end else
  if NotifyCode = ofnFileClosing then begin
    I := TModuleNotifier(FModuleNotifier).ModuleNotifierIndex[FileName];
    if I > 0 then begin
      TModuleNotifier(FModuleNotifier).RemoveModuleNotifier(FileName);
      //FSearchPathManager.OutputMessage(Format('Close %s-%d', [FileName, I]));
      (BorlandIDEServices as IOTAModuleServices).OpenModule(FileName).RemoveNotifier(I);
    end;
  end;
end;

{ TModuleNotifier }

procedure TModuleNotifier.AfterRename(const OldFileName, NewFileName: string);
begin
  FSearchPathManager.OnProjectRename(OldFileName, NewFileName);
end;

procedure TModuleNotifier.BeforeRename(const OldFileName, NewFileName: string);
begin
end;

constructor TModuleNotifier.Create(ASearchPathManager: ISearchPathManager);
begin
  FSearchPathManager := ASearchPathManager;
  FMapOfModuleNotifierIndex := TDictionary<string, Integer>.Create;
end;

destructor TModuleNotifier.Destroy;
begin
  FreeAndNil(FMapOfModuleNotifierIndex);
  inherited;
end;

function TModuleNotifier.GetModuleNotifierIndex(FileName: string): Integer;
begin
  if FMapOfModuleNotifierIndex.ContainsKey(FileName) then begin
    FMapOfModuleNotifierIndex.TryGetValue(FileName, Result);
  end else Result := 0;
end;

procedure TModuleNotifier.RemoveModuleNotifier(const FileName: string);
begin
  if FMapOfModuleNotifierIndex.ContainsKey(FileName) then begin
    FMapOfModuleNotifierIndex.Remove(FileName);
  end;
end;

var
  NotifierIndex: Integer = 0;

procedure RegisterFileNotifier(ASearchPathManager: ISearchPathManager);
begin
  var OTAServices:= BorlandIDEServices as IOTAServices;
  NotifierIndex := OTAServices.AddNotifier(TFileNotifier.Create(ASearchPathManager));
end;

procedure TModuleNotifier.SetModuleNotifierIndex(FileName: string; const Value: Integer);
begin
  FMapOfModuleNotifierIndex.Add(FileName, Value);
end;

initialization

finalization
  if NotifierIndex > 0 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(NotifierIndex);

end.
