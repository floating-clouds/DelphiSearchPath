unit SearchPath.Config;

interface

uses System.Classes, System.SysUtils;

type
  IConfig = interface(IInterface)
    ['{A29D8B30-1D60-4D9A-9116-F99E369DBD02}']
    function GetLibsRoot: string;
    procedure SetLibsRoot(AValue: string);

    property LibsRoot: string read GetLibsRoot write SetLibsRoot;
    //property DebugColor:
  end;

  TConfig = class(TInterfacedObject, IConfig)
  private
    const CONFIG_ROOT = '\SOFTWARE\Delphi Search Path';
  private
    function GetLibsRoot: string;
    procedure SetLibsRoot(AValue: string);
  private
    FEnvRoot: string;
    FLibsRoot: string;
    function GetLocalLibsRoot: string;
  public
    constructor Create;
  end;

  var Config: IConfig;

implementation

uses System.Win.Registry, Winapi.Windows, ToolsAPI, SearchPath.Main, SearchPath.Common;

{ TConfig }

function TConfig.GetLibsRoot: string;
begin
  Result := '';
  var Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(FEnvRoot, False) then
    try
      if Reg.ValueExists(LIBS_ROOT_KEY) then begin
        Result := Reg.ReadString(LIBS_ROOT_KEY).Trim;
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

function TConfig.GetLocalLibsRoot: string;
begin
  Result := '';
  var Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(CONFIG_ROOT, False) then
    try
      if Reg.ValueExists(LIBS_ROOT_KEY) then begin
        Result := Reg.ReadString(LIBS_ROOT_KEY);
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TConfig.SetLibsRoot(AValue: string);
var
  Value: string;
begin
  Value := ExcludeTrailingPathDelimiter(AValue);
  var Reg := TRegistry.Create(KEY_WRITE);
  try
    if Reg.OpenKey(FEnvRoot, True) then
    try
      Reg.WriteString(LIBS_ROOT_KEY, Value);
    finally
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

constructor TConfig.Create;
begin
  FEnvRoot := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Environment Variables';
  FLibsRoot := GetLocalLibsRoot;
end;

initialization

finalization

end.
