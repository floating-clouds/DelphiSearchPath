unit SearchPath.Common;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  WinApi.Windows,
  Vcl.Graphics, Vcl.ComCtrls, Vcl.Controls, Vcl.ActnList, Vcl.Buttons,
  Vcl.VirtualImageList, SVGIconImageCollection;

const
  LIBS_ROOT_KEY = 'delphi-libs';
  LIBS_ROOT_DEFAULT = '.\libs\';
  POM_FILE_EXT = '.pom.xml';

type
  TActionType = (atUpdate = 0, atApply = 1, atOption = 2, atDelete = 3, atAdd = 4, atHardDelete = 5, atEnvList = 6, atReArch);

  TActionTypeHelper = record helper for TActionType
  strict private
    // 这里的字符串与资源文件的名称有关系
    const ActionTypeStr: array [TActionType] of string = ('Update', 'Apply', 'Option', 'Delete', 'Add', 'Hard_Delete', 'EnvList', 'rearch');
    const HintStr: array [TActionType] of string = ('Download libs code', 'Setup project search path', 'Open options form', 'Remove config from project', 'Add config to active project', 'Delete config from file system', 'View the build system options', 'Re-arch projects folder');
  public
    function AsNativeInt: NativeInt;
    function AsString: string;
    function ToString: string;
    function ToString0: string;

    function ImageName: string;
    function DisabledImageName: string;
    function HotImageName: string;
    function Hint: string;

    class procedure InitToolBarImage(ImageCollection: TSVGIconImageCollection; ImageList: TVirtualImageList); static;
    class procedure AddSeparator(AOwner: TToolBar); static;
    class procedure InitToolButton(AOwner: TToolBar; Actions: TActionList; AImages: TVirtualImageList;
      ActionType: TActionType; AOnExecute, AOnUpdate: TNotifyEvent); static;
  end;

  TLogLevel = (llDebug = 0, llInfo = 1, llWarn = 2, llError = 3);

  TLogLevelHelper = record helper for TLogLevel
  public
    function AsString: string;
    class function From(const S: string): TLogLevel; static;
  end;

  TMessageContext = record
    FileName: string;
    BoolGroup, BoolAutoScroll: Boolean;
    FontName: string;
    Row: Integer;
    Col: Integer;
    ForeColour: TColor;
    Style: TFontStyles;
    BackColour: TColor;
    Parent: Pointer;
    LogLevel: TLogLevel;
    class operator Initialize(out Dest: TMessageContext);
    class operator Finalize(var Dest: TMessageContext);
  end;

  TEnv = reference to function(const AName, AValue: string): Boolean;
  procedure GetEnvList(AEnv: TEnv);

type
  ISearchPathManager = interface(IInterface)
    ['{D54079EB-53E4-4389-81D8-6F6EE84C9850}']
    procedure OnButtonClick(Sender: TObject; ActionType: TActionType);
    procedure OnProjectRename(const OldFileName, NewFileName: string);
    procedure ShowMessageView(const ClearView: Boolean);
    Procedure OutputMessage(strText : string); overload;
    Procedure OutputMessage(strText, strFileName, strPrefix : string; iLine, iCol : Integer); overload;
    procedure OutputMessage(AText: string; MessageContext: TMessageContext); overload;
  end;

  function RectToString(R: TRect): string;

implementation

function RectToString(R: TRect): string;
begin
  Result := Format('Left=%d, Top=%d, Width=%d, Height=%d ', [R.Left, R.Top, R.Width, R.Height]);
end;

{$WARN USE_BEFORE_DEF OFF}
procedure GetEnvList(AEnv: TEnv);
var
  envbuf, p: PWideChar;
begin
  envbuf := GetEnvironmentStrings;
  try
    if envbuf <> nil then
      p := envbuf;
      repeat
        var env := StrPas(p);
        var EqIndex := env.IndexOf('=');
        var Name := env.Substring(0, EqIndex);
        var Value := env.Substring(EqIndex + 1);
        if not AEnv(Name, Value) then
          Break;
        Inc(p, StrLen(p) + 1);
      until p^ = #0;
  finally
    FreeEnvironmentStrings(envbuf);
  end;
end;
{$WARN USE_BEFORE_DEF ON}

{ TActionTypeHelper }

class procedure TActionTypeHelper.AddSeparator(AOwner: TToolBar);
begin
  var _ToolButton := TToolButton.Create(AOwner);
  with _ToolButton do begin
    Parent := TWinControl(AOwner);
    Style := tbsSeparator;
    Width := 8;
    Align := alLeft;
  end;
end;

function TActionTypeHelper.AsNativeInt: NativeInt;
begin
  Result := Ord(Self);
end;

function TActionTypeHelper.AsString: string;
begin
  case Self of
    atUpdate: Result := 'Update';
    atApply: Result := 'Apply';
    else Result := 'Setup';
  end;
end;

function TActionTypeHelper.DisabledImageName: string;
begin
  Result := ActionTypeStr[Self] + '_g';
end;

function TActionTypeHelper.Hint: string;
begin
  Result := HintStr[Self];
end;

function TActionTypeHelper.HotImageName: string;
begin
  Result := ActionTypeStr[Self] + '_h';
end;

function TActionTypeHelper.ImageName: string;
begin
  Result := ActionTypeStr[Self] + '_n';
end;

class procedure TActionTypeHelper.InitToolBarImage(ImageCollection: TSVGIconImageCollection; ImageList: TVirtualImageList);

  procedure AddImage(ActionType: TActionType);
  var
    Image: TVirtualImageListItem;
  begin
    Image := ImageList.Images.Add;
    ImageCollection.LoadFromResource(hInstance, ActionType.ImageName, ActionType.ImageName);
    Image.CollectionName := ActionType.ImageName;

    Image := ImageList.Images.Add;
    ImageCollection.LoadFromResource(hInstance, ActionType.DisabledImageName, ActionType.DisabledImageName);
    Image.CollectionName := ActionType.ImageName;

    Image := ImageList.Images.Add;
    ImageCollection.LoadFromResource(hInstance, ActionType.HotImageName, ActionType.HotImageName);
    Image.CollectionName := ActionType.ImageName;
  end;

begin
  ImageList.ImageCollection := ImageCollection;
  ImageList.DisabledGrayscale := True;
  for var I := Ord(Low(TActionType)) to Ord(High(TActionType)) do begin
    AddImage(TActionType(I));
  end;
end;

class procedure TActionTypeHelper.InitToolButton(AOwner: TToolBar; Actions: TActionList; AImages: TVirtualImageList;
  ActionType: TActionType; AOnExecute, AOnUpdate: TNotifyEvent);
begin
    var _Action := TAction.Create(Actions);
    with _Action do begin
      OnExecute := AOnExecute;
      OnUpdate := AOnUpdate;
      Caption := '';
      Hint := ActionType.Hint;
      Tag := ActionType.AsNativeInt;
    end;
    _Action.SetParentComponent(Actions);

    var _ToolButton := TSpeedButton.Create(AOwner);
    with _ToolButton do begin
      Parent := TWinControl(AOwner);
      Action := _Action;
      Images := AImages;
      Align := alLeft;
      Flat := True;
      Height := 28;
      Width := 28;
      Top := 0;
      ImageIndex := AImages.GetIndexByName(ActionType.ImageName);
      DisabledImageIndex := AImages.GetIndexByName(ActionType.DisabledImageName);
      HotImageIndex := AImages.GetIndexByName(ActionType.HotImageName);
      Name := Format('_pom_%s', [ActionType.ToString]);
    end;
end;

function TActionTypeHelper.ToString: string;
begin
  Result := ActionTypeStr[Self];
end;

function TActionTypeHelper.ToString0: string;
begin
  Result := GetEnumName(TypeInfo(TActionType), Ord(Self));
end;

{ TMessageContext }

class operator TMessageContext.Finalize(var Dest: TMessageContext);
begin

end;

class operator TMessageContext.Initialize(out Dest: TMessageContext);
begin
  Dest.FileName := '';
  Dest.BackColour := clWindow;
  Dest.BoolAutoScroll := True;
  Dest.BoolGroup := True;
  Dest.FontName := 'Courier New'; // '微软雅黑';
  Dest.ForeColour := clRed;
  Dest.Parent := nil;
  Dest.Style := [fsItalic]; //fsBold, fsItalic, fsUnderline, fsStrikeOut
end;

{ TMessageTypeHelper }

class function TLogLevelHelper.From(const S: string): TLogLevel;
begin
  if CompareText(S, 'debug') = 0 then
    Result := llDebug
  else
  if CompareText(S, 'info') = 0 then
    Result := llInfo
  else
  if CompareText(S, 'warn') = 0 then
    Result := llWarn
  else
  if CompareText(S, 'error') = 0 then
    Result := llError
  else Result := llInfo;
end;

function TLogLevelHelper.ASString: string;
begin
  Result := GetEnumName(TypeInfo(TLogLevel), Ord(Self)).Substring(2);
end;

end.
