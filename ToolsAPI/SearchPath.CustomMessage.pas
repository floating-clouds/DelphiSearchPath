unit SearchPath.CustomMessage;

interface

{$INCLUDE 'CompilerDefinitions.inc'}

uses
  System.Classes, System.SysUtils, System.Types,
  Vcl.Themes, Vcl.Graphics, SVGIconImage,
  ToolsAPI,
  SearchPath.Common;

type
  TCustomMessage = class(TNotifierObject, IOTACustomMessage, INTACustomDrawMessage, INTAIDEThemingServicesNotifier)
    // IOTACustomMessage
    function GetColumnNumber: Integer;
    function GetFileName: string;
    function GetLineNumber: Integer;
    function GetLineText: string;
    procedure ShowHelp;
    // INTACustomDrawMessage
    procedure Draw(Canvas: TCanvas; const Rect: TRect; Wrap: Boolean);
    function CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect;
    // INTAIDEThemingServicesNotifier
    procedure ChangingTheme;
    procedure ChangedTheme;
  private
    FText: string;
    FFontName: string;
    FStyle: TFontStyles;
    FForeColour: TColor;
    FMessageContext: TMessageContext;
    FStyleServices : TCustomStyleServices;
    FSVGGraphic: TSVGIconImage;
    function GetDebugColor: TColor;
  public
    constructor Create(AText: string; ARootMessage: Boolean; AMessageContext: TMessageContext);
    destructor Destroy; override;
  end;

implementation

{ TCustomMessage }

function TCustomMessage.CalcRect(Canvas: TCanvas; MaxWidth: Integer; Wrap: Boolean): TRect;
const
  StrTextHeightTest = 'Wp';
begin
  if Wrap then begin
    Result := Trect.Create(0, 0, -100, -100);
  end else begin
    Canvas.Font.Name := FFontName;
    Canvas.Font.Style := FStyle;
    Result := Canvas.ClipRect;
    Result.Top := 0;
    Result.Left := 0;
    Result.Bottom := Result.Top + Canvas.TextHeight(StrTextHeightTest);
    Result.Right := Result.Left + MaxWidth;
  end;
end;

procedure TCustomMessage.ChangedTheme;
begin
  // 调整颜色
  FForeColour := GetDebugColor;
end;

procedure TCustomMessage.ChangingTheme;
begin
end;

constructor TCustomMessage.Create(AText: string; ARootMessage: Boolean; AMessageContext: TMessageContext);
var
  Res: string;
begin
  FStyleServices := nil;
{$IFDEF RS102}
  var ITS: IOTAIDEThemingServices;
  if Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) then
    if ITS.IDEThemingEnabled then begin
      FStyleServices := ITS.StyleServices;
      ITS.AddNotifier(Self);
    end;
{$ENDIF RS102}

  FMessageContext := AMessageContext;

  FFontName := AMessageContext.FontName;
  FForeColour := AMessageContext.ForeColour;// or $777777;
  FStyle := AMessageContext.Style;
  //FBackColour := AMessageContext.BackColour;

  case AMessageContext.LogLevel of
    llDebug: begin
      Res := 'debug';
      FForeColour := GetDebugColor;
    end;
    llInfo: begin
      Res := 'info';
      FForeColour := $E88C44;
    end;
    llWarn: begin
      Res := 'warn';
      FForeColour := $5492DF;
    end;
    else begin
      Res := 'error';
      FForeColour := $E35C3F;
    end;
  end;

  FSVGGraphic := TSVGIconImage.Create(nil);
  var R := TResourceStream.Create(hInstance, Res, RT_RCDATA);
  try
    FSVGGraphic.LoadFromStream(R);
    FSVGGraphic.SVG.ApplyFixedColorToRootOnly := False;
    FSVGGraphic.SVG.Opacity := 1;
  finally
    FreeAndNil(R);
  end;

  if ARootMessage then begin
    FText := Format('[%5-s] %s', [AMessageContext.LogLevel.AsString, AText]);
  end else begin
    FText := AText;
  end;
end;

destructor TCustomMessage.Destroy;
begin
  FreeAndNil(FSVGGraphic);

  inherited;
end;

procedure TCustomMessage.Draw(Canvas: TCanvas; const Rect: TRect; Wrap: Boolean);
var
  R: TRect;
  StrMsg: string;
  HighlightColour: TColor;
  BoolIsSelected: Boolean;
begin
  if Wrap then Exit;

  // Determine if the message is selected
  HighlightColour := clHighlight;
{$IFDEF RS102}
  if Assigned(FStyleServices) then
    HighlightColour := FStyleServices.GetSystemColor(clHighlight);
{$ENDIF RS102}
  BoolIsSelected := Canvas.Brush.Color = HighlightColour;
(*  // Draw background
  if not BoolIsSelected then begin
    Canvas.Brush.Color := FBackColour;
    if Canvas.Brush.Color = ClNone then
      Canvas.Brush.Color := ClWindow;
{$IFDEF RS102}
    if Assigned(FStyleServices) then
      Canvas.Brush.Color := FStyleServices.GetSystemColor(Canvas.Brush.Color);
{$ENDIF RS102}
  end;
  Canvas.FillRect(Rect);
  // Draw text
  if not BoolIsSelected then begin
    Canvas.Font.Color := FForeColour;
    if Canvas.Font.Color = ClNone then
      Canvas.Font.Color := ClWindowText;
{$IFDEF RS102}
    if Assigned(FStyleServices) then
      Canvas.Font.Color := FStyleServices.GetSystemColor(Canvas.Font.Color);
{$ENDIF RS102}
  end;
*)

  if BoolIsSelected then begin
    Canvas.Font.Color := clHighlightText;
    {$IFDEF RS102}
    if Assigned(FStyleServices) then begin
      Canvas.Font.Color := FStyleServices.GetSystemColor(clHighlightText);
      Canvas.Brush.Color := FStyleServices.GetSystemColor(Canvas.Brush.Color);
    end;
    {$ENDIF RS102}
  end else begin
    Canvas.Font.Color := clWindowText;
    {$IFDEF RS102}
    if Assigned(FStyleServices) then begin
      Canvas.Font.Color := FStyleServices.GetSystemColor(clWindowText);
      Canvas.Brush.Color := FStyleServices.GetSystemColor(Canvas.Brush.Color);
    end;
    {$ENDIF RS102}
    //Canvas.Font.Color := Canvas.Font.Color and FForeColour;
    Canvas.Font.Color := FForeColour;
    //Canvas.Brush.Color := FForeColour;
  end;

  R.Left := Rect.Left + Rect.Height;   // 向右移动一个图标的高度
  R.Top  := Rect.Top;
  R.Width := Rect.Width - Rect.Height; // 最右侧向左缩小一个图标的高度
  R.Height := Rect.Height;

  StrMsg := FText;
  Canvas.Font.Name := FFontName;
  Canvas.Font.Style := FStyle;

  var IconRect := TRect.Create(Rect.Left, Rect.Top, Rect.Left + Rect.Height, Rect.Top + Rect.Height);
  InflateRect(IconRect, -2, -2);
  FSVGGraphic.SVG.FixedColor := Canvas.Font.Color;
  FSVGGraphic.SVG.PaintTo(Canvas.Handle, IconRect);

  Canvas.TextRect(R, StrMsg, [tfLeft, tfSingleLine, tfVerticalCenter, tfPathEllipsis]);
end;

function TCustomMessage.GetColumnNumber: Integer;
begin
  Result := FMessageContext.Col;
end;

function TCustomMessage.GetDebugColor: TColor;
begin
  Result := clWindowText;
  {$IFDEF RS102}
  if Assigned(FStyleServices) then begin
    Result := FStyleServices.GetSystemColor(clWindowText);
    //Canvas.Brush.Color := FStyleServices.GetSystemColor(Canvas.Brush.Color);
  end;
  {$ENDIF RS102}
end;

function TCustomMessage.GetFileName: string;
begin
  Result := FMessageContext.FileName;
end;

function TCustomMessage.GetLineNumber: Integer;
begin
  Result := FMessageContext.Row;
end;

function TCustomMessage.GetLineText: string;
begin
  Result := FText;
end;

procedure TCustomMessage.ShowHelp;
begin
  //
end;

initialization

finalization

end.
