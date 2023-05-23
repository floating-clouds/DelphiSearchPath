unit SearchPath.Main;

interface

uses
  System.Classes, System.Types, System.SysUtils, System.Variants, System.TypInfo, System.Generics.Collections,
  Vcl.Graphics, Vcl.Forms,
  ToolsAPI,
  SearchPath.Common;

type
  TSearchPathManager = class(TInterfacedObject, ISearchPathManager)
    // ISearchPathManager
    procedure OnButtonClick(Sender: TObject; ActionType: TActionType);

    procedure ShowMessageView(const ClearView: Boolean);
    procedure OutputMessage(strText: string); overload;
    procedure OutputMessage(strText, StrFileName, StrPrefix: string; ILine, ICol: Integer); overload;
    procedure OutputMessage(AText: string; MessageContext: TMessageContext); overload;

    procedure OnProjectRename(const OldFileName, NewFileName: string);
  private
    const MESSAGE_GROUP_NAME = 'Search Path';
  private
    function GetEditorText(Stream: TStream): string;
    function GetCurrentProject: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  var SearchPathManager: ISearchPathManager;

procedure Register;

implementation

uses
  System.IOUtils,
  System.UITypes,
  SearchPath.CustomMessage,
  SearchPath.EditorWindowNotifier,
  SearchPath.FileNotifier,
  SearchPath.Wizard,
  SearchPath.Options,
  SearchPath.Project,
  SearchPath.Config;

procedure Register;
begin
  Config := TConfig.Create;
  SearchPathManager := TSearchPathManager.Create;
end;

{ TSearchPathManager }

procedure TSearchPathManager.OutputMessage(AText: string; MessageContext: TMessageContext);
var
  M: TCustomMessage;
  G: IOTAMessageGroup;
  Messages: TStrings;
  MessageParent: Pointer;
begin
  MessageParent := nil;
  with (BorlandIDEServices as IOTAMessageServices) do begin
    Messages := TStringList.Create;
    try
      Messages.Text := AText;
      for var S in Messages do begin
        M := TCustomMessage.Create(S, MessageParent = nil, MessageContext);
        if MessageParent = nil then begin
          G := nil;
          if MessageContext.BoolGroup then
            G := AddMessageGroup(MESSAGE_GROUP_NAME)
          else
            G := GetMessageGroup(0);
          G.AutoScroll := True;
          MessageParent := AddCustomMessagePtr(M as IOTACustomMessage, G);
        end else AddCustomMessage(M as IOTACustomMessage, MessageParent);
      end;
    finally
      FreeAndNil(Messages);
    end;
  end;
end;

function TSearchPathManager.GetEditorText(Stream: TStream): string;
const
  BufferSize = 1024;
var
  SourceEditor: IOTASourceEditor;
  Buffer: array[0..BufferSize-1] of AnsiChar;
  CharsRead: Integer;
  MessageStyle: TMessageContext;
begin
  var ModuleServices := BorlandIDEServices as IOTAModuleServices;
  // Get the module interface for the current file.
  var Module := ModuleServices.CurrentModule;
  // If no file is open, Module is nil.
  if Module = nil then
    Exit;

  // Get the interface to the source editor.
  for var I := 0 to Module.GetModuleFileCount-1 do
  begin
    var Intf := Module.GetModuleFileEditor(I);
    if Intf.QueryInterface(IOTASourceEditor, SourceEditor) = S_OK then
      Break;
  end;
  // If the file is not a source file, Editor is nil.
  if SourceEditor = nil then
    Exit;

  var EditReader  := SourceEditor.CreateReader;
  var ReaderPos := 0;
  repeat
    CharsRead := EditReader.GetText(ReaderPos, Buffer, BufferSize-1);
    Inc(ReaderPos, CharsRead);
    Stream.Write(Buffer, CharsRead);
  until CharsRead < BufferSize - 1;
  Result := Module.FileName;
end;

constructor TSearchPathManager.Create;
begin
  RegisterFileNotifier(Self);
  RegisterWizard(Self);
  RegisterEditorWindowNotifier(Self);
end;

destructor TSearchPathManager.Destroy;
begin
//  var Svc := BorlandIDEServices As IOTAMessageServices;
//  var G := Svc.GetGroup(MESSAGE_GROUP_NAME);
//  if G <> nil then
//    Svc.ClearMessageGroup(G);

  inherited;
end;

function TSearchPathManager.GetCurrentProject: string;
begin
  var ModuleServices := BorlandIDEServices as IOTAModuleServices;
  // Get the module interface for the current file.
  var Module := ModuleServices.CurrentModule;
  // If no file is open, Module is nil.
  if Module = nil then begin
    Result := '';
  end else begin
    Result := Module.FileName;
  end;
end;

procedure TSearchPathManager.OnButtonClick(Sender: TObject; ActionType: TActionType);
begin
  case ActionType of
    atUpdate: begin
      ShowMessageView(True);
      var Projects := TProjects.Create(Self);
      try
        var ProjectName := GetCurrentProject;
        Projects.Parse(ProjectName);
        Projects.UpdateProjectLibs(ProjectName);
      finally
        FreeAndNil(Projects);
      end;
    end;
    atApply: begin
      ShowMessageView(True);
      var Projects := TProjects.Create(Self);
      try
        var ProjectName := GetCurrentProject;
        Projects.Parse(ProjectName);
        Projects.ApplyProjectSearchPath(ProjectName);
      finally
        FreeAndNil(Projects);
      end;
    end;
    atEnvList: begin
      GetEnvList(function(const AName, AValue: string): Boolean begin
        Self.OutputMessage(Format('%s=%s', [AName, AValue]));
        Result := True;
      end);
    end;
    atReArch: begin
      ShowMessageView(True);
      var Projects := TProjects.Create(Self);
      try
        //Self.OutputMessage(Projects.ProjectName);
      finally
        FreeAndNil(Projects);
      end;
    end;
    else begin
      TFormOptions.Execute(Self);
    end;
  end;
end;

procedure TSearchPathManager.OnProjectRename(const OldFileName, NewFileName: string);
begin
  Self.OutputMessage(Format('%s - %s', [OldFileName, NewFileName]));
  var OldName := ChangeFileExt(OldFileName, '.pom.xml');
  if not TFile.Exists(OldName) then Exit;
  var NewName:= ChangeFileExt(NewFileName, '.pom.xml');

  var Module := (BorlandIDEServices as IOTAModuleServices).FindModule(OldName);
  if Module <> nil then begin
    Module.FileName := NewName;
    Module.Save(False, True);
  end else begin
    TFile.Move(OldName, NewName);
  end;
end;

procedure TSearchPathManager.OutputMessage(strText, strFileName, strPrefix: String; iLine, iCol: Integer);
begin
  (BorlandIDEServices As IOTAMessageServices).AddToolMessage(strFileName, strText, strPrefix, iLine, iCol);
end;

procedure TSearchPathManager.OutputMessage(strText: String);
begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(strText);
end;

procedure TSearchPathManager.ShowMessageView(const ClearView: Boolean);
var
  Svc: IOTAMessageServices;
begin
  Svc := BorlandIDEServices As IOTAMessageServices;
  var G := Svc.GetGroup(MESSAGE_GROUP_NAME);
  Svc.ClearMessageGroup(G);
  Svc.ShowMessageView(nil);
end;

end.
