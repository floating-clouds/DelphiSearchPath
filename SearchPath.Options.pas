unit SearchPath.Options;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.ExtCtrls, Vcl.ComCtrls,
  ToolsAPI,
  SVGIconImageCollection, Vcl.VirtualImageList, SearchPath.Common, Vcl.Buttons, System.ImageList, Vcl.ImgList,
  Vcl.ToolWin;

type
  TFormOptions = class(TForm)
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    Panel1: TPanel;
    ActionImages: TVirtualImageList;
    ActionList1: TActionList;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { Private declarations }
    FImageCollection: TSVGIconImageCollection;
    FActiveProject: IOTAProject;
    FPOMName: string;
    FSearchPath: ISearchPathManager;
    procedure InitToolBar;
    procedure OnToolButtonClick(Sender: TObject);
    procedure OnToolButtonUpdate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; SearchPath: ISearchPathManager); reintroduce;
    { Public declarations }
    class function Execute(SearchPath: ISearchPathManager): Boolean;
  end;

var
  FormOptions: TFormOptions;

implementation

uses System.IOUtils;

{$R *.dfm}

{ TFormOptions }

constructor TFormOptions.Create(AOwner: TComponent; SearchPath: ISearchPathManager);
begin
  inherited Create(AOwner);
  FSearchPath := SearchPath;
  InitToolBar;
end;

class function TFormOptions.Execute(SearchPath: ISearchPathManager): Boolean;
begin
  Result := False;
  with TFormOptions.Create(nil, SearchPath) do
    try
      if ShowModal = MrOK then begin
        Result := True;
      end;
    finally
      Free;
    end;
end;

procedure TFormOptions.FormCreate(Sender: TObject);
var
  ThemingServices : IOTAIDEThemingServices;
begin
  Application.Handle := (BorlandIDEServices As IOTAServices).GetParentHandle;
  if Supports(BorlandIDEServices, IOTAIDEThemingServices, ThemingServices) then begin
    // Doc/Using IDE Styles in Third-Party Plugins - RAD Studio
    ThemingServices.RegisterFormClass(TFormOptions);
    ThemingServices.ApplyTheme(Self);
  end;
  FActiveProject := GetActiveProject;
  FPOMName:= ExtractFilePath(FActiveProject.FileName) + 'pom.xml';
  var ModuleInfo := FActiveProject.FindModuleInfo(FPOMName);
  if ModuleInfo <> nil then
    ModuleInfo.OpenModule.Show;
end;

procedure TFormOptions.InitToolBar;
begin
  FImageCollection := TSVGIconImageCollection.Create(Self);
  TActionType.InitToolBarImage(FImageCollection, ActionImages);

  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atReArch, OnToolButtonClick, OnToolButtonUpdate);
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atEnvList, OnToolButtonClick, OnToolButtonUpdate);
  // 分隔条
  TActionType.AddSeparator(ToolBar1);
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atHardDelete, OnToolButtonClick, OnToolButtonUpdate);
  // 分隔条
  TActionType.AddSeparator(ToolBar1);
  // 添加删除
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atDelete, OnToolButtonClick, OnToolButtonUpdate);
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atAdd, OnToolButtonClick, OnToolButtonUpdate);
  // 分隔条
  TActionType.AddSeparator(ToolBar1);
  // 更新代码和设置SearchPath
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atUpdate, OnToolButtonClick, OnToolButtonUpdate);
  TActionType.InitToolButton(ToolBar1, ActionList1, ActionImages, atApply, OnToolButtonClick, OnToolButtonUpdate);
end;

procedure TFormOptions.OnToolButtonClick(Sender: TObject);
begin
  case TActionType(TAction(Sender).Tag) of
    atAdd: begin
      if not TFile.Exists(FPOMName) then begin
        var Stream := TResourceStream.Create(hinstance, 'pom_sample', RT_RCDATA);
        try
          Stream.SaveToFile(FPOMName);
        finally
          FreeAndNil(Stream);
        end;
      end;
      FActiveProject.AddFile(FPOMName, False);
      FActiveProject.FindModuleInfo(FPOMName).OpenModule.Show;
    end;
    atDelete: begin
      if MessageBox(Handle, 'Remove current config ?'^m^m'Config file will removed from the project.', 'Delete confirm', MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2) = ID_YES then begin
        var ModuleInfo := FActiveProject.FindModuleInfo(FPOMName);
        if ModuleInfo <> nil then begin
          ModuleInfo.OpenModule.Close;
          FActiveProject.RemoveFile(FPOMName);
        end;
      end;
    end;
    atHardDelete: begin
      if MessageBox(Handle, 'Remove current config ?'^m^m'Config file will removed from the file system.', 'Delete confirm', MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2) = ID_YES then begin
        var ModuleInfo := FActiveProject.FindModuleInfo(FPOMName);
        if ModuleInfo <> nil then begin
          ModuleInfo.OpenModule.Close;
          FActiveProject.RemoveFile(FPOMName);
          TFile.Delete(FPOMName);
        end;
      end;
    end;
    atOption: ; // 什么都不做
    else
      FSearchPath.OnButtonClick(Sender, TActionType(TAction(Sender).Tag));
  end;
end;

procedure TFormOptions.OnToolButtonUpdate(Sender: TObject);
begin
  case TActionType(TAction(Sender).Tag) of
    atAdd: TAction(Sender).Enabled := FActiveProject.FindModuleInfo(FPOMName) = nil;
    atDelete, atHardDelete: TAction(Sender).Enabled := FActiveProject.FindModuleInfo(FPOMName) <> nil;
    else TAction(Sender).Enabled := True;
  end;
end;

procedure TFormOptions.ToolButton3Click(Sender: TObject);
var
  Source: IOTASourceEditor;
begin
  var ModuleInfo := FActiveProject.FindModuleInfo(FPOMName);
  if ModuleInfo <> nil then begin
    var Editor := ModuleInfo.OpenModule.CurrentEditor;
    if Supports(Editor, IOTASourceEditor, Source) then begin
      if Source.EditViewCount > 1 then
        raise Exception.Create('');

      var View := Source.EditViews[0];

      var Writer := Source.CreateUndoableWriter;
      Writer.Insert('1234567890');

      Source.Show;
    end;
  end;
end;

end.
