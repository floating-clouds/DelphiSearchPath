unit SearchPath.EditorWindowNotifier;

interface

uses
  SearchPath.Common;

  procedure RegisterEditorWindowNotifier(ASearchPathManager: ISearchPathManager);

  var LastActiveWindow: string = '';
  var IsPomView: Boolean = False;

implementation

uses
  System.Classes, System.SysUtils, System.UITypes,
  Vcl.Controls, Vcl.ComCtrls, Vcl.Buttons, Vcl.Graphics, Vcl.ActnList,
  ToolsAPI, DockForm, Vcl.VirtualImageList, SVGIconImageCollection;

type
  TEditServiceNotifier = class(TNotifierObject, INTAEditServicesNotifier)
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  private
    FSearchPathManager: ISearchPathManager;
    FActions: TActionList;
    FImageList: TVirtualImageList;
    FSVGIconList: TSVGIconImageCollection;
    procedure OnButtonClick(Sender: TObject);
    procedure OnButtonUpdate(Sender: TObject);
  public
    constructor Create(ASearchPathManager: ISearchPathManager);
    destructor Destroy; override;
  private
    class var FToolbar: TComponent;
    class function TryFindComponentByName(const ParentComponent: TComponent; const _Name: string; out _Comp: TComponent): Boolean;
  public
    class constructor Create;
    class destructor Destroy;
  end;

{ TEditServiceNotifier }

constructor TEditServiceNotifier.Create(ASearchPathManager: ISearchPathManager);
begin
  inherited Create;
  FSearchPathManager := ASearchPathManager;
  FSVGIconList := TSVGIconImageCollection.Create(nil);
  FImageList := TVirtualImageList.Create(nil);
  TActionType.InitToolBarImage(FSVGIconList, FImageList);
  FActions := TActionList.Create(nil);
end;

destructor TEditServiceNotifier.Destroy;
begin
  FreeAndNil(FActions);
  FreeAndNil(FImageList);
  FreeAndNil(FSVGIconList);
  inherited;
end;

class constructor TEditServiceNotifier.Create;
begin
  FToolbar := nil;
end;

class destructor TEditServiceNotifier.Destroy;
var
  Btn: TComponent;
  Name: string;
begin
  if FToolbar <> nil then begin
    for var I := FToolbar.ComponentCount -1 downto 0 do begin
      Btn := FToolbar.Components[I];
      Name := Btn.Name;
      if Name.StartsWith('_pom_') then begin
        FToolbar.RemoveComponent(Btn);
        FreeAndNil(Btn);
      end;
    end;
  end;
end;

procedure TEditServiceNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TEditServiceNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TEditServiceNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin

end;

procedure TEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
var
  Ctrl: TWinControl;
begin
  if LastActiveWindow <> EditWindow.Form.Caption then begin
    LastActiveWindow := EditWindow.Form.Caption;

    IsPomView := LastActiveWindow.EndsWith('.pom.xml', True) and (GetActiveProject.FindModuleInfo(LastActiveWindow) <> nil);

    Ctrl := EditWindow.Form;
    if not Assigned(Ctrl) then
      Exit;

    if TryFindComponentByName(Ctrl, 'TEditorNavigationToolbar', FToolbar) then begin
      if FToolbar.FindComponent('_pom_separator') = nil then begin
        // ´´½¨·Ö¸ô·û
        with TToolButton.Create(FToolbar) do begin
          Parent := TWinControl(FToolbar);
          Style := tbsSeparator;
          Width := 8;
          Name := '_pom_separator';
        end;
        var Actions := FActions;//TActionList.Create(C);
        TActionType.InitToolButton(TToolBar(FToolbar), Actions, FImageList, atOption, OnButtonClick, OnButtonUpdate);
        TActionType.InitToolButton(TToolBar(FToolbar), Actions, FImageList, atEnvList, OnButtonClick, OnButtonUpdate);
        TActionType.InitToolButton(TToolBar(FToolbar), Actions, FImageList, atUpdate, OnButtonClick, OnButtonUpdate);
        TActionType.InitToolButton(TToolBar(FToolbar), Actions, FImageList, atApply, OnButtonClick, OnButtonUpdate);
      end;
    end;
  end;
end;

procedure TEditServiceNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
end;

procedure TEditServiceNotifier.OnButtonClick(Sender: TObject);
begin
  FSearchPathManager.OnButtonClick(Self, TActionType(TAction(Sender).Tag));
end;

procedure TEditServiceNotifier.OnButtonUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IsPomView;
end;

class function TEditServiceNotifier.TryFindComponentByName(const ParentComponent: TComponent; const _Name: string;
  out _Comp: TComponent): Boolean;
begin
  Result := False;
  if not Assigned(ParentComponent) then
    Exit;

  for var i := 0 to ParentComponent.ComponentCount - 1 do
  begin
    var Cmp := ParentComponent.Components[i];

    if SameText(cmp.Name, _Name) then
    begin
      _Comp := Cmp;
      Result := True;
      Exit;
    end
    else
      Result := TryFindComponentByName(Cmp, _Name, _Comp); // Recursion!
  end;
end;

procedure TEditServiceNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
end;

procedure TEditServiceNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
begin
end;

procedure TEditServiceNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
end;

procedure TEditServiceNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
end;

var
  NotifierIndex: Integer = 0;

procedure RegisterEditorWindowNotifier(ASearchPathManager: ISearchPathManager);
begin
if Assigned(BorlandIDEServices) then
  begin
    NotifierIndex := (BorlandIDEServices as IOTAEditorServices).AddNotifier(
      TEditServiceNotifier.Create(ASearchPathManager));
  end;
end;

initialization

finalization
  if NotifierIndex > 0 then
    (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(NotifierIndex);

end.
