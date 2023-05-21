unit SearchPath.Wizard;

interface

uses SearchPath.Common;

  procedure RegisterWizard(ASearchPathManager: ISearchPathManager);

implementation

uses
  System.Actions, Vcl.Forms, Vcl.Menus, Vcl.ActnList, ToolsAPI,
  SearchPath.Options;

type
  TSearchPathWizard = class(TNotifierObject, IOTAWizard)
  private
    FMenuItem: TMenuItem;
    FActions: TActionList;
    FSearchPathManager: ISearchPathManager;
    { Expert UI strings }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    { Launch the AddIn }
    procedure Execute;

    procedure OnMenuClick(Sender: TObject);
    procedure OnMenuUpdate(Sender: TObject);
  public
    constructor Create(ASearchPathManager: ISearchPathManager);
    destructor Destroy; Override;
  end;

{ TSearchPathWizard }

constructor TSearchPathWizard.Create(ASearchPathManager: ISearchPathManager);
begin
  FSearchPathManager := ASearchPathManager;
  FActions := TActionList.Create(nil);
  var Act := TAction.Create(FActions);

  with Act do begin
    OnExecute := OnMenuClick;
    OnUpdate := OnMenuUpdate;
    Caption := 'Dependencies';
  end;
  Act.SetParentComponent(FActions);

  FMenuItem := nil;
  var NTAS := (BorlandIDEServices as INTAServices);
  if (NTAS <> nil) and (NTAS.MainMenu <> nil) then begin
    var MmiViewMenu := NTAS.MainMenu.Items.Find('Tools');
    if MmiViewMenu <> nil then begin
      // : @bug Menu not fully build at this point.
      var MmiFirstDivider := MmiViewMenu.Find('-');
      if MmiFirstDivider <> nil then begin
        FMenuItem := TMenuItem.Create(MmiViewMenu);
        FMenuItem.Caption := 'Dependencies';
        //FMenuItem.OnClick := OnMenuClick;
        FMenuItem.Action := FActions.Actions[0];
        MmiViewMenu.Insert(MmiFirstDivider.MenuIndex, FMenuItem);
      end;
    end;
  end;
end;

destructor TSearchPathWizard.Destroy;
begin
  FMenuItem.Free;
  FActions.Free;
  inherited;
end;

procedure TSearchPathWizard.Execute;
begin
  //Logger.Debug('Execute');
end;

function TSearchPathWizard.GetIDString: string;
begin
  Result := '[392E5F55-54A8-4A2D-8E92-F4613F8F7487]';
end;

function TSearchPathWizard.GetName: string;
begin
  Result := 'Project search path setup';
end;

function TSearchPathWizard.GetState: TWizardState;
begin
  //var ActiveProject := GetActiveProject;
  //if ActiveProject <> nil then
    //Result := [wsEnabled]
  //else
    Result := [wsEnabled];
end;

procedure TSearchPathWizard.OnMenuClick(Sender: TObject);
begin
  TFormOptions.Execute(FSearchPathManager);
end;

procedure TSearchPathWizard.OnMenuUpdate(Sender: TObject);
begin
  var ActiveProject := GetActiveProject;
  TAction(Sender).Enabled := ActiveProject <> nil;
end;

var
  WizardIndex: Integer = 0;

procedure RegisterWizard(ASearchPathManager: ISearchPathManager);
begin
  Application.Handle := (BorlandIDEServices As IOTAServices).GetParentHandle;
  WizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(TSearchPathWizard.Create(ASearchPathManager));
end;

initialization

finalization
  if WizardIndex > 0 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(WizardIndex);

end.
