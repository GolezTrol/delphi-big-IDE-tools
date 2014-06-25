unit uBigIDESmartSave;

interface

uses
  Windows, Forms, Controls, Classes, Menus, Dialogs, SysUtils, ToolsAPI, Actnlist, Registry;

// List of supported IDE functions to override
type
  TIDEAction = (aSaveAll);
  TIDEActions = set of TIDEAction;

// Array of id's to match an action with
const
  ActionId: array[TIDEAction] of String = ('FileSaveAllCommand');

// General class
type                               
  TBigIDEOverrides = class(TInterfacedObject, IOTAWizard, IOTAMenuWizard)
  private
    FOldHandler: array[TIDEAction] of TNotifyEvent;
    FOverrides: TIDEActions;
    function GetOverrides: TIDEActions;
    procedure SetOverrides(const Value: TIDEActions);
  protected
    // Action Handlers
    procedure DoSaveAll(Sender: TObject); virtual;
  protected
    // Hooks
    function GetAllActionsList: String;
    function FindAction(Id: String): TContainedAction;

    procedure HookAction(Action: TIDEAction);
    procedure UnhookAction(Action: TIDEAction);

  protected
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    // IOTAWizard
    { Expert UI strings }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

    { Launch the AddIn }
    procedure Execute;

    // IOTAMenuWizard. Temporary. Add menu item in more flexible way later.
    function GetMenuText: string;
  protected
    // Settings
    procedure LoadActionsFromRegistry; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Overrides: TIDEActions read GetOverrides write SetOverrides;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TBigIDEOverrides.Create as IOTAWizard);

end;

{ TBigIDEOverrides }

// These methods need not be implemented, but are required by the wizard interface
procedure TBigIDEOverrides.AfterSave; begin end;
procedure TBigIDEOverrides.BeforeSave; begin end;
procedure TBigIDEOverrides.Destroyed; begin end;
procedure TBigIDEOverrides.Modified; begin end;

constructor TBigIDEOverrides.Create;
begin
  inherited;

  // Hook already (defaults)
  Overrides := [aSaveAll];
  // Make possible to disable some by registry settings.
  LoadActionsFromRegistry
end;

destructor TBigIDEOverrides.Destroy;
begin
  // Don't forget to restore all actions to normal, else you will get AV's when
  // unloading this wizard and then using any of the actions.
  Overrides := [];

  inherited;
end;

function TBigIDEOverrides.GetAllActionsList: String;
// Development feature, lists the names (and captions) of all actions.
var
  l: Integer;

  function GetComponentList(p: TComponent): String;
  var
    c: TComponent;
    i: Integer;
    s: String;
  begin
    if l = -1 then
      Result := #13#10#13#10'APPLICATION COMPONENT LIST'#13#10;

    Inc(l);
    try
      for i := 0 to p.ComponentCount - 1 do
      begin
        c := p.Components[i];
        s := #13#10 + c.ClassName + ' ('+c.Name+') ';
        if c is TControl then
          s := s + TAction(c).Caption; // Just about any control will do to reach text or caption
        Result := Result + s + GetComponentList(c);
      end;
    finally
      Dec(l);
    end;
  end;


var
  MenuService: INTAServices;
  i: Integer;
  s: String;
  c: String;
  a: TCustomAction;
  ShortCut: String;
begin
  l := -1;

  MenuService := BorlandIDEServices as INTAServices;
  s := '';
  for i := 0 to MenuService.ActionList.ActionCount - 1 do
  begin
    c := '';
    ShortCut := '';
    if MenuService.ActionList.Actions[i] is TCustomAction then
    begin
      a := TCustomAction(MenuService.ActionList.Actions[i]);
      c := a.Caption;
      ShortCut := Format('(%s)', [ShortCutToText(a.ShortCut)]);
    end;

    if (MenuService.ActionList.Actions[i] is TAction) then
    begin
    end;
    // List name and caption for each action
    s := s + Format('%s, %s %s'#13#10, [
      MenuService.ActionList.Actions[i].Name,
      c, ShortCut]);
  end;

  Result := s + GetComponentList(Application);
end;

// Some info about and config for this wizard.
function TBigIDEOverrides.GetIDString: string;
begin
  // Unique id. Company.package.wizardname by convention
  Result := 'GolezTrol.BigIDETools.ActionOverride';
end;

function TBigIDEOverrides.GetName: string;
begin
  // What is this name used for?
  Result := 'Big IDE Action Override';
end;

function TBigIDEOverrides.GetMenuText: string;
begin
  // Debug menu item to show the list of actions from
  Result := 'IDE Action List';
end;

function TBigIDEOverrides.GetState: TWizardState;
begin
  // Enable the debug menu item
  Result := [wsEnabled];
end;

procedure TBigIDEOverrides.Execute;
begin
  // Debug feature. The IDE Action List menu shows a list of actions
  ShowMessage(GetAllActionsList);
end;

function TBigIDEOverrides.FindAction(Id: String): TContainedAction;
var
  MenuService: INTAServices;
  i: Integer;
begin
  // Get the action list of the MenuService and find actions by their name.
  MenuService := BorlandIDEServices as INTAServices;
  for i := 0 to MenuService.ActionList.ActionCount - 1 do
    if SameText(MenuService.ActionList.Actions[i].Name, Id) then
    begin
      Result := MenuService.ActionList.Actions[i];
      Exit;
    end;
  Result := nil;
end;

// The overrides property allows to put an entire set of actions to override.
// It also allows -ofcourse- to read the set of currently overridden actions.
function TBigIDEOverrides.GetOverrides: TIDEActions;
begin
  Result := FOverrides;
end;

procedure TBigIDEOverrides.SetOverrides(const Value: TIDEActions);
var
  i: TIDEAction;
begin
  // Just walk the set and call hook or unhook for each action. These methods
  // will just ignore the call if the action is or isn't already hooked.
  for i := Low(TIDEAction) to High(TIDEAction) do
    if i in FOverrides then
      UnhookAction(i)
    else
      HookAction(i);
end;

procedure TBigIDEOverrides.HookAction(Action: TIDEAction);
// Hook a single action
var
  a: TContainedAction;
begin
  // If hooked already, do nothing
  if not (Action in FOverrides) then
  begin
    // Find the action
    a := FindAction(ActionId[Action]);
    if Assigned(a) then
    begin
      // Store its old execute procedure
      FOldHandler[Action] := a.OnExecute;

      // Set a new event handler
      case Action of
        aSaveAll: a.OnExecute := DoSaveAll;
      else
        raise Exception.Create('Unsupported action to override');
      end;

      // And remember that we have overridden this action
      Include(FOverrides, Action);
    end;
  end;
end;

procedure TBigIDEOverrides.LoadActionsFromRegistry;
// In case any collegue finds it undesirable to overrule any action, make it
// possible to disable the action in the registry.
var
  i       : TIDEAction;
  Actions : TIDEActions;
  Active  : Boolean;
begin
  Actions := Overrides;
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly('\Software\GolezTrol\Big IDE Actions\1.0\Actions\') then
    begin
      for i := Low(TIDEAction) to High(TIDEAction) do
      begin
        // If the name exists in the registry, its value is read or (on failure)
        // defaulted to false. If the name does not exist, the state is unchanged.
        if ValueExists(ActionId[i]) then
        begin
          try
            Active := ReadBool(ActionId[i]);
          except
            Active := False;
          end;

          if Active then
            Include(Actions, i)
          else
            Exclude(Actions, i);
        end;
      end;
    end;
  finally
    Free;
  end;
  // Set the actions
  if Actions <> Overrides then
    Overrides := Actions;
end;

procedure TBigIDEOverrides.UnhookAction(Action: TIDEAction);
// Unhook a single previously hooked action
var
  a: TContainedAction;
begin
  // If not hooked, do nothing
  if (Action in FOverrides) then
  begin
    // Find the action and restore its event handler
    a := FindAction(ActionId[Action]);
    if Assigned(a) then
      a.OnExecute := FOldHandler[Action];

    // Remove this action from the set of hooked actions
    Exclude(FOverrides, Action);
  end;
end;

// ======================================== //
// === Action handlers below this point === //
// ======================================== //
procedure TBigIDEOverrides.DoSaveAll(Sender: TObject);
const
  SaveResultGroup = 'Big SaveAll';
var
  Modules  : IOTAModuleServices;
  Module   : IOTAModule;
  i, j     : Integer;
  NotSaved : TStringList;
  Modified : Boolean;
  FileName : String;
  Group    : IOTAMessageGroup;
  p        : Pointer;
  Shown    : Boolean;
begin
  NotSaved := TStringList.Create;

  try
    Modules := (BorlandIDEServices as IOTAModuleServices);

    for i := 0 to Modules.ModuleCount - 1 do
    begin
      Module := Modules.Modules[i];

      Modified := False;
      for j := 0 to Module.ModuleFileCount - 1 do
        if Module.ModuleFileEditors[j].Modified then
        begin
          // Use the filename of the first file that could not be saved.
          // This might prevent confusion when a pas file is read-only while
          // its dfm is not.
          FileName := Module.ModuleFileEditors[j].FileName;
          Modified := True;
          Break;
        end;

      // If any of the files of the module is modified, try and save.
      if Modified then
      begin
        try
          // Save returns True if the module is saved and... raises an
          // exception when it cannot save the file.
          if not Module.Save(False, True) then
            raise Exception.Create('You''ll probably never get this exception. Save raises an exception itself, although its boolean result might suggest otherwise...');
        except
          // Do nothing yet.
          //NotSaved.Add(FileName);
        end;
      end;
    end;

    // After trying to save all units, check them again to be modified.
    // Each unit that is modified now, can't really be saved.
    for i := 0 to Modules.ModuleCount - 1 do
    begin
      Module := Modules.Modules[i];

      Modified := False;

      for j := 0 to Module.ModuleFileCount - 1 do
        if Module.ModuleFileEditors[j].Modified then
        begin
          FileName := Module.ModuleFileEditors[j].FileName;
          Modified := True;
          Break;
        end;

      if Modified then
        NotSaved.Add(FileName);
    end;

    // Some feedback to the user.
    Group := (BorlandIDEServices as IOTAMessageServices60).GetGroup(SaveResultGroup);
    // Clear previous results
    (BorlandIDEServices as IOTAMessageServices60).RemoveMessageGroup(Group);
    Group := nil;

    // If any files are not saved, show them here.
    if NotSaved.Count > 0 then
    begin
      NotSaved.Sort;

      // Message box?
      //MessageBox((BorlandIDEServices as IOTAServices).GetParentHandle, PChar('The following units could not be saved:'#13#10 + NotSaved.Text), 'Big IDE Smart Save All', MB_ICONERROR or MB_OK);

      // Show group? This seems to be done automatically. Anyway, don't do it explicitly.
      // After all, the user won't usually be interested in this information, since all
      // files that are edited in a 'checked out' state can be saved as well. Other edits
      // Are implicit edits or small tests that shouldn't be saved.
      // The user will see by the units icon that it is not saved.
      //if AShowGroup then
      //  (BorlandIDEServices as IOTAMessageServices60).ShowMessageView(Result);
      Shown := False;
      for i := 0 to NotSaved.Count - 1 do
      begin
        if not Shown then
        begin
          if Group = nil then
            Group := (BorlandIDEServices as IOTAMessageServices60).AddMessageGroup(SaveResultGroup);

          (BorlandIDEServices as IOTAMessageServices60).ShowMessageView(Group);
          Shown := True;
        end;

        (BorlandIDEServices as IOTAMessageServices60).AddToolMessage(NotSaved[i], '', 'Not saved', -1, 0, nil, p, Group);
      end;
    end;
  finally
    NotSaved.Free;
  end;
end;

end.
