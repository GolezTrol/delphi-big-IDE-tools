unit uSearchIntegration;

interface

uses
  Forms, Classes, Dialogs, SysUtils, StrUtils, ToolsApi, fSearchOptions,
  ActiveX, Controls, uBigFastSearchDFM, Masks, fSearchProgress;

const
  IID_IOTASourceEditor: TGUID = '{F17A7BD1-E07D-11D1-AB0B-00C04FB16FB3}';
  IID_IOTAFormEditor: TGUID = '{F17A7BD2-E07D-11D1-AB0B-00C04FB16FB3}';

const
  ResultGroup = 'Big DFM Search';

procedure Register;

type
  TStartFileEvent = procedure(FileName: string) of object;

  TSearchItem = class
  private
    FFileName: string;
    FCode: string;
    FSearch: string;
    FCallBack: TFoundEvent;
    FSearchOptions: TSearchOptions;
    FFindSQLDump: TStrings;
  public
    constructor Create(FileName: string; Code: string; Search: string; CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings = nil);
    function CreateThread: TThread;
  end;

  TSearchThreadList = class
  private
    //FList: TList;
    FQueue: TList;
    FOnAllThreadsDone: TNotifyEvent;
    FRunningThread: TThread;
    FOnStartFile: TStartFileEvent;
    FIOTAWizard: IOTAWizard;
    FNoMoreFiles: Boolean;
    procedure DoThreadTerminated(Sender: TObject);
    procedure StartNextThread;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddSearch(FileName: String; Code: string; Search: string; CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings = nil);
    property OnAllThreadsDone: TNotifyEvent read FOnAllThreadsDone write FOnAllThreadsDone;
    property OnStartFile: TStartFileEvent read FOnStartFile write FOnStartFile;
    procedure Cancel;
    procedure NewSearch;
    procedure NoMoreFiles;
  end;

type
  TBigSearchWizard = class(TInterfacedObject, IOTAWizard, IOTAMenuWizard)
  private
    FResultCount: Integer;
    FProgressForm: TFrmSearchProgress;
    FStartTime: TDateTime;
    FCancelled: Boolean;
    FFinished: Boolean;
  protected
    function GetGroup(AShowGroup: Boolean; AGroupName: string = ResultGroup): IOTAMessageGroup;
    procedure BeforeSearch; virtual;
    procedure AfterSearch; virtual;
    procedure DoStartFile(FileName: String); virtual;
    procedure DoAfterSearch(Sender: TObject); virtual;
    procedure SetResultCount(AResultCount: Integer);
    procedure SearchOpenFiles(ASearchString: string; SearchOptions: TSearchOptions); virtual;
    procedure SearchFilesInProject(ASearchString: string; SearchOptions: TSearchOptions); virtual;
    procedure SearchFilesInFolder(ASearchString: string; AFolder: string; ARecursive: Boolean; SearchOptions: TSearchOptions); virtual;
    procedure OutputSearchResult(AFile: string; ALine, AColumn: Integer; AText: string);
    procedure ParseAndOutput(ACode: TStream; AFileName: string; ASearchText: string; SearchOptions: TSearchOptions); overload;
    procedure ParseAndOutput(ACode: string; AFileName: string; ASearchText: string; SearchOptions: TSearchOptions); overload;
    procedure DoFoundText(AFileName, AFormClass, AObjectName, APropertyName, AValue: string; ALine: Integer);
    procedure ClearMessages;
    procedure DoSearchCancelled(Sender: TObject);
  public
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
  end;

implementation

procedure Register;
begin
  RegisterPackageWizard(TBigSearchWizard.Create as IOTAWizard);

end;

var
  _SearchThreadList: TSearchThreadList;
function SearchThreadList: TSearchThreadList;
begin
  if not Assigned(_SearchThreadList) then
    _SearchThreadList := TSearchThreadList.Create;
  Result := _SearchThreadList;
end;

{ TBigSearchWizard }

procedure TBigSearchWizard.AfterSave;
begin
  // Not needed for wizard
end;

procedure TBigSearchWizard.AfterSearch;
var
  Msg: string;
begin
  SearchThreadList.OnAllThreadsDone := nil;
  SearchThreadList.OnStartFile := nil;

  FreeAndNil(FProgressForm);

  Msg := '';

  if FCancelled then
    Msg := Msg + 'Search cancelled. ';

  if FResultCount = 0 then
    Msg := Msg + 'No results found. '
  else
    Msg := Msg + IntToStr(FResultCount) + ' results found. ';

  Msg := Msg + 'Duration: ' + FormatDateTime('nn:ss', Now - FStartTime) + '. ';

  ShowMessage(Msg);

  SearchThreadList.FIOTAWizard := nil;
end;

procedure TBigSearchWizard.BeforeSave;
begin
  // Not needed for wizard
end;

procedure TBigSearchWizard.BeforeSearch;
begin
  ClearMessages;

  FStartTime := Now;

  FCancelled := False;
  FFinished  := False;

  FProgressForm := TFrmSearchProgress.Create(Application);
  FProgressForm.Hide;
  FProgressForm.OnCancel := DoSearchCancelled;

  SearchThreadList.OnAllThreadsDone := DoAfterSearch;
  SearchThreadList.OnStartFile := DoStartFile;
  SearchThreadList.FIOTAWizard := Self;
end;

procedure TBigSearchWizard.ClearMessages;
begin
  (BorlandIDEServices as IOTAMessageServices60).ClearMessageGroup(GetGroup(False));

  SetResultCount(0);
end;

procedure TBigSearchWizard.Destroyed;
begin
  // Clean up
end;

procedure TBigSearchWizard.DoAfterSearch(Sender: TObject);
begin
  if FCancelled or FFinished then
    AfterSearch
end;

procedure TBigSearchWizard.DoFoundText(AFileName, AFormClass, AObjectName,
    APropertyName, AValue: string; ALine: Integer);
begin
  OutputSearchResult(AFileName, ALine, 0, AFormClass + '.' + AObjectName + '.' + APropertyName + '=' + AValue);
  Application.ProcessMessages;
end;

procedure TBigSearchWizard.DoSearchCancelled(Sender: TObject);
begin
  FCancelled := True;

  SearchThreadList.Cancel;
end;

procedure TBigSearchWizard.DoStartFile(FileName: String);
begin
  if Assigned(FProgressForm) then
    FProgressForm.SetFileName(FileName);
end;

procedure TBigSearchWizard.Execute;
begin
  with TFrmBigSearchOptions.Create(Application) do
  try
    if ShowModal = mrOK then
    begin
      SearchThreadList.NewSearch;
      
      BeforeSearch;
      try
        case SearchLocation of
          slOpenFiles: SearchOpenFiles(SearchString, SearchOptions);
          slProjectFiles: SearchFilesInProject(SearchString, SearchOptions);
          slDirectory: SearchFilesInFolder(SearchString, SearchFileMask, SearchRecursive, SearchOptions);
        end;
      finally
        // AfterSearch is triggered by thread manager
        //AfterSearch;

        // Tell the queue that there will be no more files.
        SearchThreadList.NoMoreFiles;
      end;
    end;
  finally
    Free;
  end;
end;

function TBigSearchWizard.GetGroup(AShowGroup: Boolean;
  AGroupName: string): IOTAMessageGroup;
begin
  Result := (BorlandIDEServices as IOTAMessageServices60).GetGroup(ResultGroup);
  if Result = nil then
    Result := (BorlandIDEServices as IOTAMessageServices60).AddMessageGroup(ResultGroup);

  if AShowGroup then
    (BorlandIDEServices as IOTAMessageServices60).ShowMessageView(Result);
end;

function TBigSearchWizard.GetIDString: string;
begin
  Result := 'GolezTrol.BigIDETools.SearchHelper';
end;

function TBigSearchWizard.GetMenuText: string;
begin
  Result := 'Big &DFM Search';
end;

function TBigSearchWizard.GetName: string;
begin
  Result := 'Big DFM Search';
end;

function TBigSearchWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TBigSearchWizard.Modified;
begin
  // Not needed for wizard
end;

procedure TBigSearchWizard.OutputSearchResult(AFile: string;
  ALine, AColumn: Integer; AText: string);
var
  p: Pointer;
  g: IOTAMessageGroup;
begin
  g := GetGroup(True);
  // If a matching pas file is found, open that instead to prevent some serious
  // errors in Delphi 7..
  // Skipped it to see how it acts in Delphi 2007.
  {if SameText(ExtractFileExt(AFile), '.dfm') then
    if FileExists(ChangeFileExt(AFile, '.pas')) then
    begin
      AFile := ChangeFileExt(AFile, '.pas');
      ALine := 0;
    end;}

  SetResultCount(Succ(FResultCount)); // Ok, that's nasty
  (BorlandIDEServices as IOTAMessageServices60).AddToolMessage(AFile, AText, ResultGroup, ALine, AColumn, nil, p, g);
end;

procedure TBigSearchWizard.ParseAndOutput(ACode: TStream; AFileName: string; ASearchText: string; SearchOptions: TSearchOptions);
var
  s: TStringStream;
begin
  // Parse from stream. Actually copies the stream to a string and parses the string.
  s := TStringStream.Create('');
  try
    s.CopyFrom(ACode, 0);
    ParseAndOutput(s.DataString, AFileName, ASearchText, SearchOptions);
  finally
    s.Free;
  end;
end;

procedure TBigSearchWizard.ParseAndOutput(ACode, AFileName,
  ASearchText: string; SearchOptions: TSearchOptions);
var
  Code: string;
  s1, s2: TStringStream;
begin
  Application.ProcessMessages;

  if FCancelled then
    Abort;
    
  Code := ACode;

  s1 := TStringStream.Create(Code);
  s2 := TStringStream.Create('');
  try

    // Try and see if the stream contains a binary dfm. If so, convert it to
    // text.
    try
      ObjectResourceToText(s1, s2);

      // Parsing as binary is OK. s2 contains the text output.
      Code := s2.DataString;
    except
      // Parsing as binary failed. Assume text dfm.
    end;

  finally
    s1.Free;
    s2.Free;
  end;

  // Threading thingy failed (no thread safe queue?) Just do it synchronous. It's fast enough.
  ScanDFM(Code, ASearchText, DoFoundText, SearchOptions);
  //SearchThreadList.AddSearch(AFileName, Code, ASearchText, DoFoundText, SearchOptions);
end;

procedure TBigSearchWizard.SearchFilesInFolder(ASearchString,
  AFolder: string; ARecursive: Boolean; SearchOptions: TSearchOptions);

var
  Folder: string;
  Mask: string;
  FileName: string;
  FolderList: TStringList;
  FileStream: TFileStream;

  sr: TSearchRec;
  res: Integer;
begin
  if not DirectoryExists(AFolder) then
  begin
    // Roughly check if a file mask is given.
    Mask := ExtractFileName(AFolder);
    Folder := ExtractFilePath(AFolder);
    if Mask = '' then
      Mask := '*.dfm';

    if not DirectoryExists(Folder) then
    begin
      ShowMessageFmt('Folder not found:'#13'%s', [AFolder]);
      Exit;
    end;
  end
  else
  begin
    Folder := IncludeTrailingPathDelimiter(AFolder);
    Mask := '*.dfm'
  end;

  // No recursion, use a Folder stack.
  FolderList := TStringList.Create;
  try
    // Add the first folder to be searched.
    FolderList.Add(Folder);

    // Repeat the folder search until the list is empty.
    while (FolderList.Count > 0) and not FCancelled do
    begin
      Folder := FolderList[0];
      FolderList.Delete(0);

      // Search all files. The mask must not be applied to the folders.
      res := FindFirst(Folder + '*.*', faAnyFile, sr);
      try
        while (res = 0) and not FCancelled do
        begin
          if ((sr.Attr and faDirectory) = faDirectory) then
          begin
            // If recursive searching is on, add folders to the folder stack,
            // except for '.' and '..'.
            if (sr.Name <> '.') and (sr.Name <> '..') and ARecursive then
              FolderList.Add(Folder + sr.Name + PathDelim);
          end
          else
          begin
            FileName := Folder + sr.Name;

            // Check if the filename matches the mask.
            if MatchesMask(sr.Name, Mask) then
            begin
              // Load the file from disk and parse it.
              // Note: files are always loaded from disk. Unsaved changes will
              // not be shown in the search result!
              FileStream := TFileStream.Create(FileName, fmOpenRead);
              try
                ParseAndOutput(FileStream, FileName, ASearchString, SearchOptions);
              finally
                FileStream.Free;
              end;
            end;
          end;

          res := FindNext(sr);
        end;

      finally
        FindClose(sr);
      end;

    end;

  finally
    FolderList.Free;
    FFinished := True;
  end;
end;

procedure TBigSearchWizard.SearchFilesInProject(ASearchString: string; SearchOptions: TSearchOptions);
var
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
  Modules: IOTAModuleServices;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;
  i, j: Integer;
  FileName: string;
  FileStream: TFileStream;
begin
  try
    Modules := (BorlandIDEServices as IOTAModuleServices);

    // Check if we can find a project group.  
    for i := 0 to Modules.ModuleCount - 1 do
    begin

      Module := Modules.Modules[i];
      if Supports(Module, IOTAProjectGroup, ProjectGroup) then
        Break;
    end;

    if Assigned(ProjectGroup) then
    begin

      // Traverse all projects in the project group.
      for i := 0 to ProjectGroup.ProjectCount - 1 do
      begin
        if FCancelled then
          Break;

        Project := ProjectGroup.Projects[i];

        // Traverse all modules in the project.
        for j := 0 to Project.GetModuleCount - 1 do
        begin
          if FCancelled then
            Break;

          ModuleInfo := Project.GetModule(j);

          // If the module is a form or a datamodule, try to load and parse it.
          if (ModuleInfo.ModuleType = omtForm) or (ModuleInfo.ModuleType =  omtDatamodule) then
          begin
            FileName := ChangeFileExt(ModuleInfo.FileName, '.dfm');

            if FileExists(FileName) then
            begin

              // Note: Files are loaded from disk. New files, or in memory
              // changes that have not been saved, are not reflected in the
              // search result.
              FileStream := TFileStream.Create(FileName, fmOpenRead);
              try
                ParseAndOutput(FileStream, FileName, ASearchString, SearchOptions);
              finally
                FileStream.Free;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FFinished := True;
  end;
end;

procedure TBigSearchWizard.SearchOpenFiles(ASearchString: string; SearchOptions: TSearchOptions);
var
  Modules: IOTAModuleServices;
  Module: IOTAModule;
  i, j: Integer;
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  Resource: TMemoryStream;
  ResourceAdapter: IStream;
  Reader: IOTAEditReader;
  Source: string;
  Position, Count: Integer;
  Buffer: array[0..4096] of Char;
begin
  try
    Modules := (BorlandIDEServices as IOTAModuleServices);

    // Search all open modules.
    for i := 0 to Modules.ModuleCount - 1 do
    begin
      if FCancelled then
        Break;

      Module := Modules.Modules[i];

      // Search all files/units of the module.
      for j := 0 to Module.ModuleFileCount - 1 do
      begin
        if FCancelled then
          Break;

        if Supports(Module.ModuleFileEditors[j], IID_IOTAFormEditor, FormEditor) then
        begin
          // If the unit supports a form editor, the form editor is open. The dfm
          // code is retreived from this form editor. This search acutally searches
          // the in memory dfm, not the one on disk.
          Resource := TMemoryStream.Create;
          ResourceAdapter := TStreamAdapter.Create(Resource, soReference);
          try
            FormEditor.GetFormResource(ResourceAdapter);
            Resource.Position := 0;

            ParseAndOutput(Resource, FormEditor.FileName, ASearchString, SearchOptions);
          finally
            ResourceAdapter := nil;
            Resource.Free;
          end;
        end
        else if Supports(Module.ModuleFileEditors[j], IID_IOTASourceEditor, SourceEditor) then
        begin
          // If the module is a source editor, but the extension is .dfm, assume
          // this is a dfm source editor (like when you press Alt+F12 on a form)
          // Read the source from the source editor. This search actually searches
          // the in memory dfm, not the one on disk.
          if SameText(ExtractFileExt(Module.FileName), '.dfm') then
          begin
            Source := '';
            Position := 0;
            Count := 4096;

            // A source editor only supports buffered reading through a reader.
            // Copy all pieces and put them in the source variable. Then parse it.
            Reader := SourceEditor.CreateReader;

            repeat
              Count := Reader.GetText(Position, Buffer, Count);
              Source := Source + Copy(Buffer, 0, Count);
              Inc(Position, Count);
            until Count = 0;
            Reader := nil;

            ParseAndOutput(Source, SourceEditor.FileName, ASearchString, SearchOptions);
          end;
        end;
      end;
    end;
  finally
    FFinished := True;
  end;
end;

procedure TBigSearchWizard.SetResultCount(AResultCount: Integer);
begin
  // Show the result count in the progress window.
  if Assigned(FProgressForm) then
    FProgressForm.SetFound(AResultCount);
  FResultCount := AResultCount;
end;

{ TSearchThreadList }

procedure TSearchThreadList.AddSearch(FileName, Code, Search: string;
  CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings);
begin
  // Create a queue item.
  FQueue.Add(TSearchItem.Create(FileName, Code, Search, CallBack, SearchOptions, FindSQLDump));
  StartNextThread;
end;

procedure TSearchThreadList.Cancel;
var
  i: Integer;
begin
  // If any thread is still running, ignore it.
  if Assigned(FRunningThread) then
    TScanDFMThread(FRunningThread).Abandon;
  FRunningThread := nil;

  // Clear the queue
  for i := FQueue.Count - 1 downto 0 do
  begin
    TObject(FQueue[i]).Free;
    FQueue.Delete(i);                     
  end;

  // Notify the wizard we're done
  if Assigned(FOnAllThreadsDone) then
    FOnAllThreadsDone(Self);
end;

constructor TSearchThreadList.Create;
begin
  inherited Create;

  FQueue := TList.Create;
end;

destructor TSearchThreadList.Destroy;
begin
  FQueue.Free;

  inherited;
end;

procedure TSearchThreadList.DoThreadTerminated(Sender: TObject);
begin
  // If the thread is not the current running thread, ignore it.
  // Could be a hanging thread from earlier searches, although that shouldn't
  // trigger this event anymore.
  if FRunningThread = Sender then
  begin
    FRunningThread := nil;
    StartNextThread;
  end;
end;

procedure TSearchThreadList.NewSearch;
begin
  FNoMoreFiles := False;
end;

procedure TSearchThreadList.NoMoreFiles;
begin
  FNoMoreFiles := True;
  StartNextThread; // Forces AllThreadsDoneEvent
end;

procedure TSearchThreadList.StartNextThread;
var
  i: Integer;
  q: TSearchItem;
begin
  // Only start the next thread when there is no thread active.
  if FRunningThread = nil then
  begin
    while FNoMoreFiles = False do
    begin
      // Start the next search, if any, or notify the wizard we're done.
      if FQueue.Count > 0 then
      begin
        // Get the next item from the queue. Notify the wizard of which file it is.
        // Create and start the thread and remove the item from the queue.
        i := FQueue.Count - 1;
        q := TSearchItem(FQueue[i]);
        FQueue.Delete(i);
        FRunningThread := q.CreateThread;
        FRunningThread.OnTerminate := DoThreadTerminated;
        if Assigned(FOnStartFile) then
          FOnStartFile(q.FFileName);
        FRunningThread.Resume;
        q.Free;
      end
      else
        Sleep(1); // Give up thread slice while waiting for more files.
    end;

    if Assigned(FOnAllThreadsDone) then
      FOnAllThreadsDone(Self);
  end;

end;

{ TSearchItem }

constructor TSearchItem.Create(FileName, Code, Search: string; CallBack: TFoundEvent;
  SearchOptions: TSearchOptions; FindSQLDump: TStrings);
begin
  inherited Create;
  
  FFileName := FileName;
  FCode := Code;
  FSearch := Search;
  FCallBack := CallBack;
  FSearchOptions := SearchOptions;
  FFindSQLDump := FindSQLDump
end;

function TSearchItem.CreateThread: TThread;
begin
  Result := TScanDFMThread.Create(FFileName, FCode, FSearch, FCallBack, FSearchOptions, FFindSQLDump);
end;

initialization
finalization
  _SearchThreadList.Free;
end.
