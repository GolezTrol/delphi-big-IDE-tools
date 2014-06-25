unit fSearchOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvBaseDlg, JvSelectDirectory, uBigFastSearchDfm;

type
  TSearchLocation = (slProjectFiles, slOpenFiles, slDirectory);

  TFrmBigSearchOptions = class(TForm)
    cmbTextToFind: TComboBox;
    lblTextToFind: TLabel;
    gbxWhere: TGroupBox;
    rbSearchAllFilesInProject: TRadioButton;
    rbSearchAllOpenFiles: TRadioButton;
    rbSearchInDirectories: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    gbxSearchDirectoryOptions: TGroupBox;
    cmbFileMask: TComboBox;
    btnBrowse: TButton;
    lblFileMask: TLabel;
    chbSearchRecursive: TCheckBox;
    dslSearchDirectory: TJvSelectDirectory;
    gbxOptions: TGroupBox;
    chbWildCardSearch: TCheckBox;
    chbSmartSpaces: TCheckBox;
    chbStartingWith: TCheckBox;
    mmoHelp: TMemo;
    chbStringlistAsOne: TCheckBox;
    procedure btnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure cmbFileMaskChange(Sender: TObject);
  private
    function GetSearchString: string;
    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;
    function GetSearchFileMask: string;
    function GetSearchLocation: TSearchLocation;
    function GetSearchRecursive: Boolean;
    function GetSearchOptions: TSearchOptions;
  public
    property SearchString: string read GetSearchString;
    property SearchLocation: TSearchLocation read GetSearchLocation;
    property SearchOptions: TSearchOptions read GetSearchOptions;
    property SearchFileMask: string read GetSearchFileMask;
    property SearchRecursive: Boolean read GetSearchRecursive;
  end;

implementation

uses Registry;

{$R *.dfm}

procedure TFrmBigSearchOptions.btnHelpClick(Sender: TObject);
begin
  MessageDlg(mmoHelp.Text, mtInformation, [mbOk], 0);
end;

function TFrmBigSearchOptions.GetSearchString: string;
begin
  Result := cmbTextToFind.Text;
end;

procedure TFrmBigSearchOptions.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TFrmBigSearchOptions.btnOKClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFrmBigSearchOptions.LoadSettings;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    with TRegistry.Create do
    try
      if OpenKeyReadOnly('\Software\GolezTrol\Big DFM Search\1.0\') then
      begin
        if ValueExists('LastFolder') then // Legacy
          cmbFileMask.Text := ReadString('LastFolder');

        if ValueExists('Where') then
          case TSearchLocation(ReadInteger('Where')) of
            slProjectFiles: rbSearchAllFilesInProject.Checked := True;
            slDirectory: rbSearchInDirectories.Checked := True;
          else
            rbSearchAllOpenFiles.Checked := True;
          end;

        if ValueExists('Recursive') then
          chbSearchRecursive.Checked := ReadBool('Recursive');

        if ValueExists('WildcardSearch') then
          chbWildCardSearch.Checked := ReadBool('WildcardSearch');

        if ValueExists('SmartSpaces') then
          chbSmartSpaces.Checked := ReadBool('SmartSpaces');

        if ValueExists('StartingWith') then
          chbStartingWith.Checked := ReadBool('StartingWith');

        if ValueExists('StringlistAsOne') then
          chbStringlistAsOne.Checked := ReadBool('StringlistAsOne');

        if OpenKeyReadOnly('\Software\GolezTrol\Big DFM Search\1.0\SearchHistory\') then
        begin
          GetValueNames(sl);
          sl.Sort;
          cmbTextToFind.Clear;
          for i := 0 to sl.Count - 1 do
          begin
            cmbTextToFind.Items.Add(ReadString(sl[i]));
          end;
          if cmbTextToFind.Items.Count > 0 then
            cmbTextToFind.ItemIndex := 0;
        end;

        if OpenKeyReadOnly('\Software\GolezTrol\Big DFM Search\1.0\FileMaskHistory\') then
        begin
          GetValueNames(sl);
          sl.Sort;
          cmbFileMask.Clear;
          for i := 0 to sl.Count - 1 do
          begin
            cmbFileMask.Items.Add(ReadString(sl[i]));
          end;
          if cmbFileMask.Items.Count > 0 then
            cmbFileMask.ItemIndex := 0;
        end;

      end;

    finally
      Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TFrmBigSearchOptions.SaveSettings;
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.Duplicates := dupIgnore;
  try
    with TRegistry.Create do
    try
      if OpenKey('\Software\GolezTrol\Big DFM Search\1.0\', True) then
      begin

        WriteInteger('Where', Integer(GetSearchLocation));
        WriteBool('Recursive', chbSearchRecursive.Checked);
        WriteBool('WildcardSearch', chbWildCardSearch.Checked);
        WriteBool('SmartSpaces', chbSmartSpaces.Checked);
        WriteBool('StartingWith', chbStartingWith.Checked);
        WriteBool('StringlistAsOne', chbStringlistAsOne.Checked);
      end;

      if OpenKey('\Software\GolezTrol\Big DFM Search\1.0\SearchHistory\', True) then
      begin
        sl.Assign(cmbTextToFind.Items);
        i := sl.IndexOf(cmbTextToFind.Text);
        if i > -1 then
          sl.Move(i, 0)
        else
          sl.Insert(0, cmbTextToFind.Text);

        while sl.Count > 26 do
          sl.Delete(26);

        for i := 0 to sl.Count - 1 do
          WriteString(chr(65 + i), sl[i]);
      end;

      if OpenKey('\Software\GolezTrol\Big DFM Search\1.0\FileMaskHistory\', True) then
      begin
        sl.Assign(cmbFileMask.Items);
        i := sl.IndexOf(cmbFileMask.Text);
        if i > -1 then
          sl.Move(i, 0)
        else
          sl.Insert(0, cmbFileMask.Text);

        while sl.Count > 26 do
          sl.Delete(26);

        for i := 0 to sl.Count - 1 do
          WriteString(chr(65 + i), sl[i]);
      end;

    finally
      Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TFrmBigSearchOptions.btnBrowseClick(Sender: TObject);
begin
  dslSearchDirectory.InitialDir := cmbFileMask.Text;
  if dslSearchDirectory.Execute then
  begin
    cmbFileMask.Text := dslSearchDirectory.Directory;
  end;
end;

function TFrmBigSearchOptions.GetSearchFileMask: string;
begin
  Result := cmbFileMask.Text;
end;

function TFrmBigSearchOptions.GetSearchLocation: TSearchLocation;
begin
  if rbSearchAllFilesInProject.Checked then
    Result := slProjectFiles
  else if rbSearchInDirectories.Checked then
    Result := slDirectory
  else
    Result := slOpenFiles;
end;
                                      
function TFrmBigSearchOptions.GetSearchOptions: TSearchOptions;
begin
  Result := [];
  // Only add wildcard flag if search string contains wildcard. If not,
  // wildcard flag is omitted. This will result in faster searching for
  // files _containing_ the search string
  if chbWildCardSearch.Checked and ((Pos('*', cmbTextToFind.Text) > 0) or (Pos('?', cmbTextToFind.Text) > 0)) then
    Include(Result, soWildcard);

  if chbSmartSpaces.Checked then
    Include(Result, soSmartSpace);

  if chbStartingWith.Checked then
    Include(Result, soStartingWith);

  if not chbStringlistAsOne.Checked then
    Include(Result, soStringsItemsAsSeparateStrings);
end;

function TFrmBigSearchOptions.GetSearchRecursive: Boolean;
begin
  Result := chbSearchRecursive.Checked;
end;

procedure TFrmBigSearchOptions.cmbFileMaskChange(Sender: TObject);
begin
  rbSearchInDirectories.Checked := True;
end;

end.

