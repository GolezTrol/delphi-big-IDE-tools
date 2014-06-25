unit uBigFastSearchDFM;

interface

uses
  Dialogs, Classes, SysUtils, Masks;

type
  TFoundEvent = procedure(AFileName, AFormClass, AObjectName, APropertyName, AValue: string; ALine: Integer) of object;
  TSearchOption = (soWildcard, soSmartSpace, soStartingWith, soStringsItemsAsSeparateStrings);
  TSearchOptions = set of TSearchOption;

type
  TScanDFMThread = class(TThread)
  private
    FFileName: string;
    FCode: string;
    FSearch: string;
    FCallBack: TFoundEvent;
    FSearchOptions: TSearchOptions;
    FFindSQLDump: TStrings;

    FFoundFormClass: string;
    FFoundObjectName: string;
    FFoundPropertyName: string;
    FFoundValue: string;
    FFoundLine: Integer;
  protected
    procedure Execute; override;
    procedure DoFound(AFileName, AFormClass, AObjectName, APropertyName, AValue: string; ALine: Integer);
    procedure HandleFound;
  public
    constructor Create(FileName: string; Code: string; Search: string; CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings = nil);
    procedure Abandon;
  end;

procedure ScanDFM(Code: string; AFileName: String; Search: string; CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings = nil);

implementation

{ TScanDFMThread }

procedure TScanDFMThread.Abandon;
begin
  // If a thread won't finish (usually because the search mask is too complex)
  // abandon the thread. Disable the events and give the thread idle priority.
  // The user will be able to continue and even start new searches, while this
  // thread continues to run in the background.
  FCallBack := nil;
  Priority := tpIdle;
  OnTerminate := nil;
end;

constructor TScanDFMThread.Create(FileName, Code, Search: string; CallBack: TFoundEvent;
  SearchOptions: TSearchOptions; FindSQLDump: TStrings);
begin
  inherited Create(True);
  FFileName := FileName;
  FCode := Code;
  FSearch := Search;
  FCallBack := CallBack;
  FSearchOptions := SearchOptions;
  FFindSQLDump := FindSQLDump;

  FreeOnTerminate := True;
  //Resume;
end;

procedure TScanDFMThread.DoFound(AFileName, AFormClass, AObjectName, APropertyName,
  AValue: string; ALine: Integer);
begin
  FFoundFormClass := AFormClass;
  FFoundObjectName := AObjectName;
  FFoundPropertyName := APropertyName;
  FFoundValue := AValue;
  FFoundLine := ALine;
  Synchronize(HandleFound);
end;

procedure TScanDFMThread.Execute;
begin
  inherited;

  if not Terminated then
  try
    ScanDFM(FCode, FFileName, FSearch, DoFound, FSearchOptions, FFindSQLDump);
  except
    on E:EAbort do
    else
      raise;
  end;
end;

procedure TScanDFMThread.HandleFound;
begin
  if Assigned(FCallBack) then
    FCallBack(FFileName, FFoundFormClass, FFoundObjectName, FFoundPropertyName, FFoundValue, FFoundLine)
  else
    Abort;
end;

type
  TExpecting = (eObject, eObjectName, eObjectClass);

procedure DeSpace(var s: string);
// Replace all subsequent whitespace characters with one single space
var
  i: Integer;
  j: Integer;
  b: Boolean;
begin
  j := 0;
  b := True;
  for i := 1 to Length(s) do
  begin
    if (s[i] in [#13,#10,#9,#32]) then
    begin
      if not b then
      begin
        Inc(j);
        s[j] := ' ';
      end;
      b := True;
    end else
    begin
      Inc(j);
      s[j] := s[i];
      b := False;
    end;
  end;
  SetLength(s, j);
end;

procedure ScanDFM(Code: string; AFileName: String; Search: string; CallBack: TFoundEvent; SearchOptions: TSearchOptions; FindSQLDump: TStrings = nil);
// This routine scans an entire dfm file and skips most characters. It scans
// words to find object names and property names. It also triggers on <quote>
// and <tab> to find strings. Numbers, binary data and operators are skipped.
var
  i: Integer; // Code pos
  j: Integer; // Code pos helper
  Len: Integer; // Length of entire code

  Buffer: string; // Buffer to hold strings
  BufferPos: Integer;

  InfoString: string; // Property name
  TempString: string; // Property info block

  FormClass: string;
  LastObjectName: string;
  LastIdentifier: string;
  LastProperty: string;
  LastPropertyItem: string;
  LastString: string;
  Haystack: string;
  LastItem: Integer;
  LastCollectionItem: Integer;
  LastCollectionItemStr: string;
  CollectionName: string;
  PropertyName: string;

  Done: Boolean; // Loop terminator

  CharacterCode: Integer; // Code of special character
  Expecting: TExpecting; // What's next?

  FoundStringParticle: Boolean;

  PropertyPrinted: Boolean; // Property name printed to FindSQL export
  ObjectNamePrinted: Boolean; // Object name printed to FindSQL export

  Line: Integer; // Current line position (of i)
  LastPropertyLine: Integer; // Line number of last property name

  procedure IncI;
  begin
    if Code[i] = #13 then
      Inc(Line);
    Inc(i);
  end;

  procedure DecI;
  begin
    Dec(i);
    if Code[i] = #13 then
      Dec(Line);
  end;

  procedure SkipWhiteSpace;
  begin
    while (i <= Len) and (Code[i] in [' ', #9, #13, #10]) do
      IncI;
  end;

begin
  Search := UpperCase(Search); // Search case-insensitive
  // If not 'Starting with', insert a wildcard at the beginning of the search string
  if (soWildcard in SearchOptions) and not (soStartingWith in SearchOptions) and
      (Copy(Search, 1, 1) <> '*') then
    Insert('*', Search, 1);
  Len := Length(Code);         // Store to eliminate multiple function calls
  Line := 1;                   // Line number of current position
  LastPropertyLine := 0;       // Line number of last property name
  i := 1;                      // Current character
  Expecting := eObject;        // Expected next word
  LastItem := -1;              // Item in stringlist property. -1 = not in stringlist
  LastCollectionItem := -1;    // Item in collection property. -1 = not in collection

  // Remember if the object name and property name are output already to a
  // the FindSQL export.
  ObjectNamePrinted := False;
  PropertyPrinted := False;

  while i < Len do
  begin
    case Code[i] of
      'a'..'z', 'A'..'Z', '_': // Identifier or keyword
      begin
        LastProperty := '';
        repeat
          j := i;
          // Find an identifier (range of alphanumerical characters) and store it
          repeat
            IncI
          until not (Code[i] in ['a'..'z', 'A'..'Z', '_', '0'..'9']);
          LastIdentifier := Copy(Code, j, i - j);
          if Code[i] <> '.' then
          begin
            // No dot: Add identifier to property and break the identifier-loop
            LastProperty := LastProperty + LastIdentifier;
            DecI;
            Break;
          end
          else
          begin
            // Dot found. If identifier is followed by a '.' keep scanning.
            // Multiple identifiers make up a propery name (like SQL.Strings)
            LastProperty := LastProperty + LastIdentifier + '.';
            IncI;
          end;
        until False;

        PropertyPrinted := False;
        LastPropertyLine := Line; // Remember line number

        // Check for object name and class
        case Expecting of
          eObject:
          begin
            // Found object. Next identifier is the object name
            if SameText(LastIdentifier, 'object') or
                SameText(LastIdentifier, 'inherited') then
              Expecting := eObjectName;
          end;
          eObjectName:
          begin
            LastObjectName := LastProperty;
            ObjectNamePrinted := False;
            SkipWhiteSpace;
            if Code[i] = ':' then // Skip the colon
              IncI;
            Expecting := eObjectClass; // Scan for object class
          end;
          eObjectClass:
          begin
            // The first object class we find is the form class. Store it
            if FormClass = '' then
              FormClass := LastProperty;
            Expecting := eObject;
          end;
        end;
      end;
      '(': LastItem := 0; // Count items for stringlist properties
      ')': LastItem := -1; // -1 = not in stringlist
      '<':
        begin
          // Count items for collection properties and remember the collection
          // property name
          LastCollectionItem := 0;
          CollectionName := LastProperty;
          SkipWhiteSpace;
        end;
      '>':
        begin
          LastCollectionItem := -1;
          CollectionName := '';
          SkipWhiteSpace;
        end;
      '''', '#':
      begin
        LastString := '';


        repeat // Scan string loop (multiple parts, separated by a '+')
          repeat // Scan string part loop (range of strings and special chars)
            FoundStringParticle := False;

            if Code[i] = '''' then // String, encapsuled in quotes
            begin
              j := i;
              FoundStringParticle := True;

              // Find the end quote of this string
              Done := False;
              repeat
               // Loop until a quote is found that is not immediately followed
               // by another. Two quotes will be translated to one, later.
                repeat
                  IncI
                until (Code[i] = '''') or (i > Len);

                if (i > Len) or (Code[Succ(i)] <> '''') then
                  Done := True
                else if Code[Succ(i)] = '''' then
                  IncI; // Skip over this next quote.
              until Done;

              // Allocate a buffer that's large enough at least
              SetLength(Buffer, i - j);
              BufferPos := 1;
              // Add all characters to the buffer, skipping each first quote
              // This way not many relocates are necessary
              while j < i do
              begin
                if Code[j] = '''' then
                  Inc(j);
                Buffer[BufferPos] := Code[j];
                Inc(j);
                Inc(BufferPos);
              end;
              // Truncate the string
              SetLength(Buffer, BufferPos - 1);
              LastString := LastString + Buffer;

              // Move to the first character behind the string.
              IncI;
            end;

            while Code[i] = '#' do // Special character
            begin
              FoundStringParticle := True;
              j := Succ(i);
              // Scan for numerals
              repeat
                IncI;
              until not (Code[i] in ['0'..'9']);
              // If TryStrToInt failed, this means there are none or too many
              // digits following the #. This means the dfm is corrupt. Skip
              // the character and try to move on.
              if TryStrToInt(Copy(Code, j, i - j), CharacterCode) then
                LastString := LastString + Chr(CharacterCode);
            end;
          until not FoundStringParticle;

          // String ended. Search for a +, because this indicates there is
          // another part
          SkipWhiteSpace;

          if (Code[i] <> '+') then // No +? Then break.
          begin

            if (soStringsItemsAsSeparateStrings in SearchOptions) or // Treat all strings items as separate strings
               (LastItem = -1) or // Not in stringlist at all
               (not (Code[i] in ['''', '#'])) // String is not continued. If LastItem > -1 and this is true, the dfm is corrupt!
            then
            begin
              DecI; // It will be incremented after the case.
              Break;
            end
            else
            begin
              // Stringlist is continued
              LastString := LastString + #13#10; // Insert enters between stringlist items
            end;

          end
          else
          begin
            IncI; // Skip the plus
            SkipWhiteSpace; // Skip whitespace after the plus
          end;
        until False;

        // Perform actual search
        Haystack := UpperCase(LastString);
        if soSmartSpace in SearchOptions then
          DeSpace(HayStack);



        if ((soWildcard in SearchOptions) and MatchesMask(Haystack, Search)) or
           ( (not (soWildcard in SearchOptions)) and
             ( ((soStartingWith in SearchOptions) and (Copy(Haystack, 1, Length(Search)) = Search)) or
               (not (soStartingWith in SearchOptions) and (Pos(Search, Haystack) > 0)) ) ) then
        begin
          if (soStringsItemsAsSeparateStrings in SearchOptions) and (LastItem > -1) then
            LastPropertyItem := Format('[%d]', [LastItem]) // Show in which item the text was found
          else
            LastPropertyItem := '';

          if LastCollectionItem > -1 then
            LastCollectionItemStr := Format('[%d]', [LastCollectionItem])
          else
            LastCollectionItemStr := '';

          if CollectionName <> '' then
            PropertyName := CollectionName + LastCollectionItemStr + '.' + LastProperty + LastPropertyItem
          else
            PropertyName := LastProperty + LastPropertyItem;

          CallBack(AFileName, FormClass, LastObjectName, PropertyName, LastString + #13#10, LastPropertyLine);
        end;

        if Assigned(FindSQLDump) then
        begin
          // Present a FindDFM 1.0-like result of these properties:
          if SameText(LastProperty, 'CommandText') or
             SameText(LastProperty, 'CommandText.Strings') or
             SameText(LastProperty, 'ProcedureName') or
             SameText(LastProperty, 'SearchQuery.Strings') or
             SameText(LastProperty, 'CountQuery.Strings')
            then
          begin
            // Print the name of this object, if not already
            if not ObjectNamePrinted then
            begin
              TempString := '| ' + FormClass + '.' + LastObjectName + ' |';
              InfoString := '+' + StringOfChar('-', Length(TempString) - 2) + '+';
              InfoString := InfoString + #13#10 + TempString + #13#10 + InfoString;
              FindSQLDump.Add(InfoString);
              ObjectNamePrinted := True; // Remember we printed it
            end;
            if not PropertyPrinted then
            begin
              // Add some information to some properties
              if SameText(LastProperty, 'SearchQuery.Strings') then
                InfoString := 'Search query:'#13#10 + LastString
              else if SameText(LastProperty, 'CountQuery.Strings') then
                InfoString := 'Count query:'#13#10 + LastString
              else if SameText(LastProperty, 'ProcedureName') then
                InfoString := 'execute ' + LastString
              else
                InfoString := LastString;

              FindSQLDump.Add(InfoString); // Property value
              PropertyPrinted := True;
            end;
          end;
        end;

        if LastItem > -1 then
          Inc(LastItem);
        if LastCollectionItem > -1 then
          Inc(LastCollectionItem);
      end; // string case
    else
      // No recognized character. Probably an operator. Ignore
    end;

    IncI;
  end;
end;

end.
