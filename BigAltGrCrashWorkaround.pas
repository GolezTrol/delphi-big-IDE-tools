unit BigAltGrCrashWorkaround;

interface

implementation

uses
  BigProcHook, Menus;

var
  FHook: TBigProcHook;

// The replacement function
function SafeIsAltGrPressed: Boolean;
begin
  try
    FHook.Hooked := False;
    try
      Result := IsAltGRPressed;
    finally
      FHook.Hooked := True;
    end;
  except
    // Exception: Error when reading keyboard layout dll.
    Result := False;
  end;
end;

initialization
  FHook := TBigProcHook.Create(@IsAltGRPressed, @SafeIsAltGrPressed);
finalization
  FHook.Hooked := False;
  FHook.Free;
end.
