{******************************************************************************

GolezTrol Big Visual Component Library
Copyright (c) 2006-2008 Jos Visser

*******************************************************************************
The contents of this file are distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied.

The Original Code is released Jan 09, 2009.
*******************************************************************************
 TBigProcHook allows to override a given function with another by inserting a
 JMP to the new function. In the new function you can disable the hook to call
 the old function again.
******************************************************************************}

unit BigProcHook;

interface

uses
  Windows, sysUtils;

type
  PHack = ^THook;
  THook = packed record
    OpCodeCall : Byte;
    OFFTo      : Integer;
    OpCodeRet  : Byte;
  end;
  TBackup = THook;

  TBigProcHook = class
  private
    FOldProc, FNewProc: Pointer;
    FBackupped: Boolean;
    FHooked: Boolean;
    FOriginal: TBackup;
    procedure SetHooked(const Value: Boolean);
  protected
    procedure InstallHook(Hook: THook);
    procedure OverwriteProc;
  public
    constructor Create(AOldProc, ANewProc: Pointer; Install: Boolean = True);
    property Hooked: Boolean read FHooked write SetHooked;
  end;

implementation

{ TBigProcHook }

constructor TBigProcHook.Create(AOldProc, ANewProc: Pointer;
  Install: Boolean);
begin
  inherited Create;

  FOldProc := AOldProc;
  FNewProc := ANewProc;

  if Install then
    SetHooked(True);
end;

procedure TBigProcHook.InstallHook(Hook: THook);
var
  OldProtect: Cardinal;
begin
  // Change protection of oldproc memory
  if VirtualProtect(FOldProc, SizeOf(THook), PAGE_EXECUTE_READWRITE, OldProtect) then
  try
    if not FBackupped then
    begin
      Move(FOldProc^, FOriginal, SizeOf(THook));
      FBackupped := True;
    end;
    // Overwrite the old procedure
    Move(Hook, FOldProc^, SizeOf(THook));
  finally
    VirtualProtect(FOldProc, SizeOf(THook), OldProtect, OldProtect);
  end
  else
  begin
    RaiseLastOSError;
  end;
end;

procedure TBigProcHook.OverwriteProc;
// Overwrites the first few calls of OldProc with a call to NewProc and a Ret.
var
  Hook: THook;
begin
  // Create a tiny little redirection
  with Hook do begin
    OpCodeCall := $E8; // = CALL}
    OFFTo      := PAnsiChar(FNewProc) - PAnsiChar(FOldProc) - 5;
    OpCodeRet  := $C3; // = RET
  end;

  InstallHook(Hook);
end;

procedure TBigProcHook.SetHooked(const Value: Boolean);
begin
  // Toggle hook.
  if FHooked <> Value then
    if Value then
      OverwriteProc
    else
      InstallHook(FOriginal);

  FHooked := Value;
end;

initialization
end.
 