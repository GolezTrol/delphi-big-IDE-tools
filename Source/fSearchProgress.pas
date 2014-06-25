 unit fSearchProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TFrmSearchProgress = class(TForm)
    aniSearch: TAnimate;
    lblFileName: TLabel;                                
    lblSearching: TLabel;
    lblFound: TLabel;
    tmrShowForm: TTimer;
    tmrUpdate: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrShowFormTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    FInvalidated: Boolean;
    FCanUpdate: Boolean;
    FOnCancel: TNotifyEvent;
  public
    procedure SlowlyInvalidate; virtual;
    procedure SetFileName(AFileName: string); virtual;
    procedure SetFound(AFound: Integer); virtual;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

{$R *.dfm}

procedure TFrmSearchProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  aniSearch.Active := False;
  tmrUpdate.Enabled := False;

  Action := caHide;

  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TFrmSearchProgress.SetFileName(AFileName: string);
begin
  lblFileName.Caption := ExtractFileName(AFileName);

  SlowlyInvalidate;
end;

procedure TFrmSearchProgress.FormShow(Sender: TObject);
begin
  aniSearch.Active := True;
end;

procedure TFrmSearchProgress.SetFound(AFound: Integer);
begin
  lblFound.Caption := 'Found: ' + IntToStr(AFound);

  SlowlyInvalidate;
end;

procedure TFrmSearchProgress.tmrShowFormTimer(Sender: TObject);
begin
  // For some reason, these progress forms are only show after a certain
  // amount of time...
  Show;
  tmrShowForm.Enabled := False;
  tmrUpdate.Interval := 500;
  tmrUpdate.Enabled := True;
end;

procedure TFrmSearchProgress.FormCreate(Sender: TObject);
begin
  tmrShowForm.Interval := 800;
  tmrShowForm.Enabled := True;
  lblSearching.Font.Style := [fsBold];
end;

procedure TFrmSearchProgress.SlowlyInvalidate;
begin
  FInvalidated := True;
  if FCanUpdate then
  begin
    Repaint;
    FCanUpdate := False;
    FInvalidated := False;
  end;
end;

procedure TFrmSearchProgress.tmrUpdateTimer(Sender: TObject);
begin
  if FInvalidated then
  begin
    Repaint;
    FInvalidated := False;
  end
  else
    FCanUpdate := True
end;

end.
