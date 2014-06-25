unit fTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Found(AFormClass, AObjectName, APropertyName, AValue: string; ALine: Integer);
    procedure Found2(AFileName, AFormClass, AObjectName, APropertyName, AValue: string; ALine: Integer);
  end;

var
  Form1: TForm1;

implementation

uses uBigFastSearchDFM;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Edit1.Text);
    ScanDFM(sl.Text, Edit2.Text, Found2, []);
  finally
    sl.Free;
  end;
end;

procedure TForm1.Found(AFormClass, AObjectName, APropertyName, AValue: string;
  ALine: Integer);
begin
  Memo1.Lines.Add(Format('%d, %s', [ALine, AFormClass + '.' + AObjectName + '.' + APropertyName + '=' + AValue]));
  Application.ProcessMessages;
end;

procedure TForm1.Found2(AFileName, AFormClass, AObjectName, APropertyName,
  AValue: string; ALine: Integer);
begin
  Memo1.Lines.Add(Format('%s: %d, %s', [AFileName, ALine, AFormClass + '.' + AObjectName + '.' + APropertyName + '=' + AValue]));
  Application.ProcessMessages;
end;

end.
