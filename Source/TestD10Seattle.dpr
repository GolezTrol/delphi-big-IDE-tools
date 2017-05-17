program TestD10Seattle;

uses
  Forms,
  fTestMain in 'fTestMain.pas' {Form1},
  uBigFastSearchDFM in 'uBigFastSearchDFM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
