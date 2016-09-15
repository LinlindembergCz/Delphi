program AureliusGettingStarted;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  DBConnection in '..\Common\DBConnection.pas',
  ConnectionDialog in '..\Common\VCL\ConnectionDialog.pas' {frmConnectionDialog},
  Entities in 'Entities.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
