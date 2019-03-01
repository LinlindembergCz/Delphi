program Project20;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit27 in 'Unit27.pas' {Form27};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm27, Form27);
  Application.Run;
end.
