
unit SqlMonitor;

interface

uses
  Generics.Collections, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  Aurelius.Drivers.Interfaces,
  Aurelius.Commands.Listeners;

type
  TFrmSqlMonitor = class(TForm, ICommandExecutionListener)
    Memo: TMemo;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btClear: TButton;
    btExit: TButton;
    procedure btClearClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    class var
      FInstance: TFrmSqlMonitor;
    procedure ExecutingCommand(SQL: string; Params: TEnumerable<TDBParam>);
  public
    class function GetInstance: TFrmSqlMonitor;
  end;

implementation
uses
  DBConnection;

{$R *.dfm}

procedure TFrmSqlMonitor.btClearClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TFrmSqlMonitor.btExitClick(Sender: TObject);
begin
  Hide;
end;

procedure TFrmSqlMonitor.ExecutingCommand(SQL: string;
  Params: TEnumerable<TDBParam>);
begin
  TDBConnection.AddLines(Memo.Lines, SQL, Params);
  Application.ProcessMessages;
end;

class function TFrmSqlMonitor.GetInstance: TFrmSqlMonitor;
begin
  if FInstance = nil then
    FInstance := TFrmSqlMonitor.Create(Application);
  Result := FInstance;
end;

end.
