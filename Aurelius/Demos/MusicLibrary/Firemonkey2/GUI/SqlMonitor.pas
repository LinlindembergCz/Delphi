unit SqlMonitor;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  Aurelius.Drivers.Interfaces,
  Aurelius.Commands.Listeners;

type
  TfrmSqlMonitor = class(TForm, ICommandExecutionListener)
    Memo: TMemo;
    PanelBottom: TPanel;
    btClear: TButton;
    btClose: TButton;
    procedure btClearClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
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

{$R *.fmx}

procedure TfrmSqlMonitor.btClearClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TfrmSqlMonitor.btCloseClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TfrmSqlMonitor.ExecutingCommand(SQL: string;
  Params: TEnumerable<TDBParam>);
begin
  TDBConnection.AddLines(Memo.Lines, SQL, Params);
end;

class function TfrmSqlMonitor.GetInstance: TFrmSqlMonitor;
begin
  if FInstance = nil then
    FInstance := TFrmSqlMonitor.Create(Application);
  Result := FInstance;
end;

end.
