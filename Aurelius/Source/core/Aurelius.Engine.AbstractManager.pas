unit Aurelius.Engine.AbstractManager;

{$I Aurelius.inc}

interface

uses
  Aurelius.Commands.CommandPerformerFactory,
  Aurelius.Commands.Listeners,
  Aurelius.Drivers.Interfaces,
  Aurelius.Mapping.Explorer,
  Aurelius.Sql.Interfaces;

type
  TAbstractManager = class abstract
  strict private
    FSQLGenerator: ISQLGenerator;
    FExplorer: TMappingExplorer;
  protected
    FConnection: IDBConnection;
    FCommandFactory: TCommandPerformerFactory;
    property SQLGenerator: ISQLGenerator read FSQLGenerator;
    property Explorer: TMappingExplorer read FExplorer;
  public
    constructor Create(Connection: IDBConnection; AExplorer: TMappingExplorer); overload; virtual;
    constructor Create(Connection: IDBConnection); overload;
    destructor Destroy; override;

    procedure AddCommandListener(Listener: ICommandExecutionListener);

    property Connection: IDBConnection read FConnection;
  end;

implementation

uses
  Aurelius.Sql.Register;

{ TAbstractManager }

procedure TAbstractManager.AddCommandListener(
  Listener: ICommandExecutionListener);
begin
  FCommandFactory.AddExecutionListener(Listener);
end;

constructor TAbstractManager.Create(Connection: IDBConnection; AExplorer: TMappingExplorer);
begin
  FConnection := Connection;
  FExplorer := AExplorer;
  if FExplorer  = nil then
    FExplorer := TMappingExplorer.DefaultInstance;
  FSQLGenerator := TSQLGeneratorRegister.GetInstance.GetGenerator(Connection.SqlDialect);
  FCommandFactory := TCommandPerformerFactory.Create(FConnection, FSQLGenerator, Self, FExplorer);
end;

constructor TAbstractManager.Create(Connection: IDBConnection);
begin
  Create(Connection, nil);
end;

destructor TAbstractManager.Destroy;
begin
  FCommandFactory.Free;
  inherited;
end;

end.
