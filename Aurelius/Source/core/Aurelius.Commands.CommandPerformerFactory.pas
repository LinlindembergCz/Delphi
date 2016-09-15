unit Aurelius.Commands.CommandPerformerFactory;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Commands.Listeners,
  Aurelius.Drivers.Interfaces,
  Aurelius.Mapping.Explorer,
  Aurelius.Sql.Interfaces;

type
  TCommandPerformerFactory = class
  private
    FConnection: IDBConnection;
    FSQLGenerator: ISQLGenerator;
    FExecutionListeners: TList<ICommandExecutionListener>;
    FEntityManager: TObject;
    FExplorer: TMappingExplorer;

  public
    constructor Create(Conn: IDBConnection; SQLGenerator: ISQLGenerator; EntityManager: TObject;
      Explorer: TMappingExplorer); virtual;
    destructor Destroy; override;

    procedure AddExecutionListener(Listener: ICommandExecutionListener);

    function GetCommand<T: constructor, TAbstractSQLPerformer>(Clazz: TClass): T;
  end;

implementation
uses
  SysUtils;

{ TCommandPerformerFactory }

procedure TCommandPerformerFactory.AddExecutionListener(
  Listener: ICommandExecutionListener);
begin
  FExecutionListeners.Add(Listener);
end;

constructor TCommandPerformerFactory.Create(Conn: IDBConnection;
  SQLGenerator: ISQLGenerator; EntityManager: TObject; Explorer: TMappingExplorer);
begin
  FConnection := Conn;
  FSQLGenerator := SQLGenerator;
  FEntityManager := EntityManager;
  FExplorer := Explorer;
  FExecutionListeners := TList<ICommandExecutionListener>.Create;
end;

destructor TCommandPerformerFactory.Destroy;
begin
  FExecutionListeners.Free;
  inherited;
end;

function TCommandPerformerFactory.GetCommand<T>(Clazz: TClass): T;
var
  L: ICommandExecutionListener;
begin
  {$IFDEF TRIAL}
  if Now > EncodeDate(2014, 6, 1) then
    raise Exception.Create('TMS Aurelius trial version expired. Please purchase a license for commercial usage.');
  {$ENDIF}
  Result := T.Create;

  // The order which we set the following injected objects is important
  // TODO: later change this to be passed in the constructor of command.
  Result.SetConnection(FConnection);
  Result.SetSQLGenerator(FSQLGenerator);
  Result.SetExplorer(FExplorer);
  Result.SetClass(Clazz);
  Result.SetEntityManager(FEntityManager);

  for L in FExecutionListeners do
    Result.AddExecutionListener(L);
end;

end.
