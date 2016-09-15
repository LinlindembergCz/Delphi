unit Aurelius.Drivers.NexusDB;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  nxDb,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TNexusDBResultSetAdapter = class(TDriverResultSetAdapter<TnxQuery>)
  end;

  TNexusDBStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FnxQuery: TnxQuery;
    function GetDataset: TDataset;
  public
    constructor Create(AnxQuery: TnxQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TNexusDBConnectionAdapter = class(TDriverConnectionAdapter<TnxDatabase>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TNexusDBTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FnxDatabase: TnxDatabase;
  public
    constructor Create(nxDatabase: TnxDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TNexusDBStatementAdapter }

uses
  Aurelius.Drivers.Exceptions;

constructor TNexusDBStatementAdapter.Create(AnxQuery: TnxQuery);
begin
  FnxQuery := AnxQuery;
end;

destructor TNexusDBStatementAdapter.Destroy;
begin
  FnxQuery.Free;
  inherited;
end;

procedure TNexusDBStatementAdapter.Execute;
begin
  FnxQuery.ExecSQL;
end;

function TNexusDBStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TnxQuery;
  I: Integer;
begin
  ResultSet := TnxQuery.Create(nil);
  try
    ResultSet.Database := FnxQuery.Database;
    ResultSet.SQL := FnxQuery.SQL;

    for I := 0 to FnxQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FnxQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FnxQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TNexusDBResultSetAdapter.Create(ResultSet);
end;

function TNexusDBStatementAdapter.GetDataset: TDataset;
begin
  Result := FnxQuery;
end;

procedure TNexusDBStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
begin
  for P in Params do
  begin
    Parameter := FnxQuery.ParamByName(P.ParamName);

    Parameter.DataType := P.ParamType;
    Parameter.Value := P.ParamValue;
  end;
end;

procedure TNexusDBStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FnxQuery.SQL.Text := SQLCommand;
end;

{ TNexusDBConnectionAdapter }

procedure TNexusDBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TNexusDBConnectionAdapter.RetrieveSqlDialect: string;
begin
  result := 'NexusDB';
end;

function TNexusDBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TNexusDBConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TnxQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TnxQuery.Create(nil);
  try
    Statement.Database := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TNexusDBStatementAdapter.Create(Statement);
end;

procedure TNexusDBConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TNexusDBConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
    Result := TNexusDBTransactionAdapter.Create(Connection);
  end else
    Result := TNexusDBTransactionAdapter.Create(nil);
end;

{ TNexusDBTransactionAdapter }

procedure TNexusDBTransactionAdapter.Commit;
begin
  if (FnxDatabase = nil) then
    Exit;

  FnxDatabase.Commit;
end;

constructor TNexusDBTransactionAdapter.Create(nxDatabase: TnxDatabase);
begin
  FnxDatabase := nxDatabase;
end;

procedure TNexusDBTransactionAdapter.Rollback;
begin
  if (FnxDatabase = nil) then
    Exit;

  FnxDatabase.Rollback;
end;

end.
