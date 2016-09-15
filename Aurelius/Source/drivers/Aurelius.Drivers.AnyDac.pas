unit Aurelius.Drivers.AnyDac;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  uADCompClient, uADStanIntf, uADStanParam,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TAnyDacResultSetAdapter = class(TDriverResultSetAdapter<TADQuery>)
  end;

  TAnyDacStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FADQuery: TADQuery;
    function GetDataset: TDataset;
  public
    constructor Create(AADQuery: TADQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TAnyDacConnectionAdapter = class(TDriverConnectionAdapter<TADConnection>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TAnyDacTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FADConnection: TADConnection;
  public
    constructor Create(ADConnection: TADConnection);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TAnyDacStatementAdapter }

uses
  Aurelius.Drivers.Exceptions;

constructor TAnyDacStatementAdapter.Create(AADQuery: TADQuery);
begin
  FADQuery := AADQuery;
end;

destructor TAnyDacStatementAdapter.Destroy;
begin
  FADQuery.Free;
  inherited;
end;

procedure TAnyDacStatementAdapter.Execute;
begin
  FADQuery.ExecSQL;
end;

function TAnyDacStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TADQuery;
  I: Integer;
begin
  ResultSet := TADQuery.Create(nil);
  try
    ResultSet.Connection := FADQuery.Connection;
    ResultSet.SQL := FADQuery.SQL;

    for I := 0 to FADQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FADQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FADQuery.Params[I].Value;
    end;

    ResultSet.OpenOrExecute;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TAnyDacResultSetAdapter.Create(ResultSet);
end;

function TAnyDacStatementAdapter.GetDataset: TDataset;
begin
  Result := FADQuery;
end;

procedure TAnyDacStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TADParam;
begin
  for P in Params do
  begin
    Parameter := FADQuery.ParamByName(P.ParamName);

    Parameter.DataType := P.ParamType;
    Parameter.Value := P.ParamValue;
  end;
end;

procedure TAnyDacStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FADQuery.SQL.Text := SQLCommand;
end;

{ TAnyDacConnectionAdapter }

procedure TAnyDacConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TAnyDacConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection = nil then
    Exit('');

  case Connection.RDBMSKind of
    mkMSSQL:
      result := 'MSSQL';
    mkMySQL:
      result := 'MySQL';
    mkInterbase:
      result := 'Firebird';
    mkOracle:
      result := 'Oracle';
    mkSQLite:
      result := 'SQLite';
    mkPostgreSQL:
      result := 'PostgreSQL';
    mkDB2:
      result := 'DB2';
    mkADS:
      result := 'ADVANTAGE';
  else
//    mkUnknown: ;
//    mkOracle: ;
//    mkMSAccess: ;
//    mkDB2: ;
//    mkASA: ;
//    mkADS: ;
//    mkSQLite: ;
//    mkPostgreSQL: ;
//    mkNexus: ;
//    mkOther: ;
    result := 'NOT_SUPPORTED';
  end;
end;

function TAnyDacConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TAnyDacConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TADQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TADQuery.Create(nil);
  try
    Statement.Connection := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TAnyDacStatementAdapter.Create(Statement);
end;

procedure TAnyDacConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TAnyDacConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
    Result := TAnyDacTransactionAdapter.Create(Connection);
  end else
    Result := TAnyDacTransactionAdapter.Create(nil);
end;

{ TAnyDacTransactionAdapter }

procedure TAnyDacTransactionAdapter.Commit;
begin
  if (FADConnection = nil) then
    Exit;

  FADConnection.Commit;
end;

constructor TAnyDacTransactionAdapter.Create(ADConnection: TADConnection);
begin
  FADConnection := ADConnection;
end;

procedure TAnyDacTransactionAdapter.Rollback;
begin
  if (FADConnection = nil) then
    Exit;

  FADConnection.Rollback;
end;

end.
