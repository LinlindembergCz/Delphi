unit Aurelius.Drivers.SQLDirect;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections, SDEngine,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TSQLDirectResultSetAdapter = class(TDriverResultSetAdapter<TSDQuery>)
  public
    function GetFieldValue(FieldIndex: Integer): Variant; overload; override;
  end;

  TSQLDirectStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FSDQuery: TSDQuery;
    function GetDataset: TDataset;
  public
    constructor Create(ASDQuery: TSDQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TSQLDirectConnectionAdapter = class(TDriverConnectionAdapter<TSDDatabase>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TSQLDirectTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    //FDBXTransaction: TDBXTransaction;
    FSDDatabase: TSDDatabase;
  public
    constructor Create(SDDatabase: TSDDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TSQLDirectStatementAdapter }

uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TSQLDirectStatementAdapter.Create(ASDQuery: TSDQuery);
begin
  FSDQuery := ASDQuery;
end;

destructor TSQLDirectStatementAdapter.Destroy;
begin
  FSDQuery.Free;
  inherited;
end;

procedure TSQLDirectStatementAdapter.Execute;
begin
  FSDQuery.ExecSQL;
end;

function TSQLDirectStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TSDQuery;
  I: Integer;
begin
  ResultSet := TSDQuery.Create(nil);
  try
    ResultSet.DatabaseName := FSDQuery.DatabaseName;
    ResultSet.SQL := FSDQuery.SQL;

    for I := 0 to FSDQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FSDQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FSDQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TSqlDirectResultSetAdapter.Create(ResultSet);
end;

function TSQLDirectStatementAdapter.GetDataset: TDataset;
begin
  Result := FSDQuery;
end;

procedure TSQLDirectStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FSDQuery.ParamByName(P.ParamName);

    if P.ParamType in [ftBlob, ftOraBlob, ftOraClob] then
    begin
      // Specific SQL-Direct workaround for blob fields. If param type is ftBlob, then we must set the
      // blob content as string, because it's the only way it works fine for all databases
      Parameter.DataType := P.ParamType;
      Bytes := TUtils.VariantToBytes(P.ParamValue);
      if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
        Parameter.Clear
      else
        Parameter.AsBlob := Bytes;
    end else
    begin
      Parameter.DataType := P.ParamType;
      Parameter.Value := P.ParamValue;
    end;
  end;
end;

procedure TSQLDirectStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FSDQuery.SQL.Text := SQLCommand;
end;

{ TSQLDirectConnectionAdapter }

procedure TSQLDirectConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TSQLDirectConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection = nil then
    Exit('');

  case Connection.ServerType of
    stSQLServer,
    stOLEDB:
      result := 'MSSQL';
    stMySQL:
      result := 'MySQL';
    stFirebird:
      result := 'Firebird';
    stInterbase:
      result := 'Interbase';
    stOracle:
      result := 'Oracle';
    stPostgreSQL:
      result := 'PostgreSQL';
    stDB2:
      result := 'DB2';
  else
//    stSQLBase:
//    stOracle:
//    stSybase:
//    stDB2:
//    stInformix:
//    stODBC:
//    stInterbase:
//    stPostgreSQL:
    result := 'NOT_SUPPORTED';
  end;
end;

function TSQLDirectConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TSQLDirectConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TSDQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TSDQuery.Create(nil);
  try
    Statement.DatabaseName := Connection.DatabaseName;
  except
    Statement.Free;
    raise;
  end;
  Result := TSQLDirectStatementAdapter.Create(Statement);
end;

procedure TSQLDirectConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TSQLDirectConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
    Result := TSQLDirectTransactionAdapter.Create(Connection);
  end else
    Result := TSQLDirectTransactionAdapter.Create(nil);
end;

{ TSQLDirectTransactionAdapter }

procedure TSQLDirectTransactionAdapter.Commit;
begin
  if (FSDDatabase = nil) then
    Exit;

  FSDDatabase.Commit;
end;

constructor TSQLDirectTransactionAdapter.Create(SDDatabase: TSDDatabase);
begin
  FSDDatabase := SDDatabase;
end;

procedure TSQLDirectTransactionAdapter.Rollback;
begin
  if (FSDDatabase = nil) then
    Exit;

  FSDDatabase.Rollback;
end;

{ TSQLDirectResultSetAdapter }

function TSQLDirectResultSetAdapter.GetFieldValue(FieldIndex: Integer): Variant;
var
  S: string;
begin
  Result := inherited GetFieldValue(FieldIndex);
  case VarType(Result) of
    varUString, varOleStr:
      begin
        S := VarToStr(Result);
        Result := VarAsType(S, varString);
      end;
  end;
end;

end.
