unit Aurelius.Drivers.ElevateDB;

{$I Aurelius.Inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  edbcomps, edbtype,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TElevateDBResultSetAdapter = class(TDriverResultSetAdapter<TEDBQuery>)
  end;

  TElevateDBStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FEDBQuery: TEDBQuery;
    function GetDataset: TDataset;
  public
    constructor Create(AEDBQuery: TEDBQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TElevateDBConnectionAdapter = class(TDriverConnectionAdapter<TEDBDatabase>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TElevateDBTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FEDBDatabase: TEDBDatabase;
  public
    constructor Create(EDBDatabase: TEDBDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TElevateDBStatementAdapter }

uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TElevateDBStatementAdapter.Create(AEDBQuery: TEDBQuery);
begin
  FEDBQuery := AEDBQuery;
end;

destructor TElevateDBStatementAdapter.Destroy;
begin
  FEDBQuery.Free;
  inherited;
end;

procedure TElevateDBStatementAdapter.Execute;
begin
  FEDBQuery.ExecSQL;
end;

function TElevateDBStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TEDBQuery;
  I: Integer;
begin
  ResultSet := TEDBQuery.Create(nil);
  try
    ResultSet.SessionName := FEDBQuery.SessionName;
    ResultSet.DatabaseName := FEDBQuery.DatabaseName;
    ResultSet.SQL := FEDBQuery.SQL;

    for I := 0 to FEDBQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FEDBQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FEDBQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TElevateDBResultSetAdapter.Create(ResultSet);
end;

function TElevateDBStatementAdapter.GetDataset: TDataset;
begin
  Result := FEDBQuery;
end;

procedure TElevateDBStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FEDBQuery.ParamByName(TEDBString(P.ParamName));

    Parameter.DataType := P.ParamType;

    if P.ParamType in [ftBlob] then
    begin
      Bytes := TUtils.VariantToBytes(P.ParamValue);
      if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
        Parameter.Clear
      else
      begin
        Parameter.DataType := P.ParamType;
        Parameter.AsBlob := Bytes;
      end;
    end
    else
    if P.ParamType in [ftMemo, ftWideMemo] then
    begin
      if VarIsNull(P.ParamValue) or (Length(VarToStr(P.ParamValue)) = 0) then
        Parameter.AsMemo := ''
      else
      begin
        Parameter.AsMemo := P.ParamValue;
      end;
    end
    else
      Parameter.Value := P.ParamValue;
  end;
end;

procedure TElevateDBStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FEDBQuery.SQL.Text := SQLCommand;
end;

{ TElevateDBConnectionAdapter }

procedure TElevateDBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TElevateDBConnectionAdapter.RetrieveSqlDialect: string;
begin
  result := 'ELEVATEDB';
end;

function TElevateDBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TElevateDBConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TEDBQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TEDBQuery.Create(nil);
  try
    Statement.SessionName := Connection.SessionName;
    Statement.DatabaseName := Connection.DatabaseName;
  except
    Statement.Free;
    raise;
  end;
  Result := TElevateDBStatementAdapter.Create(Statement);
end;

procedure TElevateDBConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TElevateDBConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction(EmptyEDBStringsArray);
    Result := TElevateDBTransactionAdapter.Create(Connection);
  end else
    Result := TElevateDBTransactionAdapter.Create(nil);
end;

{ TElevateDBTransactionAdapter }

procedure TElevateDBTransactionAdapter.Commit;
begin
  if (FEDBDatabase = nil) then
    Exit;

  FEDBDatabase.Commit;
end;

constructor TElevateDBTransactionAdapter.Create(EDBDatabase: TEDBDatabase);
begin
  FEDBDatabase := EDBDatabase;
end;

procedure TElevateDBTransactionAdapter.Rollback;
begin
  if (FEDBDatabase = nil) then
    Exit;

  FEDBDatabase.Rollback;
end;

end.
