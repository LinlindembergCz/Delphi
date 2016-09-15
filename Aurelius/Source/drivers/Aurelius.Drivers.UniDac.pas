unit Aurelius.Drivers.UniDac;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  DBAccess,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TUniDacResultSetAdapter = class(TDriverResultSetAdapter<TCustomDADataset>)
  end;

  TUniDacStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FQuery: TCustomDADataset;
    function GetDataset: TDataset;
  public
    constructor Create(AQuery: TCustomDADataset);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TUniDacConnectionAdapter = class(TDriverConnectionAdapter<TCustomDAConnection>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TUniDacTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FConnection: TCustomDAConnection;
  public
    constructor Create(AConnection: TCustomDAConnection);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TUniDacStatementAdapter }

uses
  SysUtils,
  Uni,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TUniDacStatementAdapter.Create(AQuery: TCustomDADataset);
begin
  FQuery := AQuery;
end;

destructor TUniDacStatementAdapter.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TUniDacStatementAdapter.Execute;
begin
  FQuery.ExecSQL;
end;

function TUniDacStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TCustomDADataset;
  I: Integer;
begin
  ResultSet := FQuery.Connection.CreateDataSet;
  try
    ResultSet.Connection := FQuery.Connection;
    ResultSet.SQL := FQuery.SQL;
    for I := 0 to FQuery.Params.Count - 1 do
      ResultSet.Params[I].Assign(FQuery.Params[I]);

    ResultSet.Execute;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TUniDacResultSetAdapter.Create(ResultSet);
end;

function TUniDacStatementAdapter.GetDataset: TDataset;
begin
  Result := FQuery;
end;

procedure TUniDacStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TDAParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FQuery.ParamByName(P.ParamName);
    Parameter.DataType := P.ParamType;
    Parameter.ParamType := ptInput;

    if P.ParamType in [ftOraBlob, ftOraClob, ftBlob] then
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

procedure TUniDacStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FQuery.SQL.Text := SQLCommand;
end;

{ TUniDacConnectionAdapter }

procedure TUniDacConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TUniDacConnectionAdapter.RetrieveSqlDialect: string;
var
  Provider: string;
begin
  if Connection = nil then
    Exit('');

  if Connection is TUniConnection then
  begin
    Provider := Uppercase(TUniConnection(Connection).ProviderName);
    if Provider = 'INTERBASE' then
      Result := 'Interbase'
    else
    if Provider = 'SQL SERVER' then
      Result := 'MSSQL'
    else
    if Provider = 'DB2' then
      Result := 'DB2'
    else
    if Provider = 'MYSQL' then
      Result := 'MySQL'
    else
    if Provider = 'ORACLE' then
      Result := 'Oracle'
    else
    if Provider = 'SQLITE' then
      Result := 'SQLite'
    else
    if Provider = 'POSTGRESQL' then
      Result := 'PostgreSQL'
    else
    if Provider = 'NEXUSDB' then
      REsult := 'NexusDb'
    else
      Result := 'NOT_SUPPORTED';
  end;
end;

function TUniDacConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TUniDacConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TCustomDADataset;
begin
  if Connection = nil then
    Exit(nil);

  Statement := Connection.CreateDataSet;
  try
    Statement.Connection := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TUniDacStatementAdapter.Create(Statement);
end;

procedure TUniDacConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TUniDacConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
    Result := TUniDacTransactionAdapter.Create(Connection);
  end else
    Result := TUniDacTransactionAdapter.Create(nil);
end;

{ TUniDacTransactionAdapter }

procedure TUniDacTransactionAdapter.Commit;
begin
  if (FConnection = nil) then
    Exit;

  FConnection.Commit;
end;

constructor TUniDacTransactionAdapter.Create(AConnection: TCustomDAConnection);
begin
  FConnection := AConnection;
end;

procedure TUniDacTransactionAdapter.Rollback;
begin
  if (FConnection = nil) then
    Exit;

  FConnection.Rollback;
end;

end.
