unit Aurelius.Drivers.RemoteDB;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  XData.XDataset,
  XData.XDataset.RemoteDB,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TXDatasetResultSetAdapter = class(TDriverResultSetAdapter<TXDataset>)
  public
    function GetFieldValue(FieldIndex: Integer): Variant; overload; override;
  end;

  TXDatasetStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FQuery: TXDataset;
  public
    constructor Create(AQuery: TXDataset);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TRemoteDBConnectionAdapter = class(TDriverConnectionAdapter<TRemoteDBDatabase>, IDBConnection)
  protected
    function RetrieveSqlDialect: string; override;
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
  end;

  TXDatasetTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FDatabase: TXDatabase;
  public
    constructor Create(ADatabase: TXDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TXDatasetStatementAdapter }

uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TXDatasetStatementAdapter.Create(AQuery: TXDataset);
begin
  FQuery := AQuery;
end;

destructor TXDatasetStatementAdapter.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TXDatasetStatementAdapter.Execute;
begin
  TXDatabase(FQuery.Database).ExecSQL(FQuery.SQL.Text, FQuery.Params);
end;

function TXDatasetStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TXDataset;
  I: Integer;
begin
  ResultSet := TXDataset.Create(nil);
  try
    ResultSet.Database := FQuery.Database;
    ResultSet.SQL := FQuery.SQL;

    for I := 0 to FQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TXDatasetResultSetAdapter.Create(ResultSet);
end;

procedure TXDatasetStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FQuery.Params.ParamByName(P.ParamName);

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

procedure TXDatasetStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FQuery.SQL.Text := SQLCommand;
end;

{ TXDatasetConnectionAdapter }

procedure TRemoteDBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TRemoteDBConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection = nil then
    Exit('');
  Result := Connection.SqlDialect;
end;

function TRemoteDBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TRemoteDBConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TXDataset;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TXDataset.Create(nil);
  try
    Statement.Database := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TXDatasetStatementAdapter.Create(Statement);
end;

procedure TRemoteDBConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TRemoteDBConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.BeginTransaction;
    Result := TXDatasetTransactionAdapter.Create(Connection);
  end else
    Result := TXDatasetTransactionAdapter.Create(nil);
end;

{ TXDatasetTransactionAdapter }

procedure TXDatasetTransactionAdapter.Commit;
begin
  if (FDatabase = nil) then
    Exit;

  FDatabase.Commit;
end;

constructor TXDatasetTransactionAdapter.Create(ADatabase: TXDatabase);
begin
  FDatabase := ADatabase;
end;

procedure TXDatasetTransactionAdapter.Rollback;
begin
  if (FDatabase = nil) then
    Exit;

  FDatabase.Rollback;
end;

{ TXDatasetResultSetAdapter }

function TXDatasetResultSetAdapter.GetFieldValue(FieldIndex: Integer): Variant;
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

