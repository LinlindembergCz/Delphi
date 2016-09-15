unit Aurelius.Drivers.dbExpress;

{$I Aurelius.inc}

interface

uses
  Classes, SqlExpr, DB, Variants, Generics.Collections, DBXCommon,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TDBExpressResultSetAdapter = class(TDriverResultSetAdapter<TSQLQuery>)
  end;

  TDBExpressStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FSQLQuery: TSQLQuery;
    function GetDataset: TDataset;
  public
    constructor Create(ASQLQuery: TSQLQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TDBExpressConnectionAdapter = class(TDriverConnectionAdapter<TSQLConnection>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TDBExpressTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FDBXTransaction: TDBXTransaction;
    FSQLConnection: TSQLConnection;
  public
    constructor Create(DBXTransaction: TDBXTransaction; SQLConnection: TSQLConnection);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
  end;

implementation
uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

{ TDBExpressStatementAdapter }

constructor TDBExpressStatementAdapter.Create(ASQLQuery: TSQLQuery);
begin
  FSQLQuery := ASQLQuery;
end;

destructor TDBExpressStatementAdapter.Destroy;
begin
  FSQLQuery.Free;
  inherited;
end;

procedure TDBExpressStatementAdapter.Execute;
begin
  FSQLQuery.ExecSQL;
end;

function TDBExpressStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TSQLQuery;
  I: Integer;
begin
  ResultSet := TSQLQuery.Create(nil);
  try
    ResultSet.SQLConnection := FSQLQuery.SQLConnection;
    ResultSet.CommandText := FSQLQuery.CommandText;

    for I := 0 to FSQLQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FSQLQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FSQLQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TDBExpressResultSetAdapter.Create(ResultSet);
end;

function TDBExpressStatementAdapter.GetDataset: TDataset;
begin
  Result := FSQLQuery;
end;

procedure TDBExpressStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FSQLQuery.ParamByName(P.ParamName);

    if P.ParamType in [ftOraBlob, ftOraClob] then
    begin
      // For Oracle blobs, we have an invalid LOB locator if we just set Parameter.Value
      // The we must set it using AsBlob to make it work.
      // Another workaround: is the param is null or empty blob, then neither AsBlob or Clear will work
      // We need to set it to an empty string
      Bytes := TUtils.VariantToBytes(P.ParamValue);
      if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
        Parameter.AsString := ''
      else
      begin
        Parameter.DataType := P.ParamType;
        Parameter.AsBlob := Bytes;
      end;
    end
    else
    if P.ParamType in [ftMemo, ftWideMemo] then
    begin
      // Access Violation when dealing with WideMemo in Interbase and Firebird drivers
      // Feature not supported in interbase
      if VarIsNull(P.ParamValue) or (Length(VarToStr(P.ParamValue)) = 0) then
        Parameter.AsMemo := ''
      else
      begin
//        Parameter.DataType := P.ParamType;
//        Parameter.AsWideString := P.ParamValue;
        Parameter.AsMemo := P.ParamValue;
      end;
    end
    else
    if P.ParamType in [ftGuid] then
    begin
      Parameter.AsString := GuidToString(TUtils.VariantToGuid(P.ParamValue));
    end
    else
    {$IFNDEF DELPHIXE2_LVL}
    if P.ParamType = ftBlob then
    begin
      // For Delphi 2010 we have the same problem as the oracle blob above, so we must workaround
      Bytes := TUtils.VariantToBytes(P.ParamValue);
      Parameter.DataType := P.ParamType;
      if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
        Parameter.Clear
      else
        Parameter.AsBlob := Bytes;
    end
    else
    {$ENDIF}
    begin
      Parameter.DataType := P.ParamType;
      Parameter.Value := P.ParamValue;
    end;

    // Workaround for some drivers that do not support INT64 parameters
    if Parameter.DataType = ftLargeInt then
    begin
      if (SameText(FSQLQuery.SQLConnection.DriverName, 'ORACLE')) or
        (SameText(FSQLQuery.SQLConnection.DriverName, 'DB2')) then
      begin
        Parameter.AsInteger := Integer(Parameter.AsLargeInt);
      end;
    end;
  end;
end;

procedure TDBExpressStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FSQLQuery.CommandText := SQLCommand;
end;

{ TDBExpressConnectionAdapter }

procedure TDBExpressConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TDBExpressConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection <> nil then
    Result := Connection.DriverName
  else
    Result := '';
end;

function TDBExpressConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TDBExpressConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TSQLQuery;
begin
  if Connection <> nil then
  begin
    Statement := TSQLQuery.Create(nil);
    try
      Statement.SQLConnection := Connection;
    except
      Statement.Free;
      raise;
    end;
    Result := TDBExpressStatementAdapter.Create(Statement);
  end else
    Result := nil;
end;

procedure TDBExpressConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TDBExpressConnectionAdapter.BeginTransaction: IDBTransaction;
var
  Trans: TDBXTransaction;
begin
  if Connection = nil then
    Exit(nil);

  // We must open the connection here, otherwise the BeginTransaction will not work
  // This is because BeginTransaction checks if database supports transaction, and it only
  // does that after a connection. It appears to be a bug in dbExpress
  Connection.Connected := true;

  Trans := nil;
  if not Connection.InTransaction then
    Trans := Connection.BeginTransaction;

  Result := TDBExpressTransactionAdapter.Create(Trans, Connection);
end;

{ TDBExpressTransactionAdapter }

procedure TDBExpressTransactionAdapter.Commit;
begin
  if (FDBXTransaction = nil) then
    Exit;

  FSQLConnection.CommitFreeAndNil(FDBXTransaction);
end;

constructor TDBExpressTransactionAdapter.Create(
  DBXTransaction: TDBXTransaction; SQLConnection: TSQLConnection);
begin
  FDBXTransaction := DBXTransaction;
  FSQLConnection := SQLConnection;
end;

destructor TDBExpressTransactionAdapter.Destroy;
begin
  if FDBXTransaction <> nil then
    FDBXTransaction.Free;
  inherited;
end;

procedure TDBExpressTransactionAdapter.Rollback;
begin
  if (FDBXTransaction = nil) then
    Exit;

  FSQLConnection.RollbackFreeAndNil(FDBXTransaction);
end;

end.
