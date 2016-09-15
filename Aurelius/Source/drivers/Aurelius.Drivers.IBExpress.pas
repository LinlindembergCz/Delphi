unit Aurelius.Drivers.IBExpress;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  IBQuery, IBDatabase,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TIBExpressResultSetAdapter = class(TDriverResultSetAdapter<TIBQuery>)
  end;

  TIBExpressStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FIBQuery: TIBQuery;
  public
    constructor Create(AIBQuery: TIBQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TIBExpressConnectionAdapter = class(TDriverConnectionAdapter<TIBDatabase>, IDBConnection)
  private
    FDefaultTransaction: TIBTransaction;
  public
    constructor Create(AConnection: TIBDatabase; ASQLDialect: string; AOwnsConnection: boolean); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TIBExpressTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FIBTransaction: TIBTransaction;
    FIBDatabase: TIBDatabase;
  public
    constructor Create(IBTransaction: TIBTransaction; IBDatabase: TIBDatabase);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
  end;

implementation
uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

{ TIBExpressStatementAdapter }

constructor TIBExpressStatementAdapter.Create(AIBQuery: TIBQuery);
begin
  FIBQuery := AIBQuery;
end;

destructor TIBExpressStatementAdapter.Destroy;
begin
  FIBQuery.Free;
  inherited;
end;

procedure TIBExpressStatementAdapter.Execute;
begin
  if not FIBQuery.Database.Connected then
    FIBQuery.Database.Connected := true;

  if FIBQuery.Transaction.InTransaction then
    FIBQuery.ExecSQL
  else
  begin
    FIBQuery.Transaction.StartTransaction;
    try
      FIBQuery.ExecSQL;
      FIBQuery.Transaction.Commit;
    except
      FIBQuery.Transaction.Rollback;
      raise;
    end;
  end;
end;

function TIBExpressStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TIBQuery;
  I: Integer;
begin
  ResultSet := TIBQuery.Create(nil);
  try
    ResultSet.Database := FIBQuery.Database;
    ResultSet.SQL.Text := FIBQuery.SQL.Text;

    for I := 0 to FIBQuery.Params.Count - 1 do
    begin
      ResultSet.Params[I].DataType := FIBQuery.Params[I].DataType;
      ResultSet.Params[I].Value := FIBQuery.Params[I].Value;
    end;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TIBExpressResultSetAdapter.Create(ResultSet);
end;

procedure TIBExpressStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
  Bytes: TBytes;
begin
  for P in Params do
  begin
    Parameter := FIBQuery.ParamByName(P.ParamName);

    if P.ParamType = ftBlob then
    begin
      Bytes := TUtils.VariantToBytes(P.ParamValue);
      Parameter.DataType := P.ParamType;
      if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
        Parameter.Clear
      else
        Parameter.AsBlob := Bytes;
    end
    else
    begin
      Parameter.DataType := P.ParamType;
      Parameter.Value := P.ParamValue;
    end;

    // Workaroudn for some unsupported data types
    if Parameter.DataType = ftWideMemo then
      Parameter.AsMemo := Parameter.Value;
  end;
end;

procedure TIBExpressStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FIBQuery.SQL.Text := SQLCommand;
end;

{ TIBExpressConnectionAdapter }

destructor TIBExpressConnectionAdapter.Destroy;
begin
  FDefaultTransaction.Free;
  inherited;
end;

procedure TIBExpressConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TIBExpressConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection <> nil then
    Result := 'Interbase'
  else
    Result := '';
end;

function TIBExpressConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

constructor TIBExpressConnectionAdapter.Create(AConnection: TIBDatabase;
  ASQLDialect: string; AOwnsConnection: boolean);
begin
  inherited;
  FDefaultTransaction := TIBTransaction.Create(nil);
  FDefaultTransaction.DefaultDatabase := AConnection;
  FDefaultTransaction.AutoStopAction := saCommit;
end;

function TIBExpressConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TIBQuery;
begin
  if Connection <> nil then
  begin
    Statement := TIBQuery.Create(nil);
    try
      Statement.Database := Connection;
      Statement.Transaction := FDefaultTransaction;
    except
      Statement.Free;
      raise;
    end;
    Result := TIBExpressStatementAdapter.Create(Statement);
  end else
    Result := nil;
end;

procedure TIBExpressConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TIBExpressConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  // We must open the connection here, otherwise the BeginTransaction will not work
  // This is because BeginTransaction checks if database supports transaction, and it only
  // does that after a connection. It appears to be a bug in IBExpress
  Connection.Connected := true;

  if not FDefaultTransaction.InTransaction then
  begin
    FDefaultTransaction.StartTransaction;
    Result := TIBExpressTransactionAdapter.Create(FDefaultTransaction, Connection);
  end
  else
    Result := TIBExpressTransactionAdapter.Create(nil, Connection);
end;

{ TIBExpressTransactionAdapter }

procedure TIBExpressTransactionAdapter.Commit;
begin
  if (FIBTransaction = nil) then
    Exit;

  FIBTransaction.Commit;
end;

constructor TIBExpressTransactionAdapter.Create(
  IBTransaction: TIBTransaction; IBDatabase: TIBDatabase);
begin
  FIBTransaction := IBTransaction;
  FIBDatabase := IBDatabase;
end;

destructor TIBExpressTransactionAdapter.Destroy;
begin
  inherited;
end;

procedure TIBExpressTransactionAdapter.Rollback;
begin
  if (FIBTransaction = nil) then
    Exit;

  FIBTransaction.Rollback;
end;

end.
