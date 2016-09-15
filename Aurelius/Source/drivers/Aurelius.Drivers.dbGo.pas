unit Aurelius.Drivers.dbGo;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections, ADODB,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TDbGoResultSetAdapter = class(TDriverResultSetAdapter<TADOQuery>)
  end;

  TDbGoStatementAdapter = class(TInterfacedObject, IDBStatement, IDBDatasetStatement)
  private
    FADOQuery: TADOQuery;
    function GetDataset: TDataset;
  public
    constructor Create(AADOQuery: TADOQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TDbGoConnectionAdapter = class(TDriverConnectionAdapter<TADOConnection>, IDBConnection)
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TDbGoTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    //FDBXTransaction: TDBXTransaction;
    FADOConnection: TADOConnection;
  public
    constructor Create(ADOConnection: TADOConnection);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TDbGoStatementAdapter }

uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TDbGoStatementAdapter.Create(AADOQuery: TADOQuery);
begin
  FADOQuery := AADOQuery;
end;

destructor TDbGoStatementAdapter.Destroy;
begin
  FADOQuery.Free;
  inherited;
end;

procedure TDbGoStatementAdapter.Execute;
begin
  FADOQuery.ExecSQL;
end;

function TDbGoStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TADOQuery;
begin
  ResultSet := TADOQuery.Create(nil);
  try
    ResultSet.Connection := FADOQuery.Connection;
    ResultSet.SQL := FADOQuery.SQL;
    ResultSet.Parameters.ParseSQL(ResultSet.SQL.Text, True);
    ResultSet.Parameters.AssignValues(FADOQuery.Parameters);
    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TDbGoResultSetAdapter.Create(ResultSet);
end;

function TDbGoStatementAdapter.GetDataset: TDataset;
begin
  Result := FADOQuery;
end;

procedure TDbGoStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParameter;
  BytesStream: TBytesStream;
begin
  FADOQuery.Parameters.ParseSQL(FADOQuery.SQL.Text, True);
  for P in Params do
  begin
    Parameter := FADOQuery.Parameters.FindParam(P.ParamName);

    if P.ParamType in [ftBlob, ftOraBlob, ftOraClob] then
    begin
      Parameter.DataType := P.ParamType;
      Parameter.Direction := pdInput;
      if VarIsNull(P.ParamValue) or (Length(TUtils.VariantToBytes(P.ParamValue)) = 0) then
        Parameter.Value := Null
      else
      begin
        BytesStream := TBytesStream.Create(TUtils.VariantToBytes(P.ParamValue));
        try
          Parameter.LoadFromStream(BytesStream, P.ParamType);
        finally
          BytesStream.Free;
        end;
      end;
    end else
    begin
      Parameter.DataType := P.ParamType;
      Parameter.Value := P.ParamValue;
      Parameter.Direction := pdInput;
      if (Parameter.DataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar]) and (Parameter.Size <= 0) then
        Parameter.Size := 1;
    end;
  end;
end;

procedure TDbGoStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FADOQuery.SQL.Text := SQLCommand;
end;

{ TDbGoConnectionAdapter }

procedure TDbGoConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TDbGoConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection = nil then
    Exit('');

  if Pos('DB2', Uppercase(Connection.Provider)) > 0 then
    Result := 'DB2'
  else
  if Pos('IFX', Uppercase(Connection.Provider)) > 0 then
    Result := 'Informix'
  else
    Result := 'MSSQL';
end;

function TDbGoConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TDbGoConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TADOQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TADOQuery.Create(nil);
  try
    Statement.Connection := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TDbGoStatementAdapter.Create(Statement);
end;

procedure TDbGoConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TDbGoConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.BeginTrans;
    Result := TDbGoTransactionAdapter.Create(Connection);
  end else
    Result := TDbGoTransactionAdapter.Create(nil);
end;

{ TDbGoTransactionAdapter }

procedure TDbGoTransactionAdapter.Commit;
begin
  if (FADOConnection = nil) then
    Exit;

  FADOConnection.CommitTrans;
end;

constructor TDbGoTransactionAdapter.Create(ADOConnection: TADOConnection);
begin
  FADOConnection := ADOConnection;
end;

procedure TDbGoTransactionAdapter.Rollback;
begin
  if (FADOConnection = nil) then
    Exit;

  FADOConnection.RollbackTrans;
end;

end.
