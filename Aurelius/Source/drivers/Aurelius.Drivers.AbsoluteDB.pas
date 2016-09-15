unit Aurelius.Drivers.AbsoluteDB;

{$I Aurelius.Inc}

interface

uses
  Classes, SysUtils, DB, Variants, Generics.Collections,
  ABSMain,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces,
  Aurelius.Global.Utils;

type
  TAbsoluteSResultSetAdapter = class(TDriverResultSetAdapter<TABSQuery>)
  end;

  TAbsoluteDBStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FQuery: TABSQuery;
  public
    constructor Create(AQuery: TABSQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TAbsoluteDBConnectionAdapter = class(TDriverConnectionAdapter<TABSDatabase>, IDBConnection)
  public
    constructor Create(AConnection: TABSDatabase; AOwnsConnection: boolean); override;
    function ABSDatabase: TABSDatabase;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TAbsoluteDBTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FConnection: TABSDatabase;
  public
    constructor Create(AConnection: TABSDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TABSConnectionAdapter }

function TAbsoluteDBConnectionAdapter.ABSDatabase: TABSDatabase;
begin
  Result := inherited Connection;
end;

function TAbsoluteDBConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;

  if not Connection.InTransaction then
  begin
    Connection.StartTransaction;
    Result := TAbsoluteDBTransactionAdapter.Create(Connection);
  end else
    Result := TAbsoluteDBTransactionAdapter.Create(nil);
end;

procedure TAbsoluteDBConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

constructor TAbsoluteDBConnectionAdapter.Create(AConnection: TABSDatabase; AOwnsConnection: boolean);
begin
  if not FileExists(AConnection.DatabaseFileName) then
  begin
    AConnection.CreateDatabase;
  end;
  inherited;
end;

function TAbsoluteDBConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TABSQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TABSQuery.Create(nil);
  try
    Statement.DatabaseName := Connection.DatabaseName;
  except
    Statement.Free;
    raise;
  end;
  Result := TAbsoluteDBStatementAdapter.Create(Statement);
end;

procedure TAbsoluteDBConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TAbsoluteDBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TAbsoluteDBConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection = nil then
    Exit('');
  Result := 'AbsoluteDB';
end;

{ TABSTransactionAdapter }

procedure TAbsoluteDBTransactionAdapter.Commit;
begin
  if (FConnection = nil) then
    Exit;

  FConnection.Commit;
end;

constructor TAbsoluteDBTransactionAdapter.Create(AConnection: TABSDatabase);
begin
  FConnection := AConnection;
end;

procedure TAbsoluteDBTransactionAdapter.Rollback;
begin
  if (FConnection = nil) then
    Exit;

  FConnection.Rollback;
end;

{ TABSStatementAdapter }

constructor TAbsoluteDBStatementAdapter.Create(AQuery: TABSQuery);
begin
  FQuery := AQuery;
end;

destructor TAbsoluteDBStatementAdapter.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TAbsoluteDBStatementAdapter.Execute;
begin
  FQuery.ExecSQL;
end;

function TAbsoluteDBStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TABSQuery;
  I: Integer;
begin
  ResultSet := TABSQuery.Create(nil);
  try
    ResultSet.DatabaseName := FQuery.DatabaseName;
    ResultSet.SQL := FQuery.SQL;
    for I := 0 to FQuery.Params.Count - 1 do
      ResultSet.Params.Assign(FQuery.Params);

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TAbsoluteSResultSetAdapter.Create(ResultSet);
end;

procedure TAbsoluteDBStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TParam;
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
        Parameter.AsString := ''
      else
        Parameter.AsString := P.ParamValue;
    end
    else
      Parameter.Value := P.ParamValue;
  end;

end;

procedure TAbsoluteDBStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FQuery.SQL.Text := SQLCommand;
end;

end.
