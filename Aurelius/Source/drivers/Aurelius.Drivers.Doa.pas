unit Aurelius.Drivers.Doa;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  Oracle,
  OracleData,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TDoaResultSetAdapter = class(TDriverResultSetAdapter<TOracleDataset>)
  end;

  TDoaConnectionAdapter = class(TDriverConnectionAdapter<TOracleSession>, IDBConnection, IDBTransaction)
  private
    FTransactionCount: integer;
  public
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
    function InTransaction: boolean;
    procedure Commit;
    procedure Rollback;
  end;

  TDoaStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FQuery: TOracleQuery;
    FLobs: TList<TLobLocator>;
    FConnAdapter: TDoaConnectionAdapter;
  public
    constructor Create(AQuery: TOracleQuery; ConnAdapter: TDoaConnectionAdapter);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

implementation

{ TDoaStatementAdapter }

uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TDoaStatementAdapter.Create(AQuery: TOracleQuery; ConnAdapter: TDoaConnectionAdapter);
begin
  FQuery := AQuery;
  FConnAdapter := ConnAdapter;
  FLobs := TObjectList<TLobLocator>.Create(true);
end;

destructor TDoaStatementAdapter.Destroy;
begin
  FQuery.Free;
  FLobs.Free;
  inherited;
end;

procedure TDoaStatementAdapter.Execute;
begin
  if (FQuery.Session <> nil) and not FQuery.Session.Connected then
    FQuery.Session.Connected := true;

  FQuery.Execute;
  if not FConnAdapter.InTransaction then
    FQuery.Session.Commit;
end;

function TDoaStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TOracleDataset;
begin
  ResultSet := TOracleDataset.Create(nil);
  try
    ResultSet.Session := FQuery.Session;
    ResultSet.SQL := FQuery.SQL;
    ResultSet.Variables.Assign(FQuery.Variables);
    if (ResultSet.Session <> nil) and not ResultSet.Session.Connected then
      ResultSet.Session.Connected := true;
    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TDoaResultSetAdapter.Create(ResultSet);
end;

procedure TDoaStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  OracleType: integer;
  Lob: TLobLocator;
  Bytes: TBytes;
begin
  FQuery.DeleteVariables;
  for P in Params do
  begin
    OracleType := otCursor;
    case P.ParamType of
      ftString, ftWideString:
        OracleType := otString;
      ftFixedWideChar, ftFixedChar:
        OracleType := otChar;
      ftSmallint, ftInteger, ftWord, ftShortint, ftByte, ftLongWord:
        OracleType := otInteger;
      ftFloat, ftCurrency, ftExtended, ftSingle:
        OracleType := otFloat;
      ftLargeInt:
        OracleType := otFloat;
      ftDate, ftTime, ftDateTime:
        OracleType := otDate;
      ftOraBlob, ftBlob:
        begin
          OracleType := otBlob;
          Bytes := TUtils.VariantToBytes(P.ParamValue);
          if VarIsNull(P.ParamValue) or (Length(Bytes) = 0) then
            FQuery.DeclareAndSet(P.ParamName, otSubst, 'NULL')
          else
          begin
            Lob := TLobLocator.CreateTemporary(FQuery.Session, OracleType, true);
            try
              Lob.Write(Bytes[0], Length(Bytes));
              FQuery.DeclareVariable(P.ParamName, OracleType);
              FQuery.SetComplexVariable(P.ParamName, Lob);
              FLobs.Add(Lob);
            except
              Lob.Free;
              raise;
            end;
          end;
        end;
      ftWideMemo:
        begin
          OracleType := otNCLOB;
          if VarIsNull(P.ParamValue) or (Length(VarToStr(P.ParamValue)) = 0) then
            FQuery.DeclareAndSet(P.ParamName, otSubst, 'NULL')
          else
          begin
            Lob := TLobLocator.CreateTemporary(FQuery.Session, OracleType, true);
            try
              Lob.AsWideString := P.ParamValue;
              FQuery.DeclareVariable(P.ParamName, OracleType);
              FQuery.SetComplexVariable(P.ParamName, Lob);
              FLobs.Add(Lob);
            except
              Lob.Free;
              raise;
            end;
          end;
        end;
      ftMemo:
        begin
          OracleType := otCLOB;
          if VarIsNull(P.ParamValue) or (Length(VarToStr(P.ParamValue)) = 0) then
            FQuery.DeclareAndSet(P.ParamName, otSubst, 'NULL')
          else
          begin
            Lob := TLobLocator.CreateTemporary(FQuery.Session, OracleType, true);
            try
              Lob.AsString := P.ParamValue;
              FQuery.DeclareVariable(P.ParamName, OracleType);
              FQuery.SetComplexVariable(P.ParamName, Lob);
              FLobs.Add(Lob);
            except
              Lob.Free;
              raise;
            end;
          end;
        end;
    end;

    if OracleType = otCursor then
      raise Exception.Create('Data type parameter not supported in Direct Oracle Access driver');

    if (OracleType <> otBlob) and (OracleType <> otNClob) and (OracleType <> otClob) then
    begin
      if VarIsNull(P.ParamValue) then
        FQuery.DeclareAndSet(P.ParamName, OracleType, Null)
      else
        FQuery.DeclareAndSet(P.ParamName, OracleType, P.ParamValue);
    end;
  end;
end;

procedure TDoaStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FQuery.SQL.Text := SQLCommand;
end;

{ TDoaConnectionAdapter }

procedure TDoaConnectionAdapter.Disconnect;
begin
  if Connection <> nil then
    Connection.Connected := False;
end;

function TDoaConnectionAdapter.RetrieveSqlDialect: string;
begin
  Result := 'Oracle';
end;

procedure TDoaConnectionAdapter.Rollback;
begin
  Dec(FTransactionCount);
  if FTransactionCount <= 0 then
  begin
    FTransactionCount := 0;
    if IsConnected then
      Connection.Rollback;
  end;
end;

function TDoaConnectionAdapter.InTransaction: boolean;
begin
  Result := FTransactionCount > 0;
end;

function TDoaConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

function TDoaConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TOracleQuery;
begin
  if Connection = nil then
    Exit(nil);

  Statement := TOracleQuery.Create(nil);
  try
    Statement.Session := Connection;
  except
    Statement.Free;
    raise;
  end;
  Result := TDoaStatementAdapter.Create(Statement, Self);
end;

procedure TDoaConnectionAdapter.Commit;
begin
  Dec(FTransactionCount);
  if FTransactionCount <= 0 then
  begin
    FTransactionCount := 0;
    if IsConnected then
      Connection.Commit;
  end;
end;

procedure TDoaConnectionAdapter.Connect;
begin
  if Connection <> nil then
    Connection.Connected := True;
end;

function TDoaConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  Connection.Connected := true;
  Inc(FTransactionCount);
  Result := Self;
end;

end.
