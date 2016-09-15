unit Aurelius.Drivers.FIBPlus;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  FIBQuery, FIBDatabase,
  pFIBQuery, pFIBDatabase, pFIBProps,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TFIBPlusResultSetAdapter = class(TInterfacedObject, IDBResultSet)
  private
    FDataset: TpFIBQuery;
    FFetching: Boolean;
    FAutoCommit: boolean;
  public
    constructor Create(ADataset: TpFIBQuery; AAutoCommit: boolean);
    destructor Destroy; override;
    function Next: Boolean;
    function GetFieldValue(FieldName: string): Variant; overload;
    function GetFieldValue(FieldIndex: Integer): Variant; overload;
  end;

  TFIBPlusStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FFIBQuery: TpFIBQuery;
  public
    constructor Create(AFIBQuery: TpFIBQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TFIBPlusConnectionAdapter = class(TDriverConnectionAdapter<TFIBDatabase>, IDBConnection)
  private
    FDefaultTransaction: TpFIBTransaction;
  public
    constructor Create(AConnection: TFIBDatabase; ASQLDialect: string; AOwnsConnection: boolean); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TFIBPlusTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FFIBTransaction: TpFIBTransaction;
    FFIBDatabase: TFIBDatabase;
  public
    constructor Create(FIBTransaction: TpFIBTransaction; FIBDatabase: TFIBDatabase);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
  end;

implementation
uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

{ TFIBPlusStatementAdapter }

constructor TFIBPlusStatementAdapter.Create(AFIBQuery: TpFIBQuery);
begin
  FFIBQuery := AFIBQuery;
end;

destructor TFIBPlusStatementAdapter.Destroy;
begin
  FFIBQuery.Free;
  inherited;
end;

procedure TFIBPlusStatementAdapter.Execute;
begin
  if not FFIBQuery.Database.Connected then
    FFIBQuery.Database.Open;

  if FFIBQuery.Transaction.InTransaction then
    FFIBQuery.ExecQuery
  else
  begin
    FFIBQuery.Transaction.StartTransaction;
    try
      FFIBQuery.ExecQuery;
      FFIBQuery.Transaction.Commit;
    except
      FFIBQuery.Transaction.Rollback;
      raise;
    end;
  end;
end;

function TFIBPlusStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TpFIBQuery;
  AutoCommit: boolean;
//  I: Integer;
begin
  ResultSet := TpFIBQuery.Create(nil);
  try
    ResultSet.Database := FFIBQuery.Database;
    ResultSet.SQL.Text := FFIBQuery.SQL.Text;
    ResultSet.Transaction := FFIBQuery.Transaction;

    ResultSet.Params.AssignValues(FFIBQuery.Params);
//    for I := 0 to FFIBQuery.Params.Count - 1 do
//    begin
//      ResultSet.Params[I].Assign(FFIBQuery.Params[I]);
//      ResultSet.Params[I].DataType := FFIBQuery.Params[I].DataType;
//      ResultSet.Params[I].Value := FFIBQuery.Params[I].Value;
//    end;

    if not ResultSet.Database.Connected then
      ResultSet.Database.Open;

    AutoCommit := false;
    if not ResultSet.Transaction.InTransaction then
    begin
      ResultSet.Transaction.StartTransaction;
      AutoCommit := true;
    end;
    ResultSet.ExecQuery;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TFIBPlusResultSetAdapter.Create(ResultSet, AutoCommit);
end;

procedure TFIBPlusStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Parameter: TFIBXSQLVAR;
  Bytes: TBytesStream;
begin
  for P in Params do
  begin
    Parameter := FFIBQuery.ParamByName(P.ParamName);
    if VarIsNull(P.ParamValue) then
      Parameter.Clear
    else
    begin
      case P.ParamType of
        ftString:
          Parameter.AsString := P.ParamValue;
        ftSmallint, ftInteger, ftWord:
          Parameter.AsInteger := P.ParamValue;
        ftBoolean:
          Parameter.AsBoolean := P.ParamValue;
        ftFloat:
          Parameter.AsFloat := P.ParamValue;
        ftCurrency:
          Parameter.AsCurrency := P.ParamValue;
        ftDate:
          Parameter.AsDate := P.ParamValue;
        ftTime, ftDateTime:
          Parameter.AsDateTime := P.ParamValue;
        ftBlob:
          begin
            Bytes := TBytesStream.Create(TUtils.VariantToBytes(P.ParamValue));
            try
              if VarIsNull(P.ParamValue) or (Bytes.Size = 0) then
                Parameter.Clear
              else
                Parameter.LoadFromStream(Bytes);
            finally
              Bytes.Free;
            end;
          end;
        ftWideMemo:
          Parameter.AsWideString := P.ParamValue;
        ftMemo:
          Parameter.AsString := P.ParamValue;
  //      ftGraphic: ;
  //      ftFmtMemo: ;
  //      ftParadoxOle: ;
  //      ftDBaseOle: ;
  //      ftTypedBinary: ;
  //      ftCursor: ;
        ftFixedChar:
          Parameter.AsString := P.ParamValue;
        ftWideString:
          Parameter.AsWideString := P.ParamValue;
        ftLargeint:
          Parameter.AsInt64 := P.ParamValue;
  //      ftADT: ;
  //      ftArray: ;
  //      ftReference: ;
  //      ftDataSet: ;
  //      ftOraBlob: ;
  //      ftOraClob: ;
  //      ftVariant: ;
  //      ftInterface: ;
  //      ftIDispatch: ;
  //      ftGuid: ;
        ftTimeStamp:
          Parameter.AsDateTime := P.ParamValue;
  //      ftFMTBcd: ;
        ftFixedWideChar:
          Parameter.AsWideString := P.ParamValue;
  //      ftOraTimeStamp: ;
  //      ftOraInterval: ;
        ftLongWord:
          Parameter.AsInt64 := P.ParamValue;
        ftShortint, ftByte:
          Parameter.AsInteger := P.ParamValue;
        ftExtended:
          Parameter.AsExtended := P.ParamValue;
  //      ftConnection: ;
  //      ftParams: ;
  //      ftStream: ;
  //      ftTimeStampOffset: ;
  //      ftObject: ;
        ftSingle:
          Parameter.AsFloat := P.ParamValue;
      else
        Assert(false, 'Data type parameter not supported in FIBPlus driver');
      end;
    end;
  end;
end;

procedure TFIBPlusStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FFIBQuery.SQL.Text := SQLCommand;
end;

{ TFIBPlusConnectionAdapter }

destructor TFIBPlusConnectionAdapter.Destroy;
begin
  FDefaultTransaction.Free;
  inherited;
end;

procedure TFIBPlusConnectionAdapter.Disconnect;
begin
  if (Connection <> nil) and Connection.Connected then
    Connection.Connected := False;
end;

function TFIBPlusConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection <> nil then
    Result := 'Interbase'
  else
    Result := '';
end;

function TFIBPlusConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

constructor TFIBPlusConnectionAdapter.Create(AConnection: TFIBDatabase;
  ASQLDialect: string; AOwnsConnection: boolean);
begin
  inherited;
  FDefaultTransaction := TpFIBTransaction.Create(nil);
  FDefaultTransaction.DefaultDatabase := AConnection;
  FDefaultTransaction.TimeoutAction := TTransactionAction.TACommit;
end;

function TFIBPlusConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TpFIBQuery;
begin
  if Connection <> nil then
  begin
    Statement := TpFIBQuery.Create(nil);
    try
      Statement.Database := Connection;
      Statement.Transaction := FDefaultTransaction;
    except
      Statement.Free;
      raise;
    end;
    Result := TFIBPlusStatementAdapter.Create(Statement);
  end else
    Result := nil;
end;

procedure TFIBPlusConnectionAdapter.Connect;
begin
  if (Connection <> nil) and not Connection.Connected then
    Connection.Connected := True;
end;

function TFIBPlusConnectionAdapter.BeginTransaction: IDBTransaction;
var
  Trans: TpFIBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  // We must open the connection here, otherwise the BeginTransaction will not work
  // This is because BeginTransaction checks if database supports transaction, and it only
  // does that after a connection. It appears to be a bug in FIBPlus
  if not Connection.Connected then
    Connection.Open;

  Trans := nil;
  if not FDefaultTransaction.InTransaction then
  begin
    Trans := FDefaultTransaction;
    Trans.StartTransaction;
  end;

  Result := TFIBPlusTransactionAdapter.Create(Trans, Connection);
end;

{ TFIBPlusTransactionAdapter }

procedure TFIBPlusTransactionAdapter.Commit;
begin
  if (FFIBTransaction = nil) then
    Exit;

  FFIBTransaction.Commit;
end;

constructor TFIBPlusTransactionAdapter.Create(
  FIBTransaction: TpFIBTransaction; FIBDatabase: TFIBDatabase);
begin
  FFIBTransaction := FIBTransaction;
  FFIBDatabase := FIBDatabase;
end;

destructor TFIBPlusTransactionAdapter.Destroy;
begin
  // Do not destroy the transaction object, because it's the global FDefaultTransaction!
  //  FFIBTransaction.Free;
  inherited;
end;

procedure TFIBPlusTransactionAdapter.Rollback;
begin
  if (FFIBTransaction = nil) then
    Exit;

  FFIBTransaction.Rollback;
end;

{ TFIBPlusResultSetAdapter }

constructor TFIBPlusResultSetAdapter.Create(ADataset: TpFIBQuery; AAutoCommit: boolean);
begin
  FDataset := ADataset;
  FAutoCommit := AAutoCommit;
end;

destructor TFIBPlusResultSetAdapter.Destroy;
var
  Trans: TFIBTransaction;
begin
  FDataset.Close;
  Trans := FDataset.Transaction;
  FDataset.Transaction := nil;
  try
    if FAutoCommit then
      if Trans.FIBBaseCount = 0 then
        Trans.Commit;
  finally
    FDataset.Free;
  end;
  inherited;
end;

function TFIBPlusResultSetAdapter.GetFieldValue(FieldName: string): Variant;
var
  Field: TFIBXSQLVAR;
begin
  Field := FDataset.FieldByName(FieldName);
  Result := GetFieldValue(Field.Index);
end;

function TFIBPlusResultSetAdapter.GetFieldValue(FieldIndex: Integer): Variant;
begin
  if FDataset.Fields[FieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataset.Fields[FieldIndex].Value;
end;

function TFIBPlusResultSetAdapter.Next: Boolean;
begin
  if not FFetching then
    FFetching := True
  else
    FDataset.Next;

  Result := not FDataset.Eof;
end;

end.
