unit Aurelius.Drivers.UIB;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  uiblib, uib, uibdataset,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TUIBResultSetAdapter = class(TInterfacedObject, IDBResultSet)
  private
    FDataset: TUIBQuery;
    FFetching: Boolean;
  public
    constructor Create(ADataset: TUIBQuery);
    destructor Destroy; override;
    function Next: Boolean;
    function GetFieldValue(FieldName: string): Variant; overload;
    function GetFieldValue(FieldIndex: Integer): Variant; overload;
  end;

  TUIBStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FQuery: TUIBQuery;
    FTempParams: TList<TDBParam>;
    procedure SetUIBParams(Command: TObject; Params: TEnumerable<TDBParam>);
  public
    constructor Create(AQuery: TUIBQuery);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TUIBConnectionAdapter = class(TDriverConnectionAdapter<TUIBDatabase>, IDBConnection)
  private
    FDefaultTransaction: TUIBTransaction;
  public
    constructor Create(AConnection: TUIBDatabase; ASQLDialect: string; AOwnsConnection: boolean); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function RetrieveSqlDialect: string; override;
  end;

  TUIBTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FTransaction: TUIBTransaction;
    FDatabase: TUIBDatabase;
  public
    constructor Create(ATransaction: TUIBTransaction; ADatabase: TUIBDatabase);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
  end;

implementation
uses
  SysUtils,
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

{ TUIBStatementAdapter }

constructor TUIBStatementAdapter.Create(AQuery: TUIBQuery);
begin
  FQuery := AQuery;
  FTempParams := TObjectList<TDBParam>.Create(true);
end;

destructor TUIBStatementAdapter.Destroy;
begin
  FQuery.Free;
  FTempParams.Free;
  inherited;
end;

procedure TUIBStatementAdapter.Execute;
begin
  if not FQuery.Database.Connected then
    FQuery.Database.Connected := true;

  if FQuery.Transaction.InTransaction then
  begin
    SetUIBParams(FQuery, FTempParams);
    FQuery.Execute;
  end
  else
  begin
    FQuery.Transaction.StartTransaction;
    try
      SetUIBParams(FQuery, FTempParams);
      FQuery.Execute;
      FQuery.Transaction.Commit;
    except
      FQuery.Transaction.Rollback;
      raise;
    end;
  end;
end;

function TUIBStatementAdapter.ExecuteQuery: IDBResultSet;
var
  ResultSet: TUIBQuery;
begin
  ResultSet := TUIBQuery.Create(nil);
  try
    ResultSet.Database := FQuery.Database;
    ResultSet.SQL.Text := FQuery.SQL.Text;
    ResultSet.Transaction := TUIBTransaction.Create(ResultSet);
    ResultSet.Transaction.DataBase := ResultSet.Database;
    ResultSet.FetchBlobs := true;
    SetUIBParams(ResultSet, FTempParams);

    if not ResultSet.Database.Connected then
      ResultSet.Database.Connected := true;

    ResultSet.Open;
  except
    ResultSet.Free;
    raise;
  end;
  Result := TUIBResultSetAdapter.Create(ResultSet);
end;

procedure TUIBStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  Param: TDBParam;
begin
  FTempParams.Clear;
  for Param in Params do
    FTempParams.Add(Param.Clone);
end;

procedure TUIBStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FQuery.SQL.Text := SQLCommand;
end;

procedure TUIBStatementAdapter.SetUIBParams(Command: TObject; Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  Idx: integer;
  Bytes: TBytesStream;
  BlobStr: string;
  UIBParams: uiblib.TSQLParams;
begin
  UIBParams := nil;
  if Command is TUIBQuery then
    UIBParams := TUIBQuery(Command).Params
  else
  if Command is TUIBDataset then
    UIBParams := TUIBDataset(Command).Params;
  Assert(UIBParams <> nil, 'Invalid Command parameter in SetUIBParams');

  for P in Params do
  begin
//    Parameter := FFIBQuery.ParamByName(P.ParamName);
    Idx := UIBParams.GetFieldIndex(AnsiString(P.ParamName));
    if VarIsNull(P.ParamValue) then
      UIBParams.IsNull[Idx] := true
    else
    begin
      case P.ParamType of
        ftString:
          UIBParams.AsString[Idx] := P.ParamValue;
        ftSmallint, ftInteger, ftWord:
          UIBParams.AsInteger[Idx] := P.ParamValue;
        ftBoolean:
          UIBParams.AsBoolean[Idx] := P.ParamValue;
        ftFloat:
          UIBParams.AsDouble[Idx] := P.ParamValue;
        ftCurrency:
          UIBParams.AsCurrency[Idx] := P.ParamValue;
        ftDate:
          UIBParams.AsDate[Idx] := P.ParamValue;
        ftTime, ftDateTime:
          UIBParams.AsDateTime[Idx] := P.ParamValue;
        ftBlob:
          begin
            Bytes := TBytesStream.Create(TUtils.VariantToBytes(P.ParamValue));
            try
              if VarIsNull(P.ParamValue) or (Bytes.Size = 0) then
                UIBParams.IsNull[Idx] := true
              else
              begin
                if Command is TUIBQuery then
                  TUIBQuery(Command).ParamsSetBlob(Idx, Bytes)
                else
                if Command is TUIBDataset then
                  TUIBDataset(Command).ParamsSetBlob(Idx, Bytes);
              end;
            finally
              Bytes.Free;
            end;
          end;
        ftWideMemo, ftMemo:
          begin
            BlobStr := VarToStr(P.ParamValue);
            if Command is TUIBQuery then
              TUIBQuery(Command).ParamsSetBlob(Idx, BlobStr)
            else
            if Command is TUIBDataset then
              TUIBDataset(Command).ParamsSetBlob(Idx, BlobStr);
          end;

//        ftMemo:
//          UIBParams.AsString[Idx] := P.ParamValue;
  //      ftGraphic: ;
  //      ftFmtMemo: ;
  //      ftParadoxOle: ;
  //      ftDBaseOle: ;
  //      ftTypedBinary: ;
  //      ftCursor: ;
        ftFixedChar:
          UIBParams.AsString[Idx] := P.ParamValue;
        ftWideString:
          UIBParams.AsString[Idx] := P.ParamValue;
        ftLargeint:
          UIBParams.AsInt64[Idx] := P.ParamValue;
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
          UIBParams.AsDateTime[Idx] := P.ParamValue;
  //      ftFMTBcd: ;
        ftFixedWideChar:
          UIBParams.AsString[Idx] := P.ParamValue;
  //      ftOraTimeStamp: ;
  //      ftOraInterval: ;
        ftLongWord:
          UIBParams.AsInt64[Idx] := P.ParamValue;
        ftShortint, ftByte:
          UIBParams.AsInteger[Idx] := P.ParamValue;
        ftExtended:
          UIBParams.AsDouble[Idx] := P.ParamValue;
  //      ftConnection: ;
  //      ftParams: ;
  //      ftStream: ;
  //      ftTimeStampOffset: ;
  //      ftObject: ;
        ftSingle:
          UIBParams.AsSingle[Idx] := P.ParamValue;
      else
        Assert(false, 'Data type parameter not supported in UIB driver');
      end;
    end;
  end;
end;

{ TUIBConnectionAdapter }

destructor TUIBConnectionAdapter.Destroy;
begin
  FDefaultTransaction.Free;
  inherited;
end;

procedure TUIBConnectionAdapter.Disconnect;
begin
  if (Connection <> nil) and Connection.Connected then
    Connection.Connected := False;
end;

function TUIBConnectionAdapter.RetrieveSqlDialect: string;
begin
  if Connection <> nil then
    Result := 'Interbase'
  else
    Result := '';
end;

function TUIBConnectionAdapter.IsConnected: Boolean;
begin
  if Connection <> nil then
    Result := Connection.Connected
  else
    Result := false;
end;

constructor TUIBConnectionAdapter.Create(AConnection: TUIBDatabase;
  ASQLDialect: string; AOwnsConnection: boolean);
begin
  inherited;
  FDefaultTransaction := TUIBTransaction.Create(nil);
  FDefaultTransaction.Database := AConnection;
end;

function TUIBConnectionAdapter.CreateStatement: IDBStatement;
var
  Statement: TUIBQuery;
begin
  if Connection <> nil then
  begin
    Statement := TUIBQuery.Create(nil);
    try
      Statement.Database := Connection;
      Statement.Transaction := FDefaultTransaction;
    except
      Statement.Free;
      raise;
    end;
    Result := TUIBStatementAdapter.Create(Statement);
  end else
    Result := nil;
end;

procedure TUIBConnectionAdapter.Connect;
begin
  if (Connection <> nil) and not Connection.Connected then
    Connection.Connected := True;
end;

function TUIBConnectionAdapter.BeginTransaction: IDBTransaction;
var
  Trans: TUIBTransaction;
begin
  if Connection = nil then
    Exit(nil);

  // We must open the connection here, otherwise the BeginTransaction will not work
  // This is because BeginTransaction checks if database supports transaction, and it only
  // does that after a connection. It appears to be a bug in UIB
  if not Connection.Connected then
    Connection.Connected := true;

  Trans := nil;
  if not FDefaultTransaction.InTransaction then
  begin
    Trans := FDefaultTransaction;
    Trans.StartTransaction;
  end;

  Result := TUIBTransactionAdapter.Create(Trans, Connection);
end;

{ TUIBTransactionAdapter }

procedure TUIBTransactionAdapter.Commit;
begin
  if (FTransaction = nil) then
    Exit;

  FTransaction.Commit;
end;

constructor TUIBTransactionAdapter.Create(
  ATransaction: TUIBTransaction; ADatabase: TUIBDatabase);
begin
  FTransaction := ATransaction;
  FDatabase := ADatabase;
end;

destructor TUIBTransactionAdapter.Destroy;
begin
  // Do not destroy the transaction object, because it's the global FDefaultTransaction!
  //  FFIBTransaction.Free;
  inherited;
end;

procedure TUIBTransactionAdapter.Rollback;
begin
  if (FTransaction = nil) then
    Exit;

  FTransaction.Rollback;
end;

{ TUIBResultSetAdapter }

constructor TUIBResultSetAdapter.Create(ADataset: TUIBQuery);
begin
  FDataset := ADataset;
end;

destructor TUIBResultSetAdapter.Destroy;
begin
  FDataset.Free;
  inherited;
end;

function TUIBResultSetAdapter.GetFieldValue(FieldName: string): Variant;
var
  FieldIndex: integer;
begin
  FieldIndex := FDataset.Fields.GetFieldIndex(AnsiString(FieldName));
  Result := GetFieldValue(FieldIndex);
end;

function TUIBResultSetAdapter.GetFieldValue(FieldIndex: Integer): Variant;
begin
  if FDataset.Fields.IsNull[FieldIndex] then
    Result := Variants.Null
  else
    Result := FDataset.Fields.AsVariant[FieldIndex];
end;

function TUIBResultSetAdapter.Next: Boolean;
begin
  if not FFetching then
    FFetching := True
  else
    FDataset.Next;

  Result := not FDataset.Eof;
end;

end.
