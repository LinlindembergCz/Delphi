unit Aurelius.Drivers.SQLite;

{$I Aurelius.inc}

interface

uses
  Classes, DB, Variants, Generics.Collections,
  Aurelius.Drivers.SQLite.Classes,
  Aurelius.Drivers.Base,
  Aurelius.Drivers.Interfaces;

type
  TSQLiteNativeResultSetAdapter = class(TInterfacedObject, IDBResultSet)
  private
    FStatement: TSQLiteStatement;
  public
    constructor Create(AStatement: TSQLiteStatement);
    destructor Destroy; override;
    function Next: boolean;
    function GetFieldValue(FieldIndex: Integer): Variant; overload;
    function GetFieldValue(FieldName: string): Variant; overload;
  end;

  TSQLiteNativeStatementAdapter = class(TInterfacedObject, IDBStatement)
  private
    FDB: TSQLiteDatabase;
    FSQL: string;
    FParams: TObjectList<TDBParam>;
    function PrepareStatement: TSQLiteStatement;
  public
    constructor Create(ADB: TSQLiteDatabase);
    destructor Destroy; override;
    procedure SetSQLCommand(SQLCommand: string);
    procedure SetParams(Params: TEnumerable<TDBParam>);
    procedure Execute;
    function ExecuteQuery: IDBResultSet;
  end;

  TSQLiteNativeConnectionAdapter = class(TInterfacedObject, IDBConnection)
  strict private
    FDatabase: TSQLiteDatabase;
    FFileName: string;
    procedure Execute(const SQL: string);
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    procedure EnableForeignKeys;
    procedure DisableForeignKeys;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function CreateStatement: IDBStatement;
    function BeginTransaction: IDBTransaction;
    function SqlDialect: string;
  end;

  TSQLiteNativeTransactionAdapter = class(TInterfacedObject, IDBTransaction)
  private
    FDatabase: TSQLiteDatabase;
  public
    constructor Create(ADatabase: TSQLiteDatabase);
    procedure Commit;
    procedure Rollback;
  end;

implementation

{ TSQLiteNativeStatementAdapter }

uses
  Aurelius.Drivers.Exceptions,
  Aurelius.Global.Utils;

constructor TSQLiteNativeStatementAdapter.Create(ADB: TSQLiteDatabase);
begin
  FDB := ADB;
  FParams := TObjectList<TDBParam>.Create(true);
end;

destructor TSQLiteNativeStatementAdapter.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TSQLiteNativeStatementAdapter.Execute;
var
  Statement: TSqliteStatement;
begin
  Statement := PrepareStatement;
  try
    Statement.Execute;
  finally
    Statement.Free;
  end;
end;

function TSQLiteNativeStatementAdapter.ExecuteQuery: IDBResultSet;
var
  Statement: TSqliteStatement;
begin
  Statement := PrepareStatement;
  Result := TSQLiteNativeResultSetAdapter.Create(Statement);
end;

function TSQLiteNativeStatementAdapter.PrepareStatement: TSQLiteStatement;
var
  Statement: TSQLiteStatement;
  Param: TDBParam;
  ParamIdx: integer;
begin
  Statement := FDB.Prepare(FSQL);
  try
    for Param in FParams do
    begin
      if VarIsNull(Param.ParamValue) then
        Continue;

      ParamIdx := Statement.BindParameterIndex(':' + Param.ParamName);
      case Param.ParamType of
        ftInteger, ftShortint, ftSmallint, ftLargeint:
          Statement.BindInt64(ParamIdx, Param.ParamValue);
        ftFloat, ftCurrency, ftExtended:
          Statement.BindDouble(ParamIdx, Param.ParamValue);
        ftString, ftWideString, ftFixedChar, ftMemo, ftWideMemo:
          Statement.BindText(ParamIdx, Param.ParamValue);
        ftBlob:
          Statement.BindBlob(ParamIdx, TUtils.VariantToBytes(Param.ParamValue));
      else
        Statement.BindText(ParamIdx, Param.ParamValue);
      end;
    end;
    Result := Statement;
  except
    Statement.Free;
    raise;
  end;
end;

procedure TSQLiteNativeStatementAdapter.SetParams(Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
begin
  FParams.Clear;
  for P in Params do
    FParams.Add(TDBParam.Create(P.ParamName, P.ParamType, P.ParamValue));
end;

procedure TSQLiteNativeStatementAdapter.SetSQLCommand(SQLCommand: string);
begin
  FSQL := SQLCommand;
end;

{ TSQLiteNativeConnectionAdapter }

function TSQLiteNativeConnectionAdapter.BeginTransaction: IDBTransaction;
begin
  if not IsConnected then
    Connect;

  if not FDatabase.InTransaction then
  begin
    FDatabase.BeginTransaction;
    Result := TSQLiteNativeTransactionAdapter.Create(FDatabase);
  end else
    Result := TSQLiteNativeTransactionAdapter.Create(nil);
end;

procedure TSQLiteNativeConnectionAdapter.Connect;
begin
  if not IsConnected then
    FDatabase := TSQLiteDatabase.Create(FFileName);
end;

constructor TSQLiteNativeConnectionAdapter.Create(AFileName: string);
begin
  FFilename := AFileName;
end;

function TSQLiteNativeConnectionAdapter.CreateStatement: IDBStatement;
begin
  if not IsConnected then
    Connect;
  Result := TSQLiteNativeStatementAdapter.Create(FDatabase);
end;

destructor TSQLiteNativeConnectionAdapter.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TSQLiteNativeConnectionAdapter.DisableForeignKeys;
begin
  Execute('PRAGMA foreign_keys = off');
end;

procedure TSQLiteNativeConnectionAdapter.Disconnect;
begin
  if FDatabase <> nil then
  begin
    FDatabase.Free;
    FDatabase := nil;
  end;
end;

procedure TSQLiteNativeConnectionAdapter.EnableForeignKeys;
begin
  Execute('PRAGMA foreign_keys = on');
end;

procedure TSQLiteNativeConnectionAdapter.Execute(const SQL: string);
var
  Statement: IDBStatement;
begin
  Statement := CreateStatement;
  Statement.SetSQLCommand(SQL);
  Statement.Execute;
end;

function TSQLiteNativeConnectionAdapter.IsConnected: Boolean;
begin
  Result := FDatabase <> nil;
end;

function TSQLiteNativeConnectionAdapter.SqlDialect: string;
begin
  Result := 'SQLite';
end;

{ TSQLiteNativeTransactionAdapter }

procedure TSQLiteNativeTransactionAdapter.Commit;
begin
  if (FDatabase = nil) then
    Exit;

  FDatabase.Commit;
end;

constructor TSQLiteNativeTransactionAdapter.Create(ADatabase: TSQLiteDatabase);
begin
  FDatabase := ADatabase;
end;

procedure TSQLiteNativeTransactionAdapter.Rollback;
begin
  if (FDatabase = nil) then
    Exit;

  FDatabase.Rollback;
end;

{ TSQLiteNativeResultSetAdapter }

constructor TSQLiteNativeResultSetAdapter.Create(AStatement: TSQLiteStatement);
begin
  FStatement := AStatement;
end;

destructor TSQLiteNativeResultSetAdapter.Destroy;
begin
  FStatement.Free;
  inherited;
end;

function TSQLiteNativeResultSetAdapter.GetFieldValue(FieldIndex: Integer): Variant;
begin
  case FStatement.ColumnType(FieldIndex) of
    TSQLiteFieldType.stInteger:
      Result := FStatement.ColumnInt64(FieldIndex);
    TSQLiteFieldType.stFloat:
      Result := FStatement.ColumnDouble(FieldIndex);
    TSQLiteFieldType.stText:
      Result := FStatement.ColumnText(FieldIndex);
    TSQLiteFieldType.stBlob:
      Result := FStatement.ColumnBlob(FieldIndex);
    TSQLiteFieldType.stNull:
      Result := Variants.Null;
  else
    //sftUnknown:
    // ERROR!
  end;
end;

function TSQLiteNativeResultSetAdapter.GetFieldValue(FieldName: string): Variant;
var
  FieldIndex: integer;
begin
  FieldIndex := FStatement.ColumnIndex(FieldName);
  Result := GetFieldValue(FieldIndex);
end;

function TSQLiteNativeResultSetAdapter.Next: boolean;
begin
  Result := FStatement.Next;
end;

end.
