unit Aurelius.Drivers.SQLite.Classes;

{$I Aurelius.inc}

interface

uses
	Classes, SysUtils,
  Generics.Collections,
  Aurelius.Drivers.SQLite.Import;

type
	ESQLiteException = class(Exception)
	end;

  TSqliteStatement = class;

	TSqliteDatabase = class
	private
		FHandle: Pointer;
    FTransactionCount: integer;
    procedure RaiseError(const msg: string);
	public
		constructor Create(const FileName: string);
		destructor Destroy; override;
		procedure ExecSQL(const sql: string);
		procedure BeginTransaction;
		procedure Commit;
		procedure Rollback;
    function InTransaction: boolean;
    function Prepare(ASQL: string): TSQLiteStatement;
		function GetLastInsertRowID: int64;
	end;

  TSQLiteFieldType = (stUnknown, stInteger, stFloat, stText, stBlob, stNull);

  TSqliteStatement = class
  private
    FStmt: Pointer;
    FDB: TSqliteDatabase;
    FColumnNames: TStrings;
    procedure CreateColumnNames;
  public
    constructor Create(ADatabase: TSQLiteDatabase; ASQL: string);
    destructor Destroy; override;
    procedure Execute;
    function BindParameterIndex(AParamName: string): integer;
    procedure BindInt64(AIndex: integer; AValue: Int64);
    procedure BindDouble(AIndex: integer; AValue: double);
    procedure BindText(AIndex: integer; AValue: string);
    procedure BindBlob(AIndex: integer; AValue: TBytes);
    function ColumnInt64(i: integer): Int64;
    function ColumnText(i: integer): string;
    function ColumnDouble(i: integer): double;
    function ColumnBlob(i: integer): TBytes;
    function ColumnIsNull(i: integer): boolean;
    function ColumnType(i: integer): TSQLiteFieldType;
    function ColumnIndex(AColumnName: string): integer;
    function Next: boolean;
  end;

implementation

{TSqliteDatabase}

constructor TSqliteDatabase.Create(const FileName: string);
begin
  if not LoadSQLiteLibrary then
    raise ESQLiteException.CreateFmt('Could not load SQLite library "%s', [SQLiteDLLName]);
	if sqlite3_open16(PChar(FileName), FHandle) <> 0 then
    RaiseError(Format('Database: %s', [FileName]));
end;

function TSqliteDatabase.Prepare(ASQL: string): TSQLiteStatement;
begin
  Result := TSQLiteStatement.Create(Self, ASQL);
end;

destructor TSqliteDatabase.Destroy;
begin
	sqlite3_close(FHandle);
  inherited;
end;

procedure TSqliteDatabase.ExecSQL(const sql: string);
var
  Stmt: TSQLiteStatement;
begin
  Stmt := TSQLiteStatement.Create(Self, sql);
  try
    Stmt.Execute;
  finally
    Stmt.Free;
  end;
end;

procedure TSqliteDatabase.BeginTransaction;
begin
	ExecSQL('begin immediate');
  Inc(FTransactionCount);
end;

procedure TSqliteDatabase.Commit;
begin
	ExecSQL('commit');
  Dec(FTransactionCount);
end;

procedure TSqliteDatabase.Rollback;
begin
	ExecSQL('rollback');
  Dec(FTransactionCount);
end;

procedure TSqliteDatabase.RaiseError(const msg: string);
var
	errmsg: string;
begin
	errmsg := sqlite3_errmsg16(FHandle);
	if msg <> '' then
		errmsg := Format('Error: %s%s%s', [errmsg, #13#10#13#10, msg]);
	raise ESQLiteException.Create(errmsg)
end;

function TSqliteDatabase.GetLastInsertRowID: int64;
begin
	result := sqlite3_last_insert_rowid(FHandle);
end;

function TSqliteDatabase.InTransaction: boolean;
begin
  Result := FTransactionCount > 0;
end;

{ TSqliteStatement }

procedure TSqliteStatement.BindBlob(AIndex: integer; AValue: TBytes);
begin
	sqlite3_bind_blob(FStmt, AIndex, @AValue[0], Length(AValue), SQLITE_TRANSIENT)
end;

procedure TSqliteStatement.BindDouble(AIndex: integer; AValue: double);
begin
	sqlite3_bind_double(FStmt, AIndex, AValue);
end;

procedure TSqliteStatement.BindInt64(AIndex: integer; AValue: Int64);
begin
	sqlite3_bind_int64(FStmt, AIndex, AValue);
end;

{$IFDEF NEXTGEN}
function TSqliteStatement.BindParameterIndex(AParamName: string): integer;
var
  M: TMarshaller;
begin
  Result := sqlite3_bind_parameter_index(FStmt, M.AsUtf8(AParamName).ToPointer);
  if Result <= 0 then
    raise ESQLiteException.CreateFmt('Could not bind parameter %s in SQL', [AParamName]);
end;
{$ELSE}
function TSqliteStatement.BindParameterIndex(AParamName: string): integer;
begin
  Result := sqlite3_bind_parameter_index(FStmt, PAnsiChar(UTF8Encode(AParamName)));
  if Result <= 0 then
    raise ESQLiteException.CreateFmt('Could not bind parameter %s in SQL', [AParamName]);
end;
{$ENDIF}

procedure TSqliteStatement.BindText(AIndex: integer; AValue: string);
begin
	sqlite3_bind_text16(FStmt, AIndex, PChar(AValue), Length(AValue) * SizeOf(Char), SQLITE_TRANSIENT);
end;

function TSqliteStatement.ColumnBlob(i: integer): TBytes;
var
  Size: integer;
  Buffer: Pointer;
begin
  Buffer := sqlite3_column_blob(FStmt, i);
  if Buffer = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  Size := sqlite3_column_bytes16(FStmt, i);
  SetLength(Result, Size);
  System.Move(Buffer^, Result[0], Size);
end;

function TSqliteStatement.ColumnDouble(i: integer): double;
begin
  Result := sqlite3_column_double(FStmt, i);
end;

function TSqliteStatement.ColumnIndex(AColumnName: string): integer;
begin
  if FColumnNames = nil then
    CreateColumnNames;
  Result := FColumnNames.IndexOf(AColumnName);
end;

function TSqliteStatement.ColumnInt64(i: integer): Int64;
begin
  Result := sqlite3_column_int64(FStmt, i);
end;

function TSqliteStatement.ColumnIsNull(i: integer): boolean;
begin
  Result := ColumnType(i) = TSQLiteFieldType.stNull;
end;

function TSqliteStatement.ColumnText(i: integer): string;
begin
  Result := sqlite3_column_text16(FStmt, i);
end;

function TSqliteStatement.ColumnType(i: integer): TSQLiteFieldType;
var
  ColumnType: integer;
begin
  ColumnType := sqlite3_column_type(FStmt, i);
  if (ColumnType < 0) or (ColumnType > Ord(High(TSQLiteFieldType))) then
    Result := TSQLiteFieldType.stUnknown
  else
    Result := TSQLiteFieldType(ColumnType);
end;

constructor TSqliteStatement.Create(ADatabase: TSQLiteDatabase; ASQL: string);
var
	IgnoreNextStmt: Pchar;
begin
  FDB := ADatabase;
	if sqlite3_prepare16_v2(ADatabase.FHandle, PChar(ASQL), -1, Fstmt, IgnoreNextStmt) <> 0
		then FDB.RaiseError(ASQL);
end;

procedure TSqliteStatement.CreateColumnNames;
var
  c: Integer;
  ColumnName: string;
  ColumnCount: Integer;
begin
  if FColumnNames = nil then
    FColumnNames := TStringList.Create;
  ColumnCount := sqlite3_column_count(FStmt);
  for c := 0 to ColumnCount - 1 do
  begin
    ColumnName := sqlite3_column_name16(FStmt, c);
    FColumnNames.Add(ColumnName);
  end;
end;

destructor TSqliteStatement.Destroy;
begin
  FColumnNames.Free;
  sqlite3_finalize(FStmt);
  inherited;
end;

procedure TSqliteStatement.Execute;
begin
	Next;
end;

function TSqliteStatement.Next: boolean;
var
  Status: integer;
begin
  Status := sqlite3_step(Fstmt);
  Result := False; // to avoid warnings
  case Status of
    SQLITE_ROW:
      Result := True;
    SQLITE_DONE:
      Result := False;
  else
    FDB.RaiseError('');
  end;
end;

end.

