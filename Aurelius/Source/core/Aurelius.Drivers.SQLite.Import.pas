unit Aurelius.Drivers.SQLite.Import;

{$I Aurelius.inc}

interface

const
{$IFDEF MSWINDOWS}
  SQLiteDLLName = 'sqlite3.dll';
{$ENDIF}
{$IFDEF MACOS}
  SQLiteDLLName = 'libsqlite3.dylib';
{$ENDIF}

const
  SQLITE_OK = 0;
  SQLITE_ROW = 100;
	SQLITE_DONE = 101;

  SQLITE_TRANSIENT = Pointer(-1);

var
  sqlite3_open16: function(filename: pchar; var db: pointer): integer; cdecl;
  sqlite3_close: function(db: pointer): integer; cdecl;
  sqlite3_prepare16_v2: function(db: pointer; sql: pchar; nBytes: integer; var stmt: pointer; var ztail: pchar): integer; cdecl;
  sqlite3_step: function(stmt: pointer): integer; cdecl;
  sqlite3_finalize: function(stmt: pointer): integer; cdecl;
  sqlite3_errmsg16: function(db: pointer): pchar; cdecl;
  sqlite3_errcode: function(db: pointer): integer; cdecl;
  sqlite3_last_insert_rowid: function(db: pointer): int64; cdecl;

  sqlite3_column_count: function(stmt: pointer): integer; cdecl;
  sqlite3_column_name16: function(stmt: pointer; ColNum: integer): pchar; cdecl;
  sqlite3_column_type: function(stmt: pointer; col: integer): integer; cdecl;
  sqlite3_column_bytes16: function(stmt: pointer; col: integer): integer; cdecl;
  sqlite3_column_blob: function(stmt: pointer; col: integer): pointer; cdecl;
  sqlite3_column_double: function(stmt: pointer; col: integer): double; cdecl;
  sqlite3_column_int: function(stmt: pointer; col: integer): integer; cdecl;
  sqlite3_column_int64: function(stmt: pointer; col: integer): Int64; cdecl;
  sqlite3_column_text16: function(stmt: pointer; col: integer): pchar; cdecl;

  sqlite3_bind_blob: function(stmt: pointer; param: integer; blob: pointer; size: integer; freeproc: pointer): integer; cdecl;
  sqlite3_bind_text16: function(stmt: pointer; param: integer; text: PChar; size: integer; freeproc: pointer): integer; cdecl;
  sqlite3_bind_int64: function(stmt: pointer; param: integer; value: int64): integer; cdecl;
  sqlite3_bind_double: function(stmt: pointer; param: integer; value: double): integer; cdecl;
  sqlite3_bind_null: function(stmt: pointer; param: integer): integer; cdecl;

  {$IFDEF NEXTGEN}
  sqlite3_bind_parameter_index: function(stmt: pointer; paramname: MarshaledAString): integer; cdecl;
  sqlite3_bind_parameter_name: function(stmt: pointer; paramindex: integer): MarshaledAString; cdecl;
  {$ELSE}
  sqlite3_bind_parameter_index: function(stmt: pointer; paramname: PAnsiChar): integer; cdecl;
  sqlite3_bind_parameter_name: function(stmt: pointer; paramindex: integer): PAnsiChar; cdecl;
  {$ENDIF}

function LoadSQLiteLibrary: boolean;

implementation
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils;

var
  LibraryHandle: THandle;

function LoadSQLiteLibrary: boolean;

  function LoadProc(ProcName: string): Pointer;
  begin
    Result := GetProcAddress(LibraryHandle, PChar(ProcName));
    Assert(Assigned(Result), SQLiteDLLName + ' - Could not find method: ' + ProcName);
  end;

begin
  if LibraryHandle <> 0 then
    Exit(True);

  Result := False;
  LibraryHandle := SafeLoadLibrary(PChar(SQLiteDLLName));
  if (LibraryHandle <> 0) then
  begin
    Result := True;

    sqlite3_open16 := LoadProc('sqlite3_open16');
    sqlite3_close := LoadProc('sqlite3_close');
    sqlite3_prepare16_v2 := LoadProc('sqlite3_prepare16_v2');
    sqlite3_errmsg16 := LoadProc('sqlite3_errmsg16');
    sqlite3_errcode := LoadProc('sqlite3_errcode');
    sqlite3_step := LoadProc('sqlite3_step');
    sqlite3_finalize := LoadProc('sqlite3_finalize');
    sqlite3_last_insert_rowid := LoadProc('sqlite3_last_insert_rowid');

    sqlite3_column_count := LoadProc('sqlite3_column_count');
    sqlite3_column_name16 := LoadProc('sqlite3_column_name16');
    sqlite3_column_type := LoadProc('sqlite3_column_type');
    sqlite3_column_bytes16 := LoadProc('sqlite3_column_bytes16');
    sqlite3_column_blob := LoadProc('sqlite3_column_blob');
    sqlite3_column_double := LoadProc('sqlite3_column_double');
    sqlite3_column_int := LoadProc('sqlite3_column_int');
    sqlite3_column_int64 := LoadProc('sqlite3_column_int64');
    sqlite3_column_text16 := LoadProc('sqlite3_column_text16');

    sqlite3_bind_blob := LoadProc('sqlite3_bind_blob');
    sqlite3_bind_text16 := LoadProc('sqlite3_bind_text16');
    sqlite3_bind_double := LoadProc('sqlite3_bind_double');
    sqlite3_bind_int64 := LoadProc('sqlite3_bind_int64');
    sqlite3_bind_null := LoadProc('sqlite3_bind_null');
    sqlite3_bind_parameter_index := LoadProc('sqlite3_bind_parameter_index');
    sqlite3_bind_parameter_name := LoadProc('sqlite3_bind_parameter_name');
  end;
end;

Initialization
  LibraryHandle := 0;

finalization
  if LibraryHandle <> 0 then
    FreeLibrary(LibraryHandle);

end.

