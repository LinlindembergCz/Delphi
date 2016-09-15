unit DBConnection;

{.$DEFINE DBEXPRESS} // Not available in Delphi Professional
{$DEFINE DBGO}
{$DEFINE SQLITE}

interface

{$IFNDEF MSWINDOWS}
  {$UNDEF DBGO}
{$ENDIF}

{$IFNDEF WIN32}
  {$UNDEF SQLITE}
{$ENDIF}

uses
  Generics.Collections, Classes, IniFiles,
  Aurelius.Commands.Listeners,
  Aurelius.Drivers.Interfaces,
  Aurelius.Engine.AbstractManager,
  Aurelius.Engine.ObjectManager,
  Aurelius.Engine.DatabaseManager,

  {$IFDEF DBEXPRESS}
    Aurelius.Drivers.dbExpress,
    SqlExpr,
    DBXCommon,
    DbxFirebird,
    DbxInterbase,
    DbxMySQL,
    DbxOracle,
    {$IFDEF MSWINDOWS}
    DbxMSSQL,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DBGO}
    Aurelius.Drivers.DbGo,
    ADODB,
  {$ENDIF}

  {$IFDEF SQLITE}
    Aurelius.Drivers.SQLite,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Aurelius.Sql.MSSQL,
  {$ENDIF}
  Aurelius.Sql.DB2,
  Aurelius.Sql.Firebird,
  Aurelius.Sql.Interbase,
  Aurelius.Sql.MySql,
  Aurelius.Sql.Oracle,
  Aurelius.Sql.PostgreSQL,
  Aurelius.Sql.SQLite;


type
  TDBConnection = class sealed
  private
    class var FInstance: TDBConnection;
  private
    FConnection: IDBConnection;
    FListeners: TList<ICommandExecutionListener>;
    procedure PrivateCreate;
    procedure PrivateDestroy;

    function ConnectionFileName: string;
    function CreateConnectionFromIniFile: IDBConnection;
    {$IFDEF DBEXPRESS}
    function CreateDbExpressAdapterFromIniFile(AIniFile: TMemIniFile): IDBConnection;
    {$ENDIF}
    {$IFDEF DBGO}
    function CreateDbGoAdapterFromIniFile(AIniFile: TMemIniFile): IDBConnection;
    {$ENDIF}
    {$IFDEF SQLITE}
    function CreateSQLiteAdapterFromIniFile(AIniFile: TMemIniFile): IDBConnection;
    {$ENDIF}

    function CreateConnection: IDBConnection;
    function GetConnection: IDBConnection;
    procedure AddListeners(AManager: TAbstractManager);
  public
    class function GetInstance: TDBConnection;
    procedure AddCommandListener(Listener: ICommandExecutionListener);
    class procedure AddLines(List: TStrings; SQL: string; Params: TEnumerable<TDBParam>);

    property Connection: IDBConnection read GetConnection;
    function HasConnection: boolean;
    function CreateObjectManager: TObjectManager;
    function GetNewDatabaseManager: TDatabaseManager;
    procedure UnloadConnection;

    procedure SaveDbExpressSettings(AConnectionName: string);
    procedure GetDbExpressConnections(AItems: TStrings);
    function IsDbExpressSupported: boolean;

    procedure SaveDbGoSettings(AConnectionString: string);
    function EditDbGoConnectionString(ACurrentString: string): string;
    function IsDbGoSupported: boolean;

    procedure SaveSQLiteSettings(ASQLiteFile: string);
    function DefaultSQLiteDatabase: string;
    function IsSQLiteSupported: boolean;
  end;

implementation
uses
  Variants, DB, SysUtils, TypInfo;

{ TConexaoUnica }

procedure TDBConnection.AddCommandListener(
  Listener: ICommandExecutionListener);
begin
  FListeners.Add(Listener);
end;

class procedure TDBConnection.AddLines(List: TStrings; SQL: string;
  Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  ValueAsString: string;
  HasParams: Boolean;
begin
  List.Add(SQL);

  if Params <> nil then
  begin
    HasParams := False;
    for P in Params do
    begin
      if not HasParams then
      begin
        List.Add('');
        HasParams := True;
      end;

      if P.ParamValue = Variants.Null then
        ValueAsString := 'NULL'
      else
      if P.ParamType = ftDateTime then
        ValueAsString := '"' + DateTimeToStr(P.ParamValue) + '"'
      else
      if P.ParamType = ftDate then
        ValueAsString := '"' + DateToStr(P.ParamValue) + '"'
      else
        ValueAsString := '"' + VarToStr(P.ParamValue) + '"';

      List.Add(P.ParamName + ' = ' + ValueAsString + ' (' +
        GetEnumName(TypeInfo(TFieldType), Ord(P.ParamType)) + ')');
    end;
  end;

  List.Add('');
  List.Add('================================================');
end;

procedure TDBConnection.AddListeners(AManager: TAbstractManager);
var
  Listener: ICommandExecutionListener;
begin
  for Listener in FListeners do
    AManager.AddCommandListener(Listener);
end;

function TDBConnection.ConnectionFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'conn.ini';
end;

function TDBConnection.CreateConnectionFromIniFile: IDBConnection;
var
  IniFile: TMemIniFile;
  AureliusDriver: string;
begin
  Result := nil;
  if not FileExists(ConnectionFileName) then
    Exit;

  IniFile := TMemIniFile.Create(ConnectionFileName);
  try
    AureliusDriver := LowerCase(IniFile.ReadString('Config', 'AureliusDriver', ''));
    {$IFDEF DBEXPRESS}
    if (AureliusDriver = 'dbexpress') then
      Result := CreateDbExpressAdapterFromIniFile(IniFile);
    {$ENDIF}
    {$IFDEF DBGO}
    if (AureliusDriver = 'dbgo') then
      Result := CreateDbGoAdapterFromIniFile(IniFile);
    {$ENDIF}
    {$IFDEF SQLITE}
    if (AureliusDriver = 'sqlite') then
      Result := CreateSQLiteAdapterFromIniFile(IniFile);
    {$ENDIF}
  finally
    IniFile.Free;
  end;
end;

{$IFDEF DBGO}
function TDBConnection.CreateDbGoAdapterFromIniFile(AIniFile: TMemIniFile): IDBConnection;
var
  Conn: TADOConnection;
  ConnectionString: string;
begin
  Result := nil;
  ConnectionString := AIniFile.ReadString('Config', 'ConnectionString', '');
  Conn := TADOConnection.Create(nil);
  Conn.ConnectionString := ConnectionString;
  Conn.LoginPrompt := false;
  Result := TDbGoConnectionAdapter.Create(Conn, True);
end;
{$ENDIF}

{$IFDEF DBEXPRESS}
function TDBConnection.CreateDbExpressAdapterFromIniFile(AIniFile: TMemIniFile): IDBConnection;
var
  Conn: TSQLConnection;
  ConnectionName: string;
  dbxDriver: TDBXDelegateDriver;
begin
  Result := nil;
  ConnectionName := AIniFile.ReadString('Config', 'ConnectionName', '');
  dbxDriver := TDBXConnectionFactory.GetConnectionFactory.GetConnectionDriver(ConnectionName);
  if dbxDriver <> nil then
  begin
    try
      Conn := TSQLConnection.Create(nil);
      Conn.ConnectionName := ConnectionName;
      Conn.DriverName := dbxDriver.DriverName;
      Conn.VendorLib := dbxDriver.DriverProperties[TDBXPropertyNames.VendorLib];
      Conn.LibraryName := dbxDriver.DriverProperties[TDBXPropertyNames.LibraryName];
      Conn.GetDriverFunc := dbxDriver.DriverProperties[TDBXPropertyNames.GetDriverFunc];
      Conn.LoadParamsFromIniFile;
      Conn.LoginPrompt := False;
      Conn.SQLHourGlass := True;
      Result := TDBExpressConnectionAdapter.Create(Conn, True);
    finally
      dbxDriver.Free;
    end;
  end;
end;
{$ENDIF}

procedure TDBConnection.UnloadConnection;
begin
  if FConnection <> nil then
  begin
    FConnection.Disconnect;
    FConnection := nil;
  end;
end;

function TDBConnection.CreateConnection: IDBConnection;
begin
  if FConnection <> nil then
    Exit(FConnection);

//  FConnection := CreateCustomConnection;

  if FConnection = nil then
    FConnection := CreateConnectionFromIniFile;
  if FConnection = nil then
    Exit;
  Result := FConnection;
end;

function TDBConnection.GetConnection: IDBConnection;
begin
  Result := CreateConnection;
  if Result = nil then
    raise Exception.Create('Invalid connection settings. Cannot connect to database.');
  if not Result.IsConnected then
    Result.Connect;
end;

procedure TDBConnection.GetDbExpressConnections(AItems: TStrings);
begin
  {$IFDEF DBEXPRESS}
  TDBXConnectionFactory.GetConnectionFactory.GetConnectionItems(AItems);
  {$ENDIF}
end;

class function TDBConnection.GetInstance: TDBConnection;
begin
  if FInstance = nil then
  begin
    FInstance := TDBConnection.Create;
    FInstance.PrivateCreate;
  end;
  Result := FInstance;
end;

function TDBConnection.GetNewDatabaseManager: TDatabaseManager;
begin
  Result := TDatabaseManager.Create(Connection);
  AddListeners(Result);
end;

function TDBConnection.CreateObjectManager: TObjectManager;
begin
  Result := TObjectManager.Create(Connection);
  Result.OwnsObjects := True;
  AddListeners(Result);
end;

{$IFDEF SQLITE}
function TDBConnection.CreateSQLiteAdapterFromIniFile(
  AIniFile: TMemIniFile): IDBConnection;
var
  SQLiteFile: string;
begin
  Result := nil;
  SQLiteFile := AIniFile.ReadString('Config', 'SQLiteFile', '');
  Result := TSQLiteNativeConnectionAdapter.Create(SQLiteFile);
end;
{$ENDIF}

function TDBConnection.DefaultSQLiteDatabase: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'aurelius.sqlite';
end;

function TDBConnection.EditDbGoConnectionString(ACurrentString: string): string;
begin
  {$IFDEF DBGO}
  Result := PromptDataSource(0, ACurrentString);
  {$ENDIF}
end;

function TDBConnection.HasConnection: boolean;
begin
  Result := CreateConnection <> nil;
end;

function TDBConnection.IsDbExpressSupported: boolean;
begin
  {$IFDEF DBEXPRESS}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function TDBConnection.IsDbGoSupported: boolean;
begin
  {$IFDEF DBGO}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function TDBConnection.IsSQLiteSupported: boolean;
begin
  {$IFDEF SQLITE}
  Result := true;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

procedure TDBConnection.PrivateCreate;
begin
  FListeners := TList<ICommandExecutionListener>.Create;
end;

procedure TDBConnection.PrivateDestroy;
begin
  UnloadConnection;
  FListeners.Free;
end;

procedure TDBConnection.SaveDbExpressSettings(AConnectionName: string);
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(ConnectionFileName);
  try
    IniFile.WriteString('Config', 'AureliusDriver', 'dbExpress');
    IniFile.WriteString('Config', 'ConnectionName', AConnectionName);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

procedure TDBConnection.SaveDbGoSettings(AConnectionString: string);
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(ConnectionFileName);
  try
    IniFile.WriteString('Config', 'AureliusDriver', 'dbGo');
    IniFile.WriteString('Config', 'ConnectionString', AConnectionString);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

procedure TDBConnection.SaveSQLiteSettings(ASQLiteFile: string);
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(ConnectionFileName);
  try
    IniFile.WriteString('Config', 'AureliusDriver', 'SQLite');
    IniFile.WriteString('Config', 'SQLiteFile', ASQLiteFile);
    IniFile.UpdateFile;
  finally
    IniFile.Free;
  end;
end;

initialization

finalization
  if TDBConnection.FInstance <> nil then
  begin
    TDBConnection.FInstance.PrivateDestroy;
    TDBConnection.FInstance.Free;
    TDBConnection.FInstance := nil;
  end;

end.
