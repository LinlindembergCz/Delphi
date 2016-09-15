unit Aurelius.Schema.PostgreSQL;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Schema.AbstractImporter,
  Aurelius.Sql.Metadata;

type
  TPostgreSQLSchemaImporter = class(TAbstractSchemaImporter)
  strict protected
    procedure GetDatabaseMetadata(Connection: IDBConnection; Database: TDatabaseMetadata); override;
  end;

  TPostgreSQLSchemaRetriever = class(TSchemaRetriever)
  strict private
    procedure GetTables;
    procedure GetColumns;
    procedure GetPrimaryKeys;
    procedure GetUniqueKeys;
    procedure GetForeignKeys;
    procedure GetSequences;
    procedure GetFieldDefinition(Column: TColumnMetadata; ADataType: string;
      ASize, APrecision, AScale: integer);
  public
    constructor Create(AConnection: IDBConnection; ADatabase: TDatabaseMetadata); override;
    procedure RetrieveDatabase; override;
  end;

implementation

uses
  SysUtils,
  Aurelius.Schema.Register;

{ TPostgreSQLSchemaImporter }

procedure TPostgreSQLSchemaImporter.GetDatabaseMetadata(Connection: IDBConnection;
  Database: TDatabaseMetadata);
var
  Retriever: TSchemaRetriever;
begin
  Retriever := TPostgreSQLSchemaRetriever.Create(Connection, Database);
  try
    Retriever.RetrieveDatabase;
  finally
    Retriever.Free;
  end;
end;

{ TPostgreSQLSchemaRetriever }

constructor TPostgreSQLSchemaRetriever.Create(AConnection: IDBConnection;
  ADatabase: TDatabaseMetadata);
begin
  inherited Create(AConnection, ADatabase);
end;

procedure TPostgreSQLSchemaRetriever.GetColumns;
begin
  RetrieveColumns(
    'SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, '+
    'NUMERIC_PRECISION, NUMERIC_SCALE, IS_NULLABLE '+
    'FROM INFORMATION_SCHEMA.COLUMNS '+
    'WHERE TABLE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY TABLE_NAME, ORDINAL_POSITION'
    ,
    procedure (Column: TColumnMetadata; ResultSet: IDBResultSet)
    begin
      Column.NotNull := (AsString(ResultSet.GetFieldValue('IS_NULLABLE')) = 'NO');
      GetFieldDefinition(Column,
        AsString(ResultSet.GetFieldValue('DATA_TYPE')),
        AsInteger(ResultSet.GetFieldValue('CHARACTER_MAXIMUM_LENGTH')),
        AsInteger(ResultSet.GetFieldValue('NUMERIC_PRECISION')),
        AsInteger(ResultSet.GetFieldValue('NUMERIC_SCALE'))
      );
    end
  );
end;

procedure TPostgreSQLSchemaRetriever.GetFieldDefinition(Column: TColumnMetadata;
  ADataType: string; ASize, APrecision, AScale: integer);

const
  vNoSizeTypes : array[0..28] of string =
     ('bigint', 'bigserial', 'boolean', 'box', 'bytea', 'cidr', 'circle', 'date', 'double precision',
      'inet', 'integer', 'line', 'lseg', 'macaddr', 'money', 'path', 'point', 'polygon', 'real',
      'smallint', 'smallserial', 'serial', 'text', 'tsquery', 'tsvector', 'txid_snapshot', 'uuid',
      'xml', 'json');

  function NeedSize: Boolean;
  var
    I: Integer;
  begin
    for i := 0 to high(vNoSizeTypes) do
      if vNoSizeTypes[I] = ADataType then
        Exit(false);
    Result := true;
  end;

begin
  ADataType := Trim(LowerCase(ADataType));
  if ADataType =  'character varying' then
    ADataType := 'varchar'
  else
  if ADataType =  'character' then
    ADataType := 'char'
  else
  if ADataType = 'timestamp without time zone' then
    ADataType := 'timestamp'
  else
  if ADataType = 'time without time zone' then
    ADataType := 'time';


  Column.DataType := ADataType;

  if NeedSize then
  begin
    if (ADataType = 'char') or (ADataType = 'varchar')
      or (ADataType = 'bit') or (ADataType = 'bit varying') then
      Column.Length := ASize
    else
    begin
      Column.Precision := APrecision;
      Column.Scale := AScale;
    end;
  end;
end;

procedure TPostgreSQLSchemaRetriever.GetForeignKeys;
begin
  RetrieveForeignKeys(
    'SELECT '+
    '  TC.constraint_name, '+
    '  TC.table_name as FK_TABLE_NAME, '+
    '  KP.table_name as PK_TABLE_NAME, '+
    '  KC.column_name as FK_COLUMN_NAME, '+
    '  KP.column_name as PK_COLUMN_NAME '+
    'FROM '+
    '  information_schema.table_constraints TC, '+
    '  information_schema.referential_constraints RC, '+
    '  information_schema.key_column_usage KC, '+
    '  information_schema.key_column_usage KP '+
    'WHERE '+
    '  TC.constraint_name = RC.constraint_name AND '+
    '  TC.constraint_schema = RC.constraint_schema AND '+
    '  RC.constraint_schema = KC.constraint_schema AND '+
    '  RC.constraint_name = KC.constraint_name AND '+
    '  RC.unique_constraint_name = KP.constraint_name AND '+
    '  RC.unique_constraint_schema = KP.constraint_schema AND '+
    '  KC.table_name = TC.table_name AND '+
    '  KC.position_in_unique_constraint = KP.ordinal_position '+
    '  AND TC.TABLE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY '+
    '  TC.table_name, '+
    '  TC.constraint_name, '+
    '  KC.ordinal_position '
  );
end;

procedure TPostgreSQLSchemaRetriever.GetPrimaryKeys;
begin
  RetrievePrimaryKeys(
    'SELECT C.CONSTRAINT_NAME, C.TABLE_NAME, K.COLUMN_NAME, K.ORDINAL_POSITION '+
    'FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C, INFORMATION_SCHEMA.KEY_COLUMN_USAGE K '+
    'WHERE C.CONSTRAINT_TYPE=''PRIMARY KEY'' '+
    'AND C.TABLE_NAME = K.TABLE_NAME AND C.CONSTRAINT_NAME = K.CONSTRAINT_NAME '+
    'AND C.TABLE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY C.TABLE_NAME, C.CONSTRAINT_NAME, K.ORDINAL_POSITION'
  );
end;

procedure TPostgreSQLSchemaRetriever.GetSequences;
begin
  RetrieveSequences(
    'SELECT SEQUENCE_NAME '+
    'FROM INFORMATION_SCHEMA.SEQUENCES '+
    'WHERE SEQUENCE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY SEQUENCE_NAME'
  );
end;

procedure TPostgreSQLSchemaRetriever.GetTables;
begin
  RetrieveTables(
    'SELECT TABLE_NAME '+
    'FROM INFORMATION_SCHEMA.TABLES '+
    'WHERE TABLE_TYPE=''BASE TABLE'' '+
    'AND TABLE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY TABLE_NAME'
  );
end;

procedure TPostgreSQLSchemaRetriever.GetUniqueKeys;
begin
  RetrieveUniqueKeys(
    'SELECT C.CONSTRAINT_NAME, C.TABLE_NAME, K.COLUMN_NAME, K.ORDINAL_POSITION '+
    'FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C, INFORMATION_SCHEMA.KEY_COLUMN_USAGE K '+
    'WHERE C.CONSTRAINT_TYPE=''UNIQUE'' '+
    'AND C.TABLE_NAME = K.TABLE_NAME AND C.CONSTRAINT_NAME = K.CONSTRAINT_NAME '+
    'AND C.TABLE_SCHEMA NOT IN (''pg_catalog'', ''information_schema'') '+
    'ORDER BY C.TABLE_NAME, C.CONSTRAINT_NAME, K.ORDINAL_POSITION'
  );
end;

procedure TPostgreSQLSchemaRetriever.RetrieveDatabase;
begin
  Database.Clear;
  GetTables;
  GetColumns;
  GetPrimaryKeys;
  GetUniqueKeys;
  GetForeignKeys;
  GetSequences;
end;

initialization
  TSchemaImporterRegister.GetInstance.RegisterImporter('PostgreSQL', TPostgreSQLSchemaImporter.Create);

end.

