unit Aurelius.Schema.Oracle;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Schema.AbstractImporter,
  Aurelius.Sql.Metadata;

type
  TOracleSchemaImporter = class(TAbstractSchemaImporter)
  strict protected
    procedure GetDatabaseMetadata(Connection: IDBConnection; Database: TDatabaseMetadata); override;
  end;

  TOracleSchemaRetriever = class(TSchemaRetriever)
  strict private
    procedure GetTables;
    procedure GetColumns;
    procedure GetPrimaryKeys;
    procedure GetUniqueKeys;
    procedure GetForeignKeys;
    procedure GetSequences;
    procedure GetFieldDefinition(Column: TColumnMetadata; const ADataType: string;
      ASize, APrecision, AScale: integer);
  public
    constructor Create(AConnection: IDBConnection; ADatabase: TDatabaseMetadata); override;
    procedure RetrieveDatabase; override;
  end;

implementation

uses
  SysUtils,
  Aurelius.Schema.Register;

{ TOracleSchemaImporter }

procedure TOracleSchemaImporter.GetDatabaseMetadata(Connection: IDBConnection;
  Database: TDatabaseMetadata);
var
  Retriever: TSchemaRetriever;
begin
  Retriever := TOracleSchemaRetriever.Create(Connection, Database);
  try
    Retriever.RetrieveDatabase;
  finally
    Retriever.Free;
  end;
end;

{ TOracleSchemaRetriever }

constructor TOracleSchemaRetriever.Create(AConnection: IDBConnection;
  ADatabase: TDatabaseMetadata);
begin
  inherited Create(AConnection, ADatabase);
end;

procedure TOracleSchemaRetriever.GetColumns;
begin
  RetrieveColumns(
      'SELECT C.TABLE_NAME, C.COLUMN_NAME, C.DATA_TYPE, C.CHAR_COL_DECL_LENGTH, '+
      'C.DATA_PRECISION, C.DATA_SCALE, C.NULLABLE '+
      'FROM USER_TAB_COLS C '+
      'ORDER BY C.TABLE_NAME, C.COLUMN_ID',
    procedure (Column: TColumnMetadata; ResultSet: IDBResultSet)
    begin
      Column.NotNull := (AsString(ResultSet.GetFieldValue('NULLABLE')) = 'N');
      GetFieldDefinition(Column,
        AsString(ResultSet.GetFieldValue('DATA_TYPE')),
        AsInteger(ResultSet.GetFieldValue('CHAR_COL_DECL_LENGTH')),
        AsInteger(ResultSet.GetFieldValue('DATA_PRECISION')),
        AsInteger(ResultSet.GetFieldValue('DATA_SCALE'))
      );
    end
  );
end;

procedure TOracleSchemaRetriever.GetFieldDefinition(Column: TColumnMetadata;
  const ADataType: string; ASize, APrecision, AScale: integer);

const
  vSizeTypes: array[0..5] of string =
    ('char', 'nchar', 'nvarchar2', 'raw', 'urowid', 'varchar2');
  vPrecisionTypes: array[0..3] of string =
    ('decimal', 'number', 'numeric', 'float');

  function FindType(ATypes: array of string): boolean;
  var
    I: integer;
  begin
    for I := Low(ATypes) to high(ATypes) do
      if SameText(ADataType, ATypes[I]) then
        Exit(true);
    Result := False;
  end;

begin
  Column.DataType := ADataType;
  if FindType(vSizeTypes) then
    Column.Length := ASize
  else
  if FindType(vPrecisionTypes) then
  begin
    Column.Precision := APrecision;
    Column.Scale := AScale;
  end;
end;

procedure TOracleSchemaRetriever.GetForeignKeys;
begin
  RetrieveForeignKeys(
    'SELECT C.CONSTRAINT_NAME, C.TABLE_NAME AS FK_TABLE_NAME, Cpar.TABLE_NAME AS PK_TABLE_NAME, '+
    'C.R_CONSTRAINT_NAME, F.COLUMN_NAME AS FK_COLUMN_NAME, Fpar.COLUMN_NAME AS PK_COLUMN_NAME '+
    'FROM USER_CONSTRAINTS C '+
    'INNER JOIN USER_CONSTRAINTS Cpar ON (C.R_OWNER = Cpar.OWNER AND C.R_CONSTRAINT_NAME = Cpar.CONSTRAINT_NAME) '+
    'INNER JOIN USER_CONS_COLUMNS F ON (C.OWNER = F.OWNER AND C.CONSTRAINT_NAME = F.CONSTRAINT_NAME) '+
    'INNER JOIN USER_CONS_COLUMNS Fpar ON (C.R_OWNER = Fpar.OWNER AND C.R_CONSTRAINT_NAME = Fpar.CONSTRAINT_NAME) '+
    'WHERE C.CONSTRAINT_TYPE=''R'' AND F.POSITION = Fpar.POSITION '+
    'ORDER BY C.CONSTRAINT_NAME, F.POSITION, Fpar.POSITION'
  );
end;

procedure TOracleSchemaRetriever.GetPrimaryKeys;
begin
  RetrievePrimaryKeys(
    'SELECT CO.TABLE_NAME, CO.CONSTRAINT_NAME, CC.COLUMN_NAME, CC.POSITION '+
    'FROM USER_CONSTRAINTS CO ' +
    'INNER JOIN USER_CONS_COLUMNS CC ON (CO.CONSTRAINT_NAME = CC.CONSTRAINT_NAME AND CO.OWNER = CC.OWNER AND CO.TABLE_NAME = CC.TABLE_NAME) '+
    'WHERE CO.CONSTRAINT_TYPE=''P'' '+
    'ORDER BY CO.TABLE_NAME, CO.CONSTRAINT_NAME, CC.POSITION'
  );
end;

procedure TOracleSchemaRetriever.GetSequences;
begin
  RetrieveSequences(
    'SELECT SEQUENCE_NAME '+
    'FROM USER_SEQUENCES '+
    'ORDER BY SEQUENCE_NAME'
  );
end;

procedure TOracleSchemaRetriever.GetTables;
begin
  RetrieveTables(
    'SELECT T.TABLE_NAME '+
    'FROM USER_TABLES T '+
    'WHERE T.DROPPED <> ''YES'' AND NOT (T.TABLE_NAME LIKE ''%$%'') '+
    'ORDER BY T.TABLE_NAME'
  );
end;

procedure TOracleSchemaRetriever.GetUniqueKeys;
begin
  RetrieveUniqueKeys(
    'SELECT CO.TABLE_NAME, CO.CONSTRAINT_NAME, CC.COLUMN_NAME, CC.POSITION '+
    'FROM USER_CONSTRAINTS CO ' +
    'INNER JOIN USER_CONS_COLUMNS CC ON (CO.CONSTRAINT_NAME = CC.CONSTRAINT_NAME AND CO.OWNER = CC.OWNER AND CO.TABLE_NAME = CC.TABLE_NAME) '+
    'WHERE CO.CONSTRAINT_TYPE=''U'' '+
    'ORDER BY CO.TABLE_NAME, CO.CONSTRAINT_NAME, CC.POSITION'
  );
end;

procedure TOracleSchemaRetriever.RetrieveDatabase;
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
  TSchemaImporterRegister.GetInstance.RegisterImporter('Oracle', TOracleSchemaImporter.Create);

end.

