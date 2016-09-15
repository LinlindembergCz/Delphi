unit Aurelius.Schema.Firebird;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Schema.AbstractImporter,
  Aurelius.Sql.Metadata;

type
  TFirebirdSchemaImporter = class(TAbstractSchemaImporter)
  strict protected
    procedure GetDatabaseMetadata(Connection: IDBConnection; Database: TDatabaseMetadata); override;
  end;

  TFirebirdSchemaRetriever = class(TSchemaRetriever)
  strict private
    procedure GetTables;
    procedure GetColumns;
    procedure GetPrimaryKeys;
    procedure GetUniqueKeys;
    procedure GetForeignKeys;
    procedure GetSequences;
    procedure GetFieldDefinition(Column: TColumnMetadata; AType, ASize,
      APrecision, AScale, ASubType, ASegment: integer; const ACharset: string);
  public
    constructor Create(AConnection: IDBConnection; ADatabase: TDatabaseMetadata); override;
    procedure RetrieveDatabase; override;
  end;

implementation

uses
  SysUtils,
  Aurelius.Schema.Register;

{ TFirebirdSchemaImporter }

procedure TFirebirdSchemaImporter.GetDatabaseMetadata(Connection: IDBConnection;
  Database: TDatabaseMetadata);
var
  Retriever: TSchemaRetriever;
begin
  Retriever := TFirebirdSchemaRetriever.Create(Connection, Database);
  try
    Retriever.RetrieveDatabase;
  finally
    Retriever.Free;
  end;
end;

{ TFirebirdSchemaRetriever }

constructor TFirebirdSchemaRetriever.Create(AConnection: IDBConnection;
  ADatabase: TDatabaseMetadata);
begin
  inherited Create(AConnection, ADatabase);
  TrimStringValues := true;
end;

procedure TFirebirdSchemaRetriever.GetColumns;
begin
  RetrieveColumns(
    'select '+
    'f.rdb$relation_name as TABLE_NAME, '#13#10+
    'f.rdb$field_name as COLUMN_NAME, '#13#10+
    'f.rdb$null_flag as NULL_FLAG, '#13#10+
    'fs.rdb$field_type as DATA_TYPE, '#13#10+
    'COALESCE(fs.RDB$CHARACTER_LENGTH, fs.RDB$FIELD_LENGTH) as FIELD_LENGTH, '#13#10+
    'fs.rdb$field_scale as FIELD_SCALE, '#13#10+
    'fs.rdb$field_precision as FIELD_PRECISION, '#13#10+
    'fs.rdb$field_sub_type as FIELD_SUBTYPE, '#13#10+
    'fs.rdb$segment_length as SEGMENT_LENGTH, '#13#10+
    'cr.rdb$character_set_name as CHARACTER_SET '#13#10+
    'from rdb$relation_fields f '#13#10+
    'left join rdb$fields fs on fs.rdb$field_name = f.rdb$field_source '#13#10+
    'left join rdb$field_dimensions d on d.rdb$field_name = fs.rdb$field_name '#13#10+
    'left join rdb$character_sets cr on fs.rdb$character_set_id = cr.rdb$character_set_id '#13#10+
    'order by f.rdb$relation_name, f.rdb$field_position, d.rdb$dimension',
    procedure (Column: TColumnMetadata; ResultSet: IDBResultSet)
    begin
      Column.NotNull := (AsInteger(ResultSet.GetFieldValue('NULL_FLAG')) = 1);
      GetFieldDefinition(Column,
        AsInteger(ResultSet.GetFieldValue('DATA_TYPE')),
        AsInteger(ResultSet.GetFieldValue('FIELD_LENGTH')),
        AsInteger(ResultSet.GetFieldValue('FIELD_PRECISION')),
        AsInteger(ResultSet.GetFieldValue('FIELD_SCALE')),
        AsInteger(ResultSet.GetFieldValue('FIELD_SUBTYPE')),
        AsInteger(ResultSet.GetFieldValue('SEGMENT_LENGTH')),
        AsString(ResultSet.GetFieldValue('CHARACTER_SET'))
      );
    end
  );
end;

procedure TFirebirdSchemaRetriever.GetFieldDefinition(Column: TColumnMetadata; AType, ASize,
  APrecision, AScale, ASubType, ASegment: integer; const ACharset: string);

  function CheckNumeric: boolean;
  begin
    Result := ASubType in [1, 2];
    if Result then
    begin
      Column.DataType := 'NUMERIC';
      Column.Precision := APrecision;
      Column.Scale := abs(AScale);
    end;
  end;

  function IsNChar: boolean;
  begin
    Result := SameText('ISO8859_1', ACharset);
  end;

const
  fb__blob = 261;
  fb__cstring = 40;
  fb__char = 14;
  fb__dfloat = 11;
  fb__double = 27;
  fb__float = 10;
  fb__int64 = 16;
  fb__integer = 8;
  fb__quad = 9;
  fb__smallint = 7;
  fb__date = 12;
  fb__time = 13;
  fb__timestamp = 35;
  fb__varchar = 37;
begin
  case AType of
    fb__blob, fb__cstring, fb__dfloat, fb__quad:
      begin
        if ASubType = 1 then
          Column.DataType := 'BLOB SUB_TYPE TEXT'
        else
          Column.DataType := 'BLOB';
      end;
    fb__char:
      begin
        Column.Length := ASize;
        if IsNChar then
          Column.DataType := 'NCHAR'
        else
          Column.DataType := 'CHAR'
      end;
    fb__varchar:
      begin
        Column.Length := ASize;
        if IsNChar then
          Column.DataType := 'NCHAR VARYING'
        else
          Column.DataType := 'VARCHAR'
      end;
    fb__smallint:
      if not CheckNumeric then
        Column.DataType := 'SMALLINT';
    fb__integer:
      if not CheckNumeric then
        Column.DataType := 'INTEGER';
    fb__int64:
      if not CheckNumeric then
      begin
        Column.DataType := 'NUMERIC';
        Column.Precision := 18;
      end;
    fb__double, fb__float:
        Column.DataType := 'DOUBLE PRECISION';
    fb__date:
      Column.DataType := 'DATE';
    fb__time:
      Column.DataType := 'TIME';
    fb__timestamp:
      Column.DataType := 'TIMESTAMP';
  end;
end;

procedure TFirebirdSchemaRetriever.GetForeignKeys;
begin
  RetrieveForeignKeys(
    'select '#13#10+
    'C.RDB$RELATION_NAME as PK_TABLE_NAME, '#13#10+
    'A.RDB$RELATION_NAME as FK_TABLE_NAME, '#13#10+
    'D.RDB$FIELD_NAME as PK_COLUMN_NAME, '#13#10+
    'E.RDB$FIELD_NAME as FK_COLUMN_NAME, '#13#10+
    'A.RDB$CONSTRAINT_NAME as CONSTRAINT_NAME '#13#10+
    'from RDB$REF_CONSTRAINTS B, RDB$RELATION_CONSTRAINTS A, RDB$RELATION_CONSTRAINTS C, '#13#10+
    'RDB$INDEX_SEGMENTS D, RDB$INDEX_SEGMENTS E '#13#10+
    'where (A.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') and '#13#10+
    '(A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) and '#13#10+
    '(B.RDB$CONST_NAME_UQ=C.RDB$CONSTRAINT_NAME) and '#13#10+
    '(C.RDB$INDEX_NAME=D.RDB$INDEX_NAME) and '#13#10+
    '(A.RDB$INDEX_NAME=E.RDB$INDEX_NAME) and '#13#10+
    '(D.RDB$FIELD_POSITION = E.RDB$FIELD_POSITION) '#13#10+
    'order by A.RDB$RELATION_NAME, A.RDB$CONSTRAINT_NAME,  '#13#10+
    'D.RDB$FIELD_POSITION, E.RDB$FIELD_POSITION');
end;

procedure TFirebirdSchemaRetriever.GetPrimaryKeys;
begin
  RetrievePrimaryKeys(
    'select rc.rdb$constraint_name as CONSTRAINT_NAME, '#13#10+
    'rc.rdb$relation_name as TABLE_NAME, '#13#10+
    'i.rdb$field_name as COLUMN_NAME, '#13#10+
    'i.rdb$field_position as ORDINAL_POSITION, '#13#10+
    'rc.rdb$index_name '#13#10+
    'from rdb$relation_constraints rc, rdb$index_segments i '#13#10+
    'where (i.rdb$index_name = rc.rdb$index_name) and '#13#10+
    '(rc.rdb$constraint_type = ''PRIMARY KEY'') '#13#10+
    'order by rc.rdb$relation_name, rc.rdb$constraint_name, i.rdb$field_position');
end;

procedure TFirebirdSchemaRetriever.GetSequences;
begin
  RetrieveSequences(
    'select RDB$GENERATOR_NAME AS SEQUENCE_NAME ' +
    'from RDB$GENERATORS ' +
    'where (RDB$SYSTEM_FLAG is NULL) or (RDB$SYSTEM_FLAG <> 1) ' +
    'order by RDB$GENERATOR_NAME');
end;

procedure TFirebirdSchemaRetriever.GetTables;
begin
  RetrieveTables(
    'SELECT RDB$RELATION_NAME AS TABLE_NAME '+
    'FROM RDB$RELATIONS '+
    'WHERE ' +
    'RDB$VIEW_BLR IS NULL AND '+
    '(RDB$SYSTEM_FLAG = 0 OR RDB$SYSTEM_FLAG IS NULL) '+
    'ORDER BY RDB$RELATION_NAME');
end;

procedure TFirebirdSchemaRetriever.GetUniqueKeys;
begin
  RetrieveUniqueKeys(
    'select rc.rdb$constraint_name as CONSTRAINT_NAME, '#13#10+
    'rc.rdb$relation_name as TABLE_NAME, '#13#10+
    'i.rdb$field_name as COLUMN_NAME, '#13#10+
    'i.rdb$field_position as ORDINAL_POSITION, '#13#10+
    'rc.rdb$index_name '#13#10+
    'from rdb$relation_constraints rc, rdb$index_segments i '#13#10+
    'where (i.rdb$index_name = rc.rdb$index_name) and '#13#10+
    '(rc.rdb$constraint_type = ''UNIQUE'') '#13#10+
    'order by rc.rdb$relation_name, rc.rdb$constraint_name, i.rdb$field_position');
end;

procedure TFirebirdSchemaRetriever.RetrieveDatabase;
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
  TSchemaImporterRegister.GetInstance.RegisterImporter('Firebird', TFirebirdSchemaImporter.Create);

end.

