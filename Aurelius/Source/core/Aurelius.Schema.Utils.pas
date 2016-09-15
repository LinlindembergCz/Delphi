unit Aurelius.Schema.Utils;

{$I Aurelius.Inc}

interface

uses
  Generics.Collections,
  Aurelius.Sql.Metadata;

type

  TSchemaUtils = class
  public
    class function GetReferencingForeignKeys(Database: TDatabaseMetadata; ReferencedTable: TTableMetadata): TArray<TForeignKeyMetadata>;
    class function FindTable(Database: TDatabaseMetadata; const TableName, TableSchema: string): TTableMetadata;
    class function GetTable(Database: TDatabaseMetadata; const TableName, TableSchema: string): TTableMetadata;
    class function FindSequence(Database: TDatabaseMetadata; const SequenceName: string): TSequenceMetadata;
    class function FindColumn(Table: TTableMetadata; const ColumnName: string): TColumnMetadata;
    class function GetColumn(Table: TTableMetadata; const ColumnName: string): TColumnMetadata;
    class function FindForeignKey(Table: TTableMetadata; const ForeignKeyName: string): TForeignKeyMetadata;
    class function FindUniqueKey(Table: TTableMetadata; const UniqueKeyName: string): TUniqueKeyMetadata;
  end;

implementation

uses
  SysUtils,
  Aurelius.Schema.Exceptions;

{ TSchemaUtils }

class function TSchemaUtils.FindColumn(Table: TTableMetadata;
  const ColumnName: string): TColumnMetadata;
var
  Column: TColumnMetadata;
begin
  for Column in Table.Columns do
    if SameText(Column.Name, ColumnName) then
      Exit(Column);
  Result := nil;
end;

class function TSchemaUtils.FindForeignKey(Table: TTableMetadata;
  const ForeignKeyName: string): TForeignKeyMetadata;
var
  ForeignKey: TForeignKeyMetadata;
begin
  for ForeignKey in Table.ForeignKeys do
    if SameText(ForeignKey.Name, ForeignKeyName) then
      Exit(ForeignKey);
  Result := nil;
end;

class function TSchemaUtils.FindSequence(Database: TDatabaseMetadata;
  const SequenceName: string): TSequenceMetadata;
var
  Sequence: TSequenceMetadata;
begin
  for Sequence in Database.Sequences do
    if SameText(Sequence.Name, SequenceName) then
      Exit(Sequence);
  Result := nil;
end;

class function TSchemaUtils.FindTable(Database: TDatabaseMetadata;
  const TableName, TableSchema: string): TTableMetadata;
var
  Table: TTableMetadata;
begin
  for Table in Database.Tables do
    if SameText(Table.Name, TableName) and SameText(Table.Schema, TableSchema) then
      Exit(Table);
  Result := nil;
end;

class function TSchemaUtils.FindUniqueKey(Table: TTableMetadata;
  const UniqueKeyName: string): TUniqueKeyMetadata;
var
  UniqueKey: TUniqueKeyMetadata;
begin
  for UniqueKey in Table.UniqueKeys do
    if SameText(UniqueKey.Name, UniqueKeyName) then
      Exit(UniqueKey);
  Result := nil;
end;

class function TSchemaUtils.GetColumn(Table: TTableMetadata; const ColumnName: string): TColumnMetadata;
begin
  Result := FindColumn(Table, ColumnName);
  if Result = nil then
    raise EColumnMetadataNotFound.Create(ColumnName, Table.Name);
end;

class function TSchemaUtils.GetReferencingForeignKeys(Database: TDatabaseMetadata;
  ReferencedTable: TTableMetadata): TArray<TForeignKeyMetadata>;
var
  Table: TTableMetadata;
  ForeignKey: TForeignKeyMetadata;
  ForeignKeys: TList<TForeignKeyMetadata>;
  {$IFNDEF DELPHIXE_LVL}
  I: integer;
  {$ENDIF}
begin
  ForeignKeys := TList<TForeignKeyMetadata>.Create;
  try
    for Table in Database.Tables do
      for ForeignKey in Table.ForeignKeys do
      begin
        if SameText(ForeignKey.ToTable.Name, ReferencedTable.Name)
          and SameText(ForeignKey.ToTable.Schema, ReferencedTable.Schema) then
          ForeignKeys.Add(ForeignKey);
      end;
      {$IFDEF DELPHIXE_LVL}
      Result := ForeignKeys.ToArray;
      {$ELSE}
      SetLength(Result, ForeignKeys.Count);
      for I := 0 to ForeignKeys.Count - 1 do
        Result[I] := ForeignKeys[I];
      {$ENDIF}
  finally
    ForeignKeys.Free;
  end;
end;

class function TSchemaUtils.GetTable(Database: TDatabaseMetadata;
  const TableName, TableSchema: string): TTableMetadata;
begin
  Result := FindTable(Database, TableName, TableSchema);
  if Result = nil then
    raise ETableMetadataNotFound.Create(TableName, TableSchema);
end;

end.
