unit Aurelius.Commands.TableCreator;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Sql.Metadata;

type
  TTableCreator = class(TAbstractCommandPerformer)
  public
    procedure CreateTable(Database: TDatabaseMetadata);
    procedure CreateUniqueKeys(Database: TDatabaseMetadata);
  end;

implementation

uses
  Aurelius.Global.Config,
  Aurelius.Mapping.Metadata,
  Aurelius.Schema.Utils,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Interfaces;

{ TTableCreator }

procedure TTableCreator.CreateTable(Database: TDatabaseMetadata);

  function IsPKColumn(Col: TColumn): boolean;
  var
    C: TColumn;
  begin
    Result := False;
    for C in FPKColumns do
      if C = Col then
        Exit(True);
  end;

var
  Table: TTableMetadata;
  Column: TColumnMetadata;
  UniqueKey: TUniqueKeyMetadata;
  C: TColumn;
  MappedTable: TMetaTable;
begin
  Table := TTableMetadata.Create(Database);
  Database.Tables.Add(Table);
  MappedTable := Explorer.GetTable(Self.Clazz);
  Table.Name := MappedTable.Name;
  Table.Schema := MappedTable.Schema;

  for C in FColumns do
  begin
    if C.IsForeign then
      Continue;

    Column := TColumnMetadata.Create(Table);
    Table.Columns.Add(Column);
    BuildColumnMetadata(Column ,C);

    if (TColumnProp.Unique in C.Properties) and not IsPKColumn(C) then
    begin
      UniqueKey := TUniqueKeyMetadata.Create(Table);
      Table.UniqueKeys.Add(UniqueKey);
      UniqueKey.Columns.Add(TSchemaUtils.GetColumn(Table, C.Name));
      UniqueKey.Name := SQLGenerator.GetUniqueKeyName(UniqueKey); // Set the name only after all properties are set
    end;
  end;
end;

procedure TTableCreator.CreateUniqueKeys(Database: TDatabaseMetadata);
var
  Table: TTableMetadata;
  UniqueKey: TUniqueKeyMetadata;
  C: TColumn;
  MappedTable: TMetaTable;
  UKs: TObjectList<TUniqueConstraint>;
  UK: TUniqueConstraint;
  FieldName: string;
begin
  MappedTable := Explorer.GetTable(Self.Clazz);
  Table := TSchemaUtils.GetTable(Database, MappedTable.Name, MappedTable.Schema);

  // Add primary key
  Table.IdColumns.Clear;
  for C in FPKColumns do
    Table.IdColumns.Add(TSchemaUtils.GetColumn(Table, C.Name));

  // Add composite unique keys
  UKs := Explorer.GetUniqueConstraints(Self.Clazz);
  try
    for UK in UKs do
    begin
      UniqueKey := TUniqueKeyMetadata.Create(Table);
      Table.UniqueKeys.Add(UniqueKey);
      for FieldName in UK.FieldNames do
        UniqueKey.Columns.Add(TSchemaUtils.GetColumn(Table, FieldName));
      UniqueKey.Name := SQLGenerator.GetUniqueKeyName(UniqueKey); // Set the name only after all properties are set
    end;
  finally
    UKs.Free;
  end;
end;

end.
