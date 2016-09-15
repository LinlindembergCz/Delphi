unit Aurelius.Commands.ForeignKeyCreator;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Mapping.Explorer,
  Aurelius.Mapping.Metadata,
  Aurelius.Sql.Metadata;

type
  TForeignKeyCreator = class(TAbstractCommandPerformer)
  strict private
    procedure CreateColumnsForManyValuedAssocation(Assoc: TAssociation; Database: TDatabaseMetadata);
    procedure CreateForeignKeyForJoinedInheritanceTables(Database: TDatabaseMetadata);
  public
    procedure CreateForeignKeys(Database: TDatabaseMetadata);
  end;

implementation

uses
  Aurelius.Global.Config,
  Aurelius.Schema.Utils,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Interfaces;

{ TForeignKeyCreator }

procedure TForeignKeyCreator.CreateForeignKeyForJoinedInheritanceTables(Database: TDatabaseMetadata);
var
  ForeignKey: TForeignKeyMetadata;
  ToTable: TTableMetadata;
  FromTable: TTableMetadata;
  MappedToTable: TMetaTable;
  MappedFromTable: TMetaTable;
  Cols: TArray<TColumn>;
  Col: TColumn;
begin
  Cols := Explorer.GetPrimaryJoinColumns(Self.Clazz);
  if Length(Cols) = 0 then
    Exit;

  MappedFromTable := Explorer.GetTable(Self.Clazz);
  FromTable := TSchemaUtils.GetTable(Database, MappedFromTable.Name, MappedFromTable.Schema);
  MappedToTable := Explorer.GetTable(Self.Clazz.ClassParent);
  ToTable := TSchemaUtils.GetTable(Database, MappedToTable.Name, MappedToTable.Schema);

  ForeignKey := TForeignKeyMetadata.Create(FromTable);
  FromTable.ForeignKeys.Add(ForeignKey);
  ForeignKey.ToTable := ToTable;
  for Col in Cols do
  begin
    ForeignKey.FromColumns.Add(TSchemaUtils.GetColumn(FromTable, Col.Name));
    ForeignKey.ToColumns.Add(TSchemaUtils.GetColumn(ToTable, Col.ReferencedColumn.Name));
  end;
  ForeignKey.Name := SQLGenerator.GetForeignKeyName(ForeignKey);
end;

procedure TForeignKeyCreator.CreateColumnsForManyValuedAssocation(Assoc: TAssociation; Database: TDatabaseMetadata);
var
  Column: TColumnMetadata;
  ToTable: TTableMetadata;
  MappedToTable: TMetaTable;
  C: TColumn;
begin
  if (Assoc.Kind <> TAssociationKind.ManyValued) or (Assoc.MappedBy <> '') then
    Exit;

  MappedToTable := Explorer.GetTable(Assoc.Target);
  ToTable := TSchemaUtils.GetTable(Database, MappedToTable.Name, MappedToTable.Schema);

  for C in Assoc.JoinColumns do
  begin
    Column := TColumnMetadata.Create(ToTable);
    ToTable.Columns.Add(Column);
    BuildColumnMetadata(Column, C);
  end;
end;

procedure TForeignKeyCreator.CreateForeignKeys(Database: TDatabaseMetadata);
var
  ForeignKey: TForeignKeyMetadata;
  A: TAssociation;
  Associations: TList<TAssociation>;
  ToTable: TTableMetadata;
  FromTable: TTableMetadata;
  MappedToTable: TMetaTable;
  MappedFromTable: TMetaTable;
  C: TColumn;
begin
  CreateForeignKeyForJoinedInheritanceTables(Database);

  MappedFromTable := Explorer.GetTable(Self.Clazz);
  FromTable := TSchemaUtils.GetTable(Database, MappedFromTable.Name, MappedFromTable.Schema);

  Associations := FAssociations;
  if Explorer.HasInheritance(Self.Clazz) then
    if Explorer.GetInheritanceStrategy(Self.Clazz) = TInheritanceStrategy.JoinedTables then
      Associations := Explorer.GetAssociations(Self.Clazz, False, False);

  for A in Associations do
  begin
    if (A.Kind = TAssociationKind.ManyValued) and (A.MappedBy <> '') then
      Continue;

    CreateColumnsForManyValuedAssocation(A, Database);

    MappedToTable := Explorer.GetTable(A.Target);
    ToTable := TSchemaUtils.GetTable(Database, MappedToTable.Name, MappedToTable.Schema);

    if A.Kind = TAssociationKind.SingleValued then
    begin
      ForeignKey := TForeignKeyMetadata.Create(FromTable);
      FromTable.ForeignKeys.Add(ForeignKey);
      ForeignKey.ToTable := ToTable;
    end
    else
    begin
      ForeignKey := TForeignKeyMetadata.Create(ToTable);
      ToTable.ForeignKeys.Add(ForeignKey);
      ForeignKey.ToTable := FromTable;
    end;

    for C in A.JoinColumns do
    begin
      ForeignKey.FromColumns.Add(TSchemaUtils.GetColumn(ForeignKey.FromTable, C.Name));
      ForeignKey.ToColumns.Add(TSchemaUtils.GetColumn(ForeignKey.ToTable, C.ReferencedColumn.Name));
    end;
    ForeignKey.Name := SQLGenerator.GetForeignKeyName(ForeignKey);
  end;
end;

end.
