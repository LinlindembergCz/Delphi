unit Aurelius.Sql.AbstractSQLGenerator;

{$I Aurelius.inc}

interface

uses
  Generics.Collections, DB,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Commands,
  Aurelius.Sql.Interfaces,
  Aurelius.Sql.Metadata;

type
  TAbstractSQLGenerator = class abstract(TInterfacedObject, ISQLGenerator)
  protected
    // Generators
    function GetGeneratorName: string; virtual; abstract;

    // Mapping to the connection driver
    function GetSqlDialect: string; virtual; abstract;

    // standard DML methods
    function GenerateSelect(Command: TSelectCommand): string; virtual; abstract;
    function GenerateInsert(Command: TInsertCommand): string; virtual; abstract;
    function GenerateUpdate(Command: TUpdateCommand): string; virtual; abstract;
    function GenerateDelete(Command: TDeleteCommand): string; virtual; abstract;

    // specific DML methods
    function GenerateGetNextSequenceValue(Command: TGetNextSequenceValueCommand): string; virtual; abstract;
    function GenerateGetLastInsertId(SQLField: TSQLField): string; virtual; abstract;

    // DDL methods
    function GenerateCreateTable(Table: TTableMetadata; CreateForeignKeys: boolean): string; virtual; abstract;
    function GenerateCreateForeignKey(ForeignKey: TForeignKeyMetadata): string; virtual; abstract;
    function GenerateCreateSequence(Sequence: TSequenceMetadata): string; virtual; abstract;
    function GenerateCreateUniqueKey(UniqueKey: TUniqueKeyMetadata): string; virtual; abstract;
    function GenerateDropTable(Table: TTableMetadata): string; virtual; abstract;
    function GenerateDropForeignKey(ForeignKey: TForeignKeyMetadata): string; virtual; abstract;
    function GenerateDropSequence(Sequence: TSequenceMetadata): string; virtual; abstract;
    function GenerateCreateColumn(Column: TColumnMetadata): string; virtual; abstract;
    function GenerateDropField(Column: TColumnMetadata): string; virtual; abstract;
    function GenerateDropUniqueKey(UniqueKey: TUniqueKeyMetadata): string; virtual; abstract;
    procedure DefineColumnType(Column: TColumnMetadata); virtual; abstract;

    // Disable/Enable foreign keys
    function GenerateEnableForeignKeys(AEnable: boolean): string; virtual; abstract;

    // Types/constants methods
    function GetSqlLiteral(AValue: Variant; AType: TFieldType): string; virtual; abstract;

    // Methods for compatibility between different databases
    function GetSupportedFeatures: TDBFeatures; virtual; abstract;
    function GetSupportedFieldTypes: TFieldTypeSet; virtual; abstract;
    function GetEquivalentFieldTypes: TFieldTypeEquivArray; virtual; abstract;
    function ConvertValue(Value: Variant; FromType, ToType: TFieldType): Variant; virtual; abstract;

    // Methods for fields/param naming
    function GetDefaultParamName(Field: TSQLField): string;
    function GetDefaultColumnName(Field: TSQLSelectField): string;

    function GetQualifiedColumnName(Field: TSQLSelectField): string; virtual; abstract;

    function GetForeignKeyName(ForeignKey: TForeignKeyMetadata): string; virtual; abstract;
    function GetUniqueKeyName(UniqueKey: TUniqueKeyMetadata): string; virtual; abstract;
    function GetFullTableName(const ATable, ASchema: string): string; virtual; abstract;

    // Process command
    procedure ProcessCommand(var SQL: string); virtual;
  end;

implementation
uses
  SysUtils,
  Aurelius.Sql.Exceptions;

{ TAbstractSQLGenerator }

function TAbstractSQLGenerator.GetDefaultColumnName(
  Field: TSQLSelectField): string;
var
  FunctionPrefix: string;
begin
  Result := Field.Table.Alias + '_' + Field.Field;

  if Field.IsAggregated then
  begin
    case Field.AggregateFuntion of
      TAggregateFunction.Count:   FunctionPrefix := 'COUNT_';
      TAggregateFunction.Sum:     FunctionPrefix := 'SUM_';
      TAggregateFunction.Average: FunctionPrefix := 'AVG_';
      TAggregateFunction.Max:     FunctionPrefix := 'MAX_';
      TAggregateFunction.Min:     FunctionPrefix := 'MIN_';
      TAggregateFunction.List:    FunctionPrefix := 'LIST_';
    else
      raise EUnexpectedAggregateFunction(Field.AggregateFuntion);
    end;
    Result := FunctionPrefix + Result;
  end;
end;

procedure TAbstractSQLGenerator.ProcessCommand(var SQL: string);
begin
end;

function TAbstractSQLGenerator.GetDefaultParamName(Field: TSQLField): string;
begin
  Result := Field.Table.Alias + '_' + Field.Field;
end;

end.
