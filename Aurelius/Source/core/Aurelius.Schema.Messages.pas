unit Aurelius.Schema.Messages;

{$I Aurelius.Inc}

interface

uses
  Generics.Collections,
  Aurelius.Sql.Metadata;

type
  TSchemaMessage = class
  private
    FText: string;
  protected
    function ColumnNames(Columns: TEnumerable<TColumnMetadata>): string;
    function ColumnName(Column: TColumnMetadata): string;
    function TableName(Table: TTableMetadata): string;
    function ForeignKeyName(ForeignKey: TForeignKeyMetadata): string;
    function UniqueKeyName(UniqueKey: TUniqueKeyMetadata): string;
  public
    property Text: string read FText write FText;
  end;

  TSchemaAction = class(TSchemaMessage)
  end;

  TSchemaWarning = class(TSchemaMessage)
  end;

  TSchemaError = class(TSchemaMessage)
  end;

  {$REGION 'Actions'}
  TColumnCreatedAction = class(TSchemaAction)
  public
    constructor Create(Column: TColumnMetadata);
  end;

  TForeignKeyRemovedAction = class(TSchemaAction)
  public
    constructor Create(ForeignKey: TForeignKeyMetadata);
  end;

  TSequenceCreatedAction = class(TSchemaAction)
  public
    constructor Create(Sequence: TSequenceMetadata);
  end;

  TSequenceRemovedAction = class(TSchemaAction)
  public
    constructor Create(Sequence: TSequenceMetadata);
  end;

  TTableCreatedAction = class(TSchemaAction)
  public
    constructor Create(Table: TTableMetadata);
  end;

  TTableRemovedAction = class(TSchemaAction)
  public
    constructor Create(Table: TTableMetadata);
  end;

  TUniqueKeyRemovedAction = class(TSchemaAction)
  public
    constructor Create(UniqueKey: TUniqueKeyMetadata);
  end;
  {$ENDREGION}

  {$REGION 'Errors'}
  TColumnBecomesNullableError = class(TSchemaError)
  public
    constructor Create(Column: TColumnMetadata);
  end;

  TColumnTypeMismatchError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnIdentityChangedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnLengthIncreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnPrecisionIncreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnScaleIncreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TPrimaryKeyMismatchError = class(TSchemaError)
  public
    constructor Create(OldTable, NewTable: TTableMetadata);
  end;

  TUniqueKeyRemovedError = class(TSchemaError)
  public
    constructor Create(UniqueKey: TUniqueKeyMetadata);
  end;

  TUniqueKeyCreatedError = class(TSchemaError)
  public
    constructor Create(UniqueKey: TUniqueKeyMetadata);
  end;

  TUniqueKeyChangedError = class(TSchemaError)
  public
    constructor Create(OldUniqueKey, NewUniqueKey: TUniqueKeyMetadata);
  end;

  TColumnRemovedError = class(TSchemaError)
  public
    constructor Create(Column: TColumnMetadata);
  end;

  TColumnLengthDecreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnPrecisionDecreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TColumnScaleDecreasedError = class(TSchemaError)
  public
    constructor Create(OldColumn, NewColumn: TColumnMetadata);
  end;

  TForeignKeyCreatedError = class(TSchemaError)
  public
    constructor Create(ForeignKey: TForeignKeyMetadata);
  end;

  TForeignKeyChangedError = class(TSchemaError)
  public
    constructor Create(OldForeignKey, NewForeignKey: TForeignKeyMetadata);
  end;

  TForeignKeyRemovedError = class(TSchemaError)
  public
    constructor Create(ForeignKey: TForeignKeyMetadata);
  end;

  TColumnBecomesRequiredError = class(TSchemaError)
  public
    constructor Create(Column: TColumnMetadata);
  end;

  TTableRemovedError = class(TSchemaError)
  public
    constructor Create(Table: TTableMetadata);
  end;

  TSequenceRemovedError = class(TSchemaWarning)
  public
    constructor Create(Sequence: TSequenceMetadata);
  end;
  {$ENDREGION}

  {$REGION 'Warnings'}
  TForeignKeyCreatedWarning = class(TSchemaWarning)
  public
    constructor Create(ForeignKey: TForeignKeyMetadata);
  end;

  TRequiredColumnCreatedWarning = class(TSchemaWarning)
  public
    constructor Create(Column: TColumnMetadata);
  end;

  TTableRemovedWarning = class(TSchemaWarning)
  public
    constructor Create(Table: TTableMetadata);
  end;

  TForeignKeyChangedWarning = class(TSchemaWarning)
  public
    constructor Create(OldForeignKey, NewForeignKey: TForeignKeyMetadata);
  end;
  {$ENDREGION}

implementation

uses
  SysUtils;

{ TSchemaMessage }

function TSchemaMessage.ColumnName(Column: TColumnMetadata): string;
begin
  Result := Format('%s.%s', [TableName(Column.Table), Column.Name]);
end;

function TSchemaMessage.ColumnNames(Columns: TEnumerable<TColumnMetadata>): string;
var
  Column: TColumnMetadata;
begin
  Result := '';
  for Column in Columns do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Column.Name;
  end;
end;

function TSchemaMessage.ForeignKeyName(ForeignKey: TForeignKeyMetadata): string;
begin
  Result := Format('%s.%s', [TableName(ForeignKey.FromTable), ForeignKey.Name]);
end;

function TSchemaMessage.TableName(Table: TTableMetadata): string;
begin
  if Table.Schema <> '' then
    Result := Format('%s.%s', [Table.Schema, Table.Name])
  else
    Result := Table.Name;
end;

function TSchemaMessage.UniqueKeyName(UniqueKey: TUniqueKeyMetadata): string;
begin
  Result := Format('%s.%s', [TableName(UniqueKey.Table), UniqueKey.Name]);
end;

{ TTColumnBecomesNullableWarning }

constructor TColumnBecomesNullableError.Create(Column: TColumnMetadata);
begin
  Text := Format('Column %s - Not Null constraint changed. Existing: Not Null. Target: Nullable',
    [ColumnName(Column)]);
end;

{ TTColumnBecomesRequiredWarning }

constructor TColumnBecomesRequiredError.Create(Column: TColumnMetadata);
begin
  Text := Format('Column %s - Not Null constraint changed. Existing: Nullable. Target: Not Null',
    [ColumnName(Column)]);
end;

{ TColumnTypeMismatchError }

constructor TColumnTypeMismatchError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Type mismatch. Existing: %s. Target: %s',
    [ColumnName(NewColumn), OldColumn.DataType, NewColumn.DataType]);
end;

{ TColumnIdentityChangedError }

constructor TColumnIdentityChangedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Identity changed', [ColumnName(NewColumn)]);
end;

{ TColumnLengthIncreasedError }

constructor TColumnLengthIncreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Size/Length increased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Length, NewColumn.Length]);
end;

{ TColumnPrecisionIncreasedError }

constructor TColumnPrecisionIncreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Precision increased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Precision, NewColumn.Precision]);
end;

{ TColumnScaleIncreasedError }

constructor TColumnScaleIncreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Scale increased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Scale, NewColumn.Scale]);
end;

{ TColumnLengthDecreasedWarning }

constructor TColumnLengthDecreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Size/Length decreased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Length, NewColumn.Length]);
end;

{ TColumnPrecisionDecreasedWarning }

constructor TColumnPrecisionDecreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Precision decreased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Precision, NewColumn.Precision]);
end;

{ TColumnScaleDecreasedWarning }

constructor TColumnScaleDecreasedError.Create(OldColumn, NewColumn: TColumnMetadata);
begin
  Text := Format('Column %s - Scale decreased. Existing: %d. Target: %d',
    [ColumnName(NewColumn), OldColumn.Scale, NewColumn.Scale]);
end;

{ TColumnRemovedWarning }

constructor TColumnRemovedError.Create(Column: TColumnMetadata);
begin
  Text := Format('Column: %s - Removed. ', [ColumnName(Column)]);
end;

{ TPrimaryKeyMismatchError }

constructor TPrimaryKeyMismatchError.Create(OldTable, NewTable: TTableMetadata);
begin
  Text := Format('Table %s - Primary key changed. Existing: (%s). Target: (%s)',
    [TableName(NewTable), ColumnNames(OldTable.IdColumns), ColumnNames(NewTable.IdColumns)]);
end;

{ TUniqueKeyRemovedError }

constructor TUniqueKeyRemovedError.Create(UniqueKey: TUniqueKeyMetadata);
begin
  Text := Format('Unique key: %s - Removed', [UniqueKeyName(UniqueKey)]);
end;

{ TUniqueKeyCreatedError }

constructor TUniqueKeyCreatedError.Create(UniqueKey: TUniqueKeyMetadata);
begin
  Text := Format('Unique key: %s - Created', [UniqueKeyName(UniqueKey)]);
end;

{ TUniqueKeyChangedError }

constructor TUniqueKeyChangedError.Create(OldUniqueKey,
  NewUniqueKey: TUniqueKeyMetadata);
begin
  Text := Format('Unique key: %s - Definition changed.', [UniqueKeyName(NewUniqueKey)]);
end;

{ TForeignKeyRemovedError }

constructor TForeignKeyRemovedError.Create(ForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Removed.', [ForeignKeyName(ForeignKey)]);
end;

{ TForeignKeyCreatedError }

constructor TForeignKeyCreatedWarning.Create(ForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Created. Existing data might be incompatible.', [ForeignKeyName(ForeignKey)]);
end;

{ TForeignKeyCreatedError }

constructor TForeignKeyCreatedError.Create(ForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Created.', [ForeignKeyName(ForeignKey)]);
end;

{ TForeignKeyChangedError }

constructor TForeignKeyChangedError.Create(OldForeignKey, NewForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Definition changed.', [ForeignKeyName(NewForeignKey)]);
end;

{ TTableRemovedWarning }

constructor TTableRemovedWarning.Create(Table: TTableMetadata);
begin
  Text := Format('Table: %s - Removed.', [TableName(Table)]);
end;

{ TSequenceRemovedError }

constructor TSequenceRemovedError.Create(Sequence: TSequenceMetadata);
begin
  Text := Format('Sequence: %s - Removed.', [Sequence.Name]);
end;

{ TRequiredColumnCreatedWarning }

constructor TRequiredColumnCreatedWarning.Create(Column: TColumnMetadata);
begin
  Text := Format('Column: %s - Created with not null constraint. Existing data might be incompatible.',
    [ColumnName(Column)]);
end;

{ TForeignKeyChangedWarning }

constructor TForeignKeyChangedWarning.Create(OldForeignKey,
  NewForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Definition changed. Existing data might be incompatible',
    [ForeignKeyName(NewForeignKey)]);
end;

{ TTableRemovedError }

constructor TTableRemovedError.Create(Table: TTableMetadata);
begin
  Text := Format('Table: %s - Removed.', [TableName(Table)]);
end;

{ TColumnCreatedAction }

constructor TColumnCreatedAction.Create(Column: TColumnMetadata);
begin
  Text := Format('Column: %s - Created.', [ColumnName(Column)]);
end;

{ TForeignKeyRemovedAction }

constructor TForeignKeyRemovedAction.Create(ForeignKey: TForeignKeyMetadata);
begin
  Text := Format('Foreign key: %s - Removed.', [ForeignKeyName(ForeignKey)]);
end;

{ TSequenceCreatedAction }

constructor TSequenceCreatedAction.Create(Sequence: TSequenceMetadata);
begin
  Text := Format('Sequence: %s - Created.', [Sequence.Name]);
end;

{ TSequenceRemovedAction }

constructor TSequenceRemovedAction.Create(Sequence: TSequenceMetadata);
begin
  Text := Format('Sequence: %s - Removed.', [Sequence.Name]);
end;

{ TTableCreatedAction }

constructor TTableCreatedAction.Create(Table: TTableMetadata);
begin
  Text := Format('Table: %s - Created.', [TableName(Table)]);
end;

{ TTableRemovedAction }

constructor TTableRemovedAction.Create(Table: TTableMetadata);
begin
  Text := Format('Table: %s - Removed.', [TableName(Table)]);
end;

{ TUniqueKeyRemovedAction }

constructor TUniqueKeyRemovedAction.Create(UniqueKey: TUniqueKeyMetadata);
begin
  Text := Format('Unique key: %s - Removed.', [UniqueKeyName(UniqueKey)]);
end;

end.
