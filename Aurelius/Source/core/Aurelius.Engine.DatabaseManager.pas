unit Aurelius.Engine.DatabaseManager;

{$I Aurelius.inc}

interface

uses
  Generics.Defaults,
  Generics.Collections,
  Aurelius.Drivers.Interfaces,
  Aurelius.Engine.AbstractManager,
  Aurelius.Mapping.Explorer,
  Aurelius.Schema.Commands,
  Aurelius.Schema.Interfaces,
  Aurelius.Schema.Messages,
  Aurelius.Sql.Commands,
  Aurelius.Sql.Metadata;

type
  TDatabaseManager = class(TAbstractManager)
  strict private
    FCommands: TList<TDDLCommand>;
    FWarnings: TList<TSchemaWarning>;
    FErrors: TList<TSchemaError>;
    FActions: TList<TSchemaAction>;
    FSQLStatements: TList<string>;
    FExistingDB: TDatabaseMetadata;
    FTargetDB: TDatabaseMetadata;
    FSQLExecutionEnabled: boolean;
    function GetImporter: ISchemaImporter;
    function GetCommands: TEnumerable<TDDLCommand>;
    function GetErrors: TEnumerable<TSchemaError>;
    function GetWarnings: TEnumerable<TSchemaWarning>;
    function GetActions: TEnumerable<TSchemaAction>;
    function GetActionCount: integer;
    function GetErrorCount: integer;
    function GetWarningCount: integer;
    function GetSQLStatements: TEnumerable<string>;
  strict private
    FAllowDestructiveCommands: boolean;
    FIgnoreConstraintName: boolean;
    FIgnoreDataTypes: boolean;
    function AreSQLiteForeignKeysEnabled: boolean;
  strict private
    procedure ProcessCommands(Commands: TList<TDDLCommand>);
    procedure ExecuteSQLStatements(Statements: TEnumerable<string>; IgnoreErrors: boolean);
    procedure BuildStatements(Commands: TEnumerable<TDDLCommand>);
  strict private
    procedure ActionPrimaryKeyChanged(OldTable, NewTable: TTableMetadata);
    procedure ActionUniqueKeyRemoved(UniqueKey: TUniqueKeyMetadata);
    procedure ActionUniqueKeyAdded(UniqueKey: TUniqueKeyMetadata);
    procedure ActionUniqueKeyChanged(OldUniqueKey, NewUniqueKey: TUniqueKeyMetadata);
    procedure ActionForeignKeyRemoved(ForeignKey: TForeignKeyMetadata);
    procedure ActionForeignKeyAdded(ForeignKey: TForeignKeyMetadata);
    procedure ActionForeignKeyChanged(OldForeignKey, NewForeignKey: TForeignKeyMetadata);
    procedure ActionColumnRequiredChanged(OldColumn, NewColumn: TColumnMetadata);
    procedure ActionColumnTypeChanged(OldColumn, NewColumn: TColumnMetadata);
    procedure ActionColumnIdentityChanged(OldColumn, NewColumn: TColumnMetadata);
    procedure ActionFieldSizeChanged(OldColumn, NewColumn: TColumnMetadata);
    procedure ActionColumnRemoved(Column: TColumnMetadata);
    procedure ActionColumnAdded(Column: TColumnMetadata);
    procedure ActionTableRemoved(Table: TTableMetadata);
    procedure ActionTableAdded(Table: TTableMetadata);
    procedure ActionSequenceRemoved(Sequence: TSequenceMetadata);
    procedure ActionSequenceAdded(Sequence: TSequenceMetadata);
  protected
    procedure GetUpdateDatabaseCommands(OldDB, NewDB: TDatabaseMetadata);
    procedure BuildDatabaseMetadata(Database: TDatabaseMetadata);
    property Commands: TEnumerable<TDDLCommand> read GetCommands;
    property ExistingDB: TDatabaseMetadata read FExistingDB;
    property TargetDB: TDatabaseMetadata read FTargetDB;
    property Importer: ISchemaImporter read GetImporter;
  public
    constructor Create(Connection: IDBConnection; AExplorer: TMappingExplorer); override;
    destructor Destroy; override;
    procedure BuildDatabase;
    procedure DestroyDatabase;
    function ValidateDatabase: boolean;
    procedure UpdateDatabase;
    property SQLExecutionEnabled: boolean read FSQLExecutionEnabled write FSQLExecutionEnabled;
    property SQLStatements: TEnumerable<string> read GetSQLStatements;
    property Actions: TEnumerable<TSchemaAction> read GetActions;
    property Warnings: TEnumerable<TSchemaWarning> read GetWarnings;
    property Errors: TEnumerable<TSchemaError> read GetErrors;
    property ActionCount: integer read GetActionCount;
    property WarningCount: integer read GetWarningCount;
    property ErrorCount: integer read GetErrorCount;
  end;

implementation

uses
  SysUtils, Variants,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Commands.ForeignKeyCreator,
  Aurelius.Commands.TableCreator,
  Aurelius.Commands.SequenceCreator,
  Aurelius.Engine.Exceptions,
  Aurelius.Schema.Register,
  Aurelius.Schema.Utils,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Interfaces;

{ TDatabaseManager }

procedure TDatabaseManager.ActionColumnAdded(Column: TColumnMetadata);
begin
  FCommands.Add(TCreateColumnCommand.Create(Column));
  if Column.NotNull then
    FWarnings.Add(TRequiredColumnCreatedWarning.Create(Column))
  else
    FActions.Add(TColumnCreatedAction.Create(Column));
end;

procedure TDatabaseManager.ActionColumnIdentityChanged(OldColumn,
  NewColumn: TColumnMetadata);
begin
  FErrors.Add(TColumnIdentityChangedError.Create(OldColumn, NewColumn));
end;

procedure TDatabaseManager.ActionColumnRemoved(Column: TColumnMetadata);
begin
  FErrors.Add(TColumnRemovedError.Create(Column));
end;

procedure TDatabaseManager.ActionColumnRequiredChanged(OldColumn,
  NewColumn: TColumnMetadata);
begin
  if NewColumn.NotNull then
    FErrors.Add(TColumnBecomesRequiredError.Create(NewColumn))
  else
  begin
//    if TDBFeature.AlterColumnNullable in SQLGenerator.GetSupportedFeatures then
//      Commands.Add(TAlterColumnNullable.Create())
//    else
      FErrors.Add(TColumnBecomesNullableError.Create(NewColumn));
  end;
end;

procedure TDatabaseManager.ActionColumnTypeChanged(OldColumn,
  NewColumn: TColumnMetadata);
begin
  FErrors.Add(TColumnTypeMismatchError.Create(OldColumn, NewColumn))
end;

procedure TDatabaseManager.ActionFieldSizeChanged(OldColumn,
  NewColumn: TColumnMetadata);
begin
  if OldColumn.Length < NewColumn.Length then
    FErrors.Add(TColumnLengthIncreasedError.Create(OldColumn, NewColumn));
  if OldColumn.Length > NewColumn.Length then
    FErrors.Add(TColumnLengthDecreasedError.Create(OldColumn, NewColumn));

  if OldColumn.Precision < NewColumn.Precision then
    FErrors.Add(TColumnPrecisionIncreasedError.Create(OldColumn, NewColumn));
  if OldColumn.Precision > NewColumn.Precision then
    FErrors.Add(TColumnPrecisionDecreasedError.Create(OldColumn, NewColumn));

  if OldColumn.Scale < NewColumn.Scale then
    FErrors.Add(TColumnScaleIncreasedError.Create(OldColumn, NewColumn));
  if OldColumn.Scale > NewColumn.Scale then
    FErrors.Add(TColumnScaleDecreasedError.Create(OldColumn, NewColumn));
end;

procedure TDatabaseManager.ActionForeignKeyAdded(
  ForeignKey: TForeignKeyMetadata);
begin
  if TDBFeature.AlterTableForeignKey in SQLGenerator.GetSupportedFeatures then
  begin
    FCommands.Add(TCreateForeignKeyCommand.Create(ForeignKey));
    FWarnings.Add(TForeignKeyCreatedWarning.Create(ForeignKey));
  end else
    FErrors.Add(TForeignKeyCreatedError.Create(ForeignKey));
end;

procedure TDatabaseManager.ActionForeignKeyChanged(OldForeignKey,
  NewForeignKey: TForeignKeyMetadata);
begin
  if TDBFeature.AlterTableForeignKey in SQLGenerator.GetSupportedFeatures then
  begin
    FCommands.Add(TDropForeignKeyCommand.Create(OldForeignKey));
    FCommands.Add(TCreateForeignKeyCommand.Create(NewForeignKey));
    FWarnings.Add(TForeignKeyChangedWarning.Create(OldForeignKey, NewForeignKey));
  end
  else
    FErrors.Add(TForeignKeyChangedError.Create(OldForeignKey, NewForeignKey));
end;

procedure TDatabaseManager.ActionForeignKeyRemoved(
  ForeignKey: TForeignKeyMetadata);
begin
  if TDBFeature.AlterTableForeignKey in SQLGenerator.GetSupportedFeatures then
  begin
    FCommands.Add(TDropForeignKeyCommand.Create(ForeignKey));
    FActions.Add(TForeignKeyRemovedAction.Create(ForeignKey));
  end
  else
    FErrors.Add(TForeignKeyRemovedError.Create(ForeignKey));
end;

procedure TDatabaseManager.ActionPrimaryKeyChanged(OldTable,
  NewTable: TTableMetadata);
begin
  FErrors.Add(TPrimaryKeyMismatchError.Create(OldTable, NewTable));
end;

procedure TDatabaseManager.ActionSequenceAdded(Sequence: TSequenceMetadata);
begin
  FCommands.Add(TCreateSequenceCommand.Create(Sequence));
  FActions.Add(TSequenceCreatedAction.Create(Sequence));
end;

procedure TDatabaseManager.ActionSequenceRemoved(Sequence: TSequenceMetadata);
begin
  if FAllowDestructiveCommands then
  begin
    FCommands.Add(TDropSequenceCommand.Create(Sequence));
    FActions.Add(TSequenceRemovedAction.Create(Sequence));
  end
  else
    FWarnings.Add(TSequenceRemovedError.Create(Sequence));
end;

procedure TDatabaseManager.ActionTableAdded(Table: TTableMetadata);
var
  CreateForeignKeys: boolean;
begin
  CreateForeignKeys := not (TDBFeature.AlterTableForeignKey in SQLGenerator.SupportedFeatures);
  FCommands.Add(TCreateTableCommand.Create(Table, CreateForeignKeys));
  FActions.Add(TTableCreatedAction.Create(Table));
end;

procedure TDatabaseManager.ActionTableRemoved(Table: TTableMetadata);
begin
  if FAllowDestructiveCommands then
  begin
    FCommands.Add(TDropTableCommand.Create(Table));
    FActions.Add(TTableRemovedAction.Create(Table));
  end
  else
    FErrors.Add(TTableRemovedError.Create(Table));
end;

procedure TDatabaseManager.ActionUniqueKeyAdded(UniqueKey: TUniqueKeyMetadata);
begin
//  if TDBFeature.DropUniqueKey in SQLGenerator.GetSupportedFeatures then
//    Commands.Add(TCreateUniqueKeyCommand.Create(UniqueKey))
//  else
    FErrors.Add(TUniqueKeyCreatedError.Create(UniqueKey))
end;

procedure TDatabaseManager.ActionUniqueKeyChanged(OldUniqueKey,
  NewUniqueKey: TUniqueKeyMetadata);
begin
//  if TDBFeature.DropUniqueKey in SQLGenerator.GetSupportedFeatures then
//  begin
//    FCommands.Add(TDropUniqueKeyCommand.Create(OldUniqueKey));
//    FCommands.Add(TCreateUniqueKeyCommand.Create(NewUniqueKey));
//  end
//  else
    FErrors.Add(TUniqueKeyChangedError.Create(OldUniqueKey, NewUniqueKey));
end;

procedure TDatabaseManager.ActionUniqueKeyRemoved(
  UniqueKey: TUniqueKeyMetadata);
begin
  if TDBFeature.DropUniqueKey in SQLGenerator.GetSupportedFeatures then
  begin
    FCommands.Add(TDropUniqueKeyCommand.Create(UniqueKey));
    FActions.Add(TUniqueKeyRemovedAction.Create(UniqueKey));
  end
  else
    FErrors.Add(TUniqueKeyRemovedError.Create(UniqueKey));
end;

function TDatabaseManager.AreSQLiteForeignKeysEnabled: boolean;
var
  ResultSet: IDBResultSet;
  Stmt: IDBStatement;
  V: Variant;
begin
  Stmt := Connection.CreateStatement;
  Stmt.SetSQLCommand('pragma foreign_keys');
  ResultSet := Stmt.ExecuteQuery;
  Result := false;
  if Resultset.Next then
  begin
    V := ResultSet.GetFieldValue('foreign_keys');
    if not VarIsNull(V) then
      Result := VarAsType(V, varBoolean);
  end;
end;

procedure TDatabaseManager.BuildDatabase;
var
  EmptyDatabase: TDatabaseMetadata;
begin
  EmptyDatabase := TDatabaseMetadata.Create;
  try
    BuildDatabaseMetadata(TargetDB);
    GetUpdateDatabaseCommands(EmptyDatabase, TargetDB);
    if SQLExecutionEnabled then
      ExecuteSQLStatements(SQLStatements, False);
  finally
    EmptyDatabase.Free;
  end;
end;

procedure TDatabaseManager.BuildDatabaseMetadata(Database: TDatabaseMetadata);
var
  C: TClass;
  TableCreator: TTableCreator;
  FKCreator: TForeignKeyCreator;
  SequenceCreator: TSequenceCreator;
begin
  Database.Clear;
  for C in Explorer.Hierarchy.Classes do
  begin
    if Explorer.HasTable(C) then
    begin
      TableCreator := FCommandFactory.GetCommand<TTableCreator>(C);
      try
        TableCreator.CreateTable(Database);
      finally
        TableCreator.Free;
      end;
    end;
  end;

  for C in Explorer.Hierarchy.Classes do
  begin
    if Explorer.HasTable(C) then
    begin
      FKCreator := FCommandFactory.GetCommand<TForeignKeyCreator>(C);
      try
        FKCreator.CreateForeignKeys(Database);
      finally
        FKCreator.Free;
      end;
    end;
  end;

  // Now we can create unique keys, after foreign keys are created
  // because at this point, the table objects has all columns
  // (extra columns can be created by the foreign key creator)
  for C in Explorer.Hierarchy.Classes do
  begin
    if Explorer.HasTable(C) then
    begin
      TableCreator := FCommandFactory.GetCommand<TTableCreator>(C);
      try
        TableCreator.CreateUniqueKeys(Database);
      finally
        TableCreator.Free;
      end;
    end;
  end;

  for C in Explorer.Hierarchy.Classes do
  begin
    if Explorer.HasSequence(C, False) then
    begin
      SequenceCreator := FCommandFactory.GetCommand<TSequenceCreator>(C);
      try
        SequenceCreator.CreateSequence(Database);
      finally
        SequenceCreator.Free;
      end;
    end;
  end;
end;

procedure TDatabaseManager.BuildStatements(Commands: TEnumerable<TDDLCommand>);
var
  Command: TDDLCommand;
begin
  FSQLStatements.Clear;
  for Command in Commands do
    FSQLStatements.Add(Command.BuildSQL(SQLGenerator));
end;

constructor TDatabaseManager.Create(Connection: IDBConnection;
  AExplorer: TMappingExplorer);
begin
  inherited Create(Connection, AExplorer);
  FErrors := TObjectList<TSchemaError>.Create(true);
  FWarnings := TObjectList<TSchemaWarning>.Create(true);
  FActions := TObjectList<TSchemaAction>.Create(true);
  FCommands := TObjectList<TDDLCommand>.Create(true);
  FSQLStatements := TList<string>.Create;
  FAllowDestructiveCommands := false;
  FExistingDB := TDatabaseMetadata.Create;
  FTargetDB := TDatabaseMetadata.Create;

  FIgnoreConstraintName := false;
  FIgnoreDataTypes := false;
  FSQLExecutionEnabled := true;

  // Big workaround here, but we can change it later to a more elegant solution
  // (more meta information in ISQLGenerator
  if SameText(SQLGenerator.SqlDialect, 'SQLite') then
  begin
    FIgnoreConstraintName := true;
    FIgnoreDataTypes := true;
  end;
end;

destructor TDatabaseManager.Destroy;
begin
  FExistingDB.Free;
  FTargetDB.Free;
  FErrors.Free;
  FWarnings.Free;
  FActions.Free;
  FCommands.Free;
  FSQLStatements.Free;
  inherited;
end;

procedure TDatabaseManager.DestroyDatabase;
var
  EmptyDatabase: TDatabaseMetadata;
  OldAllowDestructiveCommands: boolean;
begin
  EmptyDatabase := TDatabaseMetadata.Create;
  OldAllowDestructiveCommands := FAllowDestructiveCommands;
  try
    FAllowDestructiveCommands := true;
    BuildDatabaseMetadata(ExistingDB);
    GetUpdateDatabaseCommands(ExistingDB, EmptyDatabase);
    if SQLExecutionEnabled then
      ExecuteSQLStatements(SQLStatements, True);
  finally
    FAllowDestructiveCommands := OldAllowDestructiveCommands;
    EmptyDatabase.Free;
  end;
end;

procedure TDatabaseManager.ExecuteSQLStatements(Statements: TEnumerable<string>; IgnoreErrors: boolean);
var
  SQL: string;
  SQLPerformer: TSQLPerformer;
begin
  SQLPerformer := FCommandFactory.GetCommand<TSQLPerformer>(TObject);
  try
    for SQL in Statements do
    begin
      try
        SQLPerformer.ExecuteSQL(SQL);
      except
        if not IgnoreErrors then
          raise;
      end;
    end;
  finally
    SQLPerformer.Free;
  end;
end;

function TDatabaseManager.GetActionCount: integer;
begin
  Result := FActions.Count;
end;

function TDatabaseManager.GetActions: TEnumerable<TSchemaAction>;
begin
  Result := FActions;
end;

function TDatabaseManager.GetCommands: TEnumerable<TDDLCommand>;
begin
  Result := FCommands;
end;

function TDatabaseManager.GetErrorCount: integer;
begin
  Result := FErrors.Count;
end;

function TDatabaseManager.GetErrors: TEnumerable<TSchemaError>;
begin
  Result := FErrors;
end;

function TDatabaseManager.GetImporter: ISchemaImporter;
begin
  Result := TSchemaImporterRegister.GetInstance.GetImporter(Connection.SqlDialect);
end;

function TDatabaseManager.GetSQLStatements: TEnumerable<string>;
begin
  Result := FSQLStatements;
end;

procedure TDatabaseManager.GetUpdateDatabaseCommands(OldDB, NewDB: TDatabaseMetadata);

  function AreColumnsEqual(OldCols, NewCols: TList<TColumnMetadata>): boolean;
  var
    I: integer;
  begin
    Result := OldCols.Count = NewCols.Count;
    if Result then
      for I := 0 to OldCols.Count - 1 do
        if not SameText(OldCols[I].Name, NewCols[I].Name) then
          Exit(false);
  end;

  procedure CheckPrimaryKey(OldTable, NewTable: TTableMetadata);
  begin
    if not AreColumnsEqual(OldTable.IdColumns, NewTable.IdColumns) then
      ActionPrimaryKeyChanged(OldTable, NewTable);
  end;

  function AreUniqueKeysEqual(OldUniqueKey, UniqueKey: TUniqueKeyMetadata): boolean;
  begin
    Result := AreColumnsEqual(OldUniqueKey.Columns, UniqueKey.Columns);
  end;

  function FindUniqueKey(Table: TTableMetadata; UniqueKey: TUniqueKeyMetadata): TUniqueKeyMetadata;
  var
    CandidateUniqueKey: TUniqueKeyMetadata;
  begin
    if not FIgnoreConstraintName then
      Result := TSchemaUtils.FindUniqueKey(Table, UniqueKey.Name)
    else
    begin
      // Find unique key by definition, not by name
      for CandidateUniqueKey in Table.UniqueKeys do
        if AreUniqueKeysEqual(CandidateUniqueKey, UniqueKey) then
          Exit(CandidateUniqueKey);
      Result := nil;
    end;
  end;

  procedure CheckUniqueKeys(OldTable, NewTable: TTableMetadata);
  var
    UniqueKey: TUniqueKeyMetadata;
    OldUniqueKey: TUniqueKeyMetadata;
  begin
    // Compare table UniqueKeys
    for UniqueKey in OldTable.UniqueKeys do
      if FindUniqueKey(NewTable, UniqueKey) = nil then
        ActionUniqueKeyRemoved(UniqueKey);

    for UniqueKey in NewTable.UniqueKeys do
    begin
      OldUniqueKey := FindUniqueKey(OldTable, UniqueKey);
      if OldUniqueKey = nil then
        ActionUniqueKeyAdded(UniqueKey)
      else
      begin
        // Compare existing unique keys
        if not AreUniqueKeysEqual(OldUniqueKey, UniqueKey) then
          ActionUniqueKeyChanged(OldUniqueKey, UniqueKey);
      end;
    end;
  end;

  function AreForeignKeysEqual(OldForeignKey, ForeignKey: TForeignKeyMetadata): boolean;
  begin
    Result :=
      SameText(OldForeignKey.FromTable.Name, ForeignKey.FromTable.Name)
      and SameText(OldForeignKey.FromTable.Schema, ForeignKey.FromTable.Schema)
      and SameText(OldForeignKey.ToTable.Name, ForeignKey.ToTable.Name)
      and SameText(OldForeignKey.ToTable.Schema, ForeignKey.ToTable.Schema)
      and AreColumnsEqual(OldForeignKey.FromColumns, ForeignKey.FromColumns)
      and AreColumnsEqual(OldForeignKey.ToColumns, ForeignKey.ToColumns);
  end;

  function FindForeignKey(Table: TTableMetadata; ForeignKey: TForeignKeyMetadata): TForeignKeyMetadata;
  var
    CandidateForeignKey: TForeignKeyMetadata;
  begin
    if not FIgnoreConstraintName then
      Result := TSchemaUtils.FindForeignKey(Table, ForeignKey.Name)
    else
    begin
      // Find foreign key by definition, not by name
      for CandidateForeignKey in Table.ForeignKeys do
        if AreForeignKeysEqual(CandidateForeignKey, ForeignKey) then
          Exit(CandidateForeignKey);
      Result := nil;
    end;
  end;

  procedure CheckForeignKeys(OldTable, NewTable: TTableMetadata);
  var
    ForeignKey: TForeignKeyMetadata;
    OldForeignKey: TForeignKeyMetadata;
  begin
    if not (TDBFeature.ForeignKeys in SQLGenerator.SupportedFeatures) then
      Exit;

    // Compare table ForeignKeys
    for ForeignKey in OldTable.ForeignKeys do
      if FindForeignKey(NewTable, ForeignKey) = nil then
        ActionForeignKeyRemoved(ForeignKey);

    for ForeignKey in NewTable.ForeignKeys do
    begin
      OldForeignKey := FindForeignKey(OldTable, ForeignKey);
      if OldForeignKey = nil then
        ActionForeignKeyAdded(ForeignKey)
      else
        if not AreForeignKeysEqual(OldForeignKey, ForeignKey) then
          ActionForeignKeyChanged(OldForeignKey, ForeignKey);
    end;
  end;

  procedure CompareColumns(OldColumn, NewColumn: TColumnMetadata);

    function TypesEqual(T1, T2: string): boolean;
    var
      P: integer;
    begin
      P := Pos('(', T1);
      if P > 0 then
        T1 := Copy(T1, 1, P - 1);
      P := Pos('(', T2);
      if P > 0 then
        T2 := Copy(T2, 1, P - 1);
      Result := SameText(T1, T2);
    end;

  begin
    { required / not null constraint changed }
    if OldColumn.NotNull <> NewColumn.NotNull then
      ActionColumnRequiredChanged(OldColumn, NewColumn);

    { field type }
    if OldColumn.AutoGenerated <> NewColumn.AutoGenerated then
      ActionColumnIdentityChanged(OldColumn, NewColumn)
    else
    if not FIgnoreDataTypes then
    begin
      if not TypesEqual(OldColumn.DataType, NewColumn.DataType) and not NewColumn.AutoGenerated then
        ActionColumnTypeChanged(OldColumn, NewColumn)
      else
      begin
      if (OldColumn.Length <> NewColumn.Length) or (OldColumn.Precision <> NewColumn.Precision)
        or (OldColumn.Scale <> NewColumn.Scale) then
        ActionFieldSizeChanged(OldColumn, NewColumn);
      end;
    end;
  end;

  procedure CompareTables(OldTable, NewTable: TTableMetadata);
  var
    Column: TColumnMetadata;
    OldColumn: TColumnMetadata;
  begin
    // Compare table columns
    for Column in OldTable.Columns do
      if TSchemaUtils.FindColumn(NewTable, Column.Name) = nil then
        ActionColumnRemoved(Column);

    for Column in NewTable.Columns do
    begin
      OldColumn := TSchemaUtils.FindColumn(OldTable, Column.Name);
      if OldColumn = nil then
        ActionColumnAdded(Column)
      else
        CompareColumns(OldColumn, Column);
    end;

    // Check other table objects
    CheckPrimaryKey(OldTable, NewTable);
    CheckUniqueKeys(OldTable, NewTable);
    CheckForeignKeys(OldTable, NewTable);
  end;

var
  Table: TTableMetadata;
  OldTable: TTableMetadata;
  Sequence: TSequenceMetadata;
begin
  FActions.Clear;
  FWarnings.Clear;
  FErrors.Clear;
  FCommands.Clear;

  // Check removed tables
  for Table in OldDB.Tables do
  begin
    if TSchemaUtils.FindTable(NewDB, Table.Name, Table.Schema) = nil then
      ActionTableRemoved(Table);
  end;

  // Check new and changed tables
  for Table in NewDB.Tables do
  begin
    OldTable := TSchemaUtils.FindTable(OldDB, Table.Name, Table.Schema);
    if OldTable = nil then
      ActionTableAdded(Table)
    else
      CompareTables(OldTable, Table);
  end;

  // Check Sequences;
  for Sequence in OldDB.Sequences do
    if TSchemaUtils.FindSequence(NewDB, Sequence.Name) = nil then
      ActionSequenceRemoved(Sequence);
  for Sequence in NewDB.Sequences do
    if TSchemaUtils.FindSequence(OldDB, Sequence.Name) = nil then
      ActionSequenceAdded(Sequence);

  ProcessCommands(FCommands);
  BuildStatements(FCommands);
end;

function TDatabaseManager.GetWarningCount: integer;
begin
  Result := FWarnings.Count;
end;

function TDatabaseManager.GetWarnings: TEnumerable<TSchemaWarning>;
begin
  Result := FWarnings;
end;

procedure TDatabaseManager.ProcessCommands(Commands: TList<TDDLCommand>);
var
  Command: TDDLCommand;
  NewCommands: TList<TDDLCommand>;
  ForeignKey: TForeignKeyMetadata;
  CommandOrder: TDictionary<TClass, integer>;
begin
  NewCommands := TList<TDDLCommand>.Create;
  try
    // Extract foreign key statements to be executed separated from create/drop table.
    // We should do that because we might have circular references in foreign keys,
    // so all tables must be created before we start creating the foreign keys,
    // and all tables must be dropped after we drop foreign keys.
    // For now only SQLite doesn't require that, because it doesn't have alter table command
    // SQLite doesn't have this problem, since it allows creating table with foreign keys to a tables that don't exist yet
    if (TDBFeature.AlterTableForeignKey in SQLGenerator.SupportedFeatures) then
      for Command in Commands do
        if Command is TCreateTableCommand then
        begin
          for ForeignKey in TCreateTableCommand(Command).Table.ForeignKeys do
            NewCommands.Add(TCreateForeignKeyCommand.Create(ForeignKey));
        end
        else
        if Command is TDropTableCommand then
        begin
          // Same for drop table commands - first drop all foreign keys referencing that table
          for ForeignKey in TSchemaUtils.GetReferencingForeignKeys(
            TDropTableCommand(Command).Table.Database,
            TDropTableCommand(Command).Table) do
            NewCommands.Add(TDropForeignKeyCommand.Create(ForeignKey));
        end;

    Commands.AddRange(NewCommands);
  finally
    NewCommands.Free;
  end;

  // We can later optimize this
  CommandOrder := TDictionary<TClass, integer>.Create;
  try
    CommandOrder.Add(TDropForeignKeyCommand, -50);
    CommandOrder.Add(TDropUniqueKeyCommand, -40);
    CommandOrder.Add(TDropTableCommand, -30);
    CommandOrder.Add(TDropSequenceCommand, -20);
    CommandOrder.Add(TDropColumnCommand, -10);
    CommandOrder.Add(TCreateColumnCommand, 10);
    CommandOrder.Add(TCreateTableCommand, 20);
    CommandOrder.Add(TCreateSequenceCommand, 30);
//    CommandOrder.Add(TCreateUniqueKeyCommand, 40);
    CommandOrder.Add(TCreateForeignKeyCommand, 50);

    // Sort the commands by dependency -
    // drop foreign keys before, create after.
    Commands.Sort(TComparer<TDDLCommand>.Construct(
      function (const Left, Right: TDDLCommand): integer
      var
        LeftVal, RightVal: integer;
      begin
        if not CommandOrder.TryGetValue(Left.ClassType, LeftVal) then
          LeftVal := 0;
        if not CommandOrder.TryGetValue(Right.ClassType, RightVal) then
          RightVal := 0;
        Result := LeftVal - RightVal;
      end
      )
    );
  finally
    CommandOrder.Free;
  end;

  // Big workaround here, but we can change it later to a more elegant solution
  // (more meta information in ISQLGenerator
  if SameText(SQLGenerator.SqlDialect, 'SQLite') and FAllowDestructiveCommands then
  begin
    if AreSQLiteForeignKeysEnabled then
    begin
      // Disable foreign keys as first command, re-enable as last command
      Commands.Insert(0, TEnableForeignKeysCommand.Create(false));
      Commands.Add(TEnableForeignKeysCommand.Create(true));
    end;
  end;
end;

procedure TDatabaseManager.UpdateDatabase;
begin
  ValidateDatabase;
  if SQLExecutionEnabled then
    ExecuteSQLStatements(SQLStatements, false);
end;

function TDatabaseManager.ValidateDatabase: boolean;
begin
  ExistingDB.Clear;
  Importer.GetDatabaseMetadata(Connection, ExistingDB);
  BuildDatabaseMetadata(TargetDB);
  GetUpdateDatabaseCommands(ExistingDB, TargetDB);
  Result := (FWarnings.Count = 0) and (FActions.Count = 0) and (FErrors.Count = 0);
end;

end.
