unit Aurelius.Commands.Selecter;

{$I Aurelius.inc}

interface

uses
  DB, Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Criteria.Base,
  Aurelius.Drivers.Interfaces,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.Optimization,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Commands,
  Aurelius.Types.MasterObjectValue;

type
  TSelecter = class(TAbstractCommandPerformer)
  strict private
    type
      TNavigationRestriction = (None, Parent, Children);

      TPathMapInfo = class
      public
        Alias: string;
        constructor Create(AAlias: string);
      end;

      TCriteriaSolver = class(TInterfacedObject, ICriteriaSolver)
      private
        FSelecter: TSelecter;
        function InternalGetSqlExpression(ACriteria: TCriteria; AColumn: TColumn): string;
      private
        {$REGION 'ICriteriaSolver Methods'}
        function GetPropertySqlExpression(ACriteria: TCriteria; APropName: string): string;
        function GetColumnSqlExpression(ACriteria: TCriteria; AColumnName: string): string;
        function GetIdSqlExpressions(ACriteria: TCriteria): TArray<string>;
        {$ENDREGION}
      public
        constructor Create(ASelecter: TSelecter);
      end;
  strict private
    type
      TSelectCursor = class
      private
        FResultSet: IDBResultSet;
        FCommand: TSelectCommand;
        FCriteria: TCriteria;
        FFetching: boolean;
      public
        constructor Create;
        destructor Destroy; override;
      end;
  strict private
    FParent: TSelecter;
    FIntPathMap: TDictionary<string, TPathMapInfo>;
    function PathMapKey(APath: string; AClass: TClass): string;
    procedure AddPathMapEntry(APath: string);
    function GetPathMapEntry(APath: string; AClass: TClass): TPathMapInfo;
  strict private
    FCurrentCursor: TSelectCursor;
  strict private
    function Root: TSelecter;
    function PathMap: TDictionary<string, TPathMapInfo>;
    function CreateChildSelecter(Clazz: TClass): TSelecter;
    function CreateAnotherSelecter(Clazz: TClass): TSelecter;

    procedure AddDiscrFilterIfNecessary(Command: TSelectCommand; Params: TList<TDBParam>);

    procedure AddFieldsAndAssociations(Command: TSelectCommand; ACriteria: TCriteria; Depth: Integer;
      APath: string; ASelectFields: boolean; ANavigationRestriction: TNavigationRestriction);
    procedure AddFieldsForJoinedTablesInheritance(Command: TSelectCommand; ACriteria: TCriteria;
      Table: TSQLTable; Depth: Integer; APath: string; ASelectFields: boolean; ANavigationRestriction: TNavigationRestriction);

    function ExecuteSelectSome(Command: TSelectCommand; ACriteria: TCriteria): IDBResultSet;
    function ExecuteSelectDetails(Command: TSelectCommand; MemberInfo: TRttiOptimization; MasterKey: Variant): IDBResultSet;

    function HasNullId(ResultSet: IDBResultSet): Boolean;
    function ReadObject(ResultObj: TObject; Command: TSelectCommand; ResultSet: IDBResultSet; Depth: Integer; DontGoUpInheritance: Boolean): TObject;
    function ReadProjectionsObject(Command: TSelectCommand; ResultSet: IDBResultSet; ACriteria: TCriteria): TCriteriaResult;
    function ReadFieldFromResultset(ResultSet: IDBResultSet; Col: TColumn): Variant;

    procedure AddJoin(ATargetClass: TClass; Command: TSelectCommand; ACriteria: TCriteria;
      Required: Boolean; JoinCols: TList<TColumn>; Depth: Integer; APath: string; ASelectFields: boolean;
      ANavigationRestriction: TNavigationRestriction);
  public
    constructor Create; override;
    destructor Destroy; override;

    // SelectBegin could return a TSelectCursor, and SelectFetch could get it, so it would be a better approach.
    // For now the cursor is in FCurrentCursor.
    // But this TSelect class is an internal class, so it's unlikely we will need it - we use only one selecter per fetching.
    // we can change later if needed
    procedure SelectBegin(ACriteria: TCriteria);
    function SelectNext: boolean;
    function SelectFetch: TObject;
    procedure SelectEnd;

    procedure SelectDetails(MemberInfo: TRttiOptimization; MasterKey: Variant; ResultList: TList<TObject>; Depth: Integer); overload;
  end;

implementation
uses
  Variants, Rtti,
  Aurelius.Commands.Exceptions,
  Aurelius.Engine.ObjectFactory,
  Aurelius.Engine.ObjectManager,
  Aurelius.Global.Config;

type
  TInternalCriteria = class(TCriteria)
  end;
  TInternalProjection = class(TProjection)
  end;
  TInternalObjectManager = class(TObjectManager)
  end;

{ TSelecter }

procedure TSelecter.AddDiscrFilterIfNecessary(Command: TSelectCommand;
  Params: TList<TDBParam>);
var
  WhereField: TSQLWhereField;
  NewParamName: string;
  I: Integer;
begin
  if Length(DiscriminatorValues) > 0 then
  begin
    WhereField := TSQLWhereField.Create(CreateSQLTable, DiscriminatorColumn.Name, TWhereOperator.woEqual);
    Command.WhereFields.Add(WhereField);
    WhereField.ParamName := TDBUtils.GetValidParamName(Params);
    Params.Add(TDBParam.Create(WhereField.ParamName, DiscriminatorColumn.FieldType, DiscriminatorValues[0]));

    // Check if we have a "in" operation
    if Length(DiscriminatorValues) > 1 then
    begin
      WhereField.WhereOperator := TWhereOperator.woIn;
      for I := 1 to Length(DiscriminatorValues) - 1 do
      begin
        NewParamName := TDBUtils.GetValidParamName(Params);
        Params.Add(TDBParam.Create(NewParamName, DiscriminatorColumn.FieldType, DiscriminatorValues[I]));
        WhereField.ParamName := WhereField.ParamName + ';' + NewParamName;
      end;
    end;
  end;
end;

procedure TSelecter.AddFieldsForJoinedTablesInheritance(Command: TSelectCommand;
  ACriteria: TCriteria; Table: TSQLTable; Depth: Integer; APath: string; ASelectFields: boolean;
  ANavigationRestriction: TNavigationRestriction);
var
  OrgJoinCol, RefJoinCol: TColumn;
  JoinCols: TList<TColumn>;
  SubClasses: TEnumerable<TClass>;
  Cla: TClass;
begin
  if ANavigationRestriction <> TNavigationRestriction.Parent then
  begin
    if HasPrimaryJoinColumn then
    begin
      JoinCols := TList<TColumn>.Create;
      try
        for RefJoinCol in Explorer.GetPrimaryJoinColumns(Self.Clazz) do
          JoinCols.Add(RefJoinCol);
        AddJoin(Self.Clazz.ClassParent, Command, ACriteria, False, JoinCols, Depth, APath, ASelectFields, TNavigationRestriction.Children);
      finally
        JoinCols.Free;
      end;
    end;
  end;

  if ANavigationRestriction <> TNavigationRestriction.Children then
  begin
    if Explorer.HasInheritance(Self.Clazz) then
      if Explorer.GetInheritanceStrategy(Self.Clazz) = TInheritanceStrategy.JoinedTables then
      begin
        SubClasses := Explorer.Hierarchy.GetDirectSubClasses(Self.Clazz);
        for Cla in SubClasses do
        begin
          JoinCols := TObjectList<TColumn>.Create(true);
          try
            for RefJoinCol in Explorer.GetPrimaryJoinColumns(Cla) do
            begin
              // Join starts at Cla.ClassParent and references Cla
              // OrgJoinCol is the parent column
              OrgJoinCol := RefJoinCol.ReferencedColumn.Clone;
              JoinCols.Add(OrgJoinCol);
              OrgJoinCol.ReferencedColumn := RefJoinCol;
              OrgJoinCol.ReferencedClass := Cla;
            end;
            AddJoin(Cla, Command, ACriteria, False, JoinCols, Depth, APath, ASelectFields, TNavigationRestriction.Parent);
          finally
            JoinCols.Free;
          end;
        end;
      end;
  end;
end;

procedure TSelecter.AddJoin(ATargetClass: TClass; Command: TSelectCommand; ACriteria: TCriteria;
  Required: Boolean; JoinCols: TList<TColumn>; Depth: Integer; APath: string; ASelectFields: boolean;
  ANavigationRestriction: TNavigationRestriction);
var
  SQLJoin: TSQLJoin;
  C: TColumn;
  ChildSelecter: TSelecter;
begin
//  if Required then
//    SQLJoin := TSQLJoin.Create(TJoinType.Inner)
//  else
    SQLJoin := TSQLJoin.Create(TJoinType.Left);


  ChildSelecter := CreateChildSelecter(ATargetClass);
  try
    // Get next alias for the join and associate the class being selected with the alias
    ChildSelecter.Alias := Command.NextAlias;
    ChildSelecter.AddPathMapEntry(APath);

    for C in JoinCols do
    begin
      Assert(C.ReferencedColumn <> nil);
      if not C.IsForeign then
      begin
        SQLJoin.Segments.Add(
          TSQLJoinSegment.Create(
            TSQLField.Create(ChildSelecter.CreateSQLTable, C.ReferencedColumn.Name),
            TSQLField.Create(Self.CreateSQLTable, C.Name)))
      end else
      begin
        // with many-valued associations, the join columns are inverted. The referencedcolumn is the parent class
        // while the column is the associated class
        SQLJoin.Segments.Add(
          TSQLJoinSegment.Create(
            TSQLField.Create(ChildSelecter.CreateSQLTable, C.Name),
            TSQLField.Create(Self.CreateSQLTable, C.ReferencedColumn.Name)))
      end;
    end;

    Command.Joins.Add(SQLJoin);

    ChildSelecter.AddFieldsAndAssociations(Command, ACriteria, Depth, APath, ASelectFields, ANavigationRestriction);
  finally
    ChildSelecter.Free;
  end;
end;

procedure TSelecter.AddPathMapEntry(APath: string);
begin
  PathMap.Add(PathMapKey(APath, Self.Clazz), TPathMapInfo.Create(Self.Alias));
end;

procedure TSelecter.AddFieldsAndAssociations(Command: TSelectCommand; ACriteria: TCriteria; Depth: Integer;
  APath: string; ASelectFields: boolean; ANavigationRestriction: TNavigationRestriction);
var
  C: TColumn;
  A: TAssociation;
  table: TSQLTable;
  associations: TList<TAssociation>;
  selectField: TSQLSelectField;
  hasProjection: boolean;
  associationCriteria: TCriteria;
  associationPath: string;
begin
  hasProjection := (ACriteria <> nil) and TInternalCriteria(ACriteria).HasProjection;
  table := CreateSQLTable;
  try
    if ASelectFields and not hasProjection then
      for C in FColumns do
      begin
        if C.ReferencedClass <> nil then
          Continue;
        if C.IsForeign then
          Continue;
        if C.IsLazy then
          Continue;

        selectField := TSQLSelectField.Create(table.Clone, C.Name);
        Command.SelectFields.Add(selectField);
      end;

    AddFieldsForJoinedTablesInheritance(Command, ACriteria, table, Depth, APath, ASelectFields, ANavigationRestriction);

    if Depth <= TGlobalConfigs.GetInstance.MaxEagerFetchDepth then
    begin
      associations := FAssociations;
      if Explorer.HasInheritance(Self.Clazz) then
        if Explorer.GetInheritanceStrategy(Self.Clazz) = TInheritanceStrategy.JoinedTables then
          associations := Explorer.GetAssociations(Self.Clazz, False, False);

      for A in associations do
      begin
//        if A.Kind = TAssociationKind.ManyValued then
//          Continue;

        if ACriteria <> nil then
          associationCriteria := TInternalCriteria(ACriteria).FindByAssociation(A)
        else
          associationCriteria := nil;
        associationPath := APath + '.' + A.ClassMemberName;

        if hasProjection or not ASelectFields or (A.Kind = TAssociationKind.ManyValued) then
        begin
          // If this select has projections, then we are not retrieving objects. So eager or lazy mode is not relevant here.
          // The objects being retrieved are just values from custom select fields. So all we need to do is to check
          // if the criteria has this association. If it does, then we need to add a join with no select fields,
          // so that the where clause can find the field being filtered using the join
          // Extra note: we also don't take Depth in consideration, since the depth in criteria is unlimited
          //
          // Additional info: even if the criteria has no projections, we also enter this piece of code if the ASelectFields
          // is false. If it's false, it means that it's already coming from a criteria with association (even with no projections).
          // Thus, we only need to add joins if, again, this current association is used in criteria.
          //
          // Additional comment (25-July): support for criteria with many-valued association was added. So if the association
          // is many-valued, then it can be included as well. But only if it has criteria, so the logic is the same as of
          // queries using projections.
          if associationCriteria <> nil then
            AddJoin(A.Target, Command, associationCriteria, A.Required, A.JoinColumns, 1, associationPath, False, TNavigationRestriction.None);
        end
        else
        if not A.Lazy then
        begin
          // Add the key fields in the select
          // This is needed because if the association is part of ID, then at some point we need to check (HasNullId)
          // if this object has a valid Id, and the foreign key values must be present.
          for C in A.JoinColumns do
          begin
            if C.IsForeign then
              Continue;
            Command.SelectFields.Add(TSQLSelectField.Create(CreateSQLTable, C.Name));
          end;

          AddJoin(A.Target, Command, ACriteria, A.Required, A.JoinColumns, Depth + 1, associationPath, True, TNavigationRestriction.None);
        end
        else
        begin // ftLazy
          for C in A.JoinColumns do
          begin
            if C.IsForeign then
              Continue;
            Command.SelectFields.Add(TSQLSelectField.Create(CreateSQLTable, C.Name));
          end;

          // If we have a subcriteria related to this association (e.g., MainClass.Customer.Name = 'Jack')
          // then we must add a join to the target table of association, so that we can add expressions
          // in Where clause based on fields of associated table - we will not retrieve the fields (it's
          // a lazy load) but we need the join
          // Don't take Depth into consideration
          if associationCriteria <> nil then
            AddJoin(A.Target, Command, associationCriteria, A.Required, A.JoinColumns, 1, associationPath, False, TNavigationRestriction.None);
        end;
      end;
    end;
  finally
    table.Free;
  end;
end;

function TSelecter.PathMap: TDictionary<string, TPathMapInfo>;
begin
  Result := Root.FIntPathMap;
end;

function TSelecter.PathMapKey(APath: string; AClass: TClass): string;
begin
  Result := APath;
  if Explorer.HasInheritance(AClass) then
    case Explorer.GetInheritanceStrategy(AClass) of
      TInheritanceStrategy.JoinedTables:
        Result := APath + '.' + AClass.ClassName;
    end;
end;

constructor TSelecter.Create;
begin
  inherited;
  FIntPathMap := TObjectDictionary<string, TPathMapInfo>.Create([doOwnsValues]);
end;

function TSelecter.CreateAnotherSelecter(Clazz: TClass): TSelecter;
begin
  Result := TSelecter.Create;
  Result.SetConnection(FConnection);
  Result.SetSQLGenerator(SQLGenerator);
  Result.SetExplorer(Explorer);
  Result.SetClass(Clazz);
  Result.SetEntityManager(FEntityManager);
  Result.AddExecutionListeners(FExecutionListeners);
end;

function TSelecter.CreateChildSelecter(Clazz: TClass): TSelecter;
begin
  Result := CreateAnotherSelecter(Clazz);
  Result.FParent := Self;
end;

destructor TSelecter.Destroy;
begin
  FIntPathMap.Free;
  SelectEnd; // just in case someone forget to call it
  inherited;
end;

function TSelecter.ExecuteSelectDetails(Command: TSelectCommand; MemberInfo: TRttiOptimization; MasterKey: Variant): IDBResultSet;
var
  SQL: string;
  Params: TObjectList<TDBParam>;
  MasterClass: TClass;
  MasterCols: TList<TColumn>;
  C: TColumn;
  WhereField: TSQLWhereField;
  ParamValue: Variant;
  I: integer;
begin
  Params := TObjectList<TDBParam>.Create;
  try
    Command.Table := CreateSQLTable;
    AddPathMapEntry('');
    AddFieldsAndAssociations(Command, nil, 1, '', True, TNavigationRestriction.None);
    AddDiscrFilterIfNecessary(Command, Params);

    MasterClass := MemberInfo.MemberRef.Parent.AsInstance.MetaclassType;

    MasterCols := Explorer.GetColumns(MasterClass, True, True);
    I := 0;
    // Iterate through foreign keys
    for C in MasterCols do
    begin
      if C.Optimization.MemberName <> MemberInfo.MemberName then
        Continue;
      if HasJoinedTablesStrategy and (C.ForeignClass <> Self.Clazz) then
        Continue;

      WhereField := TSQLWhereField.Create(CreateSQLTable, C.Name, TWhereOperator.woEqual);
      Command.WhereFields.Add(WhereField);
      WhereField.ParamName := TDBUtils.GetValidParamName(Params);

      if VarIsArray(MasterKey) then
      begin
        Assert(I < Length(MasterKey));
        ParamValue := MasterKey[I];
      end else
      begin
        Assert(I = 0, 'Master key array incompatible a single foreign column');
        ParamValue := MasterKey;
      end;
      Params.Add(TDBParam.Create(WhereField.ParamName, C.FieldType, ParamValue));
    end;

    SQL := SQLGenerator.GenerateSelect(Command);
    Result := Execute(SQL, Params, True);
  finally
    Params.Free;
  end;
end;

function TSelecter.ExecuteSelectSome(Command: TSelectCommand;
  ACriteria: TCriteria): IDBResultSet;
var
  sql: string;
  params: TObjectList<TDBParam>;
  builder: TCriteriaBuilder;
  Solver: ICriteriaSolver;
begin
  Command.Table := CreateSQLTable;
  AddPathMapEntry('');
  AddFieldsAndAssociations(Command, ACriteria, 1, '', True, TNavigationRestriction.None);

  params := TObjectList<TDBParam>.Create;
  try
    Solver := TCriteriaSolver.Create(Self);
    builder := TCriteriaBuilder.Create(Solver, SQLGenerator, params);
    try
      Command.SelectStatement := builder.GetSelectFromProjections(ACriteria);
      Command.WhereStatement := builder.GetWhereStatement(ACriteria);
      Command.GroupByStatement := builder.GetGroupByStatement(ACriteria);
      Command.HavingStatement := builder.GetHavingStatement(ACriteria);
      Command.OrderByStatement := builder.GetOrderByStatement(ACriteria);
    finally
      builder.Free;
    end;

    AddDiscrFilterIfNecessary(Command, params);

    Command.FirstRow := TInternalCriteria(ACriteria).FirstRow;
    Command.MaxRows := TInternalCriteria(ACriteria).MaxRows;

    sql := SQLGenerator.GenerateSelect(Command);
    Result := Execute(sql, params, True);
  finally
    params.Free;
  end;
end;

function TSelecter.GetPathMapEntry(APath: string; AClass: TClass): TPathMapInfo;
begin
  PathMap.TryGetValue(PathMapKey(APath, AClass), Result);
end;

function TSelecter.HasNullId(ResultSet: IDBResultSet): Boolean;
var
  Field: TSQLSelectField;
  FieldName: string;
begin
  // We just need to check if any of the id columns is null, because the purpose
  // is to see if there is a record for the specified table. If problems arise, review this later
  Field := TSQLSelectField.Create(CreateSQLTable, FPKColumns[0].Name);
  try
    FieldName := SQLGenerator.GetDefaultColumnName(Field);
    Result := ReadFieldValue(ResultSet, FieldName, FPKColumns[0].FieldType) = Variants.Null;
  finally
    Field.Free;
  end;
end;

function TSelecter.ReadFieldFromResultset(ResultSet: IDBResultSet; Col: TColumn): Variant;
var
  Field: TSQLSelectField;
  FieldName: string;
begin
  Field := TSQLSelectField.Create(CreateSQLTable, Col.Name);
  try
    FieldName := SQLGenerator.GetDefaultColumnName(Field);
    Result := ReadFieldValue(ResultSet, FieldName, Col.FieldType);
  finally
    Field.Free;
  end;
end;

function TSelecter.ReadObject(ResultObj: TObject; Command: TSelectCommand;
  ResultSet: IDBResultSet; Depth: Integer; DontGoUpInheritance: Boolean): TObject;
var
  C: TColumn;
  FieldValue: Variant;
  A: TAssociation;
  ChildSelecter: TSelecter;
  ChildObject: TObject;
  ConcreteClass, ListMemberClass: TClass;
  DetailObjects: TList<TObject>;
  ChildClasses: TList<TClass>;
  I: Integer;
  DestinyTable: string;
  JoinColumns: TArray<TColumn>;
  FKFields: array of string;
  Associations: TList<TAssociation>;
  AssociationIsNull: boolean;
  J: Integer;
begin
  if ResultObj = nil then
  begin
    if HasNullId(ResultSet) then
      Exit(nil);

    Result := nil;
    if Explorer.HasInheritance(Self.Clazz) then
      if Explorer.GetInheritanceStrategy(Self.Clazz) = TInheritanceStrategy.JoinedTables then
      begin
        ChildClasses := Tlist<TClass>.Create(Explorer.Hierarchy.GetDirectSubClasses(Self.Clazz));
        try
          for I := ChildClasses.Count - 1 downto 0 do
          begin
            ChildSelecter := CreateAnotherSelecter(ChildClasses[I]);
            try
              DestinyTable := Explorer.GetTable(ChildSelecter.Clazz).Name;

              JoinColumns := Explorer.GetPrimaryJoinColumns(ChildSelecter.Clazz.ClassParent);
              if Length(JoinColumns) = 0 then
                JoinColumns := Explorer.GetIdColumns(ChildSelecter.Clazz.ClassParent);

              SetLength(FKFields, Length(JoinColumns));
              for J := 0 to Length(JoinColumns) - 1 do
                FKFields[J] := JoinColumns[J].Name;

              ChildSelecter.Alias := Command.GetAliasFromTable(Self.Alias, DestinyTable, FKFields);

              if not ChildSelecter.HasNullId(ResultSet) then
              begin
                Result := ChildSelecter.ReadObject(nil, Command, ResultSet, Depth, True);
                Break;
              end;
            finally
              ChildSelecter.Free;
            end;
          end;
        finally
          ChildClasses.Free;
        end;
      end;

    if Result <> nil then
      ConcreteClass := Result.ClassType
    else
    begin
      if (DiscriminatorColumn <> nil) then
      begin
        FieldValue := ReadFieldFromResultset(ResultSet, DiscriminatorColumn);
        ConcreteClass := Explorer.GetClassByDiscriminator(Self.Clazz, FieldValue);
      end
      else
        ConcreteClass := Self.Clazz;

      Result := TObjectFactory.GetInstance.CreateInstance(ConcreteClass);
    end;
  end
  else
  begin
    Result := ResultObj;
    ConcreteClass := Result.ClassType;
  end;

  for C in FColumns do
  begin
    if C.IsDiscriminator then
      Continue;
    if C.ReferencedClass <> nil then
      Continue;
    if not ConcreteClass.InheritsFrom(C.DeclaringClass) then
      Continue;
    if C.IsForeign then
      Continue;
    if C.IsLazy then
      Continue;
//    if (ResultObj <> nil) and C.IsId then // TODO: Review this for composited id
//      Continue;

    FieldValue := ReadFieldFromResultset(ResultSet, C);
    Explorer.SetColumnDbValue(Result, C, FieldValue);
  end;

  if not DontGoUpInheritance and HasPrimaryJoinColumn then
  begin
    ChildSelecter := CreateAnotherSelecter(Self.Clazz.ClassParent);
    try
      DestinyTable := Explorer.GetTable(ChildSelecter.Clazz).Name;

      JoinColumns := Explorer.GetPrimaryJoinColumns(Self.Clazz);
      SetLength(FKFields, Length(JoinColumns));
      for I := 0 to Length(JoinColumns) -1 do
        FKFields[I] := JoinColumns[I].Name;

      ChildSelecter.Alias := Command.GetAliasFromTable(Self.Alias, DestinyTable, FKFields);
      ChildSelecter.ReadObject(Result, Command, ResultSet, Depth, False);
    finally
      ChildSelecter.Free;
    end;
  end;

  if Depth <= TGlobalConfigs.GetInstance.MaxEagerFetchDepth then
  begin
    Associations := Explorer.GetAssociations(ConcreteClass, True, False);
    if Explorer.HasInheritance(Self.Clazz) then
      if Explorer.GetInheritanceStrategy(Self.Clazz) = TInheritanceStrategy.JoinedTables then
        Associations := Explorer.GetAssociations(Self.Clazz, False, False);

    for A in Associations do
    begin
      if A.Kind = TAssociationKind.SingleValued then
      begin
        if not A.Lazy then
        begin
          ChildSelecter := CreateAnotherSelecter(A.Target);
          try
            DestinyTable := Explorer.GetTable(ChildSelecter.Clazz).Name;
            SetLength(FKFields, A.JoinColumns.Count);
            for I := 0 to A.JoinColumns.Count -1 do
              FKFields[I] := A.JoinColumns[I].Name;
            ChildSelecter.Alias := Command.GetAliasFromTable(Self.Alias, DestinyTable, FKFields);
            ChildObject := ChildSelecter.ReadObject(nil, Command, ResultSet, Depth + 1, False);
            Explorer.SetMemberValue(Result, A.Optimization, ChildObject);
          finally
            ChildSelecter.Free;
          end;
        end
        else // ftLazy
        begin
          if A.JoinColumns.Count = 1 then
          begin
            FieldValue := ReadFieldFromResultset(ResultSet, A.JoinColumns[0]);
            AssociationIsNull := VarIsNull(FieldValue);
          end
          else
          begin
            FieldValue := VarArrayCreate([0, A.JoinColumns.Count - 1], varVariant);
            AssociationIsNull := true;
            for I := 0 to A.JoinColumns.Count - 1 do
            begin
              FieldValue[I] := ReadFieldFromResultSet(ResultSet, A.JoinColumns[I]);
              if not VarIsNull(FieldValue[I]) then
                AssociationIsNull := false;
            end;
          end;

          if AssociationIsNull then
            Explorer.SetMemberValue(Result, A.Optimization, TValue.Empty)
          else
            Explorer.SetProxyController(Result, A.Optimization,
              TInternalObjectManager(FEntityManager).CreateProxyController(
                TInternalObjectManager.TOptimizedProxyInfo.Create(A.Optimization, FieldValue)));
        end;
      end
      else // akManyValued
      begin
        if not A.Lazy then
        begin
          ChildSelecter := CreateAnotherSelecter(A.Target);
          try
            ListMemberClass := A.Optimization.MemberClass;
            if A.JoinColumns.Count = 1 then
              FieldValue := ReadFieldFromResultset(ResultSet, A.JoinColumns[0].ReferencedColumn)
            else
            begin
              FieldValue := VarArrayCreate([0, A.JoinColumns.Count - 1], varVariant);
              for I := 0 to A.JoinColumns.Count - 1 do
                FieldValue[I] := ReadFieldFromResultSet(ResultSet, A.JoinColumns[I].ReferencedColumn);
            end;

            DetailObjects := TList<TObject>(Explorer.GetMemberValue(Result, A.Optimization).AsObject);
            if DetailObjects = nil then
              DetailObjects := TList<TObject>(TObjectFactory.GetInstance.CreateInstance(ListMemberClass));
            DetailObjects.Clear;
            ChildSelecter.SelectDetails(A.Optimization, FieldValue, DetailObjects, Depth + 1);
            Explorer.SetMemberValue(Result, A.Optimization, DetailObjects);
          finally
            ChildSelecter.Free;
          end;
        end
        else
        begin
          if A.JoinColumns.Count = 1 then
            FieldValue := ReadFieldFromResultset(ResultSet, A.JoinColumns[0].ReferencedColumn)
          else
          begin
            FieldValue := VarArrayCreate([0, A.JoinColumns.Count - 1], varVariant);
            for I := 0 to A.JoinColumns.Count - 1 do
              FieldValue[I] := ReadFieldFromResultSet(ResultSet, A.JoinColumns[I].ReferencedColumn);
          end;
          Explorer.SetProxyController(Result, A.Optimization,
            TInternalObjectManager(FEntityManager).CreateProxyController(
              TInternalObjectManager.TOptimizedProxyInfo.Create(A.Optimization, FieldValue, Result)));
        end;
      end;
    end;
  end;

  // Set lazy attributes for blob
  // This must be AFTER all associations are loaded, because we might have a composite id that includes
  // an association, and thus we should have the id fields for association already loaded
  for C in FColumns do
    if C.IsLazy then
      Explorer.SetBlobController(Result, C.Optimization,
        TInternalObjectManager(FEntityManager).CreateBlobController(
          TInternalObjectManager.TBlobInfo.Create(C.Optimization, Explorer.GetIdValue(Result))));
end;

function TSelecter.ReadProjectionsObject(Command: TSelectCommand;
  ResultSet: IDBResultSet; ACriteria: TCriteria): TCriteriaResult;
var
  fieldNames: TArray<string>;
  fieldTypes: TArray<TFieldType>;
  userAliases: TArray<string>;
  fieldValue: Variant;
  i: integer;
  props: TObjectList<TCriteriaResultProp>;
  projection: TInternalProjection;
  propName: string;
  Solver: ICriteriaSolver;
begin
  projection := TInternalProjection(TInternalCriteria(ACriteria).Projection);
  fieldNames := projection.GetSqlAliases(0);
  userAliases := projection.GetUserAliases(0);
  Solver := TCriteriaSolver.Create(Self); // needs to be declared in separated variable when optimization is on to avoid mem leaks
  fieldTypes := projection.GetTypes(ACriteria, Solver);
  Assert(Length(fieldNames) = Length(fieldTypes));
  Assert(Length(fieldNames) = Length(userAliases));

  props := TObjectList<TCriteriaResultProp>.Create(True);
  try
    for i := Low(fieldNames) to High(fieldNames) do
    begin
      if userAliases[i] <> '' then
        propName := userAliases[i]
      else
        propName := fieldNames[i];
      fieldValue := ReadFieldValue(ResultSet, fieldNames[i], fieldTypes[i]);
      props.Add(TCriteriaResultProp.Create(propName, fieldTypes[i], fieldValue));
    end;
  except
    props.Free;
    raise;
  end;

  Result := TCriteriaResult.Create(props);
end;

function TSelecter.Root: TSelecter;
begin
  if FParent <> nil then
    Result := FParent.Root
  else
    Result := Self;
end;

procedure TSelecter.SelectBegin(ACriteria: TCriteria);
begin
  if (FCurrentCursor <> nil) then
    raise ESelectAlreadyOpen.Create;

  FCurrentCursor := TSelectCursor.Create;
  try
    FCurrentCursor.FCriteria := ACriteria;
    FCurrentCursor.FResultSet := ExecuteSelectSome(FCurrentCursor.FCommand, ACriteria);
  except
    FCurrentCursor.Free;
    FCurrentCursor := nil;
    raise;
  end;
end;

procedure TSelecter.SelectDetails(MemberInfo: TRttiOptimization; MasterKey: Variant;
  ResultList: TList<TObject>; Depth: Integer);
var
  ResultSet: IDBResultSet;
  Command: TSelectCommand;
  Obj: TObject;
begin
  Command := TSelectCommand.Create;
  try
    ResultSet := ExecuteSelectDetails(Command, MemberInfo, MasterKey);
    while ResultSet.Next do
    begin
      Obj := ReadObject(nil, Command, ResultSet, Depth, False);
      if Obj = nil then
        raise ENilObjectReturned.Create;
      ResultList.Add(Obj);
    end;
  finally
    Command.Free;
  end;
end;

procedure TSelecter.SelectEnd;
begin
  if FCurrentCursor <> nil then
  begin
    FCurrentCursor.Free;
    FCurrentCursor := nil;
  end;
end;

function TSelecter.SelectFetch: TObject;
begin
  if (FCurrentCursor = nil) then
    raise ESelectNotOpen.Create;
  if not FCurrentCursor.FFetching then
    raise ECursorNotFetching.Create;

  if TInternalCriteria(FCurrentCursor.FCriteria).HasProjection then
    Result := ReadProjectionsObject(FCurrentCursor.FCommand, FCurrentCursor.FResultSet, FCurrentCursor.FCriteria)
  else
    Result := ReadObject(nil, FCurrentCursor.FCommand, FCurrentCursor.FResultSet, 1, False);
  if Result = nil then
    raise ENilObjectReturned.Create;
end;

function TSelecter.SelectNext: boolean;
begin
  if (FCurrentCursor = nil) then
    raise ESelectNotOpen.Create;

  if not FCurrentCursor.FFetching then
    FCurrentCursor.FFetching := true;
  Result := FCurrentCursor.FResultSet.Next;
end;

{ TSelecter.TCriteriaSolver }

type
  TInternalSelecter = class(TSelecter)
  end;

constructor TSelecter.TCriteriaSolver.Create(ASelecter: TSelecter);
begin
  FSelecter := ASelecter;
end;

function TSelecter.TCriteriaSolver.GetColumnSqlExpression(ACriteria: TCriteria; AColumnName: string): string;
var
  Column: TColumn;
begin
  TInternalCriteria.TranslateAliasedProperty(ACriteria, AColumnName);
  Column := TInternalSelecter(FSelecter).Explorer.GetColumnByName(ACriteria.Clazz, AColumnName);
  Result := InternalGetSqlExpression(ACriteria, Column);
end;

function TSelecter.TCriteriaSolver.GetIdSqlExpressions(
  ACriteria: TCriteria): TArray<string>;
var
  Columns: TArray<TColumn>;
  Info: TPathMapInfo;
  SelectField: TSQLSelectField;
  I: Integer;
begin
  Columns := TInternalSelecter(FSelecter).Explorer.GetIdColumns(ACriteria.Clazz);
  SetLength(Result, Length(Columns));
  if Length(Columns) = 0 then Exit;

  Info := FSelecter.GetPathMapEntry(TInternalCriteria(ACriteria).FullPath, Columns[0].DeclaringClass);
  Assert(Info <> nil, 'Could not retrieve aliased id column name in criteria');

  for I := 0 to Length(Columns) - 1 do
  begin
    SelectField := TSQLSelectField.Create(
      TSQLTable.Create(TInternalSelecter(FSelecter).Explorer.GetTable(ACriteria.Clazz).Name, Info.Alias),
      Columns[I].Name);
    try
      Result[I] := FSelecter.SQLGenerator.GetQualifiedColumnName(SelectField);
    finally
      SelectField.Free;
    end;
  end;
end;

function TSelecter.TCriteriaSolver.GetPropertySqlExpression(ACriteria: TCriteria;
  APropName: string): string;
var
  Column: TColumn;
begin
  TInternalCriteria.TranslateAliasedProperty(ACriteria, APropName);
  Column := TInternalSelecter(FSelecter).Explorer.GetColumnByPropertyName(ACriteria.Clazz, APropName);
  Result := InternalGetSqlExpression(ACriteria, Column);
end;

function TSelecter.TCriteriaSolver.InternalGetSqlExpression(ACriteria: TCriteria; AColumn: TColumn): string;
var
  Info: TPathMapInfo;
  SelectField: TSQLSelectField;
  FieldName: string;
begin
  Info := FSelecter.GetPathMapEntry(TInternalCriteria(ACriteria).FullPath, AColumn.DeclaringClass);
  Assert(Info <> nil, 'Could not retrieve aliased column name in criteria');

  SelectField := TSQLSelectField.Create(
    TSQLTable.Create(TInternalSelecter(FSelecter).Explorer.GetTable(ACriteria.Clazz).Name, Info.Alias),
    AColumn.Name);
  try
    FieldName := FSelecter.SQLGenerator.GetQualifiedColumnName(SelectField);
  finally
    SelectField.Free;
  end;

  Result := FieldName;
end;

{ TSelecter.TPathMapInfo }

constructor TSelecter.TPathMapInfo.Create(AAlias: string);
begin
  Self.Alias := AAlias;
end;

{ TSelecter.TSelectCursor }

constructor TSelecter.TSelectCursor.Create;
begin
  FCommand := TSelectCommand.Create;
  FFetching := false;
end;

destructor TSelecter.TSelectCursor.Destroy;
begin
  FResultSet := nil;
  FCommand.Free;
  FCommand := nil;
  inherited;
end;

end.

