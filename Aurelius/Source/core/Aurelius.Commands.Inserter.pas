unit Aurelius.Commands.Inserter;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Types.MasterObjectValue;

type
  TInserter = class(TAbstractCommandPerformer)
  private
    function CreateAnotherInserter(Clazz: TClass): TInserter;
    procedure InsertMasterRecordForJoinedTablesInheritance(Entity: TObject; MasterObj: TMasterObjectValue);
  public
    procedure Insert(Entity: TObject; MasterObj: TMasterObjectValue);
  end;

implementation

uses
  Variants, Rtti,
  Aurelius.Commands.Exceptions,
  Aurelius.Drivers.Interfaces,
  Aurelius.Id.AbstractGenerator,
  Aurelius.Mapping.Metadata,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Commands,
  Aurelius.Sql.Interfaces
  ;

{ TInserter }

function TInserter.CreateAnotherInserter(Clazz: TClass): TInserter;
begin
  Result := TInserter.Create;

  Result.SetConnection(FConnection);
  Result.SetSQLGenerator(SQLGenerator);
  Result.SetExplorer(Explorer);
  Result.SetClass(Clazz);
  Result.SetEntityManager(FEntityManager);
  Result.AddExecutionListeners(FExecutionListeners);
end;

procedure TInserter.Insert(Entity: TObject; MasterObj: TMasterObjectValue);
var
  Command: TInsertCommand;
  C: TColumn;
  SQL, ParamName: string;
  ParamValue: Variant;
  Params: TObjectList<TDBParam>;
  Field: TSQLField;
  Trans: IDBTransaction;
  MasterClass: TClass;
  MasterCols: TList<TColumn>;
  IdGenerator: TAbstractGenerator;
  NewId: Variant;
  InsertId: boolean;
begin
  IdGenerator := Explorer.GetIdentifierGenerator(Entity.ClassType, SQLGenerator);
  try
    InsertId := true;
    if not Explorer.HasIdValue(Entity) then
    begin
      // Generate an id for the entity based on selected generator

      // If GenerateId returns empty variant, then the generator
      // is relying on the database to generate the id, so we must not
      // insert the id
      NewId := IdGenerator.GenerateId(Entity, GetCommandPerformer);
      if not VarIsEmpty(NewId) then
        Explorer.SetIdValue(Entity, NewId)
      else
        InsertId := false;
    end;

    InsertMasterRecordForJoinedTablesInheritance(Entity, MasterObj);

    Command := TInsertCommand.Create;
    Params := TObjectList<TDBParam>.Create;
    try
      Command.Table := CreateSQLTable;

      for C in FColumns do
      begin
        if C.IsForeign then
          Continue;
        if not Self.Clazz.InheritsFrom(C.DeclaringClass) then
          Continue;
        if (TColumnProp.NoInsert in C.Properties) then
          Continue;
        if not InsertId and IsIdColumn(C) then
          Continue;

        ParamValue := Explorer.GetColumnDbValue(Entity, C);
        if not (TColumnProp.Required in C.Properties) and (VarIsNull(ParamValue)) then
          Continue;

        Field := TSQLField.Create(CreateSQLTable, C.Name);
        Command.InsertFields.Add(Field);
        ParamName := SQLGenerator.GetDefaultParamName(Field);
        Params.Add(TDBParam.Create(ParamName, C.FieldType, ParamValue));
      end;

      if MasterObj.MasterObject <> nil then
      begin
        MasterClass := MasterObj.MasterObject.ClassType;

        MasterCols := Explorer.GetColumns(MasterClass, True, True);

        // Iterate through foreign keys
        for C in MasterCols do
        begin
          if C.Optimization.MemberName <> MasterObj.MasterAssocMember then
            Continue;
          if HasJoinedTablesStrategy and (C.ForeignClass <> Self.Clazz) then
            Continue;

          Field := TSQLField.Create(CreateSQLTable, C.Name);
          Command.InsertFields.Add(Field);
          ParamName := SQLGenerator.GetDefaultParamName(Field);
          ParamValue := Explorer.GetColumnDbValue(MasterObj.MasterObject, C.ReferencedColumn);
          Params.Add(TDBParam.Create(ParamName, C.FieldType, ParamValue));
        end;
      end;

      Trans := FConnection.BeginTransaction;
      try
        SQL := SQLGenerator.GenerateInsert(Command);
        Execute(SQL, Params, False);

        if not InsertId and not HasPrimaryJoinColumn then
        begin
          NewId := IdGenerator.RetrieveIdAfterInsert(Entity, GetCommandPerformer);
//          if VarIsEmpty(NewId) then
//            raise ECannotGetId
          Explorer.SetIdValue(Entity, NewId);
        end;
        Trans.Commit;
      except
        Trans.Rollback;
        raise;
      end;
    finally
      Params.Free;
      Command.Free;
    end;
  finally
    IdGenerator.Free;
  end;
end;

procedure TInserter.InsertMasterRecordForJoinedTablesInheritance(Entity: TObject; MasterObj: TMasterObjectValue);
var
  Inserter: TInserter;
begin
  if HasPrimaryJoinColumn then
  begin
    Inserter := CreateAnotherInserter(Self.Clazz.ClassParent);
    try
      Inserter.Insert(Entity, MasterObj);
    finally
      Inserter.Free;
    end;
  end;
end;

end.
