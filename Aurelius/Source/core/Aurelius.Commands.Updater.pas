unit Aurelius.Commands.Updater;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Types.MasterObjectValue;

type
  TUpdater = class(TAbstractCommandPerformer)
  private
    function CreateAnotherUpdater(Clazz: TClass): TUpdater;
    procedure UpdateMasterRecordForJoinedTablesInheritance(Entity: TObject;
      MasterObj: TMasterObjectValue; ChangedColumns: TList<string>);
    procedure ExecUpdate(Entity: TObject; MasterObj: TMasterObjectValue; ChangedColumns: TList<string>);
  public
    procedure Update(Entity: TObject); overload;
    procedure Update(Entity: TObject; ChangedColumns: TList<string>); overload;
    procedure Update(Entity: TObject; MasterObj: TMasterObjectValue; ChangedColumns: TList<string>); overload;
  end;

implementation

uses
  Rtti, Variants,
  Aurelius.Drivers.Interfaces,
  Aurelius.Mapping.Metadata,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Commands;

{ TUpdater }

function TUpdater.CreateAnotherUpdater(Clazz: TClass): TUpdater;
begin
  Result := TUpdater.Create;

  Result.SetConnection(FConnection);
  Result.SetSQLGenerator(SQLGenerator);
  Result.SetExplorer(Explorer);
  Result.SetClass(Clazz);
  Result.SetEntityManager(FEntityManager);
  Result.AddExecutionListeners(FExecutionListeners);
end;

procedure TUpdater.ExecUpdate(Entity: TObject; MasterObj: TMasterObjectValue; ChangedColumns: TList<string>);
var
  Command: TUpdateCommand;
  C: TColumn;
  SQL, ParamName: string;
  ParamValue: Variant;
  Params: TObjectList<TDBParam>;
  Field: TSQLField;
  WhereField: TSQLWhereField;
  Dummy: Integer;
  MasterClass: TClass;
  MasterCols: TList<TColumn>;
begin
  UpdateMasterRecordForJoinedTablesInheritance(Entity, MasterObj, ChangedColumns);

  if ChangedColumns <> nil then
    ChangedColumns.Sort;

  Command := TUpdateCommand.Create;
  Params := TObjectList<TDBParam>.Create;
  try
    Command.Table := CreateSQLTable;

    for C in FColumns do
    begin
      if C.IsForeign then
        Continue;
      if not Self.Clazz.InheritsFrom(C.DeclaringClass) then
        Continue;
      if (TColumnProp.NoUpdate in C.Properties) then //or (C = FIdColumn) then
        Continue;
      if (ChangedColumns <> nil) and not ChangedColumns.BinarySearch(C.Name, Dummy) then
        Continue;


      ParamValue := Explorer.GetColumnDbValue(Entity, C);

      // Do not set a null value for an association if it's a proxy that's not been loaded yet
      if (ParamValue = Null) and C.Optimization.IsProxy and not Explorer.IsProxyLoaded(Entity, C.Optimization) then
        Continue;

      Field := TSQLField.Create(CreateSQLTable, C.Name);
      ParamName := SQLGenerator.GetDefaultParamName(Field);
      Command.UpdateFields.Add(Field);
      Params.Add(TDBParam.Create(ParamName, C.FieldType, ParamValue));
    end;

    if (MasterObj.MasterObject <> nil)
      and (MasterObj.Action <> TMasterObjectAction.None) // this test is just in case, MasterObj is never passed with action=none
      then
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
        Command.UpdateFields.Add(Field);
        ParamName := SQLGenerator.GetDefaultParamName(Field);
        case MasterObj.Action of
          TMasterObjectAction.None: ;
          TMasterObjectAction.Include:
            ParamValue := Explorer.GetColumnDbValue(MasterObj.MasterObject, C.ReferencedColumn);
          TMasterObjectAction.Exclude:
            ParamValue := Null;
        end;
        Params.Add(TDBParam.Create(ParamName, C.FieldType, ParamValue));
      end;
    end;

    if Command.UpdateFields.Count > 0 then
    begin
      for C in FPKColumns do
      begin
        WhereField := TSQLWhereField.Create(CreateSQLTable, C.Name);
        Command.WhereFields.Add(WhereField);
        WhereField.ParamName := TDBUtils.GetValidParamName(Params);
        ParamValue := Explorer.GetColumnDbValue(Entity, C);
        Params.Add(TDBParam.Create(WhereField.ParamName, C.FieldType, ParamValue));
      end;

      SQL := SQLGenerator.GenerateUpdate(Command);
      Execute(SQL, Params, False);
    end;
  finally
    Command.Free;
    Params.Free;
  end;
end;

procedure TUpdater.Update(Entity: TObject);
begin
  Update(Entity, DummyMasterObject, nil);
end;

procedure TUpdater.Update(Entity: TObject; ChangedColumns: TList<string>);
begin
  Update(Entity, DummyMasterObject, ChangedColumns);
end;

procedure TUpdater.Update(Entity: TObject; MasterObj: TMasterObjectValue;
  ChangedColumns: TList<string>);
begin
  ExecUpdate(Entity, MasterObj, ChangedColumns);
end;

procedure TUpdater.UpdateMasterRecordForJoinedTablesInheritance(Entity: TObject;
  MasterObj: TMasterObjectValue; ChangedColumns: TList<string>);
var
  Updater: TUpdater;
begin
  if HasPrimaryJoinColumn then
  begin
    Updater := CreateAnotherUpdater(Self.Clazz.ClassParent);
    try
      Updater.ExecUpdate(Entity, MasterObj, ChangedColumns);
    finally
      Updater.Free;
    end;
  end;
end;

end.
