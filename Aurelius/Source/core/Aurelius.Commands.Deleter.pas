unit Aurelius.Commands.Deleter;

{$I Aurelius.inc}

interface

uses
  Aurelius.Commands.AbstractCommandPerformer;

type
  TDeleter = class(TAbstractCommandPerformer)
  private
    function CreateAnotherDeleter(Clazz: TClass): TDeleter;
    procedure RemoveMasterRecordForJoinedTablesInheritance(Entity: TObject);
  public
    procedure Delete(Entity: TObject);
  end;

implementation

uses
  Rtti, Generics.Collections,
  Aurelius.Global.Utils,
  Aurelius.Sql.Commands,
  Aurelius.Sql.BaseTypes,
  Aurelius.Drivers.Interfaces,
  Aurelius.Mapping.Metadata;

{ TDeleter }

function TDeleter.CreateAnotherDeleter(Clazz: TClass): TDeleter;
begin
  Result := TDeleter.Create;

  Result.SetConnection(FConnection);
  Result.SetSQLGenerator(SQLGenerator);
  Result.SetExplorer(Explorer);
  Result.SetClass(Clazz);
  Result.SetEntityManager(FEntityManager);
  Result.AddExecutionListeners(FExecutionListeners);
end;

procedure TDeleter.Delete(Entity: TObject);
var
  Command: TDeleteCommand;
  SQLField: TSQLWhereField;
  IdValue: Variant;
  SQL: string;
  Params: TObjectList<TDBParam>;
  Col: TColumn;
begin
  Command := TDeleteCommand.Create;
  try
    Command.Table := CreateSQLTable;
    Params := TObjectList<TDBParam>.Create;
    try
      for Col in FPKColumns do
      begin
        SQLField := TSQLWhereField.Create(CreateSQLTable, Col.Name, TWhereOperator.woEqual);

        Command.WhereFields.Add(SQLField);

        IdValue := Explorer.GetColumnDbValue(Entity, Col);
        SQLField.ParamName := TDBUtils.GetValidParamName(Params);

        Params.Add(TDBParam.Create(SQLField.ParamName, Col.FieldType, IdValue));
      end;

      SQL := SQLGenerator.GenerateDelete(Command);
      Execute(SQL, Params, False);
    finally
      Params.Free;
    end;
  finally
    Command.Free;
  end;

  RemoveMasterRecordForJoinedTablesInheritance(Entity);
end;

procedure TDeleter.RemoveMasterRecordForJoinedTablesInheritance(
  Entity: TObject);
var
  Deleter: TDeleter;
begin
  if HasPrimaryJoinColumn then
  begin
    Deleter := CreateAnotherDeleter(Self.Clazz.ClassParent);
    try
      Deleter.Delete(Entity);
    finally
      Deleter.Free;
    end;
  end;
end;

end.
