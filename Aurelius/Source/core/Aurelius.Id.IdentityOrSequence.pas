unit Aurelius.Id.IdentityOrSequence;

{$I Aurelius.Inc}

interface
uses
  Aurelius.Id.IdentifierGenerator,
  Aurelius.Sql.Interfaces;

type
  TIdentityOrSequenceGenerator = class(TIdentifierGenerator)
  public
    function GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant; override;
    function RetrieveIdAfterInsert(Entity: TObject; Performer: ICommandPerformer): Variant; override;
  end;

implementation
uses
  Variants,
  Aurelius.Drivers.Interfaces,
  Aurelius.Id.Exceptions,
  Aurelius.Mapping.Metadata,
  Aurelius.Sql.BaseTypes,
  Aurelius.Sql.Commands;

{ TIdentityOrSequenceGenerator }

function TIdentityOrSequenceGenerator.GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant;
var
  Command: TGetNextSequenceValueCommand;
  Sequence: TSequence;
  ResultSet: IDBResultSet;
  SQL: string;
begin
  Result := Unassigned;

  // if there is no sequence, then before exit we must check if identity is supported
  // if it's not, then an error must be raised.
  if not Explorer.HasSequence(EntityClass, True) then
    Exit;
  if not (TDBFeature.Sequences in SQLGenerator.SupportedFeatures) then
    Exit;
  Sequence := Explorer.GetSequence(EntityClass, True);
  if Sequence = nil then
    Exit;

  Command := TGetNextSequenceValueCommand.Create;
  try
    Command.SequenceName := Sequence.SequenceName;
//    Command.InitialValue := Sequence.InitialValue;
    Command.Increment := Sequence.Increment;

    SQL := SQLGenerator.GenerateGetNextSequenceValue(Command);

    ResultSet := Performer.Execute(SQL, nil, True);

    // TODO: what to do if nothing returns...
    Result := ResultSet.GetFieldValue(0);
  finally
    Command.Free;
  end;
end;

function TIdentityOrSequenceGenerator.RetrieveIdAfterInsert(Entity: TObject;
  Performer: ICommandPerformer): Variant;
var
  SQLField: TSQLField;
  SQL: string;
  ResultSet: IDBResultSet;
  IdentityColumn: TColumn;
  MetaId: TMetaId;
begin
  MetaId := Explorer.GetId(EntityClass);
  Assert(Length(MetaId.Columns) > 0);
  IdentityColumn := MetaId.Columns[0];
  Assert(IdentityColumn <> nil);
  SQLField := TSQLField.Create(Performer.CreateSQLTable, IdentityColumn.Name);
  try
    SQL := SQLGenerator.GenerateGetLastInsertId(SQLField);
    ResultSet := Performer.Execute(SQL, nil, True);
    try
      if not ResultSet.Next then
        raise ECannotGetLastInsertId.Create(SQLField);
      Result := ResultSet.GetFieldValue(0);
    finally
      ResultSet := nil; // Destroy result set before committing the transaction
    end;
  finally
    SQLField.Free;
  end;
end;

end.
