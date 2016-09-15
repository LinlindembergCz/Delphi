unit Aurelius.Commands.SequenceCreator;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Commands.AbstractCommandPerformer,
  Aurelius.Sql.Metadata,
  Aurelius.Sql.Commands;


type
  TSequenceCreator = class(TAbstractCommandPerformer)
  public
    procedure CreateSequence(Database: TDatabaseMetadata);
  end;

implementation

uses
  Aurelius.Global.Config,
  Aurelius.Mapping.Metadata,
  Aurelius.Sql.Interfaces;

{ TSequenceCreator }

procedure TSequenceCreator.CreateSequence(Database: TDatabaseMetadata);
var
  MappedSequence: TSequence;
  Sequence: TSequenceMetadata;
begin
  if not (TDBFeature.Sequences in SQLGenerator.SupportedFeatures) then
    Exit;

  MappedSequence := Explorer.GetSequence(Self.Clazz, False);

  Sequence := TSequenceMetadata.Create(Database);
  Database.Sequences.Add(Sequence);
  Sequence.Name := MappedSequence.SequenceName;
  Sequence.InitialValue := MappedSequence.InitialValue;
  Sequence.Increment := MappedSequence.Increment;
end;

end.
