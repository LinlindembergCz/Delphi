unit Aurelius.Id.IdentifierGenerator;

{$I Aurelius.Inc}

interface
uses
  Aurelius.Id.AbstractGenerator,
  Aurelius.Mapping.Explorer,
  Aurelius.SQL.Interfaces;

type
  TIdentifierGenerator = class abstract(TAbstractGenerator)
  strict private
    FExplorer: TMappingExplorer;
    FSQLGenerator: ISQLGenerator;
    FEntityClass: TClass;
  strict protected
    property Explorer: TMappingExplorer read FExplorer;
    property SQLGenerator: ISQLGenerator read FSQLGenerator;
    property EntityClass: TClass read FEntityClass;
  public
    constructor Create(AEntityClass: TClass; AExplorer: TMappingExplorer; ASQLGenerator: ISQLGenerator); virtual;
  end;

  TIdentifierGeneratorClass = class of TIdentifierGenerator;

  TNoneGenerator = class(TIdentifierGenerator)
  public
    function GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant; override;
  end;

implementation

{ TNoneGenerator }

function TNoneGenerator.GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant;
begin
end;

{ TIdentifierGenerator }

constructor TIdentifierGenerator.Create(AEntityClass: TClass;
  AExplorer: TMappingExplorer; ASQLGenerator: ISQLGenerator);
begin
  FEntityClass := AEntityClass;
  FExplorer := AExplorer;
  FSQLGenerator := ASQLGenerator;
end;

end.
