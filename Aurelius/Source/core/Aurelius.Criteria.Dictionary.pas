unit Aurelius.Criteria.Dictionary;

{$I Aurelius.inc}

interface

type
  TDictionaryAttribute = record
  private
    FPropName: string;
  public
    constructor Create(APropName: string);
    property PropName: string read FPropName;
  end;

  TDictionaryAssociation = record
  private
    FAssociationName: string;
  public
    constructor Create(AAssociationName: string);
  end;

implementation

{ TDictionaryAttribute }

constructor TDictionaryAttribute.Create(APropName: string);
begin
  FPropName := APropName;
end;

{ TDictionaryAssociation }

constructor TDictionaryAssociation.Create(AAssociationName: string);
begin
  FAssociationName := AAssociationName;
end;

end.
