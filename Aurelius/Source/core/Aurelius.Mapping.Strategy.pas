unit Aurelius.Mapping.Strategy;

{$I Aurelius.Inc}

interface
uses
  Rtti, Aurelius.Mapping.Metadata;

type
  TMappingStrategy = class
  public
    function HasAutomapping(RttiType: TRttiType): boolean; virtual; abstract;
    function HasTransient(Member: TRttiMember): boolean; virtual; abstract;
    function IsEntity(RttiType: TRttiType): boolean; virtual; abstract;
    function LoadAssociationByMember(Member: TRttiMember): TAssociation; virtual; abstract;
    function LoadColumnsFromMember(Member: TRttiMember): TArray<TColumn>; virtual; abstract;
    function LoadDescription(Member: TRttiMember): string; virtual; abstract;
    function LoadDiscriminatorColumn(RttiType: TRttiType): TColumn; virtual; abstract;
    function LoadDiscriminatorValue(RttiType: TRttiType): TValue; virtual; abstract;
    function LoadEnumeration(RttiType: TRttiType): TEnumeration; virtual; abstract;
    function LoadInheritance(RttiType: TRttiType): TMetaInheritance; virtual; abstract;
    function LoadMetaId(RttiType: TRttiType): TMetaId; virtual; abstract;
    function LoadPrimaryJoinColumns(RttiType: TRttiType): TArray<TColumn>; virtual; abstract;
    function LoadSequence(RttiType: TRttiType): TSequence; virtual; abstract;
    function LoadTable(RttiType: TRttiType): TMetaTable; virtual; abstract;
    function LoadUniqueConstraints(RttiType: TRttiType): TArray<TUniqueConstraint>; virtual; abstract;
  end;

implementation

end.
