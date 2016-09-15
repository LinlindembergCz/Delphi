unit Aurelius.Mapping.Attributes;

{$I Aurelius.inc}

interface

uses
  Generics.Collections, Rtti, DB,
  Aurelius.Mapping.Metadata;

type
  Entity = class(TCustomAttribute)
  end;

  Automapping = class(TCustomAttribute)
  end;

  Table = class(TCustomAttribute)
  private
    FName: string;
    FSchema: string;
  public
    constructor Create(Name: string); overload;
    constructor Create(Name, Schema: string); overload;
    property Name: string read FName write FName;
    property Schema: string read FSchema write FSchema;
  end;

  TColumnProp = Aurelius.Mapping.Metadata.TColumnProp;
  TColumnProps = Aurelius.Mapping.Metadata.TColumnProps;

  AbstractColumn = class abstract(TCustomAttribute)
  private
    FName: string;
    FProperties: TColumnProps;
  public
    constructor Create(Name: string); overload; virtual;
    constructor Create(Name: string; Properties: TColumnProps); overload; virtual;
    property Name: string read FName write FName;
    property Properties: TColumnProps read FProperties write FProperties;
  end;

  Column = class(AbstractColumn)
  private
    FLength: Integer;
    FPrecision: Integer;
    FScale: Integer;
  public
    constructor Create(Name: string); overload; override;
    constructor Create(Name: string; Properties: TColumnProps; Length: Integer); overload;
    constructor Create(Name: string; Properties: TColumnProps; Precision, Scale: Integer); overload;
    property Length: Integer read FLength write FLength;
    property Precision: Integer read FPrecision write FPrecision;
    property Scale: Integer read FScale write FScale;
  end;

  JoinColumn = class(AbstractColumn)
  private
    FReferencedColumnName: string;
  public
    constructor Create(Name: string; Properties: TColumnProps; ReferencedColumnName: string); overload;
    property ReferencedColumnName: string read FReferencedColumnName write FReferencedColumnName;
  end;

  ForeignJoinColumn = class(AbstractColumn)
  private
    FReferencedColumnName: string;
  public
    constructor Create(Name: string; Properties: TColumnProps; ReferencedColumnName: string); overload;
    property ReferencedColumnName: string read FReferencedColumnName write FReferencedColumnName;
  end;

  PrimaryJoinColumn = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(Name: string);
    property Name: string read FName write FName;
  end;

  TIdGenerator = Aurelius.Mapping.Metadata.TIdGenerator;

  Id = class(TCustomAttribute)
  private
    FMemberName: string;
    FGenerator: TIdGenerator;
  public
    constructor Create(AMemberName: string; AGenerator: TIdGenerator);
    property MemberName: string read FMemberName;
    property Generator: TIdGenerator read FGenerator;
  end;

  UniqueKey = class(TCustomAttribute)
  private
    FColumns: TList<string>;
  public
    constructor Create(Columns: string); // Specify with comma-separated values
    destructor Destroy; override;
    property Columns: TList<string> read FColumns write FColumns;
  end;

  Sequence = class(TCustomAttribute)
  private
    FSequenceName: string;
    FInitialValue: Integer;
    FIncrement: Integer;
  public
    constructor Create(SequenceName: string); overload;
    constructor Create(SequenceName: string; InitialValue, Increment: Integer); overload;
    property SequenceName: string read FSequenceName write FSequenceName;
    property InitialValue: Integer read FInitialValue write FInitialValue;
    property Increment: Integer read FIncrement write FIncrement;
  end;

  TCascadeType = Aurelius.Mapping.Metadata.TCascadeType;
  TCascadeTypes = Aurelius.Mapping.Metadata.TCascadeTypes;
  TAssociationProp = Aurelius.Mapping.Metadata.TAssociationProp;
  TAssociationProps = Aurelius.Mapping.Metadata.TAssociationProps;

const
  CascadeTypeAll = Aurelius.Mapping.Metadata.CascadeTypeAll;

type
  Association = class(TCustomAttribute)
  private
    FProperties: TAssociationProps;
    FCascade: TCascadeTypes;
  public
    constructor Create; overload;
    constructor Create(AProperties: TAssociationProps); overload;
    constructor Create(AProperties: TAssociationProps; Cascade: TCascadeTypes); overload;
    property Properties: TAssociationProps read FProperties write FProperties;
    property Cascade: TCascadeTypes read FCascade write FCascade;
  end;

  ManyValuedAssociation = class(Association)
  private
    FMappedBy: string;
  public
    constructor Create(AProperties: TAssociationProps; Cascade: TCascadeTypes; MappedBy: string); overload;
    property MappedBy: string read FMappedBy write FMappedBy;
  end;

  TInheritanceStrategy = Aurelius.Mapping.Metadata.TInheritanceStrategy;

  Inheritance = class(TCustomAttribute)
  private
    FStrategy: TInheritanceStrategy;
  public
    constructor Create(Strategy: TInheritanceStrategy);
    property Strategy: TInheritanceStrategy read FStrategy write FStrategy;
  end;

  TDiscriminatorType = (dtString, dtInteger);

  DiscriminatorColumn = class(TCustomAttribute)
  private
    FName: string;
    FDiscrType: TDiscriminatorType;
    FLength: Integer;
    function GetFieldType: TFieldType;
  public
    constructor Create(Name: string; DiscriminatorType: TDiscriminatorType); overload;
    constructor Create(Name: string; DiscriminatorType: TDiscriminatorType; Length: Integer); overload;
    property Name: string read FName write FName;
    property DiscrType: TDiscriminatorType read FDiscrType write FDiscrType;
    property Length: Integer read FLength write FLength;
    property FieldType: TFieldType read GetFieldType;
  end;

  DiscriminatorValue = class(TCustomAttribute)
  private
    FValue: TValue;
  public
    constructor Create(Value: string); overload;
    constructor Create(Value: Integer); overload;
    property Value: TValue read FValue write FValue;
  end;

  // TODO: Add emEnum and make it work for Postgre databases
  TEnumMappingType = Aurelius.Mapping.Metadata.TEnumMappingType;

  Enumeration = class(TCustomAttribute)
  private
    FMappedType: TEnumMappingType;
    FMappedValues: TList<string>;
    function ValidateEnumValue(Value: string): string;
  public
    constructor Create(MappedType: TEnumMappingType); overload;
    constructor Create(MappedType: TEnumMappingType; MappedValues: string); overload;
    destructor Destroy; override;
    property MappedType: TEnumMappingType read FMappedType write FMappedType;
    property MappedValues: TList<string> read FMappedValues write FMappedValues;
  end;

  Transient = class(TCustomAttribute)
  end;

  Description = class(TCustomAttribute)
  private
    FText: string;
  public
    constructor Create(AText: string);
    property Text: string read FText write FText;
  end;

implementation

uses
  Classes, TypInfo, SysUtils, Math,
  Aurelius.Mapping.Exceptions;

{ Table }

constructor Table.Create(Name: string);
begin
  FName := Name;
end;

constructor Table.Create(Name, Schema: string);
begin
  Create(Name);
  FSchema := Schema;
end;

{ Column }

constructor Column.Create(Name: string; Properties: TColumnProps;
  Length: Integer);
begin
  Create(Name, Properties);
  FLength := Length;
end;

constructor Column.Create(Name: string; Properties: TColumnProps;
  Precision, Scale: Integer);
begin
  Create(Name, Properties);
  FPrecision := Precision;
  FScale := Scale;
end;

constructor Column.Create(Name: string);
begin
  FName := Name;
end;

{ UniqueConstraint }

constructor UniqueKey.Create(Columns: string);
var
  List: TStringList;
  I: Integer;
begin
  FColumns := TList<string>.Create;

  List := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], PChar(Columns), List);
    for I := 0 to List.Count - 1 do
      FColumns.Add(List[I]);
  finally
    List.Free;
  end;
end;

destructor UniqueKey.Destroy;
begin
  FColumns.Free;
  inherited;
end;

{ Sequence }

constructor Sequence.Create(SequenceName: string);
begin
  FSequenceName := SequenceName;
  FInitialValue := 1;
  FIncrement := 1;
end;

constructor Sequence.Create(SequenceName: string; InitialValue,
  Increment: Integer);
begin
  Create(SequenceName);
  FInitialValue := InitialValue;
  FIncrement := Increment;
end;

{ AbstractColumn }

constructor AbstractColumn.Create(Name: string);
begin
  FName := Name;
end;

constructor AbstractColumn.Create(Name: string; Properties: TColumnProps);
begin
  Create(Name);
  FProperties := Properties;
end;

{ Association }

constructor Association.Create;
begin
  FProperties := [];
  FCascade := [];
end;

constructor Association.Create(AProperties: TAssociationProps);
begin
  Create;
  FProperties := AProperties;
end;

constructor Association.Create(AProperties: TAssociationProps; Cascade: TCascadeTypes);
begin
  Create(AProperties);
  FCascade := Cascade;
end;

{ JoinColumn }

constructor JoinColumn.Create(Name: string; Properties: TColumnProps;
  ReferencedColumnName: string);
begin
  Create(Name, Properties);
  FReferencedColumnName := ReferencedColumnName;
end;

{ Inheritance }

constructor Inheritance.Create(Strategy: TInheritanceStrategy);
begin
  FStrategy := Strategy;
end;

{ DiscriminatorColumn }

constructor DiscriminatorColumn.Create(Name: string;
  DiscriminatorType: TDiscriminatorType);
begin
  FName := Name;
  FDiscrType := DiscriminatorType;
  FLength := 30;
end;

constructor DiscriminatorColumn.Create(Name: string;
  DiscriminatorType: TDiscriminatorType; Length: Integer);
begin
  Create(Name, DiscriminatorType);
  FLength := Length;
end;

function DiscriminatorColumn.GetFieldType: TFieldType;
begin
  case DiscrType of
    TDiscriminatorType.dtString:  Result := ftString;
    TDiscriminatorType.dtInteger: Result := ftInteger;
  else
    raise EUnexpectedDiscriminatorType.Create(
      GetEnumName(TypeInfo(TDiscriminatorType), Ord(DiscrType))
    );
  end;
end;

{ DiscriminatorValue }

constructor DiscriminatorValue.Create(Value: string);
begin
  FValue := Value;
end;

constructor DiscriminatorValue.Create(Value: Integer);
begin
  FValue := Value;
end;

{ ManyValuedAssociation }

constructor ManyValuedAssociation.Create(AProperties: TAssociationProps;
  Cascade: TCascadeTypes; MappedBy: string);
begin
  inherited Create(AProperties, Cascade);
  FMappedBy := MappedBy;
end;

{ ForeignJoinColumn }

constructor ForeignJoinColumn.Create(Name: string;
  Properties: TColumnProps; ReferencedColumnName: string);
begin
  Create(Name, Properties);
  FReferencedColumnName := ReferencedColumnName;
end;

{ PrimaryJoinColumn }

constructor PrimaryJoinColumn.Create(Name: string);
begin
  FName := Name;
end;

{ Enumeration }

constructor Enumeration.Create(MappedType: TEnumMappingType);
begin
  Create(MappedType, '');
end;

constructor Enumeration.Create(MappedType: TEnumMappingType; MappedValues: string);
var
  List: TStringList;
  I: Integer;
begin
  if (MappedType = TEnumMappingType.emChar) and (Trim(MappedValues) = '') then
    raise EInvalidEnumMapping.Create('Mapped values are required for char mapping type.');

  FMappedType := MappedType;
  FMappedValues := TList<string>.Create;

  if FMappedType <> TEnumMappingType.emInteger then
  begin
    List := TStringList.Create;
    try
      List.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(MappedValues), List);
      for I := 0 to List.Count - 1 do
        FMappedValues.Add(ValidateEnumValue(List[I]));
    finally
      List.Free;
    end;
  end;
end;

destructor Enumeration.Destroy;
begin
  FMappedValues.Free;
  inherited;
end;

function Enumeration.ValidateEnumValue(Value: string): string;
begin
  Result := Value;
  if (FMappedType = TEnumMappingType.emChar) then
  begin
    {$IFDEF NEXTGEN}
    if (Value.Length <> 1) or not CharInSet(Value.Chars[0], ['A'..'Z']) then
    {$ELSE}
    if (Length(Value) <> 1) or not CharInSet(Value[1], ['A'..'Z']) then
    {$ENDIF}
      raise EInvalidEnumMapping.CreateFmt(
        'Enum mapped value "%s" is invalid for type "%s". ' +
        'Tip: For chars, specify in uppercase.', [Value,
         GetEnumName(TypeInfo(TEnumMappingType), Ord(FMappedType))]);
  end;
end;

{ Id }

constructor Id.Create(AMemberName: string; AGenerator: TIdGenerator);
begin
  FMemberName := AMemberName;
  FGenerator := AGenerator;
end;

{ Description }

constructor Description.Create(AText: string);
begin
  FText := AText;
end;

end.
