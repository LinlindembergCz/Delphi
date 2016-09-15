unit Aurelius.Mapping.Setup;

{$I Aurelius.Inc}

interface
uses
  TypInfo,
  Generics.Collections,
  Aurelius.Mapping.MappedClasses,
  Aurelius.Mapping.Metadata;

type
  TColumnProp = Aurelius.Mapping.Metadata.TColumnProp;
  TColumnProps = Aurelius.Mapping.Metadata.TColumnProps;

  TDynamicColumn = class
  private
    FLength: integer;
    FName: string;
    FPrecision: integer;
    FProperties: TColumnProps;
    FScale: integer;
  public
    constructor Create(Name: string); overload;
    constructor Create(Name: string; Properties: TColumnProps); overload;
    constructor Create(Name: string; Properties: TColumnProps; Length: Integer); overload;
    constructor Create(Name: string; Properties: TColumnProps; Precision, Scale: Integer); overload;
    function Clone: TDynamicColumn;
    property Name: string read FName write FName;
    property Properties: TColumnProps read FProperties write FProperties;
    property Length: integer read FLength write FLength;
    property Precision: integer read FPrecision write FPrecision;
    property Scale: integer read FScale write FScale;
  end;

  TDynamicProperty = class
  private
    FPropertyName: string;
    FPropertyType: PTypeInfo;
    FColumn: TDynamicColumn;
    FContainerName: string;
  public
    constructor Create(AContainerName, APropName: string; APropType: PTypeInfo; ColumnDef: TDynamicColumn);
    destructor Destroy; override;
    function Clone: TDynamicProperty;
    property ContainerName: string read FContainerName write FContainerName;
    property PropertyName: string read FPropertyName write FPropertyName;
    property PropertyType: PTypeInfo read FPropertyType write FPropertyType;
    property Column: TDynamicColumn read FColumn write FColumn;
  end;

  TMappingSetup = class
  private
    FDynamicPropsByClass: TDictionary<TClass, TList<TDynamicProperty>>;
    FMappedClasses: TMappedClasses;
    function GetDynamicProps(AClass: TClass): TList<TDynamicProperty>;
  protected
    function RegisteredDynamicProps: TEnumerable<TPair<TClass, TList<TDynamicProperty>>>;
  public
    constructor Create;
    destructor Destroy; override;
    property DynamicProps[AClass: TClass]: TList<TDynamicProperty> read GetDynamicProps;
    property MappedClasses: TMappedClasses read FMappedClasses;
  end;

implementation

{ TDynamicColumn }

constructor TDynamicColumn.Create(Name: string; Properties: TColumnProps);
begin
  Create(Name);
  FProperties := Properties;
end;

constructor TDynamicColumn.Create(Name: string);
begin
  FName := Name;
end;

function TDynamicColumn.Clone: TDynamicColumn;
begin
  Result := TDynamicColumn.Create(Self.Name, Self.Properties);
  Result.FLength := Self.FLength;
  Result.FPrecision := Self.FPrecision;
  Result.FScale := Self.FScale;
end;

constructor TDynamicColumn.Create(Name: string; Properties: TColumnProps;
  Precision, Scale: Integer);
begin
  Create(Name, Properties);
  FPrecision := Precision;
  FScale := Scale;
end;

constructor TDynamicColumn.Create(Name: string; Properties: TColumnProps;
  Length: Integer);
begin
  Create(Name, Properties);
  FLength := Length;
end;

{ TDynamicProperty }

function TDynamicProperty.Clone: TDynamicProperty;
begin
  Result := TDynamicProperty.Create(Self.ContainerName, Self.PropertyName, Self.PropertyType, Self.Column.Clone);
end;

constructor TDynamicProperty.Create(AContainerName, APropName: string;
  APropType: PTypeInfo; ColumnDef: TDynamicColumn);
begin
  FContainerName := AContainerName;
  FPropertyName := APropName;
  FPropertyType := APropType;
  FColumn := ColumnDef;
end;

destructor TDynamicProperty.Destroy;
begin
  if FColumn <> nil then
  begin
    FColumn.Free;
    FColumn := nil;
  end;
  inherited;
end;

{ TMappingSetup }

constructor TMappingSetup.Create;
begin
  FMappedClasses := TMappedClasses.Create;
  FDynamicPropsByClass := TObjectDictionary<TClass, TList<TDynamicProperty>>.Create([doOwnsValues]);
end;

destructor TMappingSetup.Destroy;
begin
  FMappedClasses.Free;
  FDynamicPropsByClass.Free;
  inherited;
end;

function TMappingSetup.RegisteredDynamicProps: TEnumerable<TPair<TClass, TList<TDynamicProperty>>>;
begin
  Result := FDynamicPropsByClass;
end;

function TMappingSetup.GetDynamicProps(AClass: TClass): TList<TDynamicProperty>;
var
  Key: TClass;
begin
  Key := AClass;
  if FDynamicPropsByClass.ContainsKey(Key) then
    Exit(FDynamicPropsByClass[Key]);

  Result := TObjectList<TDynamicProperty>.Create(True);
  FDynamicPropsByClass.Add(Key, Result);
end;

end.
