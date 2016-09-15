unit Aurelius.Engine.ObjectMap;

{$I Aurelius.inc}

interface

uses
  Generics.Defaults, Generics.Collections, SysUtils,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.Optimization,
  Aurelius.Mapping.Explorer;

type
  TObjectIdList = class
  private
    FIdKeys: TList<string>;
    FIdValues: TList<Variant>;
    FExplorer: TMappingExplorer;
    function GenerateIdKey(Id: Variant): string;
    function GetIdValue(Entity: TObject): Variant;
  public
    constructor Create(AExplorer: TMappingExplorer);
    destructor Destroy; override;
    procedure Add(Entity: TObject);
    function ContainsValue(Id: Variant): boolean;
    function ContainsValueOf(Entity: TObject): boolean;
    function IdValues: TEnumerable<Variant>;
  end;

  TCollectionState = class
  private
    FItemIds: TObjectIdList;
  public
    constructor Create(AExplorer: TMappingExplorer);
    destructor Destroy; override;
    property ItemIds: TObjectIdList read FItemIds;
  end;

  TObjectState = class
  private
    // Contains a map of ColumnName -> ColumnValue (db values, not member values)
    FValues: TDictionary<string, Variant>;
    procedure SetValues(const Value: TDictionary<string, Variant>);
  public
    destructor Destroy; override;
    property Values: TDictionary<string, Variant> read FValues write SetValues;
  end;

  TMapEntry = class
  strict private
    FEntity: TObject;
    FOldState: TObjectState;
  private
    FOldCollections: TDictionary<string, TCollectionState>;
    property OldState: TObjectState read FOldState write FOldState;
  public
    constructor Create;
    destructor Destroy; override;
    property Entity: TObject read FEntity write FEntity;
  end;

  TObjectMap = class
  private
    type
      TObjectMapKey = string;
  private
    FExplorer: TMappingExplorer;
    FMap: TDictionary<TObjectMapKey, TMapEntry>;
    FList: TList<TObject>;

    function GetIdValue(Entity: TObject): Variant;
    function GenerateKey(Entity: TObject): TObjectMapKey; overload;
    function GenerateKey(Clazz: TClass; Id: Variant): TObjectMapKey; overload;
    function BuildEntry(Entity: TObject): TMapEntry;
    procedure UpdateObjectState(Entity: TObject; State: TObjectState);
  public
    constructor Create(AExplorer: TMappingExplorer); virtual;
    destructor Destroy; override;

    function IsMapped(Entity: TObject): Boolean;
    function IsIdMapped(Entity: TObject): Boolean;

    procedure Add(Entity: TObject);
    function GetOldState(Entity: TObject): TObjectState;
    function GetOldCollectionState(Entity: TObject; MemberInfo: TRttiOptimization): TCollectionState;

    procedure Remove(Entity: TObject);
    function GetEntry(Entity: TObject): TMapEntry;
    function Get(Entity: TObject): TObject;
    function GetById(Clazz: TClass; Id: Variant): TObject;
    procedure Clear(DestroyObjects: Boolean);
    function GetList: TList<TObject>;

    function HasIdValue(Entity: TObject): Boolean;

    procedure UpdateOldState(Entity: TObject);
    procedure UpdateCollectionOldState(Entity: TObject; MemberInfo: TRttiOptimization; AListObj: TObject); overload;
    procedure UpdateCollectionOldState(Entity: TObject; MemberName: string; AListObj: TObject); overload;
  end;

implementation

uses
  Variants, Rtti,
  Aurelius.Engine.Exceptions,
  Aurelius.Global.Utils;

{ TObjectMap }

procedure TObjectMap.Add(Entity: TObject);
begin
  if not HasIdValue(Entity) then
    raise EIdNotSetException.Create(Entity.ClassType);
  if IsIdMapped(Entity) then
    raise EEngineInternalError.Create('Cannot add in object map - id already exists');

  FMap.Add(GenerateKey(Entity), BuildEntry(Entity));
end;

function TObjectMap.BuildEntry(Entity: TObject): TMapEntry;
begin
  Result := TMapEntry.Create;
  Result.Entity := Entity;
end;

procedure TObjectMap.UpdateCollectionOldState(Entity: TObject; MemberName: string; AListObj: TObject);
var
  Optimization: TRttiOptimization;
begin
  Optimization := FExplorer.GetOptimization(Entity.ClassType, MemberName);
  try
    UpdateCollectionOldState(Entity, Optimization, AListObj);
  finally
    Optimization.Free;
  end;
end;

// AListObj is used when we want to pass the collection object itself, instead of letting this function to
// load it from the member name. This is used in the TObjectManager.LoadProxyValue, because at the time
// UpdateCollectionOldState is called, the proxy is not set yet - but we already have the collection object
// Code could be later improved for a more generic and clean approach.
procedure TObjectMap.UpdateCollectionOldState(Entity: TObject; MemberInfo: TRttiOptimization; AListObj: TObject);
var
  Key: string;
  Entry: TMapEntry;
  List: IObjectList;
  ColState: TCollectionState;
  I: Integer;
begin
  if not IsMapped(Entity) then Exit;
  Entry := GetEntry(Entity);
  Assert(Entry <> nil, 'Cannot update collection. Entity is mapped but Entry not found.');

  // Delete old collection state if any
  Key := MemberInfo.MemberName;
  if Entry.FOldCollections.ContainsKey(Key) then
    Entry.FOldCollections.Remove(Key);

  // get current list
  if AListObj = nil then
    AListObj := FExplorer.GetMemberValue(Entity, MemberInfo).AsObject;
  if (AListObj <> nil) and (FExplorer.IsList(AListObj.ClassType)) then
    List := FExplorer.AsList(AListObj)
  else
    List := nil;

  // if list is nil (or lazy) avoid saving state
  if List = nil then
    Exit;

  // Add the collection state
  ColState := TCollectionState.Create(FExplorer);
  Entry.FOldCollections.Add(Key, ColState);

  // Update the collection state - list of current id's
  for I := 0 to List.Count - 1 do
    if HasIdValue(List.Item(I)) then
      ColState.ItemIds.Add(List.Item(I));
end;

procedure TObjectMap.UpdateObjectState(Entity: TObject; State: TObjectState);
begin
  // Get the current state of column fields. Use Values instead of FValues
  State.Values := FExplorer.GetColumnValues(Entity);
end;

procedure TObjectMap.Clear(DestroyObjects: Boolean);
var
  V: TMapEntry;
begin
  for V in FMap.Values do
  begin
    if DestroyObjects then
      V.Entity.Free;
  end;
  FMap.Clear;
end;

constructor TObjectMap.Create(AExplorer: TMappingExplorer);
begin
  FExplorer := AExplorer;
  FMap := TObjectDictionary<TObjectMapKey, TMapEntry>.Create([doOwnsValues]);
  FList := TList<TObject>.Create;
end;

destructor TObjectMap.Destroy;
begin
  Clear(false);
  FMap.Free;
  FList.Free;
  inherited;
end;

function TObjectMap.GenerateKey(Entity: TObject): TObjectMapKey;
begin
  Result := GenerateKey(Entity.ClassType, GetIdValue(Entity));
end;

function TObjectMap.Get(Entity: TObject): TObject;
var
  Entry: TMapEntry;
begin
  Entry := GetEntry(Entity);
  if Entry <> nil then
    Result := Entry.Entity
  else
    Result := nil;
end;

function TObjectMap.GenerateKey(Clazz: TClass; Id: Variant): TObjectMapKey;
begin
  Result := Clazz.ClassName;
  Result := Result + '_' + TUtils.VariantToString(Id); // If you change this, revise TObjectManager.TEngineCursor class
end;

function TObjectMap.GetById(Clazz: TClass; Id: Variant): TObject;
var
  Key: TObjectMapKey;
  SubClass: TClass;
begin
  Key := GenerateKey(Clazz, Id);

  if FMap.ContainsKey(Key) then
    Result := FMap.Items[Key].Entity
  else
    Result := nil;

  // If class is not mapped, maybe there is a descendant mapped. For example, we're looking for
  // TAnimal (Id = 1) but maybe there is a TDog (Id=1) which is a valid TAnimal with id=1
  if Result = nil then
    for SubClass in FExplorer.Hierarchy.GetAllSubClasses(Clazz) do
    begin
      Result := GetById(SubClass, Id);
      if Result <> nil then
        Exit;
    end;
end;

function TObjectMap.GetEntry(Entity: TObject): TMapEntry;
var
  Key: TObjectMapKey;
begin
  Key := GenerateKey(Entity);

  if FMap.ContainsKey(Key) then
    Result := FMap.Items[Key]
  else
    Result := nil;
end;

function TObjectMap.GetIdValue(Entity: TObject): Variant;
begin
  Result := FExplorer.GetIdValue(Entity);
end;

function TObjectMap.GetList: TList<TObject>;
var
  V: TMapEntry;
begin
  FList.Clear;

  for V in FMap.Values do
    FList.Add(V.Entity);

  Result := FList;
end;

function TObjectMap.GetOldCollectionState(Entity: TObject;
  MemberInfo: TRttiOptimization): TCollectionState;
var
  Entry: TMapEntry;
  Key: string;
begin
  Entry := GetEntry(Entity);
  if Entry = nil then
    Exit(nil);

  Key := MemberInfo.MemberName;
  if Entry.FOldCollections.ContainsKey(Key) then
    Result := Entry.FOldCollections[Key]
  else
    Result := nil;
end;

function TObjectMap.GetOldState(Entity: TObject): TObjectState;
begin
  Result := FMap.Items[GenerateKey(Entity)].OldState;
end;

function TObjectMap.HasIdValue(Entity: TObject): Boolean;
begin
  Result := FExplorer.HasIdValue(Entity);
end;

function TObjectMap.IsIdMapped(Entity: TObject): Boolean;
begin
  Result := FMap.ContainsKey(GenerateKey(Entity));
end;

function TObjectMap.IsMapped(Entity: TObject): Boolean;
begin
  if not IsIdMapped(Entity) then
    Exit(False);

  Result := Get(Entity) = Entity;
end;

procedure TObjectMap.Remove(Entity: TObject);
begin
  FMap.Remove(GenerateKey(Entity));
end;

procedure TObjectMap.UpdateOldState(Entity: TObject);
var
  Entry: TMapEntry;
begin
  Assert(IsMapped(Entity));
  Entry := FMap[GenerateKey(Entity)];
  if Entry.OldState = nil then
    Entry.OldState := TObjectState.Create;
  UpdateObjectState(Entity, Entry.OldState);
end;

{ TObjectState }

destructor TObjectState.Destroy;
begin
  SetValues(nil);
  inherited;
end;

procedure TObjectState.SetValues(const Value: TDictionary<string, Variant>);
begin
  if FValues <> nil then
    FValues.Free;
  FValues := Value;
end;

{ TCollectionState }

constructor TCollectionState.Create(AExplorer: TMappingExplorer);
begin
  FItemIds := TObjectIdList.Create(AExplorer);
end;

destructor TCollectionState.Destroy;
begin
  FItemIds.Free;
  inherited;
end;

{ TMapEntry }

constructor TMapEntry.Create;
begin
  FOldCollections := TObjectDictionary<string, TCollectionState>.Create([doOwnsValues]);
  FOldState := nil;
end;

destructor TMapEntry.Destroy;
begin
  FOldCollections.Free;
  FOldState.Free;
  inherited;
end;

{ TObjectIdList }

procedure TObjectIdList.Add(Entity: TObject);
begin
  FIdKeys.Add(GenerateIdKey(GetIdValue(Entity)));
  FIdValues.Add(GetIdValue(Entity));
end;

function TObjectIdList.ContainsValue(Id: Variant): boolean;
begin
  Result := FIdKeys.Contains(GenerateIdKey(Id));
end;

function TObjectIdList.ContainsValueOf(Entity: TObject): boolean;
begin
  Result := ContainsValue(GetIdValue(Entity));
end;

constructor TObjectIdList.Create(AExplorer: TMappingExplorer);
begin
  FExplorer := AExplorer;
  FIdKeys := TList<string>.Create;
  FIdValues := TList<Variant>.Create;
end;

destructor TObjectIdList.Destroy;
begin
  FIdKeys.Free;
  FIdValues.Free;
  inherited;
end;

function TObjectIdList.GenerateIdKey(Id: Variant): string;
begin
  Result := TUtils.VariantToString(Id);
end;

function TObjectIdList.GetIdValue(Entity: TObject): Variant;
begin
  Result := FExplorer.GetIdValue(Entity);
end;

function TObjectIdList.IdValues: TEnumerable<Variant>;
begin
  Result := FIdValues;
end;

end.
