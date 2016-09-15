unit Aurelius.Engine.ObjectManager;

{$I Aurelius.inc}

interface

uses
  Classes,
  Generics.Collections, Rtti,
  Aurelius.Commands.Selecter,
  Aurelius.Criteria.Base,
  Aurelius.Drivers.Interfaces,
  Aurelius.Engine.AbstractManager,
  Aurelius.Engine.ObjectMap,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.Optimization,
  Aurelius.Mapping.Explorer,
  Aurelius.Types.Blob,
  Aurelius.Types.MasterObjectValue,
  Aurelius.Types.Proxy;

type
  TObjectManager = class(TAbstractManager)
  strict private
    type
      TEngineCursor = class(TCriteriaCursor)
      private
        FManager: TObjectManager;
        FSelecter: TSelecter;
        FCriteria: TCriteria;
        FOwnsCriteria: boolean;
        FLastFetched: TObject;
        FIds: TDictionary<string, integer>; // This should be TObjectMapKey, chek TObjectMap
      public
        constructor Create(Manager: TObjectManager; Criteria: TCriteria; AOwnsCriteria: boolean);
        destructor Destroy; override;
        function Next: boolean; override;
        function Fetch: TObject; override;
        function BaseClass: TClass; override;
      end;
  strict private
    type
      TAssociationProxyController = class(TInterfacedObject, IProxyController)
      private
        FManager: TObjectManager;
        FReferencedColumns: TStrings;
        FClass: TClass;
        FProxyInfo: IProxyInfo;
        constructor Create(AManager: TObjectManager; ProxyInfo: IProxyInfo);
        function LoadProxyValue: TObject;
        function ProxyInfo: IProxyInfo;
      public
        destructor Destroy; override;
      end;
    type
      TManyValuedAssociationProxyController = class(TInterfacedObject, IProxyController)
      private
        FManager: TObjectManager;
        FProxyInfo: IProxyInfo;
        FManagedContainer: TObject;
        FMemberInfo: TRttiOptimization;
        constructor Create(AManager: TObjectManager; ProxyInfo: IProxyInfo);
        function LoadProxyValue: TObject;
        function ProxyInfo: IProxyInfo;
      public
        destructor Destroy; override;
      end;
    type
      TBlobController = class(TInterfacedObject, IBlobController)
      private
        FManager: TObjectManager;
        FBlobInfo: IBlobInfo;
        FLoaded: boolean;
        constructor Create(AManager: TObjectManager; BlobInfo: IBlobInfo);
        function ReadBlob: TArray<byte>;
        function BlobInfo: IBlobInfo;
      end;
  protected
    type
      TOptimizedProxyInfo = class(TInterfacedObject, IProxyInfo)
      private
        FMemberInfo: TRttiOptimization;
        FType: TProxyType;
        FKey: Variant;
        FManagedContainer: TObject;
        function ProxyType: TProxyType;
        function ClassName: string;
        function MemberName: string;
        function Key: Variant;
        property MemberInfo: TRttiOptimization read FMemberInfo;
        property ManagedContainer: TObject read FManagedContainer;
      public
        constructor Create(AMemberInfo: TRttiOptimization; AKey: Variant); overload;
        constructor Create(AMemberInfo: TRttiOptimization; AKey: Variant; AManagedContainer: TObject); overload;
      end;
      TBlobInfo = class(TInterfacedObject, IBlobInfo)
      private
        FMemberInfo: TRttiOptimization;
        FKey: Variant;
        function ClassName: string;
        function MemberName: string;
        function Key: Variant;
      public
        constructor Create(AMemberInfo: TRttiOptimization; AKey: Variant); overload;
      end;
  private
    FObjects: TObjectMap;
    FOwnsObjects: Boolean;
    procedure InternalSave(Entity: TObject; MasterObj: TMasterObjectValue; ProcessedObjs, DeletedObjs: TList<TObject>);
    procedure InternalUpdate(Entity: TObject; ProcessedObjs, DeletedObjs: TList<TObject>);
    procedure InternalSaveOrUpdate(Entity: TObject; MasterObj: TMasterObjectValue; ProcessedObjs, DeletedObjs: TList<TObject>);
    function InternalMerge(Entity: TObject; ProcessedObjs: TList<TObject>): TObject;
    procedure UpdateCollections(Entity: TObject; DeletedItems: TList<TObject>);
    procedure MergeList(List: IObjectList; OldState: TCollectionState; ItemClass: TClass;
      AddedItems, RemovedItems: TList<TObject>);
    procedure DoRemove(Entity: TObject; Trash: TList<TObject>);
    procedure PerformUpdate(Entity: TObject; ChangedColumns: TList<string>);
    procedure UpdateCollection(Entity: TObject; MemberInfo: TRttiOptimization; DeletedItems: TList<TObject>);
    procedure IncludeInCollection(Item: TObject; Master: TObject; MasterMemberName: string);
    procedure RemoveFromCollection(Item: TObject; Master: TObject; MasterMemberName: string);
  protected
    function CreateProxyController(ProxyInfo: IProxyInfo): IProxyController;
    function CreateBlobController(BlobInfo: IBlobInfo): IBlobController;

    function GetUniqueInstance(Obj: TObject; ProcessedObjs: TList<TObject>): TObject;
    procedure GetUniqueInstances(Objects, ProcessedObjs: TList<TObject>);
    procedure ReplaceObjectAndDestroyOld(List: TList<TObject>; Idx: Integer; New: TObject);
    procedure DestroyTransientObjectTree(Obj: TObject);

    procedure ReplaceAssociations(ManagedObj, TransientObj: TObject);

    procedure CascadeSaveUpdate(Entity: TObject; AssocKind: TAssociationKind; ProcessedObjs, DeletedObjs: TList<TObject>);
    procedure CascadeMerge(Entity: TObject; AssocKind: TAssociationKind; ProcessedObjs: TList<TObject>);
    procedure CascadeRemove(Entity: TObject; AssocKind: TAssociationKind; Trash: TList<TObject>);

    procedure CascadeDestroy(Obj: TObject); // Tirar se não for utilizado
    procedure CascadeThrowInTrash(Obj: TObject; Trash: TList<TObject>); // Tirar se não for utilizado


    procedure InternalRemove(Entity: TObject; Trash: TList<TObject>);

    procedure FindDetailObjects(AResults: TList<TObject>; DetailClass: TClass;
      MemberInfo: TRttiOptimization; MasterKey: Variant); virtual;

    procedure List(ACriteria: TCriteria; AResults: TObjectList<TObject>);
    function Open(ACriteria: TCriteria; AOwnsCriteria: boolean): TCriteriaCursor;
  public
    constructor Create(Connection: IDBConnection; AExplorer: TMappingExplorer); override;
    destructor Destroy; override;

    procedure Save(Entity: TObject);
    procedure Update(Entity: TObject);
    procedure SaveOrUpdate(Entity: TObject);

    function Merge<E: class>(Entity: E): E;
    procedure Remove(Entity: TObject);

    function Find(Clazz: TClass; IdValue: Variant): TObject; overload;

    function Find<E: class>(IdValue: Variant): E; overload;
    function FindAll<E: class>: TObjectList<E>;
    function Find<E: class>: TCriteria<E>; overload;

    function IsAttached(Entity: TObject): Boolean;

    function CreateCriteria(AClass: TClass): TCriteria; overload;
    function CreateCriteria<E: class>: TCriteria<E>; overload;

    function ProxyLoad(ProxyInfo: IProxyInfo): TObject;
    function BlobLoad(BlobInfo: IBlobInfo): TArray<byte>;

    procedure Flush;
    procedure Clear;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Explorer; // Internal use only
  end;

implementation
uses
  SysUtils, Variants,
  Aurelius.Criteria.Exceptions, // only because of Find (UniqueResult causing Internal Error)
  Aurelius.Criteria.Expression,
  Aurelius.Criteria.Projections,
  Aurelius.Commands.Deleter,
  Aurelius.Commands.Inserter,
  Aurelius.Commands.Updater,
  Aurelius.Engine.Exceptions,
  Aurelius.Engine.ObjectFactory,
  Aurelius.Global.Utils,
  Aurelius.Mapping.RttiUtils,
  Aurelius.Types.Exceptions;

type
  TInternalCriteria = class(TCriteria)
  end;

{ TObjectManager }

function TObjectManager.CreateProxyController(ProxyInfo: IProxyInfo): IProxyController;
begin
  if ProxyInfo.ProxyType = TProxyType.List then
    Result := TManyValuedAssociationProxyController.Create(Self, ProxyInfo)
  else
    Result := TAssociationProxyController.Create(Self, ProxyInfo);
end;

function TObjectManager.GetUniqueInstance(Obj: TObject; ProcessedObjs: TList<TObject>): TObject;
var
  List: TList<TObject>;
begin
  if Obj = nil then
    Exit(nil);

  List := TList<TObject>.Create;
  try
    List.Add(Obj);
    GetUniqueInstances(List, ProcessedObjs);
    Result := List[0];
  finally
    List.Free;
  end;
end;

procedure TObjectManager.GetUniqueInstances(Objects, ProcessedObjs: TList<TObject>);
var
  I: Integer;
  A: TAssociation;
  Associations: TList<TAssociation>;
  Aux, ChildObj: TObject;
  List: TList<TObject>;
  ProcessedListWasNil: Boolean;
  ProcessingObj: TObject;
begin
  ProcessedListWasNil := ProcessedObjs = nil;
  if ProcessedListWasNil then
    ProcessedObjs := TList<TObject>.Create;
  try
    for I := 0 to Objects.Count - 1 do
    begin
      if Objects[I] = nil then
        Continue;

      if ProcessedObjs.Contains(Objects[I]) then
        Continue;

      ProcessingObj := Objects[I];
      ProcessedObjs.Add(ProcessingObj);
      try
        if FObjects.IsIdMapped(Objects[I]) then
        begin
          Aux := FObjects.Get(Objects[I]);
          if (Aux <> Objects[I]) then
          begin
            ReplaceAssociations(Aux, Objects[I]);
            ReplaceObjectAndDestroyOld(Objects, I, Aux);
          end;
        end
        else
        begin
          FObjects.Add(Objects[I]);

          Associations := Explorer.GetAssociations(Objects[I].ClassType, True, False);

          // Replace transient instances of single-valued associations by unique managed instances
          for A in Associations do
          begin
            if A.Kind <> TAssociationKind.SingleValued then Continue;

            ChildObj := Explorer.GetMemberValue(Objects[I], A.Optimization).AsObject;
            Aux := GetUniqueInstance(ChildObj, ProcessedObjs);
            if Aux <> ChildObj then
              Explorer.SetMemberValue(Objects[I], A.Optimization, Aux);
          end;

          // Save snapshot of current object state, now that we have loaded it from database and updated associations
          FObjects.UpdateOldState(Objects[I]);

          // Replace transient instances of many-valued associations by unique managed instances
          for A in Associations do
          begin
            if A.Kind <> TAssociationKind.ManyValued then Continue;

            ChildObj := Explorer.GetMemberValue(Objects[I], A.Optimization).AsObject;
            if (ChildObj <> nil) and Explorer.IsList(ChildObj.ClassType) then
            begin
              // TODO: Use AsList instead of forcing type cast like that
              // But it will mostly work, unless ChildObj is a TList (which should not be used)
              List := TList<TObject>(ChildObj);
              GetUniqueInstances(List, ProcessedObjs);
            end;

            // Save a snapshot of the state of each collection in object, for later comparison
            FObjects.UpdateCollectionOldState(Objects[I], A.Optimization, nil);
          end;
        end;
      finally
        ProcessedObjs.Remove(ProcessingObj);
      end;
    end;
  finally
    if ProcessedListWasNil then
      ProcessedObjs.Free;
  end;
end;

procedure TObjectManager.InternalSave(Entity: TObject; MasterObj: TMasterObjectValue;
  ProcessedObjs, DeletedObjs: TList<TObject>);
var
  Inserter: TInserter;
begin
  // Avoid recursion
  if ProcessedObjs.IndexOf(Entity) >= 0 then Exit;
  ProcessedObjs.Add(Entity);

  // if there is a persistent object with same id (either same object or another one), raise an error
  if FObjects.IsIdMapped(Entity) then
    raise EObjectAlreadyPersistent.Create(Entity);

  // If the object already has an id value but the generator doesn't require a value, then raise an error
  if FObjects.HasIdValue(Entity) and not Explorer.GetId(Entity.ClassType).IsUserAssignedId then
    raise ECannotSaveWithId.Create(Entity);

  CascadeSaveUpdate(Entity, TAssociationKind.SingleValued, ProcessedObjs, DeletedObjs);

  Inserter := FCommandFactory.GetCommand<TInserter>(Entity.ClassType);
  try
    Inserter.Insert(Entity, MasterObj);
  finally
    Inserter.Free;
  end;

  // Add newly saved object to the cache
  FObjects.Add(Entity);
  FObjects.UpdateOldState(Entity);

  CascadeSaveUpdate(Entity, TAssociationKind.ManyValued, ProcessedObjs, DeletedObjs);

  // Update collection items. Note that when Save method becomes cached (only effective on Flush)
  // we must remove this method from here, because collections must only be updated when the object is effectively saved
  UpdateCollections(Entity, nil);
end;

procedure TObjectManager.InternalSaveOrUpdate(Entity: TObject;
  MasterObj: TMasterObjectValue; ProcessedObjs, DeletedObjs: TList<TObject>);
begin
  // If there is an Id assigned, then update. Otherwise, Save
  if FObjects.HasIdValue(Entity) then
    InternalUpdate(Entity, ProcessedObjs, DeletedObjs)
  else
    InternalSave(Entity, MasterObj, ProcessedObjs, DeletedObjs);
end;

procedure TObjectManager.InternalUpdate(Entity: TObject; ProcessedObjs, DeletedObjs: TList<TObject>);
begin
  // Avoid recursion
  if ProcessedObjs.IndexOf(Entity) >= 0 then Exit;
  ProcessedObjs.Add(Entity);

  // If there is already a persistent object, but the passed object is transient, then raise an error
  if FObjects.IsIdMapped(Entity) and not FObjects.IsMapped(Entity) then
    raise EObjectAlreadyPersistent.Create(Entity);

  // If the object does not have an id value, raise an error
  if not FObjects.HasIdValue(Entity) then
    raise ECannotUpdateWithoutId.Create(Entity);

  CascadeSaveUpdate(Entity, TAssociationKind.SingleValued, ProcessedObjs, DeletedObjs);

  begin
    // if object was not persistent, then add it to the object map
    if not FObjects.IsMapped(Entity) then
      FObjects.Add(Entity);
  end;

  CascadeSaveUpdate(Entity, TAssociationKind.ManyValued, ProcessedObjs, DeletedObjs);
end;

procedure TObjectManager.InternalRemove(Entity: TObject; Trash: TList<TObject>);
var
  Deleter: TDeleter;
begin
  if not FObjects.HasIdValue(Entity) or not FObjects.IsMapped(Entity) then
    Exit;

  CascadeRemove(Entity, TAssociationKind.ManyValued, Trash);

  // Entity pode ter sido removido pelo cascade se tiver ciclo...
  if Trash.Contains(Entity) then
    Exit;

  if not FObjects.IsMapped(Entity) then
    Exit;

  Deleter := FCommandFactory.GetCommand<TDeleter>(Entity.ClassType);
  try
    Deleter.Delete(Entity);
  finally
    Deleter.Free;
  end;

  if OwnsObjects then
    Trash.Add(Entity);
  FObjects.Remove(Entity);

  CascadeRemove(Entity, TAssociationKind.SingleValued, Trash);
end;

function TObjectManager.IsAttached(Entity: TObject): Boolean;
begin
  Result := FObjects.IsMapped(Entity);
end;

procedure TObjectManager.List(ACriteria: TCriteria; AResults: TObjectList<TObject>);
var
  Cursor: TCriteriaCursor;
begin
  Cursor := Open(ACriteria, false);
  try
    while Cursor.Next do
      AResults.Add(Cursor.Fetch);
  finally
    Cursor.Free;
  end;
end;

procedure TObjectManager.CascadeThrowInTrash(Obj: TObject; Trash: TList<TObject>);
var
  Associations: TList<TAssociation>;
  A: TAssociation;
  ChildValue: TValue;
  ChildObj: TObject;
  List: IObjectList;
  I: Integer;
begin
  if Trash.Contains(Obj) then
    Exit;

  Trash.Add(Obj);

  Associations := Explorer.GetAssociations(Obj.ClassType, True, False);

  for A in Associations do
  begin
    if not (TCascadeType.Remove in A.Cascade) then
      Continue;

    ChildValue := Explorer.GetMemberValue(Obj, A.Optimization);

    if ChildValue.IsEmpty then
      Continue;

    ChildObj := ChildValue.AsObject;
    if ChildObj = nil then
      Continue;

    if A.Kind = TAssociationKind.ManyValued then
    begin
      if Explorer.IsList(ChildObj.ClassType) then
      begin
        List := Explorer.AsList(ChildObj);
        Trash.Add(ChildObj);
        for I := 0 to List.Count - 1 do
          CascadeThrowInTrash(List.Item(I), Trash);
      end;
    end
    else
      CascadeThrowInTrash(ChildObj, Trash);
  end;
end;

function TObjectManager.BlobLoad(BlobInfo: IBlobInfo): TArray<byte>;
var
  Controller: IBlobController;
begin
  Controller := CreateBlobController(BlobInfo);
  if Controller = nil then
    raise ECannotRetrieveBlobController.Create(BlobInfo);
  Result := Controller.ReadBlob;
end;

procedure TObjectManager.CascadeDestroy(Obj: TObject);
var
  Trash: TList<TObject>;
  I: Integer;
begin
  Trash := TList<TObject>.Create;
  try
    CascadeThrowInTrash(Obj, Trash);

    for I := Trash.Count -1 downto 0 do
      if Trash[I] <> nil then
        Trash[I].Free;
  finally
    Trash.Free;
  end;
end;

procedure TObjectManager.CascadeMerge(Entity: TObject; AssocKind: TAssociationKind;
  ProcessedObjs: TList<TObject>);
var
  Associations: TList<TAssociation>;
  A: TAssociation;
  ChildValue: TValue;
  MergedChildObj: TObject;
  ChildObj: TObject;
  ChildItemObj: TObject;
  List: IObjectList;
  I: Integer;
begin
  Associations := Explorer.GetAssociations(Entity.ClassType, True, False);

  for A in Associations do
  begin
    if (A.Kind <> AssocKind) or not (TCascadeType.Merge in A.Cascade) then
      Continue;

    ChildValue := Explorer.GetMemberValue(Entity, A.Optimization);

    if ChildValue.IsEmpty then
      Continue;

    ChildObj := ChildValue.AsObject;
    if ChildObj = nil then
      Continue;

    if A.Kind = TAssociationKind.ManyValued then
    begin
      if Explorer.IsList(ChildObj.ClassType) then
      begin
        List := Explorer.AsList(ChildObj);
        for I := 0 to List.Count - 1 do
        begin
          ChildItemObj := List.Item(I);
          if FObjects.HasIdValue(ChildItemObj) then
          begin
            MergedChildObj := InternalMerge(ChildItemObj, ProcessedObjs);
            if ChildItemObj <> MergedChildObj then
              List.SetItem(I, MergedChildObj);
          end;
        end;
      end;
    end
    else
    begin
      MergedChildObj := InternalMerge(ChildObj, ProcessedObjs);
      if MergedChildObj <> ChildObj then
        Explorer.SetMemberValue(Entity, A.Optimization, MergedChildObj);
    end;
  end;
end;

procedure TObjectManager.CascadeSaveUpdate(Entity: TObject; AssocKind: TAssociationKind;
  ProcessedObjs, DeletedObjs: TList<TObject>);
var
  Associations: TList<TAssociation>;
  A: TAssociation;
  ChildValue: TValue;
  ChildObj, ChildItemObj: TObject;
  I: Integer;
  List: IObjectList;
  MasterObj: TMasterObjectValue;
begin
  Associations := Explorer.GetAssociations(Entity.ClassType, True, False);

  for A in Associations do
  begin
    if (A.Kind <> AssocKind) then Continue;

    ChildValue := Explorer.GetMemberValue(Entity, A.Optimization);

    if ChildValue.IsEmpty then
      Continue;

    ChildObj := ChildValue.AsObject;
    if ChildObj = nil then
      Continue;

    MasterObj.MasterObject := nil;
    MasterObj.MasterAssocMember := '';
    MasterObj.Action := TMasterObjectAction.Include;

    if A.Kind = TAssociationKind.ManyValued then
    begin
      // TODO: Validate duplicated objects in list

      if A.MappedBy = '' then
      begin
        MasterObj.MasterObject := Entity;
        MasterObj.MasterAssocMember := A.ClassMemberName;
        MasterObj.Action := TMasterObjectAction.Include;
      end;
      if Explorer.IsList(ChildObj.ClassType) then
      begin
        List := Explorer.AsList(ChildObj);
        for I := 0 to List.Count - 1 do
        begin
          ChildItemObj := List.Item(I);
          if TCascadeType.SaveUpdate in A.Cascade then
          begin
            InternalSaveOrUpdate(ChildItemObj, MasterObj, ProcessedObjs, DeletedObjs);
          end else
          begin
            if not FObjects.IsMapped(ChildItemObj) then
              raise EAssociationReferencesTransientObject.Create(A, Entity, ChildItemObj);
          end;
        end;
      end;
    end
    else
    begin
      if TCascadeType.SaveUpdate in A.Cascade then
      begin
        InternalSaveOrUpdate(ChildObj, MasterObj, ProcessedObjs, DeletedObjs);
//      MergedChildObj := SaveOrMergeIfNotMapped(ChildObj, MasterObj);
//      if MergedChildObj <> ChildObj then
//        TMappingExplorer.GetInstance.SetMemberValue(Entity, A.Optimization, MergedChildObj);
      end
      else
      begin
        // if CheckTransientAssociations then
        if not FObjects.IsMapped(ChildObj) then
          raise EAssociationReferencesTransientObject.Create(A, Entity, ChildObj);
      end;
    end;
  end;
end;

procedure TObjectManager.CascadeRemove(Entity: TObject; AssocKind: TAssociationKind; Trash: TList<TObject>);
var
  Associations: TList<TAssociation>;
  A: TAssociation;
  ChildValue: TValue;
  ChildObj: TObject;
  I: Integer;
  List: IObjectList;
begin
  Associations := Explorer.GetAssociations(Entity.ClassType, True, False);

  for A in Associations do
  begin
    if (A.Kind <> AssocKind) or not (TCascadeType.Remove in A.Cascade) then
      Continue;

    // if association is a proxy (lazy-loading), then force loading now. When cascading removal (deletion),
    // we need to force to load the association because we need to delete it no matter what
    if A.Optimization.IsProxy and not Explorer.IsProxyLoaded(Entity, A.Optimization) then
      Explorer.ForceProxyLoad(Entity, A.Optimization);

    ChildValue := Explorer.GetMemberValue(Entity, A.Optimization);

    if ChildValue.IsEmpty then
      Continue;

    ChildObj := ChildValue.AsObject;
    if ChildObj = nil then
      Continue;

    if A.Kind = TAssociationKind.ManyValued then
    begin
      if Explorer.IsList(ChildObj.ClassType) then
      begin
        List := Explorer.AsList(ChildObj);
        for I := 0 to List.Count - 1 do
          InternalRemove(List.Item(I), Trash);
      end;
    end
    else
      InternalRemove(ChildObj, Trash);
  end;
end;

procedure TObjectManager.Clear;
begin
  FObjects.Clear(FOwnsObjects);
end;

constructor TObjectManager.Create(Connection: IDBConnection; AExplorer: TMappingExplorer);
begin
  inherited;
  FObjects := TObjectMap.Create(Explorer);
  FOwnsObjects := True;
end;

function TObjectManager.CreateBlobController(BlobInfo: IBlobInfo): IBlobController;
begin
  Result := TBlobController.Create(Self, BlobInfo);
end;

function TObjectManager.CreateCriteria(AClass: TClass): TCriteria;
begin
  result := TCriteria.Create(AClass, Self);
end;

function TObjectManager.CreateCriteria<E>: TCriteria<E>;
begin
  result := TCriteria<E>.Create(E, Self);
end;

destructor TObjectManager.Destroy;
begin
  if FObjects <> nil then
  begin
    Clear;
    FObjects.Free;
  end;
  inherited;
end;

procedure TObjectManager.DestroyTransientObjectTree(Obj: TObject);
var
  A: TAssociation;
  Associations: TList<TAssociation>;
  Aux, Child: TObject;
  MemberClass: TClass;
  List: IObjectList;
  I: Integer;
begin
  Associations := Explorer.GetAssociations(Obj.ClassType, True, False);

  for A in Associations do
  begin
    Child := Explorer.GetMemberValue(Obj, A.Optimization).AsObject;

    if (Child <> nil) then
    begin
      if A.Kind = TAssociationKind.SingleValued then
      begin
        if not FObjects.IsMapped(Child) then
        begin
          Explorer.SetMemberValue(Obj, A.Optimization, nil);
          DestroyTransientObjectTree(Child);
        end;
      end
      else
      begin
        MemberClass := A.Optimization.MemberClass;
        if not Explorer.IsList(MemberClass) then
          Continue;
        List := Explorer.AsList(Child);
        for I := List.Count - 1 downto 0 do
          if not FObjects.IsMapped(List.Item(I)) then
          begin
            Aux := List.Item(I);
            List.Delete(I);
            DestroyTransientObjectTree(Aux);
          end;
      end;
    end;
  end;

  // Try-except to don't raise exception if the object was already
  // destroyed by another branch of this same method
  try
    Obj.Free;
  except
  end;
end;

function TObjectManager.Find(Clazz: TClass; IdValue: Variant): TObject;
var
  Criteria: TCriteria;
  Objects: TObjectList<TObject>;
begin
  Result := FObjects.GetById(Clazz, IdValue);
  if Result <> nil then
    Exit;

  Criteria := CreateCriteria(Clazz).Add(TIdentifierEqExpression.Create(IdValue));
  // Using UniqueResult causes an Internal Error in compiler, so
  // we will duplicate UniqueResult code here....
  //  Result := Criteria.UniqueResult<TObject>;
  try
    Objects := TObjectList<TObject>.Create(False);
    try
      List(Criteria, Objects);
      if Objects.Count > 1 then
        raise EResultsNotUnique.Create(Objects.Count);
      if Objects.Count = 1 then
        Result := Objects[0]
      else
        Result := nil;
    finally
      Objects.Free;
    end;
  finally
    Criteria.Free;
  end;
end;

function TObjectManager.Find<E>: TCriteria<E>;
begin
  Result := CreateCriteria<E>;
end;

function TObjectManager.Find<E>(IdValue: Variant): E;
begin
  Result := E(Find(TClass(E), IdValue));
end;

function TObjectManager.FindAll<E>: TObjectList<E>;
begin
  Result := CreateCriteria<E>.List;
end;

procedure TObjectManager.FindDetailObjects(AResults: TList<TObject>; DetailClass: TClass;
  MemberInfo: TRttiOptimization; MasterKey: Variant);
var
  Selecter: TSelecter;
begin
  Selecter := FCommandFactory.GetCommand<TSelecter>(DetailClass);
  try
    Selecter.SelectDetails(MemberInfo, MasterKey, AResults, 1);
    GetUniqueInstances(AResults, nil);
  finally
    Selecter.Free;
  end;
end;

procedure TObjectManager.Flush;
var
  Obj: TObject;
  Old: TObjectState;
  ProcessedObjs: TList<TObject>;
  DeletedObjs: TList<TObject>;
  RemovedObjs: TList<TObject>;
  Trash: TList<TObject>;
  RemovedObj: TObject;
  ChangedColumns: TList<string>;
begin
  ProcessedObjs := TList<TObject>.Create;
  DeletedObjs := TList<TObject>.Create;
  try
    // Cascade on collections to be sure child/owned objects are saved/updated
    for Obj in FObjects.GetList do
    begin
      if Processedobjs.IndexOf(Obj) >= 0 then Continue;
      CascadeSaveUpdate(Obj, TAssociationKind.SingleValued, ProcessedObjs, DeletedObjs);
      CascadeSaveUpdate(Obj, TAssociationKind.ManyValued, ProcessedObjs, DeletedObjs);
    end;

    // Execute update statements on changed objects
    ProcessedObjs.Clear; // different meaning from previous list
    for Obj in FObjects.GetList do
    begin
      // Avoid recursion
      if ProcessedObjs.IndexOf(Obj) >= 0 then Continue;

      // Avoid processing removed objects
      if DeletedObjs.IndexOf(Obj) >= 0 then Continue;

      ProcessedObjs.Add(Obj);

      Old := FObjects.GetOldState(Obj);
      if Old <> nil then
      begin
        ChangedColumns := Explorer.GetChangedColumns(Obj, Old.Values);
        try
          if ChangedColumns.Count > 0 then
            PerformUpdate(Obj, ChangedColumns);
        finally
          ChangedColumns.Free;
        end;
      end else
        PerformUpdate(Obj, nil);
    end;

    // Now update collections
    for Obj in FObjects.GetList do
    begin
      // Avoid processing removed objects
      if DeletedObjs.IndexOf(Obj) >= 0 then
        Continue;

      RemovedObjs := TList<TObject>.Create;
      Trash := TList<TObject>.Create;
      try
        UpdateCollections(Obj, RemovedObjs);
        for RemovedObj in RemovedObjs do
          DoRemove(RemovedObj, Trash);
        DeletedObjs.AddRange(Trash);
      finally
        Trash.Free;
        RemovedObjs.Free;
      end;
    end;

  finally
    DeletedObjs.Free;
    ProcessedObjs.Free;
  end;
end;

function TObjectManager.Merge<E>(Entity: E): E;
var
  ProcessedObjs: TList<TObject>;
begin
  ProcessedObjs := TList<TObject>.Create;
  try
    Result := E(InternalMerge(Entity, ProcessedObjs));
  finally
    ProcessedObjs.Free;
  end;
end;

procedure TObjectManager.MergeList(List: IObjectList; OldState: TCollectionState;
  ItemClass: TClass; AddedItems, RemovedItems: TList<TObject>);
var
  I: Integer;
  ItemId: Variant;
  RemovedObj: TObject;
  ListItemIds: TObjectIdList;
begin
  // No list, nothing to do. MergeList would probably only be called with non-nil List, but
  // this code is here just in case
  if List = nil then Exit;

  ListItemIds := TObjectIdList.Create(Explorer);
  try
    // fill new items list and check items that were added to collection
    for I := 0 to List.Count - 1 do
      if FObjects.HasIdValue(List.Item(I)) then
      begin
        ListItemIds.Add(List.Item(I));
        if (OldState = nil) or not OldState.ItemIds.ContainsValueOf(List.Item(I)) then
          AddedItems.Add(List.Item(I));
      end;

    // Find old items to be deleted
    if OldState <> nil then
    begin
      for ItemId in OldState.ItemIds.IdValues do
      begin
        // check if the old object is in new list. If not, then it should be deleted
        if not ListItemIds.ContainsValue(ItemId) then
        begin
          RemovedObj := FObjects.GetById(ItemClass, ItemId);
          if RemovedObj <> nil then
            RemovedItems.Add(RemovedObj);
        end;
      end;
    end;
  finally
    ListItemIds.Free;
  end;
end;

function TObjectManager.Open(ACriteria: TCriteria; AOwnsCriteria: boolean): TCriteriaCursor;
begin
  Result := TEngineCursor.Create(Self, ACriteria, AOwnsCriteria);
end;

procedure TObjectManager.PerformUpdate(Entity: TObject; ChangedColumns: TList<string>);
var
  Updater: TUpdater;
begin
  Updater := FCommandFactory.GetCommand<TUpdater>(Entity.ClassType);
  try
    Updater.Update(Entity, ChangedColumns);
    FObjects.UpdateOldState(Entity);
  finally
    Updater.Free;
  end;
end;

function TObjectManager.ProxyLoad(ProxyInfo: IProxyInfo): TObject;
var
  Controller: IProxyController;
begin
  Controller := CreateProxyController(ProxyInfo);
  if Controller = nil then
    raise ECannotRetrieveProxyController.Create(ProxyInfo);
  Result := Controller.LoadProxyValue;
end;

procedure TObjectManager.IncludeInCollection(Item: TObject; Master: TObject;
  MasterMemberName: string);
var
  Updater: TUpdater;
  MasterObj: TMasterObjectValue;
  ChangedColumns: TList<string>;
begin
  Updater := FCommandFactory.GetCommand<TUpdater>(Item.ClassType);
  try
    MasterObj.MasterObject := Master;
    MasterObj.MasterAssocMember := MasterMemberName;
    MasterObj.Action := TMasterObjectAction.Include;
    ChangedColumns := TList<string>.Create; // Create empty ChangedColumns so only master obj is updated - no other fields
    try
      Updater.Update(Item, MasterObj, ChangedColumns);
    finally
      ChangedColumns.Free;
    end;
  finally
    Updater.Free;
  end;
end;

procedure TObjectManager.RemoveFromCollection(Item, Master: TObject;
  MasterMemberName: string);
var
  Updater: TUpdater;
  MasterObj: TMasterObjectValue;
  ChangedColumns: TList<string>;
begin
  Updater := FCommandFactory.GetCommand<TUpdater>(Item.ClassType);
  try
    MasterObj.MasterObject := Master;
    MasterObj.MasterAssocMember := MasterMemberName;
    MasterObj.Action := TMasterObjectAction.Exclude;
    ChangedColumns := TList<string>.Create; // Create empty ChangedColumns so only master obj is updated - no other fields
    try
      Updater.Update(Item, MasterObj, ChangedColumns);
    finally
      ChangedColumns.Free;
    end;
  finally
    Updater.Free;
  end;
end;

function TObjectManager.InternalMerge(Entity: TObject; ProcessedObjs: TList<TObject>): TObject;
var
  Original: TObject;
  A: TAssociation;
begin
  if not FObjects.HasIdValue(Entity) then
    raise ECannotMergeWithoutId.Create(Entity);

  // If Entity is already persistent, then just return it
  if FObjects.IsMapped(Entity) then
    Result := Entity
  else
  begin
    // If Entity is transient, then merge
    // if the object is in memory, use it. Otherwise, load it from database.
    if FObjects.IsIdMapped(Entity) then
      Original := FObjects.Get(Entity)
    else
      Original := Find(Entity.ClassType, Explorer.GetIdValue(Entity));

    if Original = nil then
      raise EIdNotFoundInMerge.Create(Entity.ClassType, Explorer.GetIdValue(Entity));

    // If object is already processed, then dirty properties were already copied to the persistent object.
    // This check happens only if there is several cascades in merge that makes two transient copies (different or not)
    // to be merged in the same persistent object. If the two transientes are the same instance, then we don't need to
    // copy properties again. If they are different, then we will have a problem anyway, because we will have
    // two different copies being merged into a single persistent. Then we will just keep the first copy to be merged
    // and discard the second one.
    if not ProcessedObjs.Contains(Original) then
    begin
      // Go through all lazy collections. If collection proxy is unitialized in original object, but modififed
      // in new object, then force the loading of collection so we could compare new collection with old an delete values if needed
      for A in Explorer.GetAssociations(Entity.ClassType, True, False) do
        if (A.Kind = TAssociationKind.ManyValued) and A.Optimization.IsProxy then
        begin
          if Explorer.IsProxyLoaded(Entity, A.Optimization)
            and not Explorer.IsProxyLoaded(Original, A.Optimization) then
          begin
            Explorer.ForceProxyLoad(Original, A.Optimization);
          end;
        end;

      if Explorer.ObjectChanged(Original, Entity) then
        Explorer.CopyFieldValues(Entity, Original);
    end;
    Result := Original; // Always return the persisted instance
  end;

  // Avoid recursion - do not cascade again for persisted objects
  if ProcessedObjs.Contains(Result) then Exit(Result);
  ProcessedObjs.Add(Result);

  CascadeMerge(Result, TAssociationKind.SingleValued, ProcessedObjs);
  CascadeMerge(Result, TAssociationKind.ManyValued, ProcessedObjs);
end;

procedure TObjectManager.Save(Entity: TObject);
var
  ProcessedObjs: TList<TObject>;
  DeletedObjs: TList<TObject>;
begin
  // TODO: Change this for a transactional context
  ProcessedObjs := TList<TObject>.Create;
  DeletedObjs := TList<TObject>.Create;
  try
    InternalSave(Entity, DummyMasterObject, ProcessedObjs, DeletedObjs);
  finally
    DeletedObjs.Free;
    ProcessedObjs.Free;
  end;
end;

procedure TObjectManager.SaveOrUpdate(Entity: TObject);
var
  ProcessedObjs: TList<TObject>;
  DeletedObjs: TList<TObject>;
begin
  ProcessedObjs := TList<TObject>.Create;
  DeletedObjs := TList<TObject>.Create;
  try
    InternalSaveOrUpdate(Entity, DummyMasterObject, ProcessedObjs, DeletedObjs);
  finally
    DeletedObjs.Free;
    ProcessedObjs.Free;
  end;
end;

procedure TObjectManager.Update(Entity: TObject);
var
  ProcessedObjs: TList<TObject>;
  DeletedObjs: TList<TObject>;
begin
  ProcessedObjs := TList<TObject>.Create;
  DeletedObjs := TList<TObject>.Create;
  try
    InternalUpdate(Entity, ProcessedObjs, DeletedObjs);
  finally
    DeletedObjs.Free;
    ProcessedObjs.Free;
  end;
end;

procedure TObjectManager.UpdateCollections(Entity: TObject; DeletedItems: TList<TObject>);
var
  Associations: TList<TAssociation>;
  A: TAssociation;
begin
  Associations := Explorer.GetAssociations(Entity.ClassType, True, False);
  for A in Associations do
  begin
    if (A.Kind <> TAssociationKind.ManyValued) then Continue;
    UpdateCollection(Entity, A.Optimization, DeletedItems);
    FObjects.UpdateCollectionOldState(Entity, A.Optimization, nil);
  end;
end;

procedure TObjectManager.UpdateCollection(Entity: TObject; MemberInfo: TRttiOptimization;
  DeletedItems: TList<TObject>);
var
  ChildObj: TObject;
  List: IObjectList;
  OldState: TCollectionState;
  AddedItems: TList<TObject>;
  AddedItem: TObject;
  RemovedItems: TList<TObject>;
  RemovedItem: TObject;
begin
  ChildObj := Explorer.GetMemberValue(Entity, MemberInfo).AsObject;

  // get current list
  if (ChildObj <> nil) and (Explorer.IsList(ChildObj.ClassType)) then
    List := Explorer.AsList(ChildObj)
  else
    List := nil;

  // if list is nil (or lazy) avoid comparing
  if List = nil then Exit;

  // Get the original collection. In this case, we must force the list to be loaded, if it's a proxy
  // this is because the old list might have objects to be deleted, so we need to load the original list no matter what
//  if MemberInfo.IsProxy then
//    TMappingExplorer.GetInstance.ForceProxyLoad(Original, MemberInfo);
//  OriginalChildObj := TMappingExplorer.GetInstance.GetMemberValue(Original, MemberInfo).AsObject;

  OldState := FObjects.GetOldCollectionState(Entity, MemberInfo);
  AddedItems := TList<TObject>.Create;
  RemovedItems := TList<TObject>.Create;
  try
    MergeList(List, OldState, MemberInfo.SurroundedType.AsInstance.MetaclassType, AddedItems, RemovedItems);

    // Include added items in list
    for AddedItem in AddedItems do
      IncludeInCollection(AddedItem, Entity, MemberInfo.MemberName);

    // Deleted removed items from list
    for RemovedItem in RemovedItems do
      RemoveFromCollection(RemovedItem, Entity, MemberInfo.MemberName);

    // Todo: Delete orphan - if enabled, then fill DeletedItems with the ones from RemovedItems. Check for DeletedItems nil
  finally
    RemovedItems.Free;
    AddedItems.Free;
  end;
end;

procedure TObjectManager.Remove(Entity: TObject);
begin
  DoRemove(Entity, nil);
end;

procedure TObjectManager.DoRemove(Entity: TObject; Trash: TList<TObject>);
var
  O: TObject;
  TrashWasNil: boolean;
begin
  if not FObjects.IsMapped(Entity) then
    raise EObjectAlreadyDetached.Create(Entity);

  if not FObjects.HasIdValue(Entity) then
    raise ECannotRemoveWithoutId.Create(Entity);

  TrashWasNil := Trash = nil;
  if TrashWasNil then
    Trash := TList<TObject>.Create;

  try
    InternalRemove(Entity, Trash);

    for O in Trash do
    begin
      if O <> nil then
        O.Free;
    end;
  finally
    if TrashWasNil then
      Trash.Free;
  end;
end;

procedure TObjectManager.ReplaceAssociations(ManagedObj, TransientObj: TObject);
var
  A: TAssociation;
  Associations: TList<TAssociation>;
  ManagChild, TransChild: TObject;
  MemberClass: TClass;
  List: IObjectList;
  ListObj: TObject;
begin
  Associations := Explorer.GetAssociations(ManagedObj.ClassType, True, False);

  for A in Associations do
  begin
    if A.Kind = TAssociationKind.SingleValued then
    begin
      ManagChild := Explorer.GetMemberValue(ManagedObj, A.Optimization).AsObject;
      if ManagChild = nil then
      begin
        TransChild := Explorer.GetMemberValue(TransientObj, A.Optimization).AsObject;
        if TransChild <> nil then
          Explorer.SetMemberValue(ManagedObj, A.Optimization, TransChild);
      end;
    end
    else
    begin
      MemberClass := A.Optimization.MemberClass;
      if not Explorer.IsList(MemberClass) then
        Continue;

      ManagChild := Explorer.GetMemberValue(ManagedObj, A.Optimization).AsObject;
      if (ManagChild = nil) then
      begin
        TransChild := Explorer.GetMemberValue(TransientObj, A.Optimization).AsObject;
        if TransChild <> nil then
        begin
          ListObj := TObjectFactory.GetInstance.CreateInstance(MemberClass);
          List := Explorer.AsList(ListObj);
          List.Assign(Explorer.AsList(TransChild));
          Explorer.SetMemberValue(ManagedObj, A.Optimization, ListObj);
        end;
      end;
    end;

    // TODO: What if there are values in both objects and values are different?
  end;
end;

procedure TObjectManager.ReplaceObjectAndDestroyOld(List: TList<TObject>;
  Idx: Integer; New: TObject);
var
  ObjectList: TObjectList<TObject>;
  OldOwnsObjects: Boolean;
begin
  DestroyTransientObjectTree(List[Idx]);

  // When List is TObjectList<System.TObject>, the expression
  // "List is TObjectList<System.TObject>" evaluates false...
  // Doing a workaround to solve the problem:

  if Pos('TObjectList', List.ClassName) > 0 then
  begin
    ObjectList := TObjectList<TObject>(List);

    OldOwnsObjects := ObjectList.OwnsObjects;
    try
      ObjectList.OwnsObjects := False;
      List[Idx] := New;
    finally
      ObjectList.OwnsObjects := OldOwnsObjects;
    end;
  end
  else
    List[Idx] := New;
end;

{ TObjectManager.TEngineCursor }

function TObjectManager.TEngineCursor.BaseClass: TClass;
begin
  Result := FCriteria.Clazz;
end;

constructor TObjectManager.TEngineCursor.Create(Manager: TObjectManager;
  Criteria: TCriteria; AOwnsCriteria: boolean);
begin
  FManager := Manager;
  FCriteria := Criteria;
  FOwnsCriteria := AOwnsCriteria;
  FIds := TDictionary<string, integer>.Create;
  FSelecter := FManager.FCommandFactory.GetCommand<TSelecter>(FCriteria.Clazz);
  FSelecter.SelectBegin(FCriteria);
end;

destructor TObjectManager.TEngineCursor.Destroy;
begin
  FSelecter.SelectEnd;
  FSelecter.Free;
  FIds.Free;
  if FOwnsCriteria then
    FCriteria.Free;
  inherited;
end;

function TObjectManager.TEngineCursor.Fetch: TObject;
begin
  Result := FLastFetched;
end;

function TObjectManager.TEngineCursor.Next: boolean;
var
  Id: Variant;
  IdKey: string;
  SkipDuplicates: boolean;
begin
  SkipDuplicates := TInternalCriteria(FCriteria).RemoveDuplicatedEntities
    and not TInternalCriteria(FCriteria).HasProjection;

  if not SkipDuplicates then
  begin
    Result := FSelecter.SelectNext;
    if Result then
    begin
      FLastFetched := FSelecter.SelectFetch;
      if not TInternalCriteria(FCriteria).HasProjection then
        FLastFetched := FManager.GetUniqueInstance(FLastFetched, nil);
    end;
  end
  else
  begin
    repeat
      Result := FSelecter.SelectNext;
      if Result then
      begin
        FLastFetched := FSelecter.SelectFetch;
        FLastFetched := FManager.GetUniqueInstance(FLastFetched, nil);
        Id := FManager.Explorer.GetIdValue(FLastFetched);
        IdKey := TUtils.VariantToString(Id);
      end;
    until not Result or not FIds.ContainsKey(IdKey);
    if Result then
      FIds.Add(IdKey, 0);
  end;

end;

{ TObjectManager.TAssociationProxyController }

constructor TObjectManager.TAssociationProxyController.Create(AManager: TObjectManager; ProxyInfo: IProxyInfo);
var
  A: TAssociation;
  I: Integer;
  MemberInfo: TRttiOptimization;
  ContainerClass: TClass;
begin
  FManager := AManager;
  FProxyInfo := ProxyInfo;

  if ProxyInfo is TOptimizedProxyInfo then
    A := FManager.Explorer.GetAssociationByMember((ProxyInfo as TOptimizedProxyInfo).MemberInfo.MemberRef)
  else
  begin
    ContainerClass := FManager.Explorer.Hierarchy.FindClassByName(ProxyInfo.ClassName);
    if ContainerClass = nil then
      EProxyInfoClassNotFound.Create(ProxyInfo.ClassName);
    MemberInfo := FManager.Explorer.GetOptimization(ContainerClass, ProxyInfo.MemberName);
    try
      A := FManager.Explorer.GetAssociationByMember(MemberInfo.MemberRef);
    finally
      MemberInfo.Free;
    end;
  end;

  Assert(A <> nil, Format('Proxy controller not found for member "%s"', [ProxyInfo.MemberName]));
  FClass := A.Target;
  FReferencedColumns := TStringList.Create;
  // if only one column and that column is id, then we don't need custom referenced columns, otherwise, add it
  if (A.JoinColumns.Count <> 1) or not A.JoinColumns[0].ReferencedColumnIsId then
    for I := 0 to A.JoinColumns.Count - 1 do
      FReferencedColumns.Add(A.JoinColumns[I].ReferencedColumn.Name);
end;

destructor TObjectManager.TAssociationProxyController.Destroy;
begin
  FReferencedColumns.Free;
  inherited;
end;

function TObjectManager.TAssociationProxyController.ProxyInfo: IProxyInfo;
begin
  Result := FProxyInfo;
end;

function TObjectManager.TAssociationProxyController.LoadProxyValue: TObject;
var
  Criteria: TCriteria;
  I: Integer;
  Objects: TObjectList<TObject>;
  Key: Variant;
begin
  if FManager = nil then
    raise EObjectManagerNotSet.Create(FClass);

  Key := ProxyInfo.Key;
  if FReferencedColumns.Count = 0 then
  begin
    if FManager.Explorer.IsIdNull(FClass, Key) then
      Result := nil
    else
      Result := FManager.Find(FClass, Key);
  end
  else
  begin
    // Load proxy by custom referenced columns, not by id columns
    Criteria := FManager.CreateCriteria(FClass);
    try
      // Fill the criteria with column names
      if FReferencedColumns.Count = 1 then
      begin
        Criteria.Add(
          TSimpleExpression.Create(
            TColumnProjection.Create(FReferencedColumns[0]),
            Key, TExpressionOperator.Equal));
      end
      else
      begin
        Assert(VarIsArray(Key), 'Proxy composite Id value must be a variant array');
        Assert(VarArrayHighBound(
        Key, 1) = FReferencedColumns.Count - 1, 'Proxy composite Id variant array must have same length as number of referenced columns');
        for I := 0 to FReferencedColumns.Count - 1 do
          Criteria.Add(
            TSimpleExpression.Create(
              TColumnProjection.Create(FReferencedColumns[I]),
              Key[I], TExpressionOperator.Equal));
      end;

      // Get results
      Objects := TObjectList<TObject>.Create(false);
      try
        FManager.List(Criteria, Objects);
        Assert(Objects.Count = 1, 'Proxy loading failed. Results did not return a single object.');
        Result := Objects[0];
      finally
        Objects.Free;
      end;
    finally
      Criteria.Free;
    end;
  end;
end;

{ TObjectManager.TManyValuedAssociationProxyController }

constructor TObjectManager.TManyValuedAssociationProxyController.Create(AManager: TObjectManager; ProxyInfo: IProxyInfo);
var
  MemberInfo: TRttiOptimization;
  ContainerClass: TClass;
  A: TAssociation;
begin
  FManager := AManager;
  FProxyInfo := ProxyInfo;
  FManagedContainer := nil;

  if ProxyInfo is TOptimizedProxyInfo then
  begin
    A := FManager.Explorer.GetAssociationByMember((ProxyInfo as TOptimizedProxyInfo).MemberInfo.MemberRef);
    FManagedContainer := (ProxyInfo as TOptimizedProxyInfo).ManagedContainer;
  end
  else
  begin
    ContainerClass := FManager.Explorer.Hierarchy.FindClassByName(ProxyInfo.ClassName);
    if ContainerClass = nil then
      EProxyInfoClassNotFound.Create(ProxyInfo.ClassName);
    MemberInfo := FManager.Explorer.GetOptimization(ContainerClass, ProxyInfo.MemberName);
    try
      A := FManager.Explorer.GetAssociationByMember(MemberInfo.MemberRef);
    finally
      MemberInfo.Free;
    end;
  end;

  Assert(A <> nil, Format('Proxy controller not found for member "%s"', [ProxyInfo.MemberName]));
  FMemberInfo := A.Optimization;
end;

function TObjectManager.TManyValuedAssociationProxyController.ProxyInfo: IProxyInfo;
begin
  Result := FProxyInfo;
end;

destructor TObjectManager.TManyValuedAssociationProxyController.Destroy;
begin
  inherited;
end;

function TObjectManager.TManyValuedAssociationProxyController.LoadProxyValue: TObject;
var
  DetailClass: TClass;
  ListClass: TClass;
begin
  ListClass := FMemberInfo.MemberClass;
  if FManager = nil then
    raise EObjectManagerNotSet.Create(ListClass);
  DetailClass := TRttiUtils.GetInstance.GetSurroundedClass(ListClass);

  // Get results
  Result := TObjectFactory.GetInstance.CreateInstance(ListClass);
  try
    FManager.FindDetailObjects(TList<TObject>(Result), DetailClass, FMemberInfo, ProxyInfo.Key);

    // Update state of collection that has been loaded in lazy mode
    if FManagedContainer <> nil then
      FManager.FObjects.UpdateCollectionOldState(FManagedContainer, FProxyInfo.MemberName, Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TObjectManager.TOptimizedProxyInfo }

function TObjectManager.TOptimizedProxyInfo.ClassName: string;
begin
  Assert(FMemberInfo.MemberRef.Parent <> nil);
  Assert(FMemberInfo.MemberRef.Parent.IsInstance);
  Result := FMemberInfo.MemberRef.Parent.AsInstance.MetaclassType.ClassName;
end;

constructor TObjectManager.TOptimizedProxyInfo.Create(AMemberInfo: TRttiOptimization; AKey: Variant);
begin
  Create(AMemberInfo, AKey, nil);
  FType := TProxyType.Single;
end;

constructor TObjectManager.TOptimizedProxyInfo.Create(AMemberInfo: TRttiOptimization; AKey: Variant; AManagedContainer: TObject);
begin
  FMemberInfo := AMemberInfo;
  FType := TProxyType.List;
  FKey := AKey;
  FManagedContainer := AManagedContainer;
end;

function TObjectManager.TOptimizedProxyInfo.Key: Variant;
begin
  Result := FKey;
end;

function TObjectManager.TOptimizedProxyInfo.MemberName: string;
begin
  Result := FMemberInfo.MemberName;
end;

function TObjectManager.TOptimizedProxyInfo.ProxyType: TProxyType;
begin
  Result := FType;
end;

{ TObjectManager.TBlobController }

function TObjectManager.TBlobController.BlobInfo: IBlobInfo;
begin
  Result := FBlobInfo;
end;

constructor TObjectManager.TBlobController.Create(AManager: TObjectManager; BlobInfo: IBlobInfo);
begin
  FManager := AManager;
  FBlobInfo := BlobInfo;
  FLoaded := false;
end;

function TObjectManager.TBlobController.ReadBlob: TArray<byte>;
var
  ContainerClass: TClass;
  MemberName: string;
  Key: Variant;
  BlobResult: TCriteriaResult;
  Criteria: TCriteria;
begin
  SetLength(Result, 0);
  if FManager = nil then Exit;
  ContainerClass := FManager.Explorer.Hierarchy.FindClassByName(FBlobInfo.ClassName);
  MemberName := FBlobInfo.MemberName;
  Key := FBlobInfo.Key;
  if (ContainerClass <> nil) and not FManager.Explorer.IsIdNull(ContainerClass, Key) then
  begin
    Criteria := FManager.CreateCriteria(ContainerClass);
    try
      Criteria.SetProjections(TProjections.Prop(MemberName));
      Criteria.Add(TExpression.IdEq(Key));
      BlobResult := Criteria.UniqueValue;
    except
      Criteria.Free;
      raise;
    end;
    try
      {$IFDEF DELPHIXE_LVL}
      Result := TUtils.VariantToBytes(BlobResult.Values[0]);
      {$ELSE}
      Result := TArray<byte>(TUtils.VariantToBytes(BlobResult.Values[0]));
      {$ENDIF}
    finally
      BlobResult.Free;
    end;
  end;
end;

{ TObjectManager.TBlobInfo }

function TObjectManager.TBlobInfo.ClassName: string;
begin
  Assert(FMemberInfo.MemberRef.Parent <> nil);
  Assert(FMemberInfo.MemberRef.Parent.IsInstance);
  Result := FMemberInfo.MemberRef.Parent.AsInstance.MetaclassType.ClassName;
end;

constructor TObjectManager.TBlobInfo.Create(AMemberInfo: TRttiOptimization; AKey: Variant);
begin
  FMemberInfo := AMemberInfo;
  FKey := AKey;
end;

function TObjectManager.TBlobInfo.Key: Variant;
begin
  Result := FKey;
end;

function TObjectManager.TBlobInfo.MemberName: string;
begin
  Result := FMemberInfo.MemberName;
end;

end.
