unit Aurelius.Engine.Exceptions;

{$I Aurelius.inc}

interface

uses
  Aurelius.Global.Exceptions,
  Aurelius.Mapping.Metadata,
  Aurelius.Types.Blob,
  Aurelius.Types.Proxy;

type
  EIdNotSetException = class(EOPFBaseException)
  public
    constructor Create(EntityClass: TClass);
  end;

  ECannotSaveWithId = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  ECannotMergeWithoutId = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  ECannotUpdateWithoutId = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  ECannotRemoveWithoutId = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  EObjectAlreadyPersistent = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  EAssociationReferencesTransientObject = class(EOPFBaseException)
  private
    FAssociatedTransientObject: TObject;
    FAssociation: TAssociation;
    FEntity: TObject;
  public
    constructor Create(A: TAssociation; Entity, Transient: TObject);
    property Association: TAssociation read FAssociation;
    property Entity: TObject read FEntity;
    property AssociatedTransientObject: TObject read FAssociatedTransientObject;
  end;

  EObjectAlreadyDetached = class(EOPFBaseException)
  public
    constructor Create(Entity: TObject);
  end;

  EDuplicateId = class(EOPFBaseException)
  public
    constructor Create(BaseClass: TClass);
  end;

  EEngineInternalError = class(EOPFInternalError)
  end;

  EIdNotFoundInMerge = class(EOPFInternalError)
  public
    constructor Create(AClass: TClass; AId: Variant);
  end;

  EProxyInfoClassNotFound = class(EOPFInternalError)
  public
    constructor Create(ClassName: string);
  end;

  ECannotRetrieveProxyController = class(EOPFInternalError)
  public
    constructor Create(ProxyInfo: IProxyInfo);
  end;

  ECannotRetrieveBlobController = class(EOPFInternalError)
  public
    constructor Create(BlobInfo: IBlobInfo);
  end;

implementation
uses
  Aurelius.Global.Utils,
  Variants;

{ EIdNotSetException }

constructor EIdNotSetException.Create(EntityClass: TClass);
begin
  inherited CreateFmt('Id not set on entity of class %s.',
    [EntityClass.ClassName]);
end;

{ ECannotSaveWithId }

constructor ECannotSaveWithId.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot save object of class %s. The object ' +
    'already has an id, use merge instead.', [Entity.ClassName]);
end;

{ ECannotMergeWithoutId }

constructor ECannotMergeWithoutId.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot merge object of class %s. The object ' +
    'doesn''t have an id, use Save instead.', [Entity.ClassName]);
end;

{ ECannotRemoveWithoutId }

constructor ECannotRemoveWithoutId.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot remove object of class %s. The object ' +
    'doesn''t have an id.', [Entity.ClassName]);
end;

{ EObjectAlreadyPersistent }

constructor EObjectAlreadyPersistent.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot turn persistent object of class %s. ' +
    'The object is already in persistent context.', [Entity.ClassName]);
end;

{ EObjectAlreadyDetached }

constructor EObjectAlreadyDetached.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot remove or detach object of class %s. ' +
    'The object is not in persistent context.', [Entity.ClassName]);
end;

{ EDuplicateId }

constructor EDuplicateId.Create(BaseClass: TClass);
begin
  inherited CreateFmt('Duplicate id on class %s. (If you''re using ' +
    'isTablePerConcreteClass inheritance strategy, all subclasses of %s ' +
    'have to share the same id sequence).',
    [BaseClass.ClassName, BaseClass.ClassName]);
end;

{ EIdNotFoundInMerge }

constructor EIdNotFoundInMerge.Create(AClass: TClass; AId: Variant);
begin
  inherited CreateFmt('Cannot merge object of type %s. Object with Id %s does not exist.',
    [AClass.ClassName, TUtils.VariantToString(AId)]);
end;

{ ECannotUpdateWithoutId }

constructor ECannotUpdateWithoutId.Create(Entity: TObject);
begin
  inherited CreateFmt('Cannot update object of class %s. The object ' +
    'doesn''t have an id, use Save instead.', [Entity.ClassName]);
end;

{ EAssociationReferencesTransientObject }

constructor EAssociationReferencesTransientObject.Create(A: TAssociation;
  Entity, Transient: TObject);
begin
  FAssociation := A;
  FEntity := Entity;
  FAssociatedTransientObject := Transient;
  inherited Create('Association references a transient object');
end;

{ EProxyInfoClassNotFound }

constructor EProxyInfoClassNotFound.Create(ClassName: string);
begin
  inherited CreateFmt('Cannot load proxy. Container class "%s" in proxy info was not found.', [ClassName]);
end;

{ ECannotRetrieveProxyController }

constructor ECannotRetrieveProxyController.Create(ProxyInfo: IProxyInfo);
begin
  inherited CreateFmt('Cannot retrieve proxy controller for class "%", member "%s"',
    [ProxyInfo.ClassName, ProxyInfo.MemberName]);
end;

{ ECannotRetrieveBlobController }

constructor ECannotRetrieveBlobController.Create(BlobInfo: IBlobInfo);
begin
  inherited CreateFmt('Cannot retrieve blob controller for class "%", member "%s"',
    [BlobInfo.ClassName, BlobInfo.MemberName]);
end;

end.
