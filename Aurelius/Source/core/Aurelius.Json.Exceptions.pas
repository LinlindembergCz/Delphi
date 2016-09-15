unit Aurelius.Json.Exceptions;

{$I Aurelius.Inc}

interface
uses
  Aurelius.Global.Exceptions,
  Aurelius.Mapping.Optimization;

type
  EJsonSerializationException = class(EOPFBaseException)
  end;

  EVariantTypeNotSupported = class(EJsonSerializationException)
  public
    constructor Create(Value: Variant);
  end;

  EMemberTypeNotSupported = class(EJsonSerializationException)
  public
    constructor Create(Member: TRttiOptimization);
  end;

  EObjectClassNotSpecified = class(EJsonSerializationException)
  public
    constructor Create(JsonString: string);
  end;

  EDynamicPropertyNotFound = class(EJsonSerializationException)
  public
    constructor Create(AClass: TClass; APropName: string);
  end;

  EIncompatibleJsonValueType = class(EJsonSerializationException)
  public
    constructor Create;
  end;

  EReferencedObjectNotFound = class(EJsonSerializationException)
  public
    constructor Create(RefId: string);
  end;

  EInvalidListObject = class(EJsonSerializationException)
  public
    constructor Create(MemberName: string);
  end;

implementation
uses
  TypInfo, Variants;

{ EVariantTypeNotSupported }

constructor EVariantTypeNotSupported.Create(Value: Variant);
begin
  inherited CreateFmt('Could not convert variant of type %d to a Json value', [VarType(Value)]);
end;

{ EMemberTypeNotSupported }

constructor EMemberTypeNotSupported.Create(Member: TRttiOptimization);
var
  AClassName: string;
  AMemberName: string;
  ATypeName: string;
begin
  AClassName := '';
  if (Member.MemberRef <> nil) and (Member.MemberRef.Parent <> nil) then
    AClassName := Member.MemberRef.Parent.Name;

  AMemberName := '';
  if (Member.MemberRef <> nil) then
    AMemberName := Member.MemberRef.Name;

  ATypeName := '';
  if (Member.MemberType <> nil) then
    ATypeName := Member.MemberType.Name;


  inherited CreateFmt('Cannot serialize/deserialize member "%s.%s". Unsupported type: %s',
    [AClassName,
     AMemberName,
     ATypeName]);
end;

{ EObjectClassNotSpecified }

constructor EObjectClassNotSpecified.Create(JsonString: string);
begin
  inherited Create('Cannot instantiate object. "$type" not specified or invalid in Json representation. '
    + sLineBreak + JsonString);
end;

{ EDynamicPropertyNotFound }

constructor EDynamicPropertyNotFound.Create(AClass: TClass; APropName: string);
begin
  inherited CreateFmt('Dynamic property "%s" not found for class %s',
    [APropName, AClass.ClassName]);
end;

{ EIncompatibleJsonValueType }

constructor EIncompatibleJsonValueType.Create;
begin
  inherited Create('Incompatible json value. Cannot serialize to the proper type.');
end;

{ EReferencedObjectNotFound }

constructor EReferencedObjectNotFound.Create(RefId: string);
begin
  inherited CreateFmt('Referenced object not found. Object with id "%s" does not exist.', [RefId]);
end;

{ EInvalidListObject }

constructor EInvalidListObject.Create(MemberName: string);
begin
  inherited CreateFmt('List object for member "%s" not initialized. Cannot load list.', [MemberName]);
end;

end.
