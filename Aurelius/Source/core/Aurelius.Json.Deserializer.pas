unit Aurelius.Json.Deserializer;

{$I Aurelius.Inc}

interface

uses
  Generics.Collections, TypInfo, Rtti, SysUtils,
  Aurelius.Json.Interfaces,
  Aurelius.Json.Settings,
  Aurelius.Mapping.Explorer,
  Aurelius.Mapping.Optimization,
  Aurelius.Types.Blob,
  Aurelius.Types.Proxy;

type
  TCustomJsonDeserializer = class;
  TJsonTypeReverter = reference to function(Deserializer: TCustomJsonDeserializer;
    Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue;
  TObjectCreatedEvent = procedure(Sender: TObject; AObject: TObject) of object;
//  TLoadProxyEvent = procedure(Sender: TObject; ProxyInfo: IProxyInfo; var LoadedObject: TObject) of object;

  IJsonProxyLoader = interface
    function LoadProxyValue(ProxyInfo: IProxyInfo): TObject;
  end;

  IJsonBlobLoader = interface
    function ReadBlob(BlobInfo: IBlobInfo): TArray<byte>;
  end;

  TCustomJsonDeserializer = class abstract
  strict private
    type
      TJsonProxyInfo = class(TInterfacedObject, IProxyInfo)
      private
        FProxyType: TProxyType;
        FClassName: string;
        FMemberName: string;
        FKey: Variant;
        function ProxyType: TProxyType;
        function ClassName: string;
        function MemberName: string;
        function Key: Variant;
        constructor Create(AProxyType: TProxyType; AClassName, AMemberName: string; AKey: Variant);
      end;

      TJsonProxyController = class(TInterfacedObject, IProxyController)
      private
        FProxyLoader: IJsonProxyLoader;
        FProxyInfo: IProxyInfo;
        constructor Create(AProxyLoader: IJsonProxyLoader; AProxyInfo: IProxyInfo);
        function LoadProxyValue: TObject;
        function ProxyInfo: IProxyInfo;
      end;
    type
      TJsonBlobInfo = class(TInterfacedObject, IBlobInfo)
      private
        FClassName: string;
        FMemberName: string;
        FKey: Variant;
        function ClassName: string;
        function MemberName: string;
        function Key: Variant;
        constructor Create(AClassName, AMemberName: string; AKey: Variant);
      end;

      TJsonBlobController = class(TInterfacedObject, IBlobController)
      private
        FBlobLoader: IJsonBlobLoader;
        FBlobInfo: IBlobInfo;
        constructor Create(ABlobLoader: IJsonBlobLoader; ABlobInfo: IBlobInfo);
        function ReadBlob: TArray<byte>;
        function BlobInfo: IBlobInfo;
      end;
  strict private
    FExplorer: TMappingExplorer;
    FReverters: TDictionary<PTypeInfo, TJsonTypeReverter>;
    FSettings: TJsonSettings;
    FContext: TRttiContext;
    FObjectMap: TDictionary<string, TObject>;
    FOnEntityCreated: TObjectCreatedEvent;
//    FOnLoadProxy: TLoadProxyEvent;
    FEntities: TObjectList<TObject>;
    FProxyLoader: IJsonProxyLoader;
    FBlobLoader: IJsonBlobLoader;
    function TranslateObjectClass(TypeName: IJsonValue): TClass;
    function FindReferencedObject(Value: IJsonValue): TObject;
    procedure AddReferencedObject(Entity: TObject; RefId: IJsonValue);
    function JsonToVariant(Value: IJsonValue): Variant;
    function JsonToNullable(Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue): TValue;
    procedure JsonToProxy(Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue);
    function JsonToList(ListClass: TClass; List: TObject; JsonList: IJsonArray): TValue;
    function JsonToSimpleMemberValue(Entity: TObject; MemberInfo: TRttiOptimization;
      Value: IJsonValue; var Handled: boolean): TValue;
    function JsonToEntity(Value: IJsonValue): TObject;
    function JsonToObject(ObjectClass: TClass; CurrentObject: TObject; JsonValue: IJsonValue): TValue;
    procedure JsonToDynamicProperties(Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonObject);
    procedure LoadJsonProperty(Entity: TObject; MemberInfo: TRttiOptimization; PropValue: IJsonValue);
    procedure Error;
    function GetOwnsEntities: boolean;
    procedure SetOwnsEntities(const Value: boolean);
//     property OnLoadProxy: TLoadProxyEvent read FOnLoadProxy write FOnLoadProxy;
  protected
//    procedure DoLoadProxy(ProxyInfo: IProxyInfo; var LoadedObject: TObject);
    procedure DoEntityCreated(Entity: TObject);
    function FromJsonValue(JsonValue: IJsonValue; ResultClass: TClass): TObject;
    function JsonValueToProxyInfo(Value: IJsonValue): IProxyInfo;
    function JsonValueToBlobInfo(Value: IJsonValue): IBlobInfo;
//    function FromJson<T>(Value: IJsonValue): T; overload;
  public
    // ASettings parameter is destroyed by the serializer
    constructor Create; overload;
    constructor Create(AExplorer: TMappingExplorer); overload;
    constructor Create(AExplorer: TMappingExplorer; ASettings: TJsonSettings); overload;
    destructor Destroy; override;
    property OwnsEntities: boolean read GetOwnsEntities write SetOwnsEntities;
    property ProxyLoader: IJsonProxyLoader read FProxyLoader write FProxyLoader;
    property BlobLoader: IJsonBlobLoader read FBlobLoader write FBlobLoader;
    property OnEntityCreated: TObjectCreatedEvent read FOnEntityCreated write FOnEntityCreated;
  end;

  TTypedJsonDeserializer<T> = class(TCustomJsonDeserializer)
  public
    class function Wrap(Value: T): IJsonValue; virtual; abstract;
    function FromJson(JsonValue: T; ResultClass: TClass): TObject; overload;
    function FromJson<E: class>(JsonValue: T): E; overload;
    function ProxyInfoFromJson(JsonValue: T): IProxyInfo;
    function BlobInfoFromJson(JsonValue: T): IBlobInfo;
  end;

  TJsonProxyLoadFunction = reference to function(ProxyInfo: IProxyInfo): TObject;

  TJsonProxyLoader = class(TInterfacedObject, IJsonProxyLoader)
  private
    FLoad: TJsonProxyLoadFunction;
    function LoadProxyValue(ProxyInfo: IProxyInfo): TObject;
  public
    constructor Create(ALoad: TJsonProxyLoadFunction);
  end;

  TJsonBlobLoadFunction = reference to function(BlobInfo: IBlobInfo): TArray<byte>;

  TJsonBlobLoader = class(TInterfacedObject, IJsonBlobLoader)
  private
    FLoad: TJsonBlobLoadFunction;
    function ReadBlob(BlobInfo: IBlobInfo): TArray<byte>;
  public
    constructor Create(ALoad: TJsonBlobLoadFunction);
  end;

implementation

uses
  Variants,
  Aurelius.Engine.ObjectFactory,
  Aurelius.Global.Utils,
  Aurelius.Json.Exceptions,
  Aurelius.Mapping.Metadata,
  Aurelius.Types.DynamicProperties;

{ TJsonDeserializer }

type
  TInternalExplorer = class(TMappingExplorer)
  end;

procedure TCustomJsonDeserializer.AddReferencedObject(Entity: TObject; RefId: IJsonValue);
begin
  FObjectMap.Add(RefId.JsonString, Entity);
end;

constructor TCustomJsonDeserializer.Create(AExplorer: TMappingExplorer);
begin
  Create(AExplorer, TJsonSettings.Create);
end;

constructor TCustomJsonDeserializer.Create(AExplorer: TMappingExplorer; ASettings: TJsonSettings);
var
  DateReverter: TJsonTypeReverter;
begin
  FExplorer := AExplorer;
  FSettings := ASettings;
  FReverters := TDictionary<PTypeInfo, TJsonTypeReverter>.Create;
  FObjectMap := TDictionary<string, TObject>.Create;
  FEntities := TObjectList<TObject>.Create(false);

  FReverters.Add(TypeInfo(boolean),
    function(Deserializer: TCustomJsonDeserializer;
      Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue
    var
      MemberValue: TValue;
    begin
      case Value.ValueType of
        TJsonType.JBoolean:
          MemberValue := TValue.From<boolean>(Value.BoolValue);
      else
        Error;
      end;
      Result := MemberValue;
    end
  );

  DateReverter :=
    function(Deserializer: TCustomJsonDeserializer; Entity: TObject; MemberInfo:
      TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue
    var
      DateValue: TDateTime;
      MemberValue: TValue;
    begin
      DateValue := 0;
      case Value.ValueType of
        TJsonType.JString:
          begin
            DateValue := TUtils.ISOToDateTime(Value.StringValue);
          end
      else
        Error;
      end;
      TValue.Make(@DateValue, MemberInfo.RealType.Handle, MemberValue);
      Result := MemberValue;
    end;
  FReverters.Add(TypeInfo(TDateTime), DateReverter);
  FReverters.Add(TypeInfo(TDate), DateReverter);
  FReverters.Add(TypeInfo(TTime), DateReverter);

  FReverters.Add(TypeInfo(TGUID),
    function(Deserializer: TCustomJsonDeserializer;
      Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue
    var
      MemberValue: TValue;
    begin
      case Value.ValueType of
        TJsonType.JString:
          MemberValue := TValue.From<TGuid>(StringToGuid(Value.StringValue));
        TJsonType.JNull:
          MemberValue := TValue.From<TGuid>(TUtils.NullGuid);
      else
        Error;
      end;
      Result := MemberValue;
    end
  );

  FReverters.Add(TypeInfo(TBlob),
    function(Deserializer: TCustomJsonDeserializer;
      Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue
    var
      Blob: TBlob;
      BlobInfo: IBlobInfo;
    begin
      case Value.ValueType of
        TJsonType.JString:
          Blob.AsBytes := TUtils.Decode64(Value.StringValue);
        TJsonType.JNull:
          Blob.IsNull := true;
        TJsonType.JObject:
          begin
            BlobInfo := JsonValueToBlobInfo(Value);
            FExplorer.SetBlobController(Entity, MemberInfo, TJsonBlobController.Create(FBlobLoader, BlobInfo));
            Handled := true;
          end;
      else
        Error;
      end;
      Result := TValue.From<TBlob>(Blob);
    end
  );

  FContext := TRttiContext.Create;
end;

constructor TCustomJsonDeserializer.Create;
begin
  Create(TMappingExplorer.DefaultInstance);
end;

destructor TCustomJsonDeserializer.Destroy;
begin
  FReverters.Free;
  FSettings.Free;
  FObjectMap.Free;
  FEntities.Free;
  inherited;
end;

procedure TCustomJsonDeserializer.Error;
begin
  raise EIncompatibleJsonValueType.Create;
end;

function TCustomJsonDeserializer.FindReferencedObject(Value: IJsonValue): TObject;
begin
  if not FObjectMap.TryGetValue(Value.JsonString, Result) then
    raise EReferencedObjectNotFound.Create(Value.JsonString);
end;

//function TJsonDeserializer.FromJson(Value: string): TObject;
//var
//  JsonValue: IJsonValue;
//begin
//  JsonValue := TJsonObject.ParseJSONValue(BytesOf(Value), 0);
//  try
//    Result := FromJson(JsonValue);
//  finally
//    JsonValue.Free;
//  end;
//end;

//function TCustomJsonDeserializer.FromJson<T>(Value: IJsonValue): T;
//begin
//  Result := T(InternalFromJson(Value, TypeInfo(T)));
//end;

function TCustomJsonDeserializer.GetOwnsEntities: boolean;
begin
  Result := FEntities.OwnsObjects;
end;

//function TJsonDeserializer.FromJson(Value: IJsonValue): TObject;
//begin
//  Result := FromJson<TObject>(Value);
//end;

//function TJsonDeserializer.FromJson<T>(Value: string): T;
//begin
//  Result := T(FromJson(Value));
//end;

function TCustomJsonDeserializer.FromJsonValue(JsonValue: IJsonValue;
  ResultClass: TClass): TObject;
begin
  FObjectMap.Clear;
  Result := JsonToObject(ResultClass, nil, JsonValue).AsObject;
end;

function TCustomJsonDeserializer.JsonToEntity(Value: IJsonValue): TObject;
var
  JObj: IJsonObject;
  TypeValue, RefValue: IJsonValue;
  Entity: TObject;
  ObjectClass: TClass;
  PropName: string;
  PropValue: IJsonValue;
  MemberInfo: TRttiOptimization;
  I: Integer;
begin
  if not (Value.ValueType = TJsonType.JObject) then
    Exit(nil);

  JObj := Value.ObjectValue;

  // Check if json object is a reference to another existing object
  RefValue := JObj.FindProp(FSettings.RefProperty);
  if RefValue <> nil then
    Exit(FindReferencedObject(RefValue));

  // Find the object class to be instantiateds
  ObjectClass := nil;
  TypeValue := JObj.FindProp(FSettings.TypeProperty);
  if TypeValue <> nil then
    ObjectClass := TranslateObjectClass(TypeValue);
  if ObjectClass = nil then
    raise EObjectClassNotSpecified.Create(JObj.JsonString);

  Entity := TObjectFactory.GetInstance.CreateInstance(ObjectClass);
  try
    for I := 0 to JObj.PropCount - 1 do
    begin
      PropName := JObj.PropName(I);
      PropValue := JObj.PropValue(I);
      if PropName = FSettings.TypeProperty then
        Continue;
      if PropName = FSettings.RefProperty then
        Continue;
      if PropName = FSettings.RefIdProperty then
      begin
        AddReferencedObject(Entity, PropValue);
        Continue;
      end;

      MemberInfo := FExplorer.GetOptimization(ObjectClass, PropName);
      if MemberInfo = nil then
        Continue;
      try
        // Actually in the following if condition, only the second part will be true. IsDynamic will be false
        // because the TRttiOptimization only returns IsDynamic when it is explicity created with dynamic parameters
        // IN this case, MemberInfo has info about the container itself (the TDynamicProperties object) so IsDynamic won't be true
        // We have to check for the membertype directly. We have added IsDynamic here just in case
        if MemberInfo.IsDynamic or (MemberInfo.MemberType.Handle = TypeInfo(TDynamicProperties)) then
          JsonToDynamicProperties(Entity, MemberInfo, PropValue.ObjectValue)
        else
          LoadJsonProperty(Entity, MemberInfo, PropValue);
      finally
        MemberInfo.Free;
      end;
    end;

    DoEntityCreated(Entity);
    Result := Entity;
  except
    Entity.Free;
    raise;
  end;
end;

//function TJsonDeserializer.JsonToDate(Value: IJsonValue): TDateTime;
//var
//  Info: TJsonValueInfo;
//begin
//  Info := ValueInfo(Value);
//  case Info.ValueType of
//    JFloat:
//      Result := Info.FloatValue;
//    JInteger:
//      Result := Info.IntValue;
//    JString:
//      Result := StrToDateTime(Value.Value, FSettings.FormatSettings);
//  else
//    Result := 0;
//  end;
//end;
//
function TCustomJsonDeserializer.JsonValueToBlobInfo(Value: IJsonValue): IBlobInfo;
var
  JObj: IJSONObject;
  BlobMark, KeyValue: IJsonValue;
  BlobKey: Variant;
  ClassValue, MemberValue: IJsonValue;
begin
  case Value.ValueType of
    TJsonType.JObject:
      begin
        JObj := Value.ObjectValue;

        ClassValue := JObj.FindProp(FSettings.ProxyClassProperty);
        if ClassValue = nil then
          Error;
        MemberValue := JObj.FindProp(FSettings.ProxyMemberProperty);
        if MemberValue = nil then
          Error;
        // Check if json object is our specific blob object
        BlobMark := JObj.FindProp(FSettings.BlobProperty);
        if BlobMark = nil then Error;
        BlobKey := Null;
        KeyValue := jObj.FindProp(FSettings.BlobKeyProperty);
        if KeyValue <> nil then
          BlobKey := JsonToVariant(KeyValue);
        Result := TJsonBlobInfo.Create(ClassValue.StringValue, MemberValue.StringValue, BlobKey);
      end;
  else
    Error;
  end;
end;

procedure TCustomJsonDeserializer.JsonToDynamicProperties(Entity: TObject; MemberInfo: TRttiOptimization;
  Value: IJsonObject);
var
  EntityClass: TClass;
  C: TColumn;
  PropName: string;
  PropValue: IJsonValue;
  I: integer;
begin
  EntityClass := Entity.ClassType;
  for I := 0 to Value.PropCount - 1 do
  begin
    PropName := Value.PropName(I);
    PropValue := Value.PropValue(I);
    C := FExplorer.FindColumnByPropertyName(EntityClass, PropName);
    if C = nil then
      EDynamicPropertyNotFound.Create(EntityClass, PropName);
    LoadJsonProperty(Entity, C.Optimization, PropValue);
  end;
end;

function TCustomJsonDeserializer.JsonToList(ListClass: TClass;
  List: TObject; JsonList: IJsonArray): TValue;
var
  ListIntf: IObjectList;
  I: Integer;
begin
  if List = nil then
    List := TObjectFactory.GetInstance.CreateInstance(ListClass);
  ListIntf := FExplorer.AsList(List);
  Assert(ListIntf <> nil, Format('%s is not a list class', [ListClass.ClassName]));
  ListIntf.Clear;
  for I := 0 to JsonList.Count - 1 do
    ListIntf.Add(JsonToEntity(JsonList.Get(I)));
  TValue.Make(@List, ListClass.ClassInfo, Result);
end;

function TCustomJsonDeserializer.JsonToNullable(Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue): TValue;
var
  Handled: boolean;
begin
  if Value.ValueType = TJsonType.JNull then
    Result := TValue.Empty
  else
    Result := JsonToSimpleMemberValue(Entity, MemberInfo, Value, Handled);
end;

function TCustomJsonDeserializer.JsonToObject(ObjectClass: TClass;
  CurrentObject: TObject; JsonValue: IJsonValue): TValue;
begin
  if FExplorer.IsList(ObjectClass) then
  begin
    case JsonValue.ValueType of
      TJsonType.JArray:
        Result := JsonToList(
          ObjectClass,
          CurrentObject,
          JsonValue.ArrayValue);
    else
      Error;
    end;
  end
  else
  begin
    case JsonValue.ValueType of
      TJsonType.JObject:
        Result := JsonToEntity(JsonValue.ObjectValue);
      TJsonType.JNull:
        Result := TValue.Empty;
    else
      Error;
    end;
  end;
end;

procedure TCustomJsonDeserializer.JsonToProxy(Entity: TObject; MemberInfo: TRttiOptimization; Value: IJsonValue);
var
  ProxyInfo: IProxyInfo;
  JObj: IJsonObject;
  TypeValue: IJsonValue;
begin
  case Value.ValueType of
    TJsonType.JNull:
      FExplorer.SetMemberValue(Entity, MemberInfo, TValue.Empty); // set proxy object to nil
    TJsonType.JObject:
      begin
        JObj := Value.ObjectValue;

        // Check if json object is a reference to another existing object
        TypeValue := JObj.FindProp(FSettings.ProxyProperty);
        if TypeValue = nil then
        begin
          // not a proxy, but a real object. Set the proxy object directly
          FExplorer.SetMemberValue(Entity, MemberInfo,
            JsonToObject(MemberInfo.RealType.AsInstance.MetaclassType, Entity, Value));
        end
        else
        begin
          ProxyInfo := JsonValueToProxyInfo(Value);
          FExplorer.SetProxyController(Entity, MemberInfo, TJsonProxyController.Create(FProxyLoader, ProxyInfo))
        end;
      end;
    TJsonType.JArray:
      begin
        // not a proxy, but an array. Try to convert it to a list (if the object member is a list)
        FExplorer.SetMemberValue(Entity, MemberInfo,
          JsonToObject(MemberInfo.RealType.AsInstance.MetaclassType,
            FExplorer.GetMemberValue(Entity, MemberInfo).AsObject, Value));
      end;
  else
    Error;
  end;
end;

function TCustomJsonDeserializer.JsonValueToProxyInfo(Value: IJsonValue): IProxyInfo;
var
  JObj: IJSONObject;
  TypeValue, KeyValue: IJsonValue;
  ProxyKey: Variant;
  ProxyType: TProxyType;
  ClassValue, MemberValue: IJsonValue;
begin
  case Value.ValueType of
    TJsonType.JObject:
      begin
        JObj := Value.ObjectValue;

        // Check if json object is a reference to another existing object
        TypeValue := JObj.FindProp(FSettings.ProxyProperty);
        if TypeValue = nil then
          Error
        else
        begin
          ClassValue := JObj.FindProp(FSettings.ProxyClassProperty);
          if ClassValue = nil then
            Error;
          MemberValue := JObj.FindProp(FSettings.ProxyMemberProperty);
          if MemberValue = nil then
            Error;
          ProxyType := TProxyType.Single;
          if SameText(TypeValue.StringValue, FSettings.ProxyListIndicator) then
            ProxyType := TProxyType.List;

          ProxyKey := Null;
          KeyValue := JObj.FindProp(FSettings.ProxyKeyProperty);
          if KeyValue <> nil then
            ProxyKey := JsonToVariant(KeyValue);
          Result := TJsonProxyInfo.Create(ProxyType, ClassValue.StringValue, MemberValue.StringValue, ProxyKey);
        end;
      end
  else
    Error;
  end;
end;

function TCustomJsonDeserializer.JsonToVariant(Value: IJsonValue): Variant;
begin
  case Value.ValueType of
    TJsonType.JString:
      Result := Value.StringValue;
    TJsonType.JInteger:
      Result := Value.IntValue;
    TJsonType.JFloat:
      Result := Value.FloatValue;
    TJsonType.JBoolean:
      Result := Value.BoolValue;
//    TJsonType.JObject:
//    TJsonType.JArray:
//    TJsonType.JNull:
  else
    Result := Variants.Null;
  end;
end;

procedure TCustomJsonDeserializer.LoadJsonProperty(Entity: TObject; MemberInfo: TRttiOptimization; PropValue: IJsonValue);
var
  Reverter: TJsonTypeReverter;
  MemberValue: TValue;
  Handled: boolean;
begin
  Handled := false;
  if FReverters.TryGetValue(MemberInfo.MemberType.Handle, Reverter) then
    MemberValue := Reverter(Self, Entity, MemberInfo, PropValue, Handled)
  else
  if MemberInfo.IsNullable then
    MemberValue := JsonToNullable(Entity, MemberInfo, PropValue)
  else
  if MemberInfo.IsProxy then
  begin
    JsonToProxy(Entity, MemberInfo, PropValue);
    Exit; // Proxy is set directly using higher level methods - equivalent to handled = true
  end
  else
  begin
    MemberValue := JsonToSimpleMemberValue(Entity, MemberInfo, PropValue, Handled);
  end;
  if not Handled then
    FExplorer.SetMemberValue(Entity, MemberInfo, MemberValue);
end;

procedure TCustomJsonDeserializer.SetOwnsEntities(const Value: boolean);
begin
  FEntities.OwnsObjects := Value;
end;

procedure TCustomJsonDeserializer.DoEntityCreated(Entity: TObject);
begin
  if Assigned(FOnEntityCreated) then
    FOnEntityCreated(Self, Entity);
  FEntities.Add(Entity);
end;

//procedure TJsonDeserializer.DoLoadProxy(ProxyInfo: IProxyInfo; var LoadedObject: TObject);
//begin
//  if Assigned(FOnLoadProxy) then
//    FOnLoadProxy(Self, ProxyInfo, LoadedObject);
//end;

function TCustomJsonDeserializer.JsonToSimpleMemberValue(Entity: TObject;
  MemberInfo: TRttiOptimization; Value: IJsonValue; var Handled: boolean): TValue;
var
  MemberValue: TValue;
  Reverter: TJsonTypeReverter;
  bytes: TBytes;
  {$IFNDEF NEXTGEN}
  asWide: WideString;
  {$ENDIF}
begin
  if FReverters.TryGetValue(MemberInfo.RealType.Handle, Reverter) then
  begin
    MemberValue := Reverter(Self, Entity, MemberInfo, Value, Handled);
    Exit(MemberValue.Cast(MemberInfo.RealType.Handle));
  end;

  case MemberInfo.RealType.TypeKind of
    tkInteger:
      case Value.ValueType of
        TJsonType.JString:  MemberValue := TValue.From<integer>(StrToInt(Value.StringValue));
        TJsonType.JInteger: MemberValue := TValue.From<integer>(integer(Value.IntValue));
      else
        Error;
      end;
    tkInt64:
      case Value.ValueType of
        TJsonType.JString:  MemberValue := TValue.From<Int64>(StrToInt(Value.StringValue));
        TJsonType.JInteger: MemberValue := TValue.From<Int64>(Value.IntValue);
      else
        Error;
      end;
    tkChar:
      case Value.ValueType of
        TJsonType.JString:
          if Length(Value.StringValue) >= 1 then
            MemberValue := TValue.From<Char>(Value.StringValue[1])
          else
            MemberValue := TValue.From<Char>(#0);
        TJsonType.JInteger:
          MemberValue := TValue.From<Char>(Chr(Value.IntValue));
      else
        Error;
      end;
    tkFloat:
      case Value.ValueType of
        TJsonType.JString:
          MemberValue := TValue.From<double>(StrToFloat(Value.StringValue));
        TJsonType.JInteger:
          MemberValue := TValue.From<double>(Value.IntValue);
        TJsonType.JFloat:
          MemberValue := TValue.From<double>(Value.FloatValue);
      else
        Error;
      end;

    tkString, tkLString, tkUString:
      case Value.ValueType of
        TJsonType.JString:
          MemberValue := TValue.From<string>(Value.StringValue);
        TJsonType.JInteger:
          MemberValue := TValue.From<string>(IntToStr(Value.IntValue));
        TJsonType.JFloat:
          MemberValue := TValue.From<string>(FloatToStr(Value.FloatValue));
        TJsonType.JBoolean:
          if Value.BoolValue then
            MemberValue := TValue.From<string>('true')
          else
            MemberValue := TValue.From<string>('false');
      else
        Error;
      end;

    // special treatment for widestring (search for Widestring in Aurelius.Mapping.Explorer)
    {$IFNDEF NEXTGEN}
    tkWString:
      begin
        case Value.ValueType of
          TJsonType.JString:
            asWide := WideString(Value.StringValue);
          TJsonType.JInteger:
            asWide := WideString(IntToStr(Value.IntValue));
          TJsonType.JFloat:
            asWide := WideString(FloatToStr(Value.FloatValue));
          TJsonType.JBoolean:
            if Value.BoolValue then
              asWide := WideString('true')
            else
              asWide := WideString('false');
        else
          Error;
        end;
        TValue.Make(@asWide, MemberInfo.RealType.Handle, MemberValue);
      end;
    {$ENDIF}

    tkWChar:
      case Value.ValueType of
        TJsonType.JString:
          if Length(Value.StringValue) >= 1 then
            MemberValue := TValue.From<WideChar>(Value.StringValue[1])
          else
            MemberValue := TValue.From<WideChar>(#0);
        TJsonType.JInteger:
          MemberValue := TValue.From<WideChar>(Chr(Value.IntValue));
      else
        Error;
      end;

    tkEnumeration:
      case Value.ValueType of
        TJsonType.JString:
          MemberValue := TValue.FromOrdinal(MemberInfo.MemberType.Handle, GetEnumValue(MemberInfo.MemberType.Handle, Value.StringValue));
        TJsonType.JInteger:
          MemberValue := TValue.FromOrdinal(MemberInfo.MemberType.Handle, Value.IntValue);
      else
        Error;
      end;

    tkClass:
      begin
        MemberValue := JsonToObject(
          MemberInfo.RealType.AsInstance.MetaclassType,
          FExplorer.GetMemberValue(Entity, MemberInfo).AsObject,
          Value);
      end;

    tkVariant:
      MemberValue := TValue.FromVariant(JsonToVariant(Value));

    tkDynArray:
      begin
        SetLength(bytes, 0);
        case Value.ValueType of
          TJsonType.JString:
            bytes := TUtils.Decode64(Value.StringValue);
          TJsonType.JNull:
            SetLength(bytes, 0);
        else
          Error;
        end;
        TValue.Make(@bytes, MemberInfo.RealType.Handle, MemberValue);
      end
  else
    raise EMemberTypeNotSupported.Create(MemberInfo);
//          // Types that might be supported some day
//          tkRecord:
//          tkArray:
//          tkDynArray:
//
//          // Types that probably will never be supported
//          tkMethod:
//          tkInterface:
//          tkClassRef:
//          tkPointer:
//          tkProcedure:
//          tkUnknown:
//          tkSet:
  end;

  // Force the exact type of the member info type
  Result := MemberValue.Cast(MemberInfo.RealType.Handle);
end;

function TCustomJsonDeserializer.TranslateObjectClass(TypeName: IJsonValue): TClass;
var
  RttiType: TRttiType;
begin
  Result := nil;
  RttiType := FContext.FindType(TypeName.StringValue);
  if (RttiType <> nil) and RttiType.IsInstance then
    Result := RttiType.AsInstance.MetaclassType;
end;

{ TJsonProxyController }

constructor TCustomJsonDeserializer.TJsonProxyController.Create(AProxyLoader: IJsonProxyLoader; AProxyInfo: IProxyInfo);
begin
  FProxyLoader := AProxyLoader;
  FProxyInfo := AProxyInfo;
end;

function TCustomJsonDeserializer.TJsonProxyController.ProxyInfo: IProxyInfo;
begin
  Result := FProxyInfo;
end;

function TCustomJsonDeserializer.TJsonProxyController.LoadProxyValue: TObject;
begin
  Result := nil;
  if FProxyLoader <> nil then
    Result := FProxyLoader.LoadProxyValue(FProxyInfo);
end;

{ TJsonDeserializer.TJsonProxyInfo }

function TCustomJsonDeserializer.TJsonProxyInfo.ClassName: string;
begin
  Result := FClassName;
end;

constructor TCustomJsonDeserializer.TJsonProxyInfo.Create(AProxyType: TProxyType; AClassName, AMemberName: string; AKey: Variant);
begin
  FProxyType := AProxyType;
  FClassName := AClassName;
  FMemberName := AMemberName;
  FKey := AKey;
end;

function TCustomJsonDeserializer.TJsonProxyInfo.Key: Variant;
begin
  Result := FKey;
end;

function TCustomJsonDeserializer.TJsonProxyInfo.MemberName: string;
begin
  Result := FMemberName;
end;

function TCustomJsonDeserializer.TJsonProxyInfo.ProxyType: TProxyType;
begin
  Result := FProxyType;
end;

{ TJsonProxyLoader }

constructor TJsonProxyLoader.Create(ALoad: TJsonProxyLoadFunction);
begin
  FLoad := ALoad;
end;

function TJsonProxyLoader.LoadProxyValue(ProxyInfo: IProxyInfo): TObject;
begin
  Result := FLoad(ProxyInfo);
end;

{ TJsonDeserializer.TJsonBlobInfo }

function TCustomJsonDeserializer.TJsonBlobInfo.ClassName: string;
begin
  Result := FClassName;
end;

constructor TCustomJsonDeserializer.TJsonBlobInfo.Create(AClassName, AMemberName: string; AKey: Variant);
begin
  FClassName := AClassName;
  FMemberName := AMemberName;
  FKey := AKey;
end;

function TCustomJsonDeserializer.TJsonBlobInfo.Key: Variant;
begin
  Result := FKey;
end;

function TCustomJsonDeserializer.TJsonBlobInfo.MemberName: string;
begin
  Result := FMemberName;
end;

{ TJsonDeserializer.TJsonBlobController }

function TCustomJsonDeserializer.TJsonBlobController.BlobInfo: IBlobInfo;
begin
  Result := FBlobInfo;
end;

constructor TCustomJsonDeserializer.TJsonBlobController.Create(ABlobLoader: IJsonBlobLoader; ABlobInfo: IBlobInfo);
begin
  FBlobLoader := ABlobLoader;
  FBlobInfo := ABlobInfo;
end;

function TCustomJsonDeserializer.TJsonBlobController.ReadBlob: TArray<byte>;
begin
  if FBlobLoader <> nil then
    Result := FBlobLoader.ReadBlob(FBlobInfo)
  else
    SetLength(Result, 0);
end;

{ TJsonBlobLoader }

constructor TJsonBlobLoader.Create(ALoad: TJsonBlobLoadFunction);
begin
  FLoad := ALoad;
end;

function TJsonBlobLoader.ReadBlob(BlobInfo: IBlobInfo): TArray<byte>;
begin
  Result := FLoad(BlobInfo);
end;

{ TTypedJsonDeserializer<T> }

function TTypedJsonDeserializer<T>.BlobInfoFromJson(JsonValue: T): IBlobInfo;
begin
  Result := JsonValueToBlobInfo(Wrap(JsonValue));
end;

function TTypedJsonDeserializer<T>.FromJson(JsonValue: T; ResultClass: TClass): TObject;
begin
  Result := FromJsonValue(Wrap(JsonValue), ResultClass);
end;

function TTypedJsonDeserializer<T>.FromJson<E>(JsonValue: T): E;
begin
  Result := E(FromJson(JsonValue, TClass(E)));
end;

function TTypedJsonDeserializer<T>.ProxyInfoFromJson(JsonValue: T): IProxyInfo;
begin
  Result := JsonValueToProxyInfo(Wrap(JsonValue));
end;

end.
