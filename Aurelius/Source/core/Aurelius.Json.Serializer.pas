unit Aurelius.Json.Serializer;

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
  TCustomJsonSerializer = class;
  TJsonTypeConverter = reference to function(Serializer: TCustomJsonSerializer;
    MemberInfo: TRttiOptimization; Value: TValue): IJsonValue;

{$IFNDEF DELPHIXE2_LVL}
  IntPtr = integer;
{$ENDIF}

  TCustomJsonSerializer = class abstract
  strict private
    FExplorer: TMappingExplorer;
    FConverters: TDictionary<PTypeInfo, TJsonTypeConverter>;
    FSettings: TJsonSettings;
    FObjectMap: TDictionary<IntPtr, integer>;
    FCurId: integer;
    FJsonFactory: IJsonFactory;
    function FullTypeName(Entity: TObject): string;
    function VariantToJson(Value: Variant): IJsonValue;
    function DateToJson(Value: TDateTime): IJsonValue;
    function ListToJson(List: IObjectList): IJsonValue;
    function ReferenceToJson(RefId: integer): IJsonValue;
    function NullableToJson(MemberInfo: TRttiOptimization; Nullable: TValue): IJsonValue;
    function ProxyToJson(MemberInfo: TRttiOptimization; Proxy: TValue): IJsonValue;
    function ProxyInfoToJson(ProxyInfo: IProxyInfo): IJsonValue;
    function BlobInfoToJson(BlobInfo: IBlobInfo): IJsonValue;
    function DynamicPropsToJson(MemberInfo: TRttiOptimization; Value: TValue): IJsonValue;
    function SimpleMemberValueToJson(MemberInfo: TRttiOptimization; Value: TValue): IJsonValue;
    procedure AddObjectProperty(JObj: IJsonObject; MemberInfo: TRttiOptimization; Value: TValue);
    function EntityToJson(Entity: TObject): IJsonValue;
    function ObjectToJson(Obj: TObject): IJsonValue;
  protected
    function CreateJsonFactory: IJsonFactory; virtual; abstract;
    function ToJsonValue(Entity: TObject): IJsonValue; overload;
    function ToJsonValue(ProxyInfo: IProxyInfo): IJsonValue; overload;
    function ToJsonValue(BlobInfo: IBlobInfo): IJsonValue; overload;
  protected
    property JsonFactory: IJsonFactory read FJsonFactory;
  public
    // ASettings parameter is destroyed by the serializer
    constructor Create; overload;
    constructor Create(AExplorer: TMappingExplorer); overload;
    constructor Create(AExplorer: TMappingExplorer; ASettings: TJsonSettings); overload;
    destructor Destroy; override;
  end;

  TTypedJsonSerializer<T> = class(TCustomJsonSerializer)
  public
    class function Unwrap(JsonValue: IJsonValue): T; virtual; abstract;
    function ToJson(Entity: TObject): T; overload;
    function ToJson(ProxyInfo: IProxyInfo): T; overload;
    function ToJson(BlobInfo: IBlobInfo): T; overload;
  end;

implementation

uses
  Variants, FmtBcd,
  Aurelius.Global.Utils,
  Aurelius.Mapping.Metadata,
  Aurelius.Json.Exceptions,
  Aurelius.Types.DynamicProperties;

{ TJsonEntitySerializer }

type
  TInternalExplorer = class(TMappingExplorer)
  end;

procedure TCustomJsonSerializer.AddObjectProperty(JObj: IJsonObject; MemberInfo: TRttiOptimization;
  Value: TValue);
var
  Converter: TJsonTypeConverter;
  PairValue: IJsonValue;
begin
  if FConverters.TryGetValue(MemberInfo.MemberType.Handle, Converter) then
    PairValue := Converter(Self, MemberInfo, Value)
  else
  if MemberInfo.IsNullable then
    PairValue := NullableToJson(MemberInfo, Value)
  else
  if MemberInfo.IsProxy then
    PairValue := ProxyToJson(MemberInfo, Value)
  else
  begin
    PairValue := SimpleMemberValueToJson(MemberInfo, Value);
  end;
  // Deal with lists
  JObj.AddProp(MemberInfo.MemberName, PairValue);
end;

constructor TCustomJsonSerializer.Create(AExplorer: TMappingExplorer);
begin
  Create(AExplorer, TJsonSettings.Create);
end;

function TCustomJsonSerializer.BlobInfoToJson(BlobInfo: IBlobInfo): IJsonValue;
var
  JObj: IJSONObject;
begin
  JObj := JsonFactory.CreateObject;
  try
    JObj.AddProp(FSettings.BlobProperty, JsonFactory.CreateTrue);
    JObj.AddProp(FSettings.BlobKeyProperty, VariantToJson(BlobInfo.Key));
    JObj.AddProp(FSettings.BlobClassProperty, JsonFactory.CreateString(BlobInfo.ClassName));
    JObj.AddProp(FSettings.BlobMemberProperty, JsonFactory.CreateString(BlobInfo.MemberName));
  except
    JObj.Release;
    raise;
  end;
  Result := JObj;
end;

constructor TCustomJsonSerializer.Create(AExplorer: TMappingExplorer; ASettings: TJsonSettings);
var
  DateConverter: TJsonTypeConverter;
begin
  FExplorer := AExplorer;
  FSettings := ASettings;
  FConverters := TDictionary<PTypeInfo, TJsonTypeConverter>.Create;
  FObjectMap := TDictionary<IntPtr, integer>.Create;
  FCurId := 0;
  FJsonFactory := CreateJsonFactory;

  DateConverter :=
    function(Serializer: TCustomJsonSerializer; MemberInfo: TRttiOptimization; Value: TValue): IJsonValue
    begin
      Result := DateToJson(Value.AsType<TDateTime>);
    end;

  FConverters.Add(TypeInfo(TDateTime), DateConverter);
  FConverters.Add(TypeInfo(TDate), DateConverter);
  FConverters.Add(TypeInfo(TTime), DateConverter);

  FConverters.Add(TypeInfo(Boolean),
    function(Serializer: TCustomJsonSerializer; MemberInfo: TRttiOptimization; Value: TValue): IJsonValue
    begin
      if Value.AsBoolean then
        Result := Serializer.JsonFactory.CreateTrue
      else
        Result := Serializer.JsonFactory.CreateFalse;
    end
  );

  FConverters.Add(TypeInfo(TGUID),
    function(Serializer: TCustomJsonSerializer; MemberInfo: TRttiOptimization; Value: TValue): IJsonValue
    var
      Guid: TGuid;
    begin
      Guid := Value.AsType<TGuid>;
      if TUtils.IsNullGuid(Guid) then
        Result := Serializer.JsonFactory.CreateNull
      else
        Result := Serializer.JsonFactory.CreateString(GUIDToString(Guid));
    end
  );

  FConverters.Add(TypeInfo(TBlob),
    function(Serializer: TCustomJsonSerializer; MemberInfo: TRttiOptimization; Value: TValue): IJsonValue
    var
      Blob: TBlob;
      IntExplorer: TInternalExplorer;
    begin
      Blob := Value.AsType<TBlob>;
      if Blob.Loaded then
      begin
        if Blob.IsNull then
          Result := Serializer.JsonFactory.CreateNull
        else
          Result := Serializer.JsonFactory.CreateString(TUtils.Encode64(Blob.AsBytes));
      end
      else
      begin
        // blob with proxy
        IntExplorer := TInternalExplorer(FExplorer);
        Result := BlobInfoToJson(IntExplorer.GetBlobInfo(MemberInfo, Value));
      end;
    end
  );
end;

function TCustomJsonSerializer.DateToJson(Value: TDateTime): IJsonValue;
begin
  Result := JsonFactory.CreateString(TUtils.DateTimeToISO(Value));
end;

destructor TCustomJsonSerializer.Destroy;
begin
  FConverters.Free;
  FObjectMap.Free;
  FSettings.Free;
  inherited;
end;

function TCustomJsonSerializer.DynamicPropsToJson(MemberInfo: TRttiOptimization; Value: TValue): IJsonValue;
var
  Props: TDynamicProperties;
  Prop: TPair<string, TValue>;
  C: TColumn;
  JObj: IJsonObject;
  EntityClass: TClass;
begin
  JObj := JsonFactory.CreateObject;
  try
    EntityClass := MemberInfo.MemberRef.Parent.AsInstance.MetaclassType;
    Props := Value.AsType<TDynamicProperties>;
    for Prop in Props.Props do
    begin
      C := FExplorer.FindColumnByPropertyName(EntityClass, Prop.Key);
      AddObjectProperty(JObj, C.Optimization, Prop.Value);
    end;
  except
    JObj.Release;
    raise;
  end;
  Result := JObj;
end;

function TCustomJsonSerializer.FullTypeName(Entity: TObject): string;
begin
  Result := Entity.UnitName + FSettings.TypeSeparator + Entity.ClassName;
end;

function TCustomJsonSerializer.ListToJson(List: IObjectList): IJsonValue;
var
  JList: IJsonArray;
  I: integer;
begin
  JList := JsonFactory.CreateArray;
  try
    if List <> nil then
      for I := 0 to List.Count - 1 do
        JList.Add(ObjectToJson(List.Item(I)));
  except
    JList.Release;
    raise;
  end;
  Result := JList;
end;

function TCustomJsonSerializer.NullableToJson(MemberInfo: TRttiOptimization; Nullable: TValue): IJsonValue;
var
  RealValue: TValue;
begin
  RealValue := TInternalExplorer(FExplorer).GetNullableValue(MemberInfo, Nullable);
  if RealValue.IsEmpty then
    Result := JsonFactory.CreateNull
  else
    Result := SimpleMemberValueToJson(MemberInfo, RealValue);
end;

function TCustomJsonSerializer.ObjectToJson(Obj: TObject): IJsonValue;
begin
  if (Obj <> nil) and FExplorer.IsList(Obj.ClassType) then
    Result := ListToJson(FExplorer.AsList(Obj))
  else
    Result := EntityToJson(Obj);
end;

function TCustomJsonSerializer.ProxyInfoToJson(ProxyInfo: IProxyInfo): IJsonValue;
var
  JObj: IJSONObject;
begin
  JObj := JsonFactory.CreateObject;
  try
    if ProxyInfo.ProxyType = TProxyType.List then
      JObj.AddProp(FSettings.ProxyProperty, JsonFactory.CreateString(FSettings.ProxyListIndicator))
    else
      JObj.AddProp(FSettings.ProxyProperty, JsonFactory.CreateString(FSettings.ProxySingleIndicator));
    JObj.AddProp(FSettings.ProxyKeyProperty, VariantToJson(ProxyInfo.Key));
    JObj.AddProp(FSettings.ProxyClassProperty, JsonFactory.CreateString(ProxyInfo.ClassName));
    JObj.AddProp(FSettings.ProxyMemberProperty, JsonFactory.CreateString(ProxyInfo.MemberName));
  except
    JObj.Release;
    raise;
  end;
  Result := JObj;
end;

function TCustomJsonSerializer.ProxyToJson(MemberInfo: TRttiOptimization; Proxy: TValue): IJsonValue;
var
  IntExplorer: TInternalExplorer;
  ProxyInfo: IProxyInfo;
begin
  IntExplorer := TInternalExplorer(FExplorer);
  ProxyInfo := IntExplorer.GetProxyInfo(MemberInfo, Proxy);
  if IntExplorer.IsProxyLoaded(MemberInfo, Proxy) or (ProxyInfo = nil) then
    Exit(ObjectToJson(IntExplorer.GetProxyValue(MemberInfo, Proxy).AsObject));
  Result := ProxyInfoToJson(ProxyInfo);
end;

function TCustomJsonSerializer.SimpleMemberValueToJson(MemberInfo: TRttiOptimization; Value: TValue): IJsonValue;
var
  Converter: TJsonTypeConverter;
begin
  // Check if there is a converter for the real type as well
  if FConverters.TryGetValue(MemberInfo.RealType.Handle, Converter) then
    Exit(Converter(Self, MemberInfo, Value));

  case MemberInfo.RealType.TypeKind of
    tkInteger:
      Result := JsonFactory.CreateNumber(Value.AsInteger);
    tkInt64:
      Result := JsonFactory.CreateNumber(Value.AsInt64);
    tkChar:
      if Value.AsType<Char> = #0 then
        Result := JsonFactory.CreateString('')
      else
        Result := JsonFactory.CreateString(Value.AsString);
    tkFloat:
      Result := JsonFactory.CreateNumber(Value.AsExtended);
    tkString, tkLString, tkWString, tkUString:
      Result := JsonFactory.CreateString(Value.AsString);
    tkWChar:
      if Value.AsType<WideChar> = #0 then
        Result := JsonFactory.CreateString('')
      else
        Result := JsonFactory.CreateString(Value.AsString);
    tkEnumeration:
      if FSettings.EnumMode = TJsonSettings.TEnumMode.Ordinal then
        Result := JsonFactory.CreateNumber(Value.AsOrdinal)
      else
        Result := JsonFactory.CreateString(GetEnumName(MemberInfo.RealType.Handle, Value.AsOrdinal));
    tkClass:
      begin
        Result := ObjectToJson(Value.AsObject);
      end;
    tkVariant:
      Result := VariantToJson(Value.AsVariant);

    tkDynArray:
      begin
        // We are treating dynarray as blob here. That's what Aurelius is doing for now.
        // Search for IsDynamicArray in Aurelius.Mapping.Explorer
        if Value.IsEmpty then
          Result := JsonFactory.CreateNull
        else
        begin
          {$IFDEF DELPHIXE_LVL}
          Result := JsonFactory.CreateString(TUtils.Encode64(Value.AsType<TBytes>));
          {$ELSE}
          Result := JsonFactory.CreateString(TUtils.Encode64(TBytes(Value.AsType<TArray<byte>>)));
          {$ENDIF}
        end;
      end;
  else
    raise EMemberTypeNotSupported.Create(MemberInfo);
//          // Types that might be supported some day
//          tkRecord:
//          tkArray:
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
end;

function TCustomJsonSerializer.ToJsonValue(ProxyInfo: IProxyInfo): IJsonValue;
begin
  Result := ProxyInfoToJson(ProxyInfo);
end;

function TCustomJsonSerializer.ToJsonValue(BlobInfo: IBlobInfo): IJsonValue;
begin
  Result := BlobInfoToJson(BlobInfo);
end;

//function TCustomJsonSerializer.ToJson(Entity: TObject): IJsonValue;
//begin
////  Result := ToJson
//end;

function TCustomJsonSerializer.ToJsonValue(Entity: TObject): IJsonValue;
begin
  FCurId := 0;
  FObjectMap.Clear;
  Result := ObjectToJson(Entity);
end;

constructor TCustomJsonSerializer.Create;
begin
  Create(TMappingExplorer.DefaultInstance);
end;

function TCustomJsonSerializer.EntityToJson(Entity: TObject): IJsonValue;
var
  JObj: IJSONObject;
  EntityMembers: TDictionary<TRttiOptimization, TValue>;
  EntityMember: TPair<TRttiOptimization, TValue>;
  RefId: integer;
begin
  if Entity = nil then
    Exit(JsonFactory.CreateNull);

  // Check if object has been already serialized, and if it did, just return a reference to it
  if FObjectMap.TryGetValue(IntPtr(Entity), RefId) then
    Exit(ReferenceToJson(RefId));

  Inc(FCurId);
  FObjectMap.Add(IntPtr(Entity), FCurId);

  JObj := JsonFactory.CreateObject;
  try
    // Serialize type
    JObj.AddProp(FSettings.TypeProperty, JsonFactory.CreateString(FullTypeName(Entity)));
    JObj.AddProp(FSettings.RefIdProperty, JsonFactory.CreateNumber(FCurId));

    // Serialize object state
    EntityMembers := FExplorer.GetMemberValues(Entity);
    try
      for EntityMember in EntityMembers do
      begin
        if EntityMember.Key.IsDynamic then
          JObj.AddProp(
            EntityMember.Key.MemberRef.Name,
            DynamicPropsToJson(EntityMember.Key, EntityMember.Value)
          )
        else
          AddObjectProperty(JObj, EntityMember.Key, EntityMember.Value);
      end;
    finally
      EntityMembers.Free;
    end;
  except
    JObj.Release;
    raise;
  end;
  Result := JObj;
end;

function TCustomJsonSerializer.ReferenceToJson(RefId: integer): IJsonValue;
var
  JObj: IJsonObject;
begin
  JObj := JsonFactory.CreateObject;
  JObj.AddProp(FSettings.RefProperty, JsonFactory.CreateNumber(RefId));
  Result := JObj;
end;

function TCustomJsonSerializer.VariantToJson(Value: Variant): IJsonValue;
begin
  case VarType(Value) of
    varEmpty, varNull:
      Result := JsonFactory.CreateNull;
    varBoolean:
      if TVarData(Value).VBoolean then
        Result := JsonFactory.CreateTrue
      else
        Result := JsonFactory.CreateFalse;
    varShortInt:
      Result := JsonFactory.CreateNumber(TVarData(Value).VShortInt);
    varSmallint:
      Result := JsonFactory.CreateNumber(TVarData(Value).VSmallInt);
    varInteger:
      Result := JsonFactory.CreateNumber(TVarData(Value).VInteger);
    varSingle:
      Result := JsonFactory.CreateNumber(TVarData(Value).VSingle);
    varDouble:
      Result := JsonFactory.CreateNumber(TVarData(Value).VDouble);
    varCurrency:
      Result := JsonFactory.CreateNumber(TVarData(Value).VCurrency);
    varDate:
      Result := DateToJson(TVarData(Value).VDate);
    varOleStr:
      Result := JsonFactory.CreateString(string(TVarData(Value).VOleStr));
    varError:
      Result := JsonFactory.CreateNumber(TVarData(Value).VError);
    varByte:
      Result := JsonFactory.CreateNumber(TVarData(Value).VByte);
    varWord:
      Result := JsonFactory.CreateNumber(TVarData(Value).VWord);
    varLongWord:
      Result := JsonFactory.CreateNumber(TVarData(Value).VLongWord);
    varInt64:
      Result := JsonFactory.CreateNumber(TVarData(Value).VInt64);
    varUInt64:
      Result := JsonFactory.CreateNumber(TVarData(Value).VUInt64);
    varString:
      {$IFNDEF NEXTGEN}
      Result := JsonFactory.CreateString(string(AnsiString(TVarData(Value).VString)));
      {$ELSE}
      Result := JsonFactory.CreateString(string(TVarData(Value).VString));
      {$ENDIF}
    varUString:
      Result := JsonFactory.CreateString(UnicodeString(TVarData(Value).VUString));
  else
    if VarIsFMTBcd(Value) then
      Result := JsonFactory.CreateNumber(BcdToDouble(VarToBcd(Value)))
    else
      raise EVariantTypeNotSupported.Create(Value);
  end;
end;

{ TTypedJsonSerializer<T> }

function TTypedJsonSerializer<T>.ToJson(Entity: TObject): T;
begin
  Result := Unwrap(ToJsonValue(Entity));
end;

function TTypedJsonSerializer<T>.ToJson(ProxyInfo: IProxyInfo): T;
begin
  Result := Unwrap(ToJsonValue(ProxyInfo));
end;

function TTypedJsonSerializer<T>.ToJson(BlobInfo: IBlobInfo): T;
begin
  Result := Unwrap(ToJsonValue(BlobInfo));
end;

end.

