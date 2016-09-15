unit Aurelius.Json.DataSnap;

{$I Aurelius.Inc}

interface
uses
  TypInfo, DbxJson,
  Aurelius.Json.Interfaces,
  Aurelius.Json.Serializer,
  Aurelius.Json.Deserializer;

type
  TDataSnapJsonSerializer = class(TTypedJsonSerializer<TJsonValue>)
  protected
    function CreateJsonFactory: IJsonFactory; override;
  public
    class function Unwrap(JsonValue: IJsonValue): TJsonValue; override;
  end;

  TDataSnapJsonDeserializer = class(TTypedJsonDeserializer<TJsonValue>)
  public
    class function Wrap(Value: TJsonValue): IJsonValue; override;
  end;

  TJsonProxyLoader = Aurelius.Json.Deserializer.TJsonProxyLoader;
  TJsonBlobLoader = Aurelius.Json.Deserializer.TJsonBlobLoader;

implementation
uses
  SysUtils;

type
  TDataSnapJsonFactory = class(TInterfacedObject, IJsonFactory)
  private
    function CreateTrue: IJsonValue;
    function CreateFalse: IJsonValue;
    function CreateNull: IJsonValue;
    function CreateObject: IJsonObject;
    function CreateArray: IJsonArray;
    function CreateNumber(D: double): IJsonValue;
    function CreateString(const S: string): IJsonValue;
  end;

  TJsonValueWrapper = class(TInterfacedObject, IJsonValue)
  strict private
    FValue: TJsonValue;
    function ValueType: TJsonType;
    function StringValue: string;
    function BoolValue: boolean;
    function ObjectValue: IJsonObject;
    function ArrayValue: IJsonArray;
    function IntValue: Int64;
    function FloatValue: double;
    function Wrapped: TObject;
    function JsonString: string;
    procedure Release;
  strict protected
    property JsonValue: TJsonValue read FValue;
  public
    constructor Create(AValue: TJsonValue);
  end;

{ TDataSnapJsonSerializer }

function TDataSnapJsonSerializer.CreateJsonFactory: IJsonFactory;
begin
  Result := TDataSnapJsonFactory.Create;
end;

class function TDataSnapJsonSerializer.Unwrap(JsonValue: IJsonValue): TJsonValue;
begin
  Result := TJsonValue(JsonValue.Wrapped);
end;

{ TDataSnapJsonDeserializer }

class function TDataSnapJsonDeserializer.Wrap(Value: TJsonValue): IJsonValue;
begin
  Result := TJsonValueWrapper.Create(Value);
end;

type
  TJsonArrayWrapper = class(TJsonValueWrapper, IJsonArray)
  strict private
    procedure Add(Value: IJsonValue);
    function Get(I: integer): IJsonValue;
    function Count: integer;
    function JsonArray: TJsonArray;
  end;

  TJsonObjectWrapper = class(TJsonValueWrapper, IJsonObject)
  strict private
    procedure AddProp(Name: string; Value: IJsonValue);
    function FindProp(Name: string): IJsonValue;
    function PropName(I: integer): string;
    function PropValue(I: integer): IJsonValue;
    function PropCount: integer;
    function JsonObject: TJsonObject;
  end;

{ TDataSnapJsonFactory }

function TDataSnapJsonFactory.CreateArray: IJsonArray;
begin
  Result := TJsonArrayWrapper.Create(TJsonArray.Create);
end;

function TDataSnapJsonFactory.CreateFalse: IJsonValue;
begin
  Result := TJsonValueWrapper.Create(TJsonFalse.Create);
end;

function TDataSnapJsonFactory.CreateNull: IJsonValue;
begin
  Result := TJsonValueWrapper.Create(TJsonNull.Create);
end;

function TDataSnapJsonFactory.CreateNumber(D: double): IJsonValue;
begin
  Result := TJsonValueWrapper.Create(TJsonNumber.Create(D));
end;

function TDataSnapJsonFactory.CreateObject: IJsonObject;
begin
  Result := TJsonObjectWrapper.Create(TJsonObject.Create);
end;

function TDataSnapJsonFactory.CreateString(const S: string): IJsonValue;
begin
  Result := TJsonValueWrapper.Create(TJsonString.Create(S));
end;

function TDataSnapJsonFactory.CreateTrue: IJsonValue;
begin
  Result := TJsonValueWrapper.Create(TJsonTrue.Create);
end;

{ TJsonValueWrapper }

function TJsonValueWrapper.ArrayValue: IJsonArray;
begin
  Assert(ValueType = TJsonType.JArray);
  Result := TJsonArrayWrapper.Create(FValue);
end;

function TJsonValueWrapper.BoolValue: boolean;
begin
  Assert(ValueType = TJsonType.JBoolean);
  Result := FValue is TJSONTrue;
end;

constructor TJsonValueWrapper.Create(AValue: TJsonValue);
begin
  FValue := AValue;
end;

function TJsonValueWrapper.FloatValue: double;
begin
  Assert(ValueType = TJsonType.JFloat);
  Result := TJsonNumber(FValue).AsDouble;
end;

function TJsonValueWrapper.IntValue: Int64;
begin
  Assert(ValueType = TJsonType.JInteger);
  Result := StrToInt64(TJsonNumber(FValue).ToString);
end;

function TJsonValueWrapper.JsonString: string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, FValue.EstimatedByteSize);
  SetLength(Bytes, FValue.ToBytes(Bytes, 0));
  Result := StringOf(Bytes);
end;

function TJsonValueWrapper.ObjectValue: IJsonObject;
begin
  Assert(ValueType = TJsonType.JObject);
  Result := TJsonObjectWrapper.Create(FValue);
end;

procedure TJsonValueWrapper.Release;
begin
  FValue.Free;
  FValue := nil;
end;

function TJsonValueWrapper.StringValue: string;
begin
  Result := FValue.Value;
end;

function TJsonValueWrapper.ValueType: TJsonType;
var
  I: Int64;
begin
  if FValue.Null then
    Result := TJsonType.JNull
  else
  if FValue is TJsonNumber then
  begin
    if TryStrToInt64(TJsonNumber(FValue).ToString, I) then
      Result := TJsonType.JInteger
    else
      Result := TJsonType.JFloat;
  end
  else
  if FValue is TJsonString then
    Result := TJsonType.JString
  else
  if FValue is TJsonTrue then
    Result := TJsonType.JBoolean
  else
  if FValue is TJsonFalse then
    Result := TJsonType.JBoolean
  else
  if FValue is TJsonObject then
    Result := TJsonType.JObject
  else
  if FValue is TJSONArray then
    Result := TJsonType.JArray
  else
    Result := TJsonType.JNull;
end;

function TJsonValueWrapper.Wrapped: TObject;
begin
  Result := FValue;
end;

{ TJsonArrayWrapper }

procedure TJsonArrayWrapper.Add(Value: IJsonValue);
begin
  JsonArray.AddElement(TJsonValue(Value.Wrapped));
end;

function TJsonArrayWrapper.Count: integer;
begin
  Result := JsonArray.Size;
end;

function TJsonArrayWrapper.Get(I: integer): IJsonValue;
begin
  Result := TJsonValueWrapper.Create(JsonArray.Get(I));
end;

function TJsonArrayWrapper.JsonArray: TJsonArray;
begin
  Result := TJsonArray(JsonValue);
end;

{ TJsonObjectWrapper }

procedure TJsonObjectWrapper.AddProp(Name: string; Value: IJsonValue);
begin
  JsonObject.AddPair(Name, TJsonValue(Value.Wrapped));
end;

function TJsonObjectWrapper.FindProp(Name: string): IJsonValue;
var
  Pair: TJsonPair;
  {$IFNDEF DELPHIXE_LVL}
  I: integer;
  {$ENDIF}
begin
  {$IFDEF DELPHIXE_LVL}
  Pair := JsonObject.Get(Name);
  {$ELSE}
  Pair := nil;
  for I := 0 to JsonObject.Size - 1 do
    if JsonObject.Get(I).JsonString.Value = Name then
      Pair := JsonObject.Get(I);
  {$ENDIF}
  if Pair = nil then
    Result := nil
  else
    Result := TJsonValueWrapper.Create(Pair.JsonValue);
end;

function TJsonObjectWrapper.JsonObject: TJsonObject;
begin
  Result := TJsonObject(JsonValue);
end;

function TJsonObjectWrapper.PropCount: integer;
begin
  Result := JsonObject.Size;
end;

function TJsonObjectWrapper.PropName(I: integer): string;
begin
  Result := JsonObject.Get(I).JsonString.Value;
end;

function TJsonObjectWrapper.PropValue(I: integer): IJsonValue;
begin
  Result := TJsonValueWrapper.Create(JsonObject.Get(I).JsonValue);
end;

end.

