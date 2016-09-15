unit Aurelius.Json.SuperObject;

{$I Aurelius.Inc}

interface
uses
  TypInfo,
  SuperObject,
  Aurelius.Json.Interfaces,
  Aurelius.Json.Serializer,
  Aurelius.Json.Deserializer;

type
  TSuperObjectJsonSerializer = class(TTypedJsonSerializer<ISuperObject>)
  protected
    function CreateJsonFactory: IJsonFactory; override;
  public
    class function Unwrap(JsonValue: IJsonValue): ISuperObject; override;
  end;

  TSuperObjectJsonDeserializer = class(TTypedJsonDeserializer<ISuperObject>)
  public
    class function Wrap(Value: ISuperObject): IJsonValue; override;
  end;

  TJsonProxyLoader = Aurelius.Json.Deserializer.TJsonProxyLoader;
  TJsonBlobLoader = Aurelius.Json.Deserializer.TJsonBlobLoader;

implementation
uses
  SysUtils;

type
  TSuperObjectJsonFactory = class(TInterfacedObject, IJsonFactory)
  private
    function CreateTrue: IJsonValue;
    function CreateFalse: IJsonValue;
    function CreateNull: IJsonValue;
    function CreateObject: IJsonObject;
    function CreateArray: IJsonArray;
    function CreateNumber(D: double): IJsonValue;
    function CreateString(const S: string): IJsonValue;
  end;

  TSuperObjectWrapper = class(TInterfacedObject, IJsonValue, IJsonArray, IJsonObject)
  strict private
    FValue: ISuperObject;
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
  strict private
    procedure Add(Value: IJsonValue);
    function Get(I: integer): IJsonValue;
    function Count: integer;
  strict private
    procedure AddProp(Name: string; Value: IJsonValue);
    function FindProp(Name: string): IJsonValue;
    function PropName(I: integer): string;
    function PropValue(I: integer): IJsonValue;
    function PropCount: integer;
  public
    constructor Create(AValue: ISuperObject);
  end;

{ TSuperObjectJsonSerializer }

function TSuperObjectJsonSerializer.CreateJsonFactory: IJsonFactory;
begin
  Result := TSuperObjectJsonFactory.Create;
end;

class function TSuperObjectJsonSerializer.Unwrap(JsonValue: IJsonValue): ISuperObject;
begin
  Supports(JsonValue.Wrapped, ISuperObject, Result);
end;

{ TSuperObjectJsonDeserializer }

class function TSuperObjectJsonDeserializer.Wrap(Value: ISuperObject): IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(Value);
end;

{ TSuperObjectJsonFactory }

function TSuperObjectJsonFactory.CreateArray: IJsonArray;
begin
  Result := TSuperObjectWrapper.Create(SA([]));
end;

function TSuperObjectJsonFactory.CreateFalse: IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(SO(false));
end;

function TSuperObjectJsonFactory.CreateNull: IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(TSuperObject.Create(stNull));
end;

function TSuperObjectJsonFactory.CreateNumber(D: double): IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(SO(D));
end;

function TSuperObjectJsonFactory.CreateObject: IJsonObject;
begin
  Result := TSuperObjectWrapper.Create(TSuperObject.Create);
end;

function TSuperObjectJsonFactory.CreateString(const S: string): IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(TSuperObject.Create(S));
end;

function TSuperObjectJsonFactory.CreateTrue: IJsonValue;
begin
  Result := TSuperObjectWrapper.Create(SO(true));
end;

{ TSupperObjectWrapper }

function TSuperObjectWrapper.ArrayValue: IJsonArray;
begin
  Assert(ValueType = TJsonType.JArray);
  Result := Self;
end;

function TSuperObjectWrapper.BoolValue: boolean;
begin
  Assert(ValueType = TJsonType.JBoolean);
  Result := SO.AsBoolean;
end;

constructor TSuperObjectWrapper.Create(AValue: ISuperObject);
begin
  FValue := AValue;
end;

function TSuperObjectWrapper.FloatValue: double;
begin
  Assert(ValueType = TJsonType.JFloat);
  Result := FValue.AsDouble;
end;

function TSuperObjectWrapper.IntValue: Int64;
begin
  Assert(ValueType = TJsonType.JInteger);
  Result := FValue.AsInteger;
end;

function TSuperObjectWrapper.JsonString: string;
begin
  if FValue <> nil then
    Result := FValue.AsJson
  else
    Result := 'null';
end;

function TSuperObjectWrapper.ObjectValue: IJsonObject;
begin
  Assert(ValueType = TJsonType.JObject);
  Result := Self;
end;

procedure TSuperObjectWrapper.Release;
begin
  FValue := nil;
end;

function TSuperObjectWrapper.StringValue: string;
begin
  if FValue <> nil then
    Result := FValue.AsString
  else
    Result := '';
end;

function TSuperObjectWrapper.ValueType: TJsonType;
var
  I: Int64;
begin
  if FValue = nil then Exit(TJsonType.JNull);

  case FValue.DataType of
    stNull:
      Result := TJsonType.JNull;
    stBoolean:
      Result := TJsonType.JBoolean;
    stDouble, stCurrency:
      if TryStrToInt64(FloatToStr(FValue.AsDouble), I) then
        Result := TJsonType.JInteger
      else
        Result := TJsonType.JFloat;
    stInt:
      Result := TJsonType.JInteger;
    stObject:
      Result := TJsonType.JObject;
    stArray:
      Result := TJsonType.JArray;
    stString:
      Result := TJsonType.JString;
  else
    Result := TJsonType.JNull;
  end;
end;

function TSuperObjectWrapper.Wrapped: TObject;
begin
  if FValue <> nil then
    Result := FValue as TSuperObject
  else
    Result := TSuperObject.Create(stNull);
end;

procedure TSuperObjectWrapper.Add(Value: IJsonValue);
begin
  Assert(FValue <> nil, 'not an array');
  FValue.AsArray.Add(TSuperObject(Value.Wrapped));
end;

function TSuperObjectWrapper.Count: integer;
begin
  Assert(FValue <> nil, 'not an array');
  Result := FValue.AsArray.Length;
end;

function TSuperObjectWrapper.Get(I: integer): IJsonValue;
begin
  Assert(FValue <> nil, 'not an array');
  Result := TSuperObjectWrapper.Create(FValue.AsArray[I]);
end;

procedure TSuperObjectWrapper.AddProp(Name: string; Value: IJsonValue);
begin
  Assert(FValue <> nil, 'not an object');
  FValue.AsObject.O[Name] := TSuperObject(Value.Wrapped);
end;

function TSuperObjectWrapper.FindProp(Name: string): IJsonValue;
begin
  Assert(FValue <> nil, 'not an object');
  if FValue.AsObject.Exists(Name) then
    Result := TSuperObjectWrapper.Create(FValue.AsObject[Name])
  else
    Result := nil;
end;

function TSuperObjectWrapper.PropCount: integer;
begin
  Assert(FValue <> nil, 'not an object');
  Result := FValue.AsObject.Count;
end;

function TSuperObjectWrapper.PropName(I: integer): string;
begin
  Assert(FValue <> nil, 'not an object');
  Result := FValue.AsObject.GetNames.AsArray[I].AsString;
end;

function TSuperObjectWrapper.PropValue(I: integer): IJsonValue;
begin
  Assert(FValue <> nil, 'not an object');
  Result := TSuperObjectWrapper.Create(FValue.AsObject.GetValues.AsArray[I]);
end;

end.

