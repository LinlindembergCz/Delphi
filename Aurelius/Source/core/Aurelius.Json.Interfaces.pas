unit Aurelius.Json.Interfaces;

{$I Aurelius.Inc}

interface
uses
  SysUtils;

type
  TJsonType = (JString, JInteger, JFloat, JObject, JArray, JBoolean, JNull);

  IJsonObject = interface;
  IJsonArray = interface;

  IJsonValue = interface
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
  end;

  IJsonObject = interface(IJsonValue)
    procedure AddProp(Name: string; Value: IJsonValue);
    function FindProp(Name: string): IJsonValue;
    function PropName(I: integer): string;
    function PropValue(I: integer): IJsonValue;
    function PropCount: integer;
  end;

  IJsonArray = interface(IJsonValue)
    procedure Add(Value: IJsonValue);
    function Get(I: integer): IJsonValue;
    function Count: integer;
  end;

  IJsonFactory = interface
    function CreateTrue: IJsonValue;
    function CreateFalse: IJsonValue;
    function CreateNull: IJsonValue;
    function CreateObject: IJsonObject;
    function CreateArray: IJsonArray;
    function CreateNumber(D: double): IJsonValue;
    function CreateString(const S: string): IJsonValue;
  end;

implementation


end.
