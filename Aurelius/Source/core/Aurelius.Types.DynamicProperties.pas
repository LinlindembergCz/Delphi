unit Aurelius.Types.DynamicProperties;

{$I Aurelius.Inc}

interface
uses
  Rtti,
  Generics.Collections;

type
  TDynamicProperties = class
  private
    FProps: TDictionary<string, TValue>;
    function GetItem(const PropName: string): TValue;
    procedure SetItem(const PropName: string; const Value: TValue);
    function GetProps: TEnumerable<TPair<string, TValue>>;
  public
    constructor Create;
    destructor Destroy; override;
    property Prop[const PropName: string]: TValue read GetItem write SetItem; default;
    property Props: TEnumerable<TPair<string, TValue>> read GetProps;
  end;

implementation
uses
  SysUtils;

{ TDynamicProperties }

constructor TDynamicProperties.Create;
begin
  FProps := TDictionary<string, TValue>.Create;
end;

destructor TDynamicProperties.Destroy;
begin
  FProps.Free;
  inherited;
end;

function TDynamicProperties.GetItem(const PropName: string): TValue;
begin
  if not FProps.TryGetValue(LowerCase(PropName), Result) then
    Result := TValue.Empty;
end;

function TDynamicProperties.GetProps: TEnumerable<TPair<string, TValue>>;
begin
  Result := FProps;
end;

procedure TDynamicProperties.SetItem(const PropName: string;
  const Value: TValue);
begin
  FProps.AddOrSetValue(LowerCase(PropName), Value);
end;

end.
