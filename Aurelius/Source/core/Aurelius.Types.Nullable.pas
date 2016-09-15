unit Aurelius.Types.Nullable;

{$I Aurelius.Inc}

interface

type
  TNullRecord = record
  end;

  Nullable<T> = record
  private
    FValue: T;
    FHasValue: Boolean; // Default False

    class procedure CheckNullOperation(Left, Right: Nullable<T>); static;
    function GetIsNull: Boolean;
    function GetValue: T;
    procedure SetValue(const Value: T);
    function GetValueOrDefault: T;
    {$IFDEF BINDINGS}
    class constructor Create;
    {$ENDIF}
  public
    constructor Create(Value: T); overload;

    property HasValue: Boolean read FHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    property ValueOrDefault: T read GetValueOrDefault;

    class operator Implicit(Value: TNullRecord): Nullable<T>;

    class operator Implicit(Value: T): Nullable<T>;
    class operator Implicit(Value: Nullable<T>): T;

    {$IFDEF EXPLICIT_CONVERSION}
    class operator Explicit(Value: T): Nullable<T>;
    class operator Explicit(Value: Nullable<T>): T;
    {$ENDIF}

    class operator Equal(Left, Right: Nullable<T>): Boolean;
    class operator NotEqual(Left, Right: Nullable<T>): Boolean;

    class operator GreaterThan(Left, Right: Nullable<T>): Boolean;
    class operator GreaterThanOrEqual(Left, Right: Nullable<T>): Boolean;

    class operator LessThan(Left, Right: Nullable<T>): Boolean;
    class operator LessThanOrEqual(Left, Right: Nullable<T>): Boolean;
  end;

var
  SNull: TNullRecord;

implementation

uses
  Generics.Defaults, Variants,
  {$IFDEF BINDINGS}
  System.TypInfo,
  System.Rtti,
  System.Bindings.Outputs,
  {$ENDIF}
  Aurelius.Global.Utils,
  Aurelius.Types.Exceptions;

{ Nullable<T> }

class procedure Nullable<T>.CheckNullOperation(Left, Right: Nullable<T>);
begin
  if not Left.FHasValue or not Right.FHasValue then
    raise ENullValueException.Create;
end;

constructor Nullable<T>.Create(Value: T);
begin
  FHasValue := False;
  FValue := Value;
end;

{$IFDEF BINDINGS}
class constructor Nullable<T>.Create;
begin
  TValueRefConverterFactory.RegisterConversion(TypeInfo(Nullable<T>), TypeInfo(T),
    TConverterDescription.Create(
    procedure(const InValue: TValue; var OutValue: TValue)
    begin
      OutValue := TValue.From<T>(InValue.AsType<Nullable<T>>.ValueOrDefault);
    end,
    'NullableTo' + GetTypeName(TypeInfo(T)),
    'NullableTo' + GetTypeName(TypeInfo(T)),
    '', True, '', nil)
  );

  TValueRefConverterFactory.RegisterConversion(TypeInfo(T), TypeInfo(Nullable<T>),
    TConverterDescription.Create(
    procedure(const InValue: TValue; var OutValue: TValue)
    var
      Rec: Nullable<T>;
    begin
      Rec.Value := InValue.AsType<T>;
      OutValue := TValue.From<Nullable<T>>(Rec);
    end,
    GetTypeName(TypeInfo(T)) + 'ToNullable',
    GetTypeName(TypeInfo(T)) + 'ToNullable',
    '', True, '', nil)
  );

  if TypeInfo(T) <> TypeInfo(string) then
  begin
    TValueRefConverterFactory.RegisterConversion(TypeInfo(Nullable<T>), TypeInfo(string),
      TConverterDescription.Create(
      procedure(const InValue: TValue; var OutValue: TValue)
      begin
        if InValue.AsType<Nullable<T>>.HasValue then
        begin
          OutValue := TValue.From<T>(InValue.AsType<Nullable<T>>.ValueOrDefault);
          // Convert OutValue from T to Variant and then string otherwise it can't be later converted to string
          OutValue := VarToStr(TUtils.ValueToVariant(OutValue));
        end
        else
          OutValue := '';
      end,
      'Nullable' + GetTypeName(TypeInfo(T)) + 'ToString',
      'Nullable' + GetTypeName(TypeInfo(T)) + 'ToString',
      '', True, '', nil)
    );

    // Converting from string to T will probably require some manual conversion (check each data type manually)
//    TValueRefConverterFactory.RegisterConversion(TypeInfo(string), TypeInfo(Nullable<T>),
//      TConverterDescription.Create(
//      procedure(const InValue: TValue; var OutValue: TValue)
//      var
//        Rec: Nullable<T>;
//        V: Variant;
//        VariantValue: TValue;
//      begin
//        if not InValue.IsEmpty and (InValue.AsString <> '') then
//        begin
//          // Convert the internal type of InValue from tkString to tkVariant
//          V := InValue.AsVariant;
//          InValue.TryCast(TypeInfo(Variant), VariantValue);
//
//          // TODO: Check if T is TDateTime/TDate/TTime and do a explicit StrToDateTime conversion here
//
//          // Now that internal type is tkVariant, it can convert to type T
//          Rec.Value := VariantValue.AsType<T>;
//        end;
//        OutValue := TValue.From<Nullable<T>>(Rec);
//      end,
//      'StringToNullable' + GetTypeName(TypeInfo(T)),
//      'StringToNullable' + GetTypeName(TypeInfo(T)),
//      '', True, '', nil)
//    );
  end;
end;
{$ENDIF}

class operator Nullable<T>.Implicit(Value: T): Nullable<T>;
begin
  Result.FValue := Value;
  Result.FHasValue := True;
end;

class operator Nullable<T>.Implicit(Value: Nullable<T>): T;
begin
  Result := Value.GetValue;
end;

class operator Nullable<T>.Equal(Left, Right: Nullable<T>): Boolean;
begin
  if Left.FHasValue and Right.FHasValue then
    Result := TEqualityComparer<T>.Default.Equals(Left.FValue, Right.FValue)
  else
    Result := Left.FHasValue = Right.FHasValue;
end;

{$IFDEF EXPLICIT_CONVERSION}
class operator Nullable<T>.Explicit(Value: T): Nullable<T>;
begin
  Result.FValue := Value;
  Result.FHasValue := True;
end;
{$ENDIF}

{$IFDEF EXPLICIT_CONVERSION}
class operator Nullable<T>.Explicit(Value: Nullable<T>): T;
begin
  Result := Value.GetValue;
end;
{$ENDIF}

function Nullable<T>.GetIsNull: Boolean;
begin
  Result := not FHasValue;
end;

function Nullable<T>.GetValue: T;
begin
  if not FHasValue then
    raise ENullConvertException<T>.Create;

  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if FHasValue then
    Result := FValue
  else
    Result := Default(T);
end;

class operator Nullable<T>.GreaterThan(Left, Right: Nullable<T>): Boolean;
begin
  CheckNullOperation(Left, Right);
  Result := TComparer<T>.Default.Compare(Left, Right) > 0;
end;

class operator Nullable<T>.GreaterThanOrEqual(Left, Right: Nullable<T>): Boolean;
begin
  CheckNullOperation(Left, Right);
  Result := TComparer<T>.Default.Compare(Left, Right) >= 0;
end;

class operator Nullable<T>.LessThanOrEqual(Left, Right: Nullable<T>): Boolean;
begin
  CheckNullOperation(Left, Right);
  Result := TComparer<T>.Default.Compare(Left, Right) <= 0;
end;

class operator Nullable<T>.LessThan(Left, Right: Nullable<T>): Boolean;
begin
  CheckNullOperation(Left, Right);
  Result := TComparer<T>.Default.Compare(Left, Right) < 0;
end;

class operator Nullable<T>.NotEqual(Left, Right: Nullable<T>): Boolean;
begin
  if Left.FHasValue and Right.FHasValue then
    Result := not TEqualityComparer<T>.Default.Equals(Left.FValue, Right.FValue)
  else
    Result := Left.FHasValue <> Right.FHasValue;
end;

procedure Nullable<T>.SetValue(const Value: T);
begin
  FValue := Value;
  FHasValue := True;
end;

class operator Nullable<T>.Implicit(Value: TNullRecord): Nullable<T>;
begin
  Result.FHasValue := False;
end;

end.
