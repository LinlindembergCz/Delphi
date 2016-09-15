unit Aurelius.Mapping.MappedClasses;

{$I Aurelius.inc}

interface

uses
  Rtti, Generics.Collections;

type
  TMappedClasses = class
  strict private
    FRegisteredClasses: TList<TClass>;
    function GetClasses: TEnumerable<TClass>;
  public
    class function GetEntityClasses: TArray<TClass>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterClass(Clazz: TClass);
    procedure RegisterClasses(AClasses: TEnumerable<TClass>); overload;
    procedure RegisterClasses(AClasses: TArray<TClass>); overload;
    procedure UnregisterClass(Clazz: TClass);
    procedure Clear;
    function IsEmpty: boolean;
    property Classes: TEnumerable<TClass> read GetClasses;
  end;

implementation
uses
  SysUtils,
  Aurelius.Global.Config,
  Aurelius.Mapping.Attributes;

{ TMappedClasses }

procedure TMappedClasses.RegisterClass(Clazz: TClass);
begin
  if not FRegisteredClasses.Contains(Clazz) then
    FRegisteredClasses.Add(Clazz);
end;

procedure TMappedClasses.RegisterClasses(AClasses: TArray<TClass>);
var
  Clazz: TClass;
begin
  for Clazz in AClasses do
    RegisterClass(Clazz);
end;

procedure TMappedClasses.RegisterClasses(AClasses: TEnumerable<TClass>);
var
  Clazz: TClass;
begin
  for Clazz in AClasses do
    RegisterClass(Clazz);
end;

procedure TMappedClasses.UnregisterClass(Clazz: TClass);
begin
  FRegisteredClasses.Remove(Clazz);
end;

procedure TMappedClasses.Clear;
begin
  FRegisteredClasses.Clear;
end;

constructor TMappedClasses.Create;
begin
  FRegisteredClasses := TList<TClass>.Create;
end;

destructor TMappedClasses.Destroy;
begin
  FRegisteredClasses.Free;
  inherited;
end;

function TMappedClasses.GetClasses: TEnumerable<TClass>;
begin
  Result := FRegisteredClasses;
end;

class function TMappedClasses.GetEntityClasses: TArray<TClass>;
var
  Context: TRttiContext;
  AllTypes: TArray<TRttiType>;
  T: TRttiType;
  A: TCustomAttribute;
  EntityClasses: TList<TClass>;
  I: Integer;
begin
  EntityClasses := TList<TClass>.Create;
  try
    Context := TRttiContext.Create;
    try
      AllTypes := Context.GetTypes;
      for T in AllTypes do
        if T.IsInstance then
          for A in T.GetAttributes do
            if A is Entity then
              EntityClasses.Add(T.AsInstance.MetaclassType);
      SetLength(Result, EntityClasses.Count);
      for I := 0 to EntityClasses.Count - 1 do
        Result[I] := EntityClasses[I];
    finally
      Context.Free;
    end;
  finally
    EntityClasses.Free;
  end;
end;

function TMappedClasses.IsEmpty: boolean;
begin
  Result := FRegisteredClasses.Count = 0;
end;

end.
