unit Aurelius.Engine.ObjectFactory;

{$I Aurelius.inc}

interface

type
  TObjectFactory = class
  private
    class var
      FInstance: TObjectFactory;
  private
    procedure PrivateCreate;
    procedure PrivateDestroy;
    function CreateInstanceWithRtti(AClass: TClass): TObject;
  public
    class function GetInstance: TObjectFactory;
    function CreateInstance(AClass: TClass): TObject;
  end;

implementation
uses
  Rtti;

function TObjectFactory.CreateInstance(AClass: TClass): TObject;
begin
  Result := CreateInstanceWithRtti(AClass);
end;

function TObjectFactory.CreateInstanceWithRtti(AClass: TClass): TObject;
var
  C: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
begin
  C := TRttiContext.Create;
  RttiType := C.GetType(AClass);
  for Method in RttiType.GetMethods do
  begin
    if Method.IsConstructor and (Length(Method.GetParameters) = 0) then
      Exit(Method.Invoke(AClass, []).AsObject);
  end;
  Result := nil;
  C.Free;
end;

class function TObjectFactory.GetInstance: TObjectFactory;
begin
  if FInstance = nil then
  begin
    FInstance := TObjectFactory.Create;
    FInstance.PrivateCreate;
  end;
  Result := FInstance;
end;

procedure TObjectFactory.PrivateCreate;
begin

end;

procedure TObjectFactory.PrivateDestroy;
begin

end;

initialization

finalization
  if TObjectFactory.FInstance <> nil then
  begin
    TObjectFactory.FInstance.PrivateDestroy;
    TObjectFactory.FInstance.Free;
    TObjectFactory.FInstance := nil;
  end;

end.

