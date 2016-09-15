unit Aurelius.Types.Proxy;

{$I Aurelius.inc}

interface

type
  TProxyType = (Single, List);

  IProxyInfo = interface
    function ProxyType: TProxyType;
    function ClassName: string;
    function MemberName: string;
    function Key: Variant;
  end;

  IProxyController = interface
    function LoadProxyValue: TObject;
    function ProxyInfo: IProxyInfo;
  end;

  Proxy<T: class> = record
  private
    FValue: T;
    FController: IProxyController;
    FLoaded: Boolean;
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    procedure Load;
    procedure SetInitialValue(AValue: T);
    procedure DestroyValue;

    class operator Equal(Left, Right: Proxy<T>): Boolean;
    class operator NotEqual(Left, Right: Proxy<T>): Boolean;

    property Value: T read GetValue write SetValue;
  end;

implementation

{ Proxy<T> }

procedure Proxy<T>.DestroyValue;
begin
  if FValue <> nil then
    FValue.Free;
end;

class operator Proxy<T>.Equal(Left, Right: Proxy<T>): Boolean;
begin
  Result := Left.GetValue = Right.GetValue;
end;

function Proxy<T>.GetValue: T;
begin
  // No need to check for FLoaded, just optimization
  if not FLoaded then Load;
  Result := FValue;
end;

procedure Proxy<T>.Load;
var
  OldValue: T;
begin
  if FLoaded then Exit;

  // If you change any code from below, review TMappingExplorer.ForceProxyLoad!
  if FController <> nil then
  begin
    OldValue := FValue;
    FValue := T(FController.LoadProxyValue);
    if (FController.ProxyInfo <> nil) and (FController.ProxyInfo.ProxyType = TProxyType.List) and (OldValue <> nil) then
      OldValue.Free;
  end;
  FLoaded := True;
end;

class operator Proxy<T>.NotEqual(Left, Right: Proxy<T>): Boolean;
begin
  Result := Left.GetValue <> Right.GetValue;
end;

procedure Proxy<T>.SetInitialValue(AValue: T);
begin
  FValue := AValue;
end;

procedure Proxy<T>.SetValue(const Value: T);
begin
  FValue := Value;
  FLoaded := True;
end;

end.

