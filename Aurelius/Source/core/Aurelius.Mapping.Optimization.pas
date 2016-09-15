unit Aurelius.Mapping.Optimization;

{$I Aurelius.inc}

interface
uses
  Rtti;

type
  TMemberKind = (Field, Prop);

  TRttiOptimization = class
  private
    FMember: TRttiMember;
    FRealType: TRttiType;
    FSurroundedType: TRttiType;
    FMemberType: TRttiType;
    FMemberKind: TMemberKind;
    FIsNullable: boolean;
    FIsNullableLoaded: boolean;
    FIsProxy: boolean;
    FIsProxyLoaded: boolean;
    FNullableValueField: TRttiField;
    FNullableHasValueField: TRttiField;
    FProxyValueField: TRttiField;
    FProxyLoadedField: TRttiField;
    FProxyControllerField: TRttiField;
    FBlobControllerField: TRttiField;
    FDynamicName: string;
    FDynamicType: TRttiType;
    FIsDynamic: boolean;
    function GetMemberType: TRttiType;
    function GetRealType: TRttiType;
    function GetIsNullable: boolean;
    function GetIsProxy: boolean;
    function GetNullableValueField: TRttiField;
    function GetNullableHasValueField: TRttiField;
    function GetProxyLoadedField: TRttiField;
    function GetProxyControllerField: TRttiField;
    function GetProxyValueField: TRttiField;
    function GetSurroundedType: TRttiType;
    function GetMemberName: string;
    function GetMemberKind: TMemberKind;
    function GetMemberClass: TClass;
    function GetBlobControllerField: TRttiField;
    function GetParentClass: TClass;
  public
    constructor Create(AMember: TRttiMember); overload;
    constructor Create(AMember: TRttiMember; ADynamicName: string; ADynamicType: TRttiType); overload;
    function Clone: TRttiOptimization;
    property MemberRef: TRttiMember read FMember;
    property MemberName: string read GetMemberName;
    property MemberClass: TClass read GetMemberClass;
    property MemberType: TRttiType read GetMemberType;
    property MemberKind: TMemberKind read GetMemberKind;
    property ParentClass: TClass read GetParentClass;
    property RealType: TRttiType read GetRealType;
    property SurroundedType: TRttiType read GetSurroundedType;
    property IsNullable: boolean read GetIsNullable;
    property IsProxy: boolean read GetIsProxy;
    property NullableValueField: TRttiField read GetNullableValueField;
    property NullableHasValueField: TRttiField read GetNullableHasValueField;

    property ProxyValueField: TRttiField read GetProxyValueField;
    property ProxyLoadedField: TRttiField read GetProxyLoadedField;
    property ProxyControllerField: TRttiField read GetProxyControllerField;

    property BlobControllerField: TRttiField read GetBlobControllerField;

    property IsDynamic: boolean read FIsDynamic;
  end;

implementation
uses
  Aurelius.Mapping.RttiUtils;

{ TRttiOptimization }

function TRttiOptimization.Clone: TRttiOptimization;
begin
  if FIsDynamic then
    Result := TRttiOptimization.Create(FMember, FDynamicName, FDynamicType)
  else
    Result := TRttiOptimization.Create(FMember);
end;

constructor TRttiOptimization.Create(AMember: TRttiMember);
begin
  FMember := AMember;
  FIsDynamic := False;

  if FMember <> nil then
    FMemberKind := TRttiUtils.GetInstance.GetMemberKind(FMember)
  else
    FMemberKind := TMemberKind.Field; // dummy
end;

constructor TRttiOptimization.Create(AMember: TRttiMember;
  ADynamicName: string; ADynamicType: TRttiType);
begin
  Create(AMember);
  FDynamicName := ADynamicName;
  FDynamicType := ADynamicType;
  FIsDynamic := True;
end;

function TRttiOptimization.GetBlobControllerField: TRttiField;
begin
  if FBlobControllerField <> nil then
    Exit(FBlobControllerField);
  if (MemberType <> nil) then
  begin
    FBlobControllerField := MemberType.GetField('FController');
    Exit(FBlobControllerField);
  end else
    result := nil;
end;

function TRttiOptimization.GetIsNullable: boolean;
begin
  if not FIsNullableLoaded then
  begin
    FIsNullable := TRttiUtils.GetInstance.IsNullable(MemberType);
    FIsNullableLoaded := true;
  end;
  Result := FIsNullable;
end;

function TRttiOptimization.GetIsProxy: boolean;
begin
  if not FIsProxyLoaded then
  begin
    FIsProxy := TRttiUtils.GetInstance.IsProxy(MemberType);
    FIsProxyLoaded := true;
  end;
  Result := FIsProxy;
end;

function TRttiOptimization.GetMemberType: TRttiType;
begin
  if FMemberType = nil then
  begin
    if FIsDynamic then
      FMemberType := FDynamicType
    else
      FMemberType := TRttiUtils.GetInstance.GetMemberRttiType(FMember);
  end;
  Result := FMemberType;
end;

function TRttiOptimization.GetNullableHasValueField: TRttiField;
begin
  if FNullableHasValueField <> nil then
    Exit(FNullableHasValueField);
  if IsNullable and (MemberType <> nil) then
  begin
    FNullableHasValueField := MemberType.GetField('FHasValue');
    Exit(FNullableHasValueField);
  end else
    result := nil;
end;

function TRttiOptimization.GetNullableValueField: TRttiField;
begin
  if FNullableValueField <> nil then
    Exit(FNullableValueField);
  if IsNullable and (MemberType <> nil) then
  begin
    FNullableValueField := MemberType.GetField('FValue');
    Exit(FNullableValueField);
  end else
    result := nil;
end;

function TRttiOptimization.GetMemberClass: TClass;
begin
  if RealType.IsInstance then
    Result := RealType.AsInstance.MetaclassType
  else
    Result := nil;
end;

function TRttiOptimization.GetMemberKind: TMemberKind;
begin
  Result := FMemberKind;
end;

function TRttiOptimization.GetMemberName: string;
begin
  if FIsDynamic then
    Result := FDynamicName
  else
  if FMember <> nil then
    Result := FMember.Name
  else
    Result := '';
end;

function TRttiOptimization.GetProxyLoadedField: TRttiField;
begin
  if FProxyLoadedField <> nil then
    Exit(FProxyLoadedField);
  if IsProxy and (MemberType <> nil) then
  begin
    FProxyLoadedField := MemberType.GetField('FLoaded');
    Exit(FProxyLoadedField);
  end else
    result := nil;
end;

function TRttiOptimization.GetParentClass: TClass;
begin
 if (FMember <> nil) and FMember.Parent.IsInstance then
   Result := FMember.Parent.AsInstance.MetaclassType
 else
   Result := nil;
end;

function TRttiOptimization.GetProxyControllerField: TRttiField;
begin
  if FProxyControllerField <> nil then
    Exit(FProxyControllerField);
  if IsProxy and (MemberType <> nil) then
  begin
    FProxyControllerField := MemberType.GetField('FController');
    Exit(FProxyControllerField);
  end else
    result := nil;
end;

function TRttiOptimization.GetProxyValueField: TRttiField;
begin
  if FProxyValueField <> nil then
    Exit(FProxyValueField);
  if IsProxy and (MemberType <> nil) then
  begin
    FProxyValueField := MemberType.GetField('FValue');
    Exit(FProxyValueField);
  end else
    result := nil;
end;

function TRttiOptimization.GetRealType: TRttiType;
begin
  if FRealType = nil then
    FRealType := TRttiUtils.GetInstance.GetRealType(MemberType);
  Result := FRealType;
end;

function TRttiOptimization.GetSurroundedType: TRttiType;
begin
  if FSurroundedType = nil then
    FSurroundedType := TRttiUtils.GetInstance.GetSurroundedType(RealType);
  Result := FSurroundedType;
end;

end.
