unit Aurelius.Mapping.RttiUtils;

{$I Aurelius.Inc}

interface
uses
  Rtti, TypInfo,
  Aurelius.Mapping.Optimization;

type
  TRttiUtils = class
  strict private
    class var
      FInstance: TRttiUtils;
  strict private
    FContext: TRttiContext;
    procedure PrivateCreate;
    procedure PrivateDestroy;
  public
    class function GetInstance: TRttiUtils; static;
    class destructor Destroy;

    function FindMember(Clazz: TClass; MemberName: string): TRttiMember;
    function GetMember(Clazz: TClass; MemberName: string): TRttiMember;
    function GetMemberKind(Member: TRttiMember): TMemberKind;
    function GetMemberRttiType(Member: TRttiMember): TRttiType;
    function GetRealType(RttiType: TRttiType): TRttiType;
    function GetSurroundedClass(ListClass: TClass): TClass;
    function GetSurroundedType(RttiType: TRttiType): TRttiType;
    function IsBlob(ATypeInfo: PTypeInfo): boolean;
    function IsNullable(RttiType: TRttiType): boolean;
    function IsProxy(RttiType: TRttiType): boolean;
  end;

implementation
uses
  SysUtils, StrUtils,
  Aurelius.Mapping.Exceptions,
  Aurelius.Types.Blob;

{ TRttiUtils }

class destructor TRttiUtils.Destroy;
begin
  if FInstance <> nil then
  begin
    FInstance.PrivateDestroy;
    FInstance.Free;
    FInstance := nil;
  end;
end;

function TRttiUtils.FindMember(Clazz: TClass; MemberName: string): TRttiMember;
var
  RttiType: TRttiType;
begin
  RttiType := FContext.GetType(Clazz);
  Result := RttiType.GetField(MemberName);
  if Result = nil then
    Result := RttiType.GetProperty(MemberName);
end;

class function TRttiUtils.GetInstance: TRttiUtils;
begin
  if FInstance = nil then
  begin
    FInstance := TRttiUtils.Create;
    FInstance.PrivateCreate;
  end;
  Result := FInstance;
end;

function TRttiUtils.GetMember(Clazz: TClass; MemberName: string): TRttiMember;
begin
  Result := FindMember(Clazz, MemberName);
  if Result = nil then
    raise EMappingInternalError.CreateFmt('Member "%s" not found on class "%s".',
      [MemberName, Clazz.ClassName]);
end;

function TRttiUtils.GetMemberKind(Member: TRttiMember): TMemberKind;
begin
  if Member is TRttiField then
    Exit(TMemberKind.Field);

  if Member is TRttiProperty then
    Exit(TMemberKind.Prop);

  raise EMappingInternalError.Create('Unexpected member type.');
end;

function TRttiUtils.GetMemberRttiType(Member: TRttiMember): TRttiType;
begin
  if Member is TRttiField then
    Exit(TRttiField(Member).FieldType);

  if Member is TRttiProperty then
    Exit(TRttiProperty(Member).PropertyType);

  raise EMappingInternalError.CreateFmt('Unexpected member type: %s.',
    [Member.ClassName]);
end;

function TRttiUtils.GetRealType(RttiType: TRttiType): TRttiType;
begin
  if IsNullable(RttiType) then
    Result := GetRealType(RttiType.GetField('FValue').FieldType)
  else
  if IsProxy(RttiType) then
    Result := GetRealType(RttiType.GetField('FValue').FieldType)
  else
    Result := RttiType;
end;

function TRttiUtils.GetSurroundedClass(ListClass: TClass): TClass;
var
  SurroundedType: TRttiType;
begin
  SurroundedType := GetSurroundedType(FContext.GetType(ListClass));

  if not SurroundedType.IsInstance then
    raise EMappingInternalError.CreateFmt('Invalid surrounded type: "%s".',
      [SurroundedType.ToString]);

  Result := SurroundedType.AsInstance.MetaclassType;
end;

function TRttiUtils.GetSurroundedType(RttiType: TRttiType): TRttiType;
var
  TypeName: string;
begin
  repeat
    TypeName := RttiType.ToString;
    if StartsStr('TList<', TypeName) and EndsStr('>', TypeName) then
    begin
      TypeName := AnsiRightStr(TypeName, Length(TypeName) - Length('TList<'));
      TypeName := AnsiLeftStr(TypeName, Length(TypeName) - Length('>'));
      Exit(FContext.FindType(TypeName));
    end;

    if StartsStr('TObjectList<', TypeName) and EndsStr('>', TypeName) then
    begin
      TypeName := AnsiRightStr(TypeName, Length(TypeName) - Length('TObjectList<'));
      TypeName := AnsiLeftStr(TypeName, Length(TypeName) - Length('>'));
      Exit(FContext.FindType(TypeName));
    end;
    RttiType := RttiType.AsInstance.BaseType;
  until RttiType = nil;

  raise ECannotGetSurroundedType.Create(TypeName);
end;

function TRttiUtils.IsBlob(ATypeInfo: PTypeInfo): boolean;
begin
  Result := ATypeInfo = TypeInfo(TBlob);
end;

function TRttiUtils.IsNullable(RttiType: TRttiType): Boolean;
begin
  Result := (RttiType is TRttiRecordType) and
    (Pos('Nullable', RttiType.QualifiedName) > 0);
end;

function TRttiUtils.IsProxy(RttiType: TRttiType): boolean;
begin
  Result := (RttiType is TRttiRecordType) and
    (Pos('Proxy', RttiType.Name) > 0);
end;

procedure TRttiUtils.PrivateCreate;
begin
  FContext := TRttiContext.Create;
end;

procedure TRttiUtils.PrivateDestroy;
begin
  FContext.Free;
end;

end.
