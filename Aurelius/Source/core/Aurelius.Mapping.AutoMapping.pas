unit Aurelius.Mapping.Automapping;

{$I Aurelius.inc}

interface

uses
  Rtti,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.Explorer;

type
  TAutomapping = class(TBaseAutomapping)
  protected
    function IsValidMember(Member: TRttiMember): Boolean;
    function CamelCaseToSqlName(Value: string): string;
    function ClassNameToSql(Value: string): string;
    function FieldNameToSql(Value: string): string;
  public
    function AutoGetTable(Clazz: TClass): TMetaTable; override;
    function AutoGetId(AClass: TClass): TMetaId; override;
    function AutoGetColumnsFromMember(Member: TRttiMember): TArray<TColumn>; override;
    function AutoGetAssociationByMember(Member: TRttiMember): TAssociation; override;
    function AutoGetEnumeration(RttiType: TRttiType): TEnumeration; override;
    function AutoGetSequence(Clazz: TClass): TSequence; override;
  end;

implementation

uses
  SysUtils, DB, TypInfo, Generics.Collections,
  Aurelius.Global.Utils,
  Aurelius.Global.Config,
  Aurelius.Mapping.Optimization,
  Aurelius.Mapping.RttiUtils;

{ TAutomapping }

function TAutomapping.CamelCaseToSqlName(Value: string): string;
var
  I: Integer;
  Current, Before: Char;
begin
  Result := Value;
  I := 2;
  while I <= Length(Result) do
  begin
    // <= 90 : maiúsculas
    // >= 97 : minúsculas
    Current := Result[I];
    Before := Result[I - 1];
    if (Ord(Current) <= 90) and (Before <> '_') and (Ord(Before) >= 97) then
    begin
      Insert('_', Result, I);
      Inc(I, 2);
    end
    else
      Inc(I);
  end;
  Result := UpperCase(Result);
end;

function TAutomapping.ClassNameToSql(Value: string): string;
begin
  Result := Value;

  if Result[1] = 'T' then
    Delete(Result, 1, 1);

  Result := CamelCaseToSqlName(Result);
end;

function TAutomapping.FieldNameToSql(Value: string): string;
begin
  Result := Value;

  if Result[1] = 'F' then
    Delete(Result, 1, 1);

  Result := CamelCaseToSqlName(Result);
end;

function TAutomapping.AutoGetAssociationByMember(Member: TRttiMember): TAssociation;
var
  MemberType, RealType: TRttiType;
  Columns: TArray<TColumn>;
  Col: TColumn;
  IsManyValued, IsProxy: Boolean;
//  Assocs: TList<TAssociation>;
//  A: TAssociation;
begin
  if not IsValidMember(Member) then
    Exit(nil);

  MemberType := TRttiUtils.GetInstance.GetMemberRttiType(Member);
  RealType := TRttiUtils.GetInstance.GetRealType(MemberType);

  if not RealType.IsInstance then
    Exit(nil);

  IsManyValued := Explorer.IsList(RealType.AsInstance.MetaclassType);
  if IsManyValued then
    RealType := TRttiUtils.GetInstance.GetSurroundedType(RealType);

  if not RealType.IsInstance then
    Exit(nil);

  Columns := Explorer.GetColumnsFromMember(Member);
  if Length(Columns) = 0 then
    Exit(nil);

  IsProxy := TRttiUtils.GetInstance.IsProxy(MemberType);

  Result := TAssociation.Create;
  Result.ClassMemberName := Member.Name;
  Result.Target := RealType.AsInstance.MetaclassType;
  Result.Optimization := Explorer.GetOptimization({Result.Target, }Member);
  if IsManyValued then
  begin
    Result.Kind := TAssociationKind.ManyValued;
    Result.Cascade := TGlobalConfigs.GetInstance.AutoMappingDefaultCascadeManyValued;
  end
  else
  begin
    Result.Kind := TAssociationKind.SingleValued;
    Result.Cascade := TGlobalConfigs.GetInstance.AutoMappingDefaultCascade;
  end;
  Result.Lazy := IsProxy;
  Result.Required := false;
  for Col in Columns do
    Result.JoinColumns.Add(Col);

//  if IsManyValued then
//  begin
//    Assocs := TMappingExplorer.GetInstance.GetAssociations(Result.Target, True, False);
//    for A in Assocs do
//    begin
//      if A.Target = Member.Parent.AsInstance.MetaclassType then
//        if (A.Kind = TAssociationKind.SingleValued) and (A.JoinColumns.Count > 0) then
//        begin
//          Result.MappedBy := A.JoinColumns[0].Name;
//          Break;
//        end;
//    end;
//  end;
end;

function TAutomapping.AutoGetColumnsFromMember(Member: TRttiMember): TArray<TColumn>;
var
  MemberType, RealType: TRttiType;
  RefCols: TArray<TColumn>;
  Metaclass, Clazz, ItemClass: TClass;
  Col: TColumn;
  I: Integer;
begin
  if not IsValidMember(Member) then
    Exit(nil);

  MemberType := TRttiUtils.GetInstance.GetMemberRttiType(Member);
  RealType := TRttiUtils.GetInstance.GetRealType(MemberType);

  Col := TColumn.Create;
  Col.DeclaringClass := Member.Parent.AsInstance.MetaclassType;
  Col.Optimization := Explorer.GetOptimization({Col.DeclaringClass, }Member);

  Col.Name := FieldNameToSql(Member.Name);

  if not RealType.IsInstance then
  begin
    Col.Properties := [];
    if not TRttiUtils.GetInstance.IsNullable(MemberType) then
      Col.Properties := Col.Properties + [TColumnProp.Required];
    if Col.Name = 'ID' then
      Col.Properties := Col.Properties + [TColumnProp.Unique, TColumnProp.NoUpdate];
    Col.FieldType := Explorer.ResolveFieldType(RealType, Col.Length);

    // If field type is blob or memo, then do not set as required even without nullable
    if Col.FieldType in [ftBlob, ftMemo, ftWideMemo] then
      Col.Properties := Col.Properties - [TColumnProp.Required];

    SetLength(Result, 1);
    Result[0] := Col;
  end
  else
  begin
    Metaclass := RealType.AsInstance.MetaclassType;
    if Explorer.IsList(Metaclass) then
    begin
      ItemClass := TRttiUtils.GetInstance.GetSurroundedType(RealType).AsInstance.MetaclassType;
      Clazz := Member.Parent.AsInstance.MetaclassType;

      Col.IsForeign := True;
      Col.ReferencedClass := Clazz;
      Col.ForeignClass := ItemClass;

      RefCols := Explorer.GetPrimaryJoinColumns(Clazz);
      if Length(RefCols) = 0 then
        RefCols := Explorer.GetIdColumns(Clazz);

      SetLength(Result, Length(RefCols));
      for I := 0 to Length(RefCols) - 1 do
      begin
        Result[I] := Col.Clone;
        Result[I].Name := Result[I].Name + '_' + ClassNameToSql(Clazz.ClassName) + '_' + RefCols[I].Name;
        Result[I].ReferencedColumn := RefCols[I];

        Result[I].FieldType := RefCols[I].FieldType;

        Result[I].Length := RefCols[I].Length;
        Result[I].Precision := RefCols[I].Precision;
        Result[I].Scale := RefCols[I].Scale;
      end;
      Col.Free;
    end
    else
    begin
      Col.ReferencedClass := Metaclass;
      RefCols := Explorer.GetPrimaryJoinColumns(Metaclass);
      if Length(RefCols) = 0 then
        RefCols := Explorer.GetIdColumns(Metaclass);

      SetLength(Result, Length(RefCols));
      for I := 0 to Length(RefCols) - 1 do
      begin
        Result[I] := Col.Clone;
        Result[I].Name := Result[I].Name + '_' + RefCols[I].Name;
        Result[I].ReferencedColumn := RefCols[I];
        Result[I].FieldType := RefCols[I].FieldType;

        Result[I].Length := RefCols[I].Length;
        Result[I].Precision := RefCols[I].Precision;
        Result[I].Scale := RefCols[I].Scale;
      end;
      Col.Free;
    end;
  end;
end;

function TAutomapping.AutoGetEnumeration(RttiType: TRttiType): TEnumeration;
begin
  Result := TEnumeration.Create;
  Result.OrdinalType := RttiType.AsOrdinal;
  Result.MappedType := TEnumMappingType.emString;
  // TODO: Implement algorithm to remove enumeration prefixes
end;

function TAutomapping.AutoGetId(AClass: TClass): TMetaId;
begin
  Result := TMetaId.Create;
  Result.Columns := Explorer.GetColumnsFromMember(
    TRttiUtils.GetInstance.GetMember(AClass, 'FID'));
  Result.IdGenerator := TIdGenerator.IdentityOrSequence;
end;

function TAutomapping.AutoGetSequence(Clazz: TClass): TSequence;
var
  Name: string;
begin
  Name := Clazz.ClassName;
  if Name[1] = 'T' then
    Delete(Name, 1, 1);

  Result := TSequence.Create;
  Result.SequenceName := 'SEQ_' + CamelCaseToSqlName(Name);
  Result.InitialValue := 1;
  Result.Increment := 1;
end;

function TAutomapping.AutoGetTable(Clazz: TClass): TMetaTable;
begin
  Result := TMetaTable.Create;
  Result.Name := ClassNameToSql(Clazz.ClassName);
end;

function TAutomapping.IsValidMember(Member: TRttiMember): Boolean;
var
  MemberType: TRttiType;
  RealType: TRttiType;
begin
  if TRttiUtils.GetInstance.GetMemberKind(Member) <> TMemberKind.Field then
    Exit(False);

  MemberType := TRttiUtils.GetInstance.GetMemberRttiType(Member);
  RealType := TRttiUtils.GetInstance.GetRealType(MemberType);
  if Explorer.IsDynamicContainer(RealType) then
    Exit(False);
  Result := not Explorer.IsTransient(Member);
end;

end.
