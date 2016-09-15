unit Aurelius.Mapping.AttributeStrategy;

{$I Aurelius.Inc}

interface
uses
  Rtti, TypInfo,
  Generics.Collections,
  Aurelius.Mapping.Attributes,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.Explorer,
  Aurelius.Mapping.Strategy;

type
  TMappingAttributeStrategy = class(TMappingStrategy)
  private
    FExplorer: TMappingExplorer;
    function GetScalarColumnFromMember(Member: TRttiMember; Col: Column): TColumn;
    function GetJoinColumnsFromMember(Member: TRttiMember; JoinCols: TList<JoinColumn>): TArray<TColumn>;
    function GetForeignJoinColumnsFromMember(Member: TRttiMember; ForeignJoinCols: TList<ForeignJoinColumn>): TArray<TColumn>;
  public
    constructor Create(AExplorer: TMappingExplorer);
    function HasAutomapping(RttiType: TRttiType): boolean; override;
    function HasTransient(Member: TRttiMember): boolean; override;
    function IsEntity(RttiType: TRttiType): boolean; override;
    function LoadAssociationByMember(Member: TRttiMember): TAssociation; override;
    function LoadColumnsFromMember(Member: TRttiMember): TArray<TColumn>; override;
    function LoadDescription(Member: TRttiMember): string; override;
    function LoadDiscriminatorColumn(RttiType: TRttiType): TColumn; override;
    function LoadDiscriminatorValue(RttiType: TRttiType): TValue; override;
    function LoadEnumeration(RttiType: TRttiType): TEnumeration; override;
    function LoadInheritance(RttiType: TRttiType): TMetaInheritance; override;
    function LoadMetaId(RttiType: TRttiType): TMetaId; override;
    function LoadPrimaryJoinColumns(RttiType: TRttiType): TArray<TColumn>; override;
    function LoadSequence(RttiType: TRttiType): TSequence; override;
    function LoadTable(RttiType: TRttiType): TMetaTable; override;
    function LoadUniqueConstraints(RttiType: TRttiType): TArray<TUniqueConstraint>; override;
  end;

implementation
uses
  DB,
  Classes,
  Aurelius.Mapping.Exceptions,
  Aurelius.Mapping.RttiUtils,
  Aurelius.Global.Config;

type
  TInternalMappingExplorer = class(TMappingExplorer)
  end;

{ TMappingAttributeStrategy }

constructor TMappingAttributeStrategy.Create(AExplorer: TMappingExplorer);
begin
  FExplorer := AExplorer;
end;

function TMappingAttributeStrategy.GetForeignJoinColumnsFromMember(
  Member: TRttiMember;
  ForeignJoinCols: TList<ForeignJoinColumn>): TArray<TColumn>;
var
  IdRefCols: TArray<TColumn>;
  Clazz: TClass;
  AssociationClass: TClass;
  RealType: TRttiType;
  I: Integer;
  Col: TColumn;
  RefCol: TColumn;
begin
  Clazz := Member.Parent.AsInstance.MetaclassType;

  // Ensure that member type is a TList of a class type
  RealType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(Member));
  if not RealType.IsInstance or not FExplorer.IsList(RealType.AsInstance.MetaclassType) then
    raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);
  RealType := TRttiUtils.GetInstance.GetSurroundedType(RealType);
  if not RealType.IsInstance then
    raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);

  AssociationClass := RealType.AsInstance.MetaclassType;

  IdRefCols := FExplorer.GetPrimaryJoinColumns(Clazz);
  if Length(IdRefCols) = 0 then
    IdRefCols := FExplorer.GetIdColumns(Clazz);

  SetLength(Result, ForeignJoinCols.Count);
  for I := 0 to ForeignJoinCols.Count - 1 do
  begin
    Col := TColumn.Create;
    Col.DeclaringClass := Clazz;
    Col.Optimization := FExplorer.GetOptimization({Col.DeclaringClass, }Member);
    Col.Name := ForeignJoinCols[I].Name;
    Col.Properties := ForeignJoinCols[I].Properties;

    if ForeignJoinCols[I].ReferencedColumnName = '' then
    begin
      Col.ReferencedColumnIsId := True;
      if I < Length(IdRefCols) then
        RefCol := IdRefCols[I]
      else
        raise ETooManyJoinColumns.Create(Clazz.ClassName, Member.Name);
    end
    else
    begin
      Col.ReferencedColumnIsId := False;
      RefCol := TInternalMappingExplorer(FExplorer).FindColumnByName(Clazz, ForeignJoinCols[I].ReferencedColumnName);
      if RefCol = nil then
        raise EInvalidReferencedColumnName.Create(ForeignJoinCols[I].ReferencedColumnName, Clazz.ClassName);
    end;

    Col.ReferencedClass := Clazz;
    Col.ReferencedColumn := RefCol;
    Col.FieldType := RefCol.FieldType;
    Col.Length := RefCol.Length;
    Col.Precision := RefCol.Precision;
    Col.Scale := RefCol.Scale;
    Col.IsForeign := True;
    Col.ForeignClass := AssociationClass;
    Result[I] := Col;
  end;
end;

function TMappingAttributeStrategy.GetJoinColumnsFromMember(Member: TRttiMember;
  JoinCols: TList<JoinColumn>): TArray<TColumn>;
var
  IdRefCols: TArray<TColumn>;
  Clazz: TClass;
  AssociationClass: TClass;
  RealType: TRttiType;
  I: Integer;
  Col: TColumn;
  RefCol: TColumn;
begin
  Clazz := Member.Parent.AsInstance.MetaclassType;
  RealType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(Member));
  if not RealType.IsInstance then
    raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);

  AssociationClass := RealType.AsInstance.MetaclassType;
  IdRefCols := FExplorer.GetPrimaryJoinColumns(AssociationClass);
  if Length(IdRefCols) = 0 then
    IdRefCols := FExplorer.GetIdColumns(AssociationClass);

  SetLength(Result, JoinCols.Count);
  for I := 0 to JoinCols.Count - 1 do
  begin
    Col := TColumn.Create;
    Col.DeclaringClass := Clazz;
    Col.Optimization := FExplorer.GetOptimization({Col.DeclaringClass, }Member);
    Col.Name := JoinCols[I].Name;
    Col.Properties := JoinCols[I].Properties;

    if JoinCols[I].ReferencedColumnName = '' then
    begin
      Col.ReferencedColumnIsId := True;
      if I < Length(IdRefCols) then
        RefCol := IdRefCols[I]
      else
        raise ETooManyJoinColumns.Create(Clazz.ClassName, Member.Name);
    end
    else
    begin
      Col.ReferencedColumnIsId := False;
      RefCol := TInternalMappingExplorer(FExplorer).FindColumnByName(AssociationClass, JoinCols[I].ReferencedColumnName);
      if RefCol = nil then
        raise EInvalidReferencedColumnName.Create(JoinCols[I].ReferencedColumnName, Clazz.ClassName);
    end;

    Col.ReferencedColumn := RefCol;
    Col.ReferencedClass := AssociationClass;
    Col.FieldType := RefCol.FieldType;
    Col.Length := RefCol.Length;
    Col.Precision := RefCol.Precision;
    Col.Scale := RefCol.Scale;

    Result[I] := Col;
  end;
end;

function TMappingAttributeStrategy.GetScalarColumnFromMember(
  Member: TRttiMember; Col: Column): TColumn;
var
  RealType: TRttiType;
  Clazz: TClass;
begin
  Clazz := Member.Parent.AsInstance.MetaclassType;
  RealType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(Member));

  Result := TColumn.Create;
  try
    Result.DeclaringClass := Clazz;
    Result.Optimization := FExplorer.GetOptimization({Result.DeclaringClass, }Member);
    Result.Name := Col.Name;
    Result.Properties := Col.Properties;
    Result.IsLazy := (TColumnProp.Lazy in Col.Properties) and TRttiUtils.GetInstance.IsBlob(RealType.Handle);
    Result.Length := Col.Length;
    Result.Precision := Col.Precision;
    Result.Scale := Col.Scale;
    Result.FieldType := FExplorer.ResolveFieldType(RealType, Result.Length);
  except
    Result.Free;
    raise;
  end;
end;

function TMappingAttributeStrategy.HasAutomapping(RttiType: TRttiType): boolean;
var
  A: TCustomAttribute;
begin
  for A in RttiType.GetAttributes do
    if A is Automapping then
      Exit(True);
  Exit(False);
end;

function TMappingAttributeStrategy.HasTransient(Member: TRttiMember): boolean;
var
  A: TCustomAttribute;
begin
  for A in Member.GetAttributes do
    if A is Transient then
      Exit(True);
  Exit(False);
end;

function TMappingAttributeStrategy.IsEntity(RttiType: TRttiType): boolean;
var
  I: TCustomAttribute;
begin
  for I in RttiType.GetAttributes do
    if I is Entity then
      Exit(True);
  Result := False;
end;

function TMappingAttributeStrategy.LoadAssociationByMember(
  Member: TRttiMember): TAssociation;
var
  A: TCustomAttribute;
  Col: TColumn;
  RttiType: TRttiType;
  IsManyValued: Boolean;
begin
  Result := nil;
  for A in Member.GetAttributes do
    if A is Association then
    begin
      RttiType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(Member));

      IsManyValued := A is ManyValuedAssociation;
      if IsManyValued then
        RttiType := TRttiUtils.GetInstance.GetSurroundedType(RttiType);

      if not RttiType.IsInstance then
        raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);

      Result := TAssociation.Create;
      Result.Target := RttiType.AsInstance.MetaclassType;
      Result.ClassMemberName := Member.Name;
      Result.Optimization := FExplorer.GetOptimization({Result.Target, }Member);
      if IsManyValued then
        Result.Kind := TAssociationKind.ManyValued
      else
        Result.Kind := TAssociationKind.SingleValued;
      Result.Properties := Association(A).Properties;
      Result.Cascade := Association(A).Cascade;

      for Col in FExplorer.GetColumnsFromMember(Member) do
        Result.JoinColumns.Add(Col);

      if IsManyValued then
        Result.MappedBy := ManyValuedAssociation(A).MappedBy;
      Break;
    end;
end;

function TMappingAttributeStrategy.LoadColumnsFromMember(
  Member: TRttiMember): TArray<TColumn>;
var
  Clazz: TClass;
  A: TCustomAttribute;
  RealType: TRttiType;
  ForeignClass: TClass;
  ManyVldAssoc: ManyValuedAssociation;
  JoinCols: TList<JoinColumn>;
  ForeignJoinCols: TList<ForeignJoinColumn>;
  Cols: TList<Column>;
  ForeignCols: TArray<TColumn>;
  I: Integer;
  MappedMember: TRttiMember;
  MappedMemberRealType: TRttiType;
begin
  Clazz := Member.Parent.AsInstance.MetaclassType;
  JoinCols := TList<JoinColumn>.Create;
  ForeignJoinCols := TList<ForeignJoinColumn>.Create;
  Cols := TList<Column>.Create;
  try
    for A in Member.GetAttributes do
    begin
      if A is Column then
        Cols.Add(Column(A));
      if A is JoinColumn then
        JoinCols.Add(JoinColumn(A));
      if A is ForeignJoinColumn then
        ForeignJoinCols.Add(ForeignJoinColumn(A));
    end;

    // Check for what has been mapped
    if Cols.Count > 0 then
    begin
      if (Cols.Count > 1) or (JoinCols.Count > 0) or (ForeignJoinCols.Count > 0) then
        raise EMultipleColumnsFoundForAttribute.Create(Clazz.ClassName, Member.Name);

      SetLength(Result, 1);
      Result[0] := GetScalarColumnFromMember(Member, Cols[0]);
    end
    else
    if JoinCols.Count > 0 then
    begin
      if ForeignJoinCols.Count > 0 then
        raise EMultipleColumnsFoundForAttribute.Create(Clazz.ClassName, Member.Name);
      Result := GetJoinColumnsFromMember(Member, JoinCols);
    end else
    if ForeignJoinCols.Count > 0 then
      Result := GetForeignJoinColumnsFromMember(Member, ForeignJoinCols);

    // If no column attributes found, then check if there is a manyvaluedassociation
    // with a mapped by attribute. In this the columns will come from the mapped by field
    if Length(Result) = 0 then
    begin
      for A in Member.GetAttributes do
      begin
        if not (A is ManyValuedAssociation) then
          Continue;

        ManyVldAssoc := ManyValuedAssociation(A);
        if (ManyVldAssoc.MappedBy = '') then
          Continue;

        RealType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(Member));
        if not RealType.IsInstance or not FExplorer.IsList(RealType.AsInstance.MetaclassType) then
          raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);

        RealType := TRttiUtils.GetInstance.GetSurroundedType(RealType);
        if not RealType.IsInstance then
          raise EInvalidAssociation.Create(Member.Parent.Name, Member.Name);

        ForeignClass := RealType.AsInstance.MetaclassType;

        MappedMember := TRttiUtils.GetInstance.GetMember(ForeignClass, ManyVldAssoc.MappedBy);

        // Just test if the mapped member points to an instance of this class, otherwise the bidirectional mapping is wrong!
        MappedMemberRealType := TRttiUtils.GetInstance.GetRealType(TRttiUtils.GetInstance.GetMemberRttiType(MappedMember));
        if not MappedMemberRealType.IsInstance then
          raise EInvalidMappedByReference.Create(Clazz, Member.Name);
        if not Clazz.InheritsFrom(MappedMemberRealType.AsInstance.MetaclassType) then
          raise EInvalidMappedByReference.Create(Clazz, Member.Name);
        ForeignCols := FExplorer.GetColumnsFromMember(MappedMember);
        if Length(ForeignCols) = 0 then
          raise EColumnNotFoundForAttribute.Create(ForeignClass.ClassName, ManyVldAssoc.MappedBy);

        SetLength(Result, Length(ForeignCols));
        for I := 0 to Length(ForeignCols) - 1 do
        begin
          Result[I] := ForeignCols[I].Clone;
          Result[I].DeclaringClass := Clazz;
          Result[I].Optimization := FExplorer.GetOptimization({Result[I].DeclaringClass, }Member);
          Result[I].ForeignClass := ForeignClass;
          Result[I].IsForeign := True;
        end;
      end;
    end;

  finally
    JoinCols.Free;
    ForeignJoinCols.Free;
    Cols.Free;
  end;
end;

function TMappingAttributeStrategy.LoadDescription(Member: TRttiMember): string;
var
  A: TCustomAttribute;
begin
  Result := '';
  for A in Member.GetAttributes do
    if A is Description then
      Exit(Description(A).Text);
end;

function TMappingAttributeStrategy.LoadDiscriminatorColumn(
  RttiType: TRttiType): TColumn;
var
  A: TCustomAttribute;
  Discr: DiscriminatorColumn;
  Clazz: TClass;
begin
  Clazz := RttiType.AsInstance.MetaclassType;
  Result := nil;
  for A in RttiType.GetAttributes do
    if A is DiscriminatorColumn then
    begin
      Discr := DiscriminatorColumn(A);

      Result := TColumn.Create;
      Result.DeclaringClass := Clazz;
      Result.Optimization := FExplorer.GetOptimization(nil);
      Result.Name := Discr.Name;
      Result.FieldType := Discr.FieldType;
      Result.Properties := [TColumnProp.Required];
      Result.Length := Discr.Length;
      Result.IsDiscriminator := True;
      Break;
    end;
end;

function TMappingAttributeStrategy.LoadDiscriminatorValue(
  RttiType: TRttiType): TValue;
var
  A: TCustomAttribute;
begin
  for A in RttiType.GetAttributes do
    if A is DiscriminatorValue then
      Exit(DiscriminatorValue(A).Value);
  Exit(TValue.Empty);
end;

function TMappingAttributeStrategy.LoadEnumeration(
  RttiType: TRttiType): TEnumeration;
var
  A: TCustomAttribute;
  Enum: Enumeration;
begin
  Result := nil;
  for A in RttiType.GetAttributes do
    if A is Enumeration then
    begin
      Enum := Enumeration(A);
      Result := TEnumeration.Create;
      Result.OrdinalType := RttiType.AsOrdinal;
      Result.MappedType := Enum.MappedType;
      Result.MappedValues := Enum.MappedValues;
      Break;
    end;
end;

function TMappingAttributeStrategy.LoadInheritance(
  RttiType: TRttiType): TMetaInheritance;
var
  A: TCustomAttribute;
begin
  Result := nil;
  for A in RttiType.GetAttributes do
    if A is Inheritance then
    begin
      Result := TMetaInheritance.Create;
      Result.Strategy := Inheritance(A).Strategy;
      Exit;
    end;
end;

function TMappingAttributeStrategy.LoadMetaId(RttiType: TRttiType): TMetaId;
var
  Cols: TList<TColumn>;
  A: TCustomAttribute;
  TempCols: TArray<TColumn>;
  TempCol: TColumn;
  EntityClass: TClass;
  I: integer;
begin
  // Load columns from attributes. Multiple Id attributes are considered composite id
  Result := nil;
  EntityClass := RttiType.AsInstance.MetaclassType;
  Cols := TList<TColumn>.Create;
  try
    for A in RttiType.GetAttributes do
      if A is Id then
      begin
        if Result = nil then
        begin
          // Single Id
          Result := TMetaId.Create;
          Result.IdGenerator := Id(A).Generator;
        end else
          // Composite Id
          Result.IdGenerator := TIdGenerator.None;

        TempCols := FExplorer.GetColumnsFromMember(TRttiUtils.GetInstance.GetMember(EntityClass, Id(A).MemberName));
        if Length(TempCols) = 0 then
          raise EInvalidIdMemberName.Create(Id(A).MemberName, EntityClass.ClassName);

        for TempCol in TempCols do
          Cols.Add(TempCol);
      end;

    if Result <> nil then
    begin
      SetLength(TempCols, Cols.Count);
      for I := 0 to Cols.Count - 1 do
        TempCols[I] := Cols[I];
      Result.Columns := TempCols;
    end;
  finally
    Cols.Free;
  end;
end;

function TMappingAttributeStrategy.LoadPrimaryJoinColumns(
  RttiType: TRttiType): TArray<TColumn>;
var
  A: TCustomAttribute;
  IdRefCols: TArray<TColumn>;
  PrimaryJoinCols: TList<PrimaryJoinColumn>;
  I: Integer;
  RefCol: TColumn;
  PJoinCol: PrimaryJoinColumn;
  Clazz: TClass;
begin
  Clazz := RttiType.AsInstance.MetaclassType;
  IdRefCols := FExplorer.GetPrimaryJoinColumns(Clazz.ClassParent);
  if Length(IdRefCols) = 0 then
    IdRefCols := FExplorer.GetIdColumns(Clazz.ClassParent);

  PrimaryJoinCols := TList<PrimaryJoinColumn>.Create;
  try
    for A in RttiType.GetAttributes do
      if A is PrimaryJoinColumn then
        PrimaryJoinCols.Add(PrimaryJoinColumn(A));

    // Primary join columns will be same as id ref cols
    // We will use the primary join col attribute just to define a different name
    SetLength(Result, Length(IdRefCols));
    for I := 0 to Length(IdRefCols) - 1 do
    begin
      RefCol := IdRefCols[I];

      if I < PrimaryJoinCols.Count then
        PJoinCol := PrimaryJoinCols[I]
      else
        PJoinCol := nil;

      Result[I] := RefCol.Clone;
      Result[I].DeclaringClass := Clazz;
      Result[I].ReferencedColumn := RefCol;
      if (PJoinCol <> nil) and (PJoinCol.Name <> '') then
        Result[I].Name := PrimaryJoinCols[I].Name;
      Result[I].IsPrimaryJoin := True;
    end;
  finally
    PrimaryJoinCols.Free;
  end;
end;

function TMappingAttributeStrategy.LoadSequence(RttiType: TRttiType): TSequence;
var
  A: TCustomAttribute;
begin
  Result := nil;
  for A in RttiType.GetAttributes do
    if A is Sequence then
    begin
      Result := TSequence.Create;
      Result.SequenceName := Sequence(A).SequenceName;
      Result.InitialValue := Sequence(A).InitialValue;
      Result.Increment := Sequence(A).Increment;
      Break;
    end;
end;

function TMappingAttributeStrategy.LoadTable(RttiType: TRttiType): TMetaTable;
var
  A: TCustomAttribute;
begin
  Result := nil;
  for A in RttiType.GetAttributes do
    if A is Table then
    begin
      Result := TMetaTable.Create;
      Result.Name := Table(A).Name;
      Result.Schema := Table(A).Schema;
    end;
end;

function TMappingAttributeStrategy.LoadUniqueConstraints(
  RttiType: TRttiType): TArray<TUniqueConstraint>;
var
  A: TCustomAttribute;
  UK: TUniqueConstraint;
  C: TColumn;
  FieldNames: TStringList;
  List: TObjectList<TUniqueConstraint>;
  Clazz: TClass;
  I: Integer;
begin
  Clazz := RttiType.AsInstance.MetaclassType;
  List := TObjectList<TUniqueConstraint>.Create;
  FieldNames := TStringList.Create;
  try
    for C in FExplorer.GetColumns(Clazz, True, True) do
      FieldNames.Add(C.Name);

    FieldNames.Sort;

    for A in RttiType.GetAttributes do
      if A is UniqueKey then
      begin
        // Code below must be commented for now because we might have unique keys
        // that include foreign columns (columns defined with ForeignJoinColumn.
        // Such columns are not retrieved by FExplorer.GetColumns because they
        // are declared in another class
//        for S in UniqueKey(A).Columns do
//          if FieldNames.IndexOf(S) < 0 then
//            raise EInvalidUniqueColumnName.Create(S, Clazz.ClassName);

        UK := TUniqueConstraint.Create;
        List.Add(UK);
        UK.FieldNames.AddRange(UniqueKey(A).Columns);
      end;

    I := 0;
    SetLength(Result, List.Count);
    while List.Count > 0 do
    begin
      Result[I] := List[0];
      List.Extract(List[0]);
      Inc(I);
    end;
  finally
    FieldNames.Free;
    List.Free;
  end;
end;

end.
