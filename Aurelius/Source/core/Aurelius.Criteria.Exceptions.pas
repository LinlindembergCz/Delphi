unit Aurelius.Criteria.Exceptions;

{$I Aurelius.inc}

interface
uses
  DB, TypInfo,
  Aurelius.SQL.Interfaces,
  Aurelius.Global.Exceptions;

type
  ECriteriaNotProjection = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EAliasNotFound = class(EOPFBaseException)
  public
    constructor Create(Alias: string);
  end;

  EResultsNotUnique = class(EOPFBaseException)
  public
    constructor Create(AResultCount: integer);
  end;

  ENotGroupProjection = class(EOPFBaseException)
  public
    constructor Create;
  end;

  ETypesNotEqualInWhenCondition = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EUnsupportedFieldType = class(EOPFBaseException)
  public
    constructor Create(ASQLGenerator: ISQLGenerator; FieldType: TFieldType);
  end;

  ECriteriaResultPropertyNotFound = class(EOPFBaseException)
  public
    constructor Create(APropName: string);
  end;

  EIncorrectValueTypePairsInSQLCondition = class(EOPFBaseException)
    constructor Create;
  end;

implementation

{ ECriteriaNotProjection }

constructor ECriteriaNotProjection.Create;
begin
  inherited Create('Cannot return values for Criteria with not projections.');
end;

{ EAliasNotFound }

constructor EAliasNotFound.Create(Alias: string);
begin
  inherited CreateFmt('Alias "%s" not found in criteria', [Alias]);
end;

{ EResultsNotUnique }

constructor EResultsNotUnique.Create(AResultCount: integer);
begin
  inherited CreateFmt('Criteria did not return an unique object. Rows returned: %d',
    [AResultCount]);
end;

{ ENotGroupProjection }

constructor ENotGroupProjection.Create;
begin
  inherited Create('Cannot retrieve group SQL - projection is not a group');
end;

{ ETypesNotEqualInWhenCondition }

constructor ETypesNotEqualInWhenCondition.Create;
begin
  inherited Create('Result types are not equal in conditional projection. Types returned when condition is true or false must be the same.');
end;

{ EUnsupportedFieldType }

constructor EUnsupportedFieldType.Create(ASQLGenerator: ISQLGenerator;
  FieldType: TFieldType);
begin
  inherited CreateFmt('SQLGeneration: Field type %s is not supported on %s.',
    [GetEnumName(TypeInfo(TFieldType), Ord(FieldType)),
    ASQLGenerator.GeneratorName]);
end;

{ ECriteriaResultPropertyNotFound }

constructor ECriteriaResultPropertyNotFound.Create(APropName: string);
begin
  inherited CreateFmt('Property/Projection "%s" not found in criteria result object', [APropName]);
end;

{ EIncorrectValueTypePairsInSQLCondition }

constructor EIncorrectValueTypePairsInSQLCondition.Create;
begin
  inherited Create('Values and types do not match in sql projection. You must provide the same number of param values and param types.');
end;

end.
