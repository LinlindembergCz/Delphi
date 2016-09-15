unit Aurelius.Criteria.Expression;

{$I Aurelius.inc}

interface
uses
  Aurelius.Criteria.Base;

type
  TExpression = class sealed
  public
    class function Eq(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function Eq(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function Eq(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;

    class function IdEq(AValue: Variant): TCustomCriterion;

    class function GreaterThan(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function GreaterThan(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function GreaterThan(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;
    class function Gt(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function Gt(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function Gt(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;

    class function GreaterOrEqual(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function GreaterOrEqual(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function GreaterOrEqual(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;
    class function Ge(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function Ge(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function Ge(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;

    class function LessThan(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function LessThan(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function LessThan(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;
    class function Lt(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function Lt(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function Lt(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;

    class function LessOrEqual(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function LessOrEqual(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function LessOrEqual(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;
    class function Le(APropName: string; AValue: Variant): TSimpleExpression; overload;
    class function Le(AProjection: TSimpleProjection; AValue: Variant): TSimpleExpression; overload;
    class function Le(ALeft, ARight: TSimpleProjection): TCustomCriterion; overload;

    class function Like(APropName: string; AValue: string): TCustomCriterion; overload;
    class function Like(AProjection: TSimpleProjection; AValue: string): TCustomCriterion; overload;

    class function IsNull(APropName: string): TCustomCriterion; overload;
    class function IsNull(AProjection: TSimpleProjection): TCustomCriterion; overload;

    class function IsNotNull(APropName: string): TCustomCriterion; overload;
    class function IsNotNull(AProjection: TSimpleProjection): TCustomCriterion; overload;

    class function Or_(ALeft, ARight: TCustomCriterion): TCustomCriterion;
    class function And_(ALeft, ARight: TCustomCriterion): TCustomCriterion;
    class function Not_(Condition: TCustomCriterion): TCustomCriterion;

    class function Sql(ASQL: string): TCustomCriterion; overload;
    class function Sql<T>(ASQL: string; const AValue: T): TCustomCriterion; overload;
    class function Sql<T1, T2>(ASQL: string; const AValue1: T1; const AValue2: T2): TCustomCriterion; overload;
  end;

implementation
uses
  Rtti;

{ TExpression }

class function TExpression.And_(ALeft,
  ARight: TCustomCriterion): TCustomCriterion;
begin
  result := TAndExpression.Create(ALeft, ARight);
end;

class function TExpression.Eq(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Eq(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.Ge(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Ge(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.GreaterOrEqual(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Ge(APropName, AValue);
end;

class function TExpression.GreaterThan(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Gt(APropName, AValue);
end;

class function TExpression.Gt(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Gt(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.IsNotNull(APropName: string): TCustomCriterion;
begin
  Result := IsNotNull(TPropertyProjection.Create(APropName));
end;

class function TExpression.IsNull(APropName: string): TCustomCriterion;
begin
  Result := IsNull(TPropertyProjection.Create(APropName));
end;

class function TExpression.Le(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Le(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.LessOrEqual(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Le(APropName, AValue);
end;

class function TExpression.LessThan(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Lt(APropName, AValue);
end;

class function TExpression.Like(APropName, AValue: string): TCustomCriterion;
begin
  Result := Like(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.Lt(APropName: string;
  AValue: Variant): TSimpleExpression;
begin
  result := Lt(TPropertyProjection.Create(APropName), AValue);
end;

class function TExpression.Or_(ALeft,
  ARight: TCustomCriterion): TCustomCriterion;
begin
  Result := TOrExpression.Create(ALeft, ARight);
end;

class function TExpression.Sql(ASQL: string): TCustomCriterion;
begin
  Result := TSqlCriterion.Create(ASQL, [], []);
end;

class function TExpression.Sql<T1, T2>(ASQL: string; const AValue1: T1;
  const AValue2: T2): TCustomCriterion;
begin
  Result := TSqlCriterion.Create(ASQL,
    [TValue.From<T1>(AValue1), TValue.From<T2>(AValue2)],
    [TypeInfo(T1), TypeInfo(T2)]);
end;

class function TExpression.Sql<T>(ASQL: string;
  const AValue: T): TCustomCriterion;
begin
  Result := TSqlCriterion.Create(ASQL, [TValue.From<T>(AValue)], [TypeInfo(T)]);
end;

class function TExpression.Eq(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.Equal);
end;

class function TExpression.Eq(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := TProjectionExpression.Create(ALeft, ARight, TExpressionOperator.Equal);
end;

class function TExpression.Ge(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.GreaterEqual);
end;

class function TExpression.GreaterOrEqual(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := Ge(AProjection, AValue);
end;

class function TExpression.GreaterThan(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := Gt(AProjection, AValue);
end;

class function TExpression.Gt(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.Greater);
end;

class function TExpression.IdEq(AValue: Variant): TCustomCriterion;
begin
  Result := TIdentifierEqExpression.Create(AValue);
end;

class function TExpression.IsNotNull(
  AProjection: TSimpleProjection): TCustomCriterion;
begin
  Result := TNotNullExpression.Create(AProjection);
end;

class function TExpression.IsNull(AProjection: TSimpleProjection): TCustomCriterion;
begin
  Result := TNullExpression.Create(AProjection);
end;

class function TExpression.Le(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.LessEqual);
end;

class function TExpression.LessOrEqual(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := Le(AProjection, AValue);
end;

class function TExpression.LessOrEqual(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := Le(ALeft, ARight);
end;

class function TExpression.LessThan(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := Lt(ALeft, ARight);
end;

class function TExpression.LessThan(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := Lt(AProjection, AValue);
end;

class function TExpression.Like(AProjection: TSimpleProjection;
  AValue: string): TCustomCriterion;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.Like);
end;

class function TExpression.Lt(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := TProjectionExpression.Create(ALeft, ARight, TExpressionOperator.Less);
end;

class function TExpression.Lt(AProjection: TSimpleProjection;
  AValue: Variant): TSimpleExpression;
begin
  Result := TSimpleExpression.Create(AProjection, AValue, TExpressionOperator.Less);
end;

class function TExpression.Not_(Condition: TCustomCriterion): TCustomCriterion;
begin
  result := TNotExpression.Create(Condition);
end;

class function TExpression.Gt(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := TProjectionExpression.Create(ALeft, ARight, TExpressionOperator.Greater);
end;

class function TExpression.GreaterOrEqual(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := Ge(ALeft, ARight);
end;

class function TExpression.GreaterThan(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := Gt(ALeft, ARight);
end;

class function TExpression.Ge(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := TProjectionExpression.Create(ALeft, ARight, TExpressionOperator.GreaterEqual);
end;

class function TExpression.Le(ALeft, ARight: TSimpleProjection): TCustomCriterion;
begin
  Result := TProjectionExpression.Create(ALeft, ARight, TExpressionOperator.LessEqual);
end;

end.
