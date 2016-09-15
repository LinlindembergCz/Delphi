unit Aurelius.Criteria.Linq;

{$I Aurelius.inc}

interface
uses
  Aurelius.Criteria.Base;

type
  TLinqExpression = record
  private
    FCriterion: TCustomCriterion;
  public
    class operator Implicit(ASource: TCustomCriterion): TLinqExpression;
    class operator Implicit(ASource: TLinqExpression): TCustomCriterion;
    class operator LogicalAnd(ALeft, ARight: TLinqExpression): TLinqExpression;
    class operator LogicalOr(ALeft, ARight: TLinqExpression): TLinqExpression;
    class operator LogicalNot(AExpr: TLinqExpression): TLinqExpression;
  end;

  TLinq = class
  public
    class function Eq(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function Eq(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function Eq(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function IdEq(AValue: Variant): TLinqExpression;

    class function GreaterThan(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function GreaterThan(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function GreaterThen(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function Gt(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function Gt(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function Gt(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function GreaterOrEqual(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function GreaterOrEqual(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function GreaterOrEqual(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function Ge(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function Ge(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function Ge(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function LessThan(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function LessThan(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function LessThan(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function Lt(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function Lt(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function Lt(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function LessOrEqual(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function LessOrEqual(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function LessOrEqual(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function Le(APropName: string; AValue: Variant): TLinqExpression; overload;
    class function Le(AProjection: TSimpleProjection; AValue: Variant): TLinqExpression; overload;
    class function Le(ALeft, ARight: TSimpleProjection): TLinqExpression; overload;

    class function Like(APropName: string; AValue: string): TLinqExpression; overload;
    class function Like(AProjection: TSimpleProjection; AValue: string): TLinqExpression; overload;

    class function IsNull(APropName: string): TLinqExpression; overload;
    class function IsNull(AProjection: TSimpleProjection): TLinqExpression; overload;

    class function IsNotNull(APropName: string): TLinqExpression; overload;
    class function IsNotNull(AProjection: TSimpleProjection): TLinqExpression; overload;

    class function Sql(ASQL: string): TLinqExpression; overload;
    class function Sql<T>(ASQL: string; const AValue: T): TLinqExpression; overload;
    class function Sql<T1, T2>(ASQL: string; const AValue1: T1; const AValue2: T2): TLinqExpression; overload;
  end;

implementation
uses
  Rtti,
  Aurelius.Criteria.Expression;

{ TExpression }

class function TLinq.Eq(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.Eq(APropName, AValue);
end;

class function TLinq.Ge(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.Ge(APropName, AValue);
end;

class function TLinq.GreaterOrEqual(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.GreaterOrEqual(APropName, AValue);
end;

class function TLinq.GreaterThan(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.GreaterThan(APropName, AValue);
end;

class function TLinq.Gt(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.Gt(APropName, AValue);
end;

class function TLinq.IsNotNull(APropName: string): TLinqExpression;
begin
  Result := TExpression.IsNotNull(APropName);
end;

class function TLinq.IsNull(APropName: string): TLinqExpression;
begin
  Result := TExpression.IsNull(APropName);
end;

class function TLinq.Le(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.Le(APropName, AValue);
end;

class function TLinq.LessOrEqual(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.LessOrEqual(APropName, AValue);
end;

class function TLinq.LessThan(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.LessThan(APropName, AValue);
end;

class function TLinq.Like(APropName, AValue: string): TLinqExpression;
begin
  Result := TExpression.Like(APropName, AValue);
end;

class function TLinq.Lt(APropName: string;
  AValue: Variant): TLinqExpression;
begin
  result := TExpression.Lt(APropName, AValue);
end;

{ TLinqExpression }

class operator TLinqExpression.Implicit(
  ASource: TCustomCriterion): TLinqExpression;
begin
  Result.FCriterion := ASource;
end;

class operator TLinqExpression.Implicit(
  ASource: TLinqExpression): TCustomCriterion;
begin
  Result := ASource.FCriterion;
end;

class operator TLinqExpression.LogicalAnd(ALeft,
  ARight: TLinqExpression): TLinqExpression;
begin
  Result := TExpression.And_(ALeft, ARight);
end;

class operator TLinqExpression.LogicalNot(
  AExpr: TLinqExpression): TLinqExpression;
begin
  Result := TExpression.Not_(AExpr);
end;

class operator TLinqExpression.LogicalOr(ALeft,
  ARight: TLinqExpression): TLinqExpression;
begin
  Result := TExpression.Or_(ALeft, ARight);
end;

class function TLinq.Eq(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.Eq(AProjection, AValue);
end;

class function TLinq.Eq(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.Eq(ALeft, ARight);
end;

class function TLinq.Ge(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.Ge(AProjection, AValue);
end;

class function TLinq.GreaterOrEqual(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.GreaterOrEqual(AProjection, AValue);
end;

class function TLinq.Ge(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.Ge(ALeft, ARight);
end;

class function TLinq.GreaterOrEqual(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.GreaterOrEqual(ALeft, ARight);
end;

class function TLinq.GreaterThan(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.GreaterThan(AProjection, AValue);
end;

class function TLinq.GreaterThen(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.GreaterThan(ALeft, ARight);
end;

class function TLinq.Gt(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.Gt(ALeft, ARight);
end;

class function TLinq.Gt(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.Gt(AProjection, AValue);
end;

class function TLinq.IdEq(AValue: Variant): TLinqExpression;
begin
  Result := TExpression.IdEq(AValue);
end;

class function TLinq.IsNotNull(AProjection: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.IsNotNull(AProjection);
end;

class function TLinq.IsNull(AProjection: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.IsNull(AProjection);
end;

class function TLinq.Le(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.Le(AProjection, AValue);
end;

class function TLinq.LessOrEqual(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.LessOrEqual(AProjection, AValue);
end;

class function TLinq.LessThan(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.LessThan(ALeft, ARight);
end;

class function TLinq.LessThan(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.LessThan(AProjection, AValue);
end;

class function TLinq.Like(AProjection: TSimpleProjection;
  AValue: string): TLinqExpression;
begin
  Result := TExpression.Like(AProjection, AValue);
end;

class function TLinq.Lt(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.Lt(ALeft, ARight);
end;

class function TLinq.Lt(AProjection: TSimpleProjection;
  AValue: Variant): TLinqExpression;
begin
  Result := TExpression.Lt(AProjection, AValue);
end;

class function TLinq.Sql(ASQL: string): TLinqExpression;
begin
  Result := TSqlCriterion.Create(ASQL, [], []);
end;

class function TLinq.Sql<T1, T2>(ASQL: string; const AValue1: T1; const AValue2: T2): TLinqExpression;
begin
  Result := TSqlCriterion.Create(ASQL,
    [TValue.From<T1>(AValue1), TValue.From<T2>(AValue2)],
    [TypeInfo(T1), TypeInfo(T2)]);
end;

class function TLinq.Sql<T>(ASQL: string; const AValue: T): TLinqExpression;
begin
  Result := TSqlCriterion.Create(ASQL, [TValue.From<T>(AValue)], [TypeInfo(T)]);
end;

class function TLinq.LessOrEqual(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.LessOrEqual(ALeft, ARight);
end;

class function TLinq.Le(ALeft, ARight: TSimpleProjection): TLinqExpression;
begin
  Result := TExpression.Le(ALeft, ARight);
end;

end.
