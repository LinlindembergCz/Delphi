unit Aurelius.Criteria.Projections;

{$I Aurelius.inc}

interface
uses
  Aurelius.Criteria.Base;

type
  TProjections = class sealed
  public
    class function Sum(APropName: string): TSimpleProjection; overload;
    class function Sum(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Max(APropName: string): TSimpleProjection; overload;
    class function Max(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Min(APropName: string): TSimpleProjection; overload;
    class function Min(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Avg(APropName: string): TSimpleProjection; overload;
    class function Avg(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Count(APropName: string): TSimpleProjection; overload;
    class function Count(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Prop(APropName: string): TSimpleProjection;
    class function Group(APropName: string): TSimpleProjection; overload;
    class function Group(AProjection: TSimpleProjection): TSimpleProjection; overload;
    class function Condition(ACondition: TCustomCriterion; AWhenTrue, AWhenFalse: TSimpleProjection): TSimpleProjection;
    class function Value<T>(const AValue: T): TSimpleProjection;
    class function Literal<T>(const AValue: T): TSimpleProjection;
    class function ProjectionList: TProjectionList; overload;
    class function Alias(AProjection: TSimpleProjection; AProjectionAlias: string): TAliasedProjection;
    class function Sql<TResult>(ASQL: string): TSimpleProjection;
  end;

implementation

{ TProjections }

class function TProjections.Alias(AProjection: TSimpleProjection;
  AProjectionAlias: string): TAliasedProjection;
begin
  Result := TAliasedProjection.Create(AProjection, AProjectionAlias);
end;

class function TProjections.Avg(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('avg', AProjection);
end;

class function TProjections.Avg(APropName: string): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('avg', Prop(APropName));
end;

class function TProjections.Count(APropName: string): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('count', Prop(APropName));
end;

class function TProjections.Condition(ACondition: TCustomCriterion; AWhenTrue,
  AWhenFalse: TSimpleProjection): TSimpleProjection;
begin
  Result := TConditionalProjection.Create(ACondition, AWhenTrue, AWhenFalse);
end;

class function TProjections.Count(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('count', AProjection);
end;

class function TProjections.Group(APropName: string): TSimpleProjection;
begin
  Result := TGroupProjection.Create(Prop(APropName));
end;

class function TProjections.Group(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TGroupProjection.Create(AProjection);
end;

class function TProjections.Literal<T>(const AValue: T): TSimpleProjection;
begin
  Result := TConstantProjection<T>.Create(AValue, True);
end;

class function TProjections.Max(APropName: string): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('max', Prop(APropName));
end;

class function TProjections.Max(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('max', AProjection);
end;

class function TProjections.Min(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('min', AProjection);
end;

class function TProjections.ProjectionList: TProjectionList;
begin
  Result := TProjectionList.Create;
end;

class function TProjections.Prop(APropName: string): TSimpleProjection;
begin
  Result := TPropertyProjection.Create(APropName);
end;

class function TProjections.Min(APropName: string): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('min', Prop(APropName));
end;

class function TProjections.Sql<TResult>(ASQL: string): TSimpleProjection;
begin
  Result := TSqlProjection.Create(ASQL, TypeInfo(TResult));
end;

class function TProjections.Sum(AProjection: TSimpleProjection): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('sum', AProjection);
end;

class function TProjections.Value<T>(const AValue: T): TSimpleProjection;
begin
  Result := TConstantProjection<T>.Create(AValue);
end;

class function TProjections.Sum(APropName: string): TSimpleProjection;
begin
  Result := TAggregateProjection.Create('sum', Prop(APropName));
end;

end.
