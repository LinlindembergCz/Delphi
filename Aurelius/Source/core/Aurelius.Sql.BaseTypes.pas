unit Aurelius.Sql.BaseTypes;

{$I Aurelius.inc}

interface

uses
  Generics.Collections, DB;

type
  TSQLTable = class
  private
    FName: string;
    FSchema: string;
    FAlias: string;
  public
    property Name: string read FName write FName;
    property Schema: string read FSchema write FSchema;
    property Alias: string read FAlias write FAlias;
    function Clone: TSQLTable;
    constructor Create(Name, Alias: string); overload;
    constructor Create(Name, Schema, Alias: string); overload;
  end;

  TSQLField = class
  private
    FTable: TSQLTable;
    FField: string;
  public
    property Table: TSQLTable read FTable write FTable;
    property Field: string read FField write FField;
    constructor Create(Table: TSQLTable; Field: string); virtual;
    destructor Destroy; override;
  end;

  TJoinType = (Inner, Left);

  TSQLJoinSegment = class
  private
    FPKField: TSQLField;
    FFKField: TSQLField;
  public
    property PKField: TSQLField read FPKField write FPKField;
    property FKField: TSQLField read FFKField write FFKField;
    constructor Create(PKField, FKField: TSQLField);
    destructor Destroy; override;
  end;

  TSQLJoin = class
  private
    FJoinType: TJoinType;
    FSegments: TObjectList<TSQLJoinSegment>;
  public
    property Segments: TObjectList<TSQLJoinSegment> read FSegments write FSegments;
    property JoinType: TJoinType read FJoinType write FJoinType;
    constructor Create(JoinType: TJoinType = TJoinType.Inner); virtual;
    destructor Destroy; override;
  end;

  TAggregateFunction = (Count, Sum, Average, Max, Min, List);

  TAggregateFunctionSet = set of TAggregateFunction;

  TSQLSelectField = class(TSQLField)
  private
    FIsAggregated: Boolean;
    FAggregateFuntion: TAggregateFunction;
  public
    property IsAggregated: Boolean read FIsAggregated write FIsAggregated;
    property AggregateFuntion: TAggregateFunction read FAggregateFuntion write FAggregateFuntion;
    constructor Create(Table: TSQLTable; Field: string); overload; override;
    constructor Create(Table: TSQLTable; Field: string; AggregateFunction: TAggregateFunction); reintroduce; overload; virtual;
  end;

  TWhereOperator = (woEqual, woDifferent, woGreater, woLess, woGreaterOrEqual,
    woLessOrEqual, woLike, woIsNull, woIsNotNull, woIn);

  TWhereOperationSet = set of TWhereOperator;

  TSQLWhereField = class(TSQLField)
  private
    FWhereOperator: TWhereOperator;
    FParamName: string;
  public
    property WhereOperator: TWhereOperator read FWhereOperator write FWhereOperator;
    property ParamName: string read FParamName write FParamName;
    constructor Create(Table: TSQLTable; Field: string); overload; override;
    constructor Create(Table: TSQLTable; Field: string; WhereOperator: TWhereOperator); reintroduce; overload; virtual;
  end;

  TOrderDirection = (Ascendant, Descendant);

  TSQLOrderField = class(TSQLField)
  private
    FDirection: TOrderDirection;
  public
    property Direction: TOrderDirection read FDirection write FDirection;
    constructor Create(Table: TSQLTable; Field: string); overload; override;
    constructor Create(Table: TSQLTable; Field: string; Direction: TOrderDirection); reintroduce; overload; virtual;
  end;

implementation

{ TSQLField }

constructor TSQLField.Create(Table: TSQLTable; Field: string);
begin
  FTable := Table;
  FField := Field;
end;

destructor TSQLField.Destroy;
begin
  FTable.Free;
  inherited;
end;

{ TSQLJoin }

constructor TSQLJoin.Create(JoinType: TJoinType);
begin
  FJoinType := JoinType;
  FSegments := TObjectList<TSQLJoinSegment>.Create;
end;

destructor TSQLJoin.Destroy;
begin
  FSegments.Free;
  inherited;
end;

{ TSQLTable }

constructor TSQLTable.Create(Name, Alias: string);
begin
  FName := Name;
  FAlias := Alias;
end;

function TSQLTable.Clone: TSQLTable;
begin
  Result := TSQLTable.Create(FName, FSchema, FAlias);
end;

constructor TSQLTable.Create(Name, Schema, Alias: string);
begin
  Create(Name, Alias);
  FSchema := Schema;
end;

{ TSQLWhereField }

constructor TSQLWhereField.Create(Table: TSQLTable; Field: string;
  WhereOperator: TWhereOperator);
begin
  inherited Create(Table, Field);
  FWhereOperator := WhereOperator;
end;

constructor TSQLWhereField.Create(Table: TSQLTable; Field: string);
begin
  inherited;
end;

{ TSQLOrderField }

constructor TSQLOrderField.Create(Table: TSQLTable; Field: string;
  Direction: TOrderDirection);
begin
  inherited Create(Table, Field);
  FDirection := Direction;
end;

constructor TSQLOrderField.Create(Table: TSQLTable; Field: string);
begin
  inherited;
end;

{ TSQLSelectField }

constructor TSQLSelectField.Create(Table: TSQLTable; Field: string;
  AggregateFunction: TAggregateFunction);
begin
  inherited Create(Table, Field);
  FAggregateFuntion := AggregateFunction;
  FIsAggregated := True;
end;

constructor TSQLSelectField.Create(Table: TSQLTable; Field: string);
begin
  inherited;
end;

{ TSQLJoinSegment }

constructor TSQLJoinSegment.Create(PKField, FKField: TSQLField);
begin
  FPKField := PKField;
  FFKField := FKField;
end;

destructor TSQLJoinSegment.Destroy;
begin
  FPKField.Free;
  FFKField.Free;
  inherited;
end;

end.
