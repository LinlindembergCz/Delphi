unit Aurelius.Sql.Commands;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Sql.BaseTypes;

type
  TDMLCommand = class abstract
  private
    FTable: TSQLTable;
  public
    property Table: TSQLTable read FTable write FTable;
    destructor Destroy; override;
  end;

  TSelectCommand = class(TDMLCommand)
  private
    FSelectFields: TObjectList<TSQLSelectField>;
    FJoins: TObjectList<TSQLJoin>;
    FWhereFields: TObjectList<TSQLWhereField>;
    FGroupByFields: TObjectList<TSQLField>;
    FOrderByFields: TObjectList<TSQLOrderField>;
    FWhereStatement: string;
    FSelectStatement: string;
    FGroupByStatement: string;
    FHavingStatement: string;
    FOrderByStatement: string;
    FFirstRow: integer;
    FMaxRows: integer;
    function GetLastAlias: string;
    function GetNextAlias: string;
    function GetHasLimits: boolean;
    function GetHasFirstRow: boolean;
    function GetHasMaxRows: boolean;
    function GetLastRow: integer;
  public
    property SelectFields: TObjectList<TSQLSelectField> read FSelectFields write FSelectFields;
    property SelectStatement: string read FSelectStatement write FSelectStatement;

    property Joins: TObjectList<TSQLJoin> read FJoins write FJoins;

    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields write FWhereFields;
    property WhereStatement: string read FWhereStatement write FWhereStatement;

    property GroupByFields: TObjectList<TSQLField> read FGroupByFields write FGroupByFields;
    property GroupByStatement: string read FGroupByStatement write FGroupByStatement;

    property HavingStatement: string read FHavingStatement write FHavingStatement;

    property OrderByFields: TObjectList<TSQLOrderField> read FOrderByFields write FOrderByFields;
    property OrderByStatement: string read FOrderByStatement write FOrderByStatement;

    property LastAlias: string read GetLastAlias;
    property NextAlias: string read GetNextAlias;

    property FirstRow: integer read FFirstRow write FFirstRow;
    property MaxRows: integer read FMaxRows write FMaxRows default -1;
    property HasLimits: boolean read GetHasLimits;
    property HasFirstRow: boolean read GetHasFirstRow;
    property HasMaxRows: boolean read GetHasMaxRows;
    property LastRow: integer read GetLastRow;

    function GetAliasFromTable(OriginAlias, DestinyTable: string; FKFields: array of string): string;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TInsertCommand = class(TDMLCommand)
  private
    FInsertFields: TObjectList<TSQLField>;
  public
    property InsertFields: TObjectList<TSQLField> read FInsertFields write FInsertFields;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TUpdateCommand = class(TDMLCommand)
  private
    FUpdateFields: TObjectList<TSQLField>;
    FWhereFields: TObjectList<TSQLWhereField>;
  public
    property UpdateFields: TObjectList<TSQLField> read FUpdateFields write FUpdateFields;
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields write FWhereFields;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TDeleteCommand = class(TDMLCommand)
  private
    FWhereFields: TObjectList<TSQLWhereField>;
  public
    property WhereFields: TObjectList<TSQLWhereField> read FWhereFields write FWhereFields;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TGetNextSequenceValueCommand = class(TDMLCommand)
  private
    FSequenceName: string;
    FIncrement: integer;
  public
    property SequenceName: string read FSequenceName write FSequenceName;
    property Increment: integer read FIncrement write FIncrement;
  end;

implementation

uses
  Classes,
  Aurelius.Global.Utils,
  Aurelius.Sql.Exceptions;

{ TInsertCommand }

constructor TInsertCommand.Create;
begin
  FInsertFields := TObjectList<TSQLField>.Create;
end;

destructor TInsertCommand.Destroy;
begin
  FInsertFields.Free;
  inherited;
end;

{ TUpdateCommand }

constructor TUpdateCommand.Create;
begin
  FUpdateFields := TObjectList<TSQLField>.Create;
  FWhereFields := TObjectList<TSQLWhereField>.Create;
end;

destructor TUpdateCommand.Destroy;
begin
  FUpdateFields.Free;
  FWhereFields.Free;
  inherited;
end;

{ TDeleteCommand }

constructor TDeleteCommand.Create;
begin
  FWhereFields := TObjectList<TSQLWhereField>.Create;
end;

destructor TDeleteCommand.Destroy;
begin
  FWhereFields.Free;
  inherited;
end;

{ TSelectCommand }

constructor TSelectCommand.Create;
begin
  FSelectFields := TObjectList<TSQLSelectField>.Create;
  FJoins := TObjectList<TSQLJoin>.Create;
  FWhereFields := TObjectList<TSQLWhereField>.Create;
  FGroupByFields := TObjectList<TSQLField>.Create;
  FOrderByFields := TObjectList<TSQLOrderField>.Create;
  FMaxRows := -1;
end;

destructor TSelectCommand.Destroy;
begin
  FSelectFields.Free;
  FJoins.Free;
  FWhereFields.Free;
  FGroupByFields.Free;
  FOrderByFields.Free;
  inherited;
end;

function TSelectCommand.GetAliasFromTable(OriginAlias,
  DestinyTable: string; FKFields: array of string): string;
var
  I, J: Integer;
  FKAlias, PKTable: string;
  OrderedParamFields, OrderedJoinFields: TStringList;
begin
  OrderedParamFields := TStringList.Create;
  OrderedJoinFields := TStringList.Create;
  try
    OrderedParamFields.Sorted := True;
    OrderedJoinFields.Sorted := True;
    for I := 0 to Length(FKFields) - 1 do
      OrderedParamFields.Add(FKFields[I]);

    for I := 0 to FJoins.Count - 1 do
    begin
      FKAlias := FJoins[I].Segments[0].FKField.Table.Alias;
      PKTable := FJoins[I].Segments[0].PKField.Table.Name;

      if (FKAlias = OriginAlias) and (PKTable = DestinyTable) then
      begin
        OrderedJoinFields.Clear;
        for J := 0 to FJoins[I].Segments.Count - 1 do
          OrderedJoinFields.Add(FJoins[I].Segments[J].FKField.Field);

        if OrderedJoinFields.Equals(OrderedParamFields) then
          Exit(FJoins[I].Segments[0].PKField.Table.Alias);
      end;
    end;

    raise ESQLInternalError.CreateFmt(
      'No alias found for table "%s". Origin alias="%s", FKFields="%s"',
      [DestinyTable, OriginAlias, TUtils.ConcatString(FKFields)]);
  finally
    OrderedParamFields.Free;
    OrderedJoinFields.Free;
  end;
end;

function TSelectCommand.GetHasFirstRow: boolean;
begin
  Result := FirstRow > 0;
end;

function TSelectCommand.GetHasLimits: boolean;
begin
  Result := HasFirstRow or HasMaxRows;
end;

function TSelectCommand.GetHasMaxRows: boolean;
begin
  Result := MaxRows > -1;
end;

function TSelectCommand.GetLastAlias: string;
var
  I: Integer;
  Current: string;
begin
  if FTable = nil then
    Result := ''
  else
    Result := FTable.Alias;

  for I := 0 to FJoins.Count - 1 do
  begin
    Current := FJoins[I].Segments[0].PKField.Table.Alias;

    if TUtils.AliasToInt(Current) > TUtils.AliasToInt(Result) then
      Result := Current;
  end;
end;

function TSelectCommand.GetLastRow: integer;
begin
  Result := MaxInt - 1;
  if HasMaxRows then
  begin
    if HasFirstRow then
      Result := FirstRow + MaxRows - 1
    else
      Result := MaxRows - 1;

    if Result < FirstRow - 1 then
      Result := FirstRow - 1;
  end;
end;

function TSelectCommand.GetNextAlias: string;
begin
  Result := TUtils.IntToAlias(TUtils.AliasToInt(GetLastAlias) + 1);
end;

{ TDMLCommand }

destructor TDMLCommand.Destroy;
begin
  FTable.Free;
  inherited;
end;

end.
