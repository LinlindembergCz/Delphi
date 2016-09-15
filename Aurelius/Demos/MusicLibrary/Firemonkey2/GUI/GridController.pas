unit GridController;

interface
uses
  Generics.Collections, FMX.Grid;

type
  TGridFillRow<T> = reference to procedure(Obj: T; Cols: TArray<string>);

  TGridController<T: class> = class
  private
    FGrid: TStringGrid;
    FItems: TList<T>;
    FFillRow: TGridFillRow<T>;
    procedure FillGrid;
    function GetSelected: T;
  public
    constructor Create(AGrid: TStringGrid; AItems: TList<T>; AFillRow: TGridFillRow<T>);
    destructor Destroy; override;
    property Selected: T read GetSelected;
  end;

implementation

{ TGridController<T> }

constructor TGridController<T>.Create(AGrid: TStringGrid; AItems: TList<T>; AFillRow: TGridFillRow<T>);
begin
  FGrid := AGrid;
  FItems := AItems;
  FFillRow := AFillRow;
  FillGrid;
end;

destructor TGridController<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGridController<T>.FillGrid;
var
  Item: T;
  I, J: Integer;
  Cols: TArray<string>;
begin
  for I := 0 to FGrid.RowCount - 1 do
    for J := 0 to FGrid.ColumnCount - 1 do
      FGrid.Cells[J, I] := '';
  FGrid.RowCount := 1;

  if FItems.Count > 0 then
  begin
    FGrid.RowCount := FItems.Count;

    for I := 0 to FItems.Count - 1 do
    begin
      Item := FItems[I];
      SetLength(Cols, FGrid.ColumnCount);
      FFillRow(Item, Cols);
      for J := 0 to FGrid.ColumnCount - 1 do
        FGrid.Cells[J, I] := Cols[J];
    end;
  end;
end;


function TGridController<T>.GetSelected: T;
begin
  if FGrid.Selected < 0 then Exit(nil);
  Result := FItems[FGrid.Selected];
//  Result := T(FGrid.Columns[0].CellControlByRow(FGrid.Selected).Tag);
end;

end.
