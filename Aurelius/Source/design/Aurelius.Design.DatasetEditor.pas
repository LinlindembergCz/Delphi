unit Aurelius.Design.DatasetEditor;

interface
uses
  DBReg, DSDesign;

type
  TAureliusDataSetEditor = class(TDataSetEditor)
  protected
    function GetDSDesignerClass: TDSDesignerClass; override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation
uses
  DB,
  Aurelius.Design.DatasetDesigner;

{ TObjectDataSetEditor }

procedure TAureliusDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index - inherited GetVerbCount of
    0: TfmFieldLoader.Execute(GetComponent as TDataset);
  else
    inherited ExecuteVerb(Index);
  end;
end;

function TAureliusDataSetEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TObjectDatasetDesigner;
end;

function TAureliusDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index - inherited GetVerbCount of
    0: Result := 'Load Field Definitions...';
  else
    Result := inherited GetVErb(Index);
  end;
end;

function TAureliusDataSetEditor.GetVerbCount: Integer;
begin
  Result := 1 + inherited GetVerbCount;
end;

end.
