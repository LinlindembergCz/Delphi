unit Aurelius.Schema.Register;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Schema.Interfaces;

type
  TSchemaImporterRegister = class
  private
    class var FInstance: TSchemaImporterRegister;
  private
    FImporters: TDictionary<string, ISchemaImporter>;
    procedure PrivateCreate;
    procedure PrivateDestroy;
  public
    class function GetInstance: TSchemaImporterRegister;

    procedure RegisterImporter(const SqlDialect: string; SchemaImporter: ISchemaImporter);
    function GetImporter(const SqlDialect: string): ISchemaImporter;
  end;

implementation

uses
  Aurelius.Schema.Exceptions,
  SysUtils;

{ TSchemaImporterRegister }

procedure TSchemaImporterRegister.RegisterImporter(const SqlDialect: string;
  SchemaImporter: ISchemaImporter);
begin
  if not FImporters.ContainsKey(UpperCase(SqlDialect)) then
    FImporters.Add(UpperCase(SqlDialect), SchemaImporter)
  else
    FImporters[UpperCase(SqlDialect)] := SchemaImporter;
end;

function TSchemaImporterRegister.GetImporter(const SqlDialect: string): ISchemaImporter;
begin
  if not FImporters.ContainsKey(UpperCase(SqlDialect)) then
    raise ESchemaImporterNotFound.Create(SqlDialect);

  Result := FImporters[UpperCase(SqlDialect)];
end;

class function TSchemaImporterRegister.GetInstance: TSchemaImporterRegister;
begin
  if FInstance = nil then
  begin
    FInstance := TSchemaImporterRegister.Create;
    FInstance.PrivateCreate;
  end;
  Result := FInstance;
end;

procedure TSchemaImporterRegister.PrivateCreate;
begin
  FImporters := TDictionary<string, ISchemaImporter>.Create;
end;

procedure TSchemaImporterRegister.PrivateDestroy;
begin
  FImporters.Free;
end;

initialization

finalization
  if TSchemaImporterRegister.FInstance <> nil then
  begin
    TSchemaImporterRegister.FInstance.PrivateDestroy;
    TSchemaImporterRegister.FInstance.Free;
    TSchemaImporterRegister.FInstance := nil;
  end;

end.
