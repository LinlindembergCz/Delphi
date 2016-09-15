unit Aurelius.Sql.Register;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Sql.Interfaces;

type
  TSQLGeneratorRegister = class
  private
    class var FInstance: TSQLGeneratorRegister;
  private
    FAllGenerators: TDictionary<string, ISQLGenerator>;
    procedure PrivateCreate;
    procedure PrivateDestroy;
  public
    class function GetInstance: TSQLGeneratorRegister;

    procedure RegisterGenerator(SQLGenerator: ISQLGenerator);
    function GetGenerator(SqlDialect: string): ISQLGenerator;
  end;

implementation

uses
  Rtti, SysUtils,
  Aurelius.Sql.AbstractSqlGenerator,
  Aurelius.Sql.AnsiSqlGenerator,
  Aurelius.Sql.Exceptions;

{ TSQLGeneratorRegister }

procedure TSQLGeneratorRegister.RegisterGenerator(SQLGenerator: ISQLGenerator);
begin
  if not FAllGenerators.ContainsKey(UpperCase(SQLGenerator.SqlDialect)) then
    FAllGenerators.Add(UpperCase(SQLGenerator.SqlDialect), SQLGenerator)
  else
    FAllGenerators[UpperCase(SQLGenerator.SqlDialect)] := SQLGenerator;
end;

function TSQLGeneratorRegister.GetGenerator(SqlDialect: string): ISQLGenerator;
begin
  if not FAllGenerators.ContainsKey(UpperCase(SqlDialect)) then
    raise ESQLGeneratorNotFound.Create(SqlDialect);

  Result := FAllGenerators[UpperCase(SqlDialect)];
end;

class function TSQLGeneratorRegister.GetInstance: TSQLGeneratorRegister;
begin
  if FInstance = nil then
  begin
    FInstance := TSQLGeneratorRegister.Create;
    FInstance.PrivateCreate;
  end;
  Result := FInstance;
end;

procedure TSQLGeneratorRegister.PrivateCreate;
begin
  FAllGenerators := TDictionary<string, ISQLGenerator>.Create;
end;

procedure TSQLGeneratorRegister.PrivateDestroy;
begin
  FAllGenerators.Free;
end;

initialization

finalization
  if TSQLGeneratorRegister.FInstance <> nil then
  begin
    TSQLGeneratorRegister.FInstance.PrivateDestroy;
    TSQLGeneratorRegister.FInstance.Free;
    TSQLGeneratorRegister.FInstance := nil;
  end;

end.
