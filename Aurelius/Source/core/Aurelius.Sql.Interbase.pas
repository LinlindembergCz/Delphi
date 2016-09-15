unit Aurelius.Sql.Interbase;

{$I Aurelius.inc}

interface
uses
  Aurelius.Sql.Firebird;

type
  TInterbaseSQLGenerator = class(TFirebirdSQLGenerator)
  protected
    function GetGeneratorName: string; override;
    function GetSqlDialect: string; override;
  end;


implementation
uses
  Aurelius.Sql.Register;

{ TInterbaseSQLGenerator }

function TInterbaseSQLGenerator.GetGeneratorName: string;
begin
  Result := 'Interbase SQL Generator';
end;

function TInterbaseSQLGenerator.GetSqlDialect: string;
begin
  Result := 'Interbase';
end;

initialization
  TSQLGeneratorRegister.GetInstance.RegisterGenerator(TInterbaseSQLGenerator.Create);

end.
