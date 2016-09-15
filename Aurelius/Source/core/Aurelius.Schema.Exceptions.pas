unit Aurelius.Schema.Exceptions;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Global.Exceptions,
  Aurelius.Sql.Metadata;

type
  ESchemaImporterNotFound = class(EOPFBaseException)
  public
    constructor Create(const SqlDialect: string);
  end;

  ETableMetadataNotFound = class(EOPFInternalError)
  public
    constructor Create(const ATableName, ASchema: string);
  end;

  EColumnMetadataNotFound = class(EOPFInternalError)
  public
    constructor Create(const AColumnName, ATableName: string);
  end;

implementation


{ ESchemaImporterNotFound }

constructor ESchemaImporterNotFound.Create(const SqlDialect: string);
begin
  inherited CreateFmt('There is no Schema Importer registered for SQL dialect "%s".'#13#10+
    'Add unit Aurelius.Schema.XXX to your project, where XXX is the SQL dialect you want to use', [SqlDialect]);
end;

{ ETableMetadataNotFound }

constructor ETableMetadataNotFound.Create(const ATableName, ASchema: string);
begin
  inherited CreateFmt('Table "%s" not found in database metadata', [ATableName]);
end;

{ EColumnMetadataNotFoud }

constructor EColumnMetadataNotFound.Create(const AColumnName,
  ATableName: string);
begin
  inherited CreateFmt('Column "%s" not found in table metadata "%s"', [AColumnName, ATableName]);
end;

end.
