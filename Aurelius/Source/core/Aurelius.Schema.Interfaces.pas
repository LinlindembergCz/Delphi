unit Aurelius.Schema.Interfaces;

{$I Aurelius.inc}

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Sql.Metadata;

type
  ISchemaImporter = interface
    procedure GetDatabaseMetadata(Connection: IDBConnection; Database: TDatabaseMetadata);
  end;

implementation

end.
