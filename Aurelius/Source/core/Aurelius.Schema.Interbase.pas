unit Aurelius.Schema.Interbase;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Drivers.Interfaces,
  Aurelius.Schema.AbstractImporter,
  Aurelius.Schema.Firebird,
  Aurelius.Sql.Metadata;

type
  TInterbaseSchemaImporter = class(TFirebirdSchemaImporter)
  end;

implementation

uses
  Aurelius.Schema.Register;

initialization
  TSchemaImporterRegister.GetInstance.RegisterImporter('Interbase', TInterbaseSchemaImporter.Create);

end.

