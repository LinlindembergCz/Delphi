unit Aurelius.Id.Exceptions;

{$I Aurelius.Inc}

interface

uses
  SysUtils,
  Aurelius.Global.Exceptions,
  Aurelius.Sql.BaseTypes;

type
  ECannotGetLastInsertId = class(EOPFBaseException)
  public
    constructor Create(SQLField: TSQLField);
  end;


implementation

{ ECannotGetLastInsertId }

constructor ECannotGetLastInsertId.Create(SQLField: TSQLField);
begin
  inherited CreateFmt('Cannot get Last Insert Id from table %s, field %s.',
    [SQLField.Table.Schema + '.' + SQLField.Table.Name, SQLField.Field]);
end;

end.
