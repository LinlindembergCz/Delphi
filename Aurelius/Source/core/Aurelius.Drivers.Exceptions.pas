unit Aurelius.Drivers.Exceptions;

{$I Aurelius.inc}

interface

uses
  Aurelius.Global.Exceptions;

type
  ETransactionNotOpen = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EConnectionNotOpen = class(EOPFBaseException)
  public
    constructor Create;
  end;

implementation

{ ETransactionNotOpen }

constructor ETransactionNotOpen.Create;
begin
  inherited Create('Cannot commit or rollback. Transaction is not open.');
end;

{ EConnectionNotOpen }

constructor EConnectionNotOpen.Create;
begin
  inherited Create('Cannot perform operation, connection is not open.');
end;

end.
