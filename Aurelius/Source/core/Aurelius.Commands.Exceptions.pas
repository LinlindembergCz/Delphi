unit Aurelius.Commands.Exceptions;

{$I Aurelius.inc}

interface

uses
  Aurelius.Global.Exceptions;

type
  ESelectAlreadyOpen = class(EOPFBaseException)
  public
    constructor Create;
  end;

  ESelectNotOpen = class(EOPFBaseException)
    constructor Create;
  end;

  ECursorNotFetching = class(EOPFBaseException)
    constructor Create;
  end;

  ENilObjectReturned = class(EOPFBaseException)
    constructor Create;
  end;

implementation

{ ESelectAlreadyOpen }

constructor ESelectAlreadyOpen.Create;
begin
  inherited Create('Cannot start selecting objects. A select operation has been already started.');
end;

{ ESelectNotOpen }

constructor ESelectNotOpen.Create;
begin
  inherited Create('Cannot fetch object. A select operation has not been started.');
end;

{ ECursorNotFetching }

constructor ECursorNotFetching.Create;
begin
  inherited Create('Cannot fetch object, cursor fetching has not started.');
end;

{ ENilObjectReturned }

constructor ENilObjectReturned.Create;
begin
  inherited Create('No object returned from database. Check if id column in database has a valid value.');
end;

end.
