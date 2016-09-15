unit Aurelius.Bind.Exceptions;

{$I Aurelius.inc}

interface

uses
  Aurelius.Global.Exceptions;

type
  EInvalidListObject = class(EOPFBaseException)
  public
    constructor Create(Obj: TObject);
  end;

  ESourceListNotAssigned = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EObjectClassNotSpecified = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EMasterRecordNotAvailable = class(EOPFBaseException)
  public
    constructor Create;
  end;

  ECannotSetProperty = class(EOPFBaseException)
  public
    constructor Create(APropName: string);
  end;

implementation

{ EInvalidListObject }

constructor EInvalidListObject.Create(Obj: TObject);
begin
  inherited Create('Specified object is not a valid list object. ' +
    'You must provide a TList<T> object');
end;

{ ESourceListNotAssigned }

constructor ESourceListNotAssigned.Create;
begin
  inherited Create('Source list not assigned. Please specify an object list where data should come from.');
end;

{ EMasterRecordNotAvailable }

constructor EMasterRecordNotAvailable.Create;
begin
  inherited Create('Cannot open nested dataset. Master record not available.');
end;

{ EObjectClassNotSpecified }

constructor EObjectClassNotSpecified.Create;
begin
  inherited Create('Object class not specified. Please specify the base class which represents all records in dataset. ');
end;

{ ECannotSetProperty }

constructor ECannotSetProperty.Create(APropName: string);
begin
  inherited CreateFmt('Cannot set value of property "%s". Either property name is invalid or subproperty is inaccessible', [APropName]);
end;

end.
