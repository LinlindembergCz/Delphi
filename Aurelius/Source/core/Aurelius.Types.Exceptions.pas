unit Aurelius.Types.Exceptions;

{$I Aurelius.inc}

interface

uses
  SysUtils, Rtti,
  Aurelius.Global.Exceptions;

type
  ENullValueException = class(EOPFBaseException)
  public
    constructor Create;
  end;

  ENullConvertException<T> = class(EOPFBaseException)
  public
    constructor Create;
  end;

  EObjectManagerNotSet = class(EOPFBaseException)
  public
    constructor Create(Clazz: TClass);
  end;

  EBlobObjectManagerNotSet = class(EOPFBaseException)
  public
    constructor Create;
  end;

  ETypesInternalError = class(EOPFInternalError)
  end;

implementation

{ ENullValueException }

constructor ENullValueException.Create;
begin
  inherited Create('Nullable: Cannot operate with SNull value.');
end;

{ ENullConvertException<T> }

constructor ENullConvertException<T>.Create;
var
  Context: TRttiContext;
  StrType: string;
begin
  Context := TRttiContext.Create;
  try
    StrType := Context.GetType(TypeInfo(T)).ToString;
  finally
    Context.Free;
  end;

  inherited Create('Nullable: Cannot convert SNull into ' + StrType + '.');
end;

{ EObjectManagerNotSet }

constructor EObjectManagerNotSet.Create(Clazz: TClass);
begin
  inherited CreateFmt('Proxy: Object Manager not set on proxy attribute ' +
    'pointing to class %s.', [Clazz.ClassName]);
end;

{ EBlobObjectManagerNotSet }

constructor EBlobObjectManagerNotSet.Create;
begin
  inherited Create('Blob: Object Manager not set on blob attribute');
end;

end.
