unit Aurelius.Global.Exceptions;

{$I Aurelius.inc}

interface

uses
  SysUtils;

type
  EOPFBaseException = class(Exception)
  end;

  EOPFInternalError = class(Exception)
  public
    constructor Create(Msg: string); reintroduce;
    constructor CreateFmt(Msg: string; Args: array of const); reintroduce;
  end;

  EFeatureNotSupported = class(EOPFBaseException)
  public
    constructor Create(FeatureName: string);
  end;

implementation

{ EOPFInternalError }

constructor EOPFInternalError.Create(Msg: string);
begin
  inherited Create('OPF Internal error: ' + Msg);
end;

constructor EOPFInternalError.CreateFmt(Msg: string; Args: array of const);
begin
  inherited CreateFmt('OPF Internal error: ' + Msg, Args);
end;

{ EFeatureNotSupported }

constructor EFeatureNotSupported.Create(FeatureName: string);
begin
  inherited CreateFmt('%s not yet supported.', [FeatureName]);
end;

end.
