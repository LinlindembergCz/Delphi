unit Aurelius.Id.Uuid;

{$I Aurelius.Inc}

interface
uses
  Aurelius.Id.IdentifierGenerator,
  Aurelius.Sql.Interfaces;

type
  TUuid38Generator = class(TIdentifierGenerator)
  public
    function GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant; override;
  end;

  TUuid36Generator = class(TIdentifierGenerator)
  public
    function GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant; override;
  end;

  TUuid32Generator = class(TIdentifierGenerator)
  public
    function GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant; override;
  end;

implementation
uses
  SysUtils,
  Aurelius.Global.Utils;

{ TUuidGenerator }

function TUuid38Generator.GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant;
var
  S: string;
begin
  S := GuidToString(TUtils.NewGuid);
  Result := S;
end;

{ TUuid36Generator }

function TUuid36Generator.GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant;
var
  S: string;
begin
  S := GuidToString(TUtils.NewGuid);
  S := Copy(S, 2, 36);
  Result := S;
end;

{ TUuid32Generator }

function TUuid32Generator.GenerateId(Entity: TObject; Performer: ICommandPerformer): Variant;
var
  S: string;
begin
  S := GuidToString(TUtils.NewGuid);
  S := Copy(S, 2, 8) + Copy(S, 11, 4) + Copy(S, 16, 4) + Copy(S, 21, 4) + Copy(S, 26, 12);
  Result := S;
end;

end.
