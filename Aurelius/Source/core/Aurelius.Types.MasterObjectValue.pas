unit Aurelius.Types.MasterObjectValue;

{$I Aurelius.inc}

interface

type
  TMasterObjectAction = (None, Include, Exclude);

  TMasterObjectValue = record
  private
    FMasterObject: TObject;
    FMasterAssocMember: string;
    FMasterObjectAction: TMasterObjectAction;
  public
    property MasterObject: TObject read FMasterObject write FMasterObject;
    property MasterAssocMember: string read FMasterAssocMember write FMasterAssocMember;
    property Action: TMasterObjectAction read FMasterObjectAction write FMasterObjectAction;
  end;

function DummyMasterObject: TMasterObjectValue;

implementation

function DummyMasterObject: TMasterObjectValue;
begin
  Result.MasterObject := nil;
  Result.MasterAssocMember := '';
  Result.Action := TMasterObjectAction.None;
end;

end.
