unit Entities;

interface
uses
  Aurelius.Mapping.Attributes;

type
  [Entity]
  [Automapping]
  TPerson = class
  private
    FId: integer;
    FLastName: string;
    FFirstName: string;
    FEmail: string;
  public
    property Id: integer read FId;
    property LastName: string read FLastName write FLastName;
    property FirstName: string read FFirstName write FFirstName;
    property Email: string read FEmail write FEmail;
  end;

implementation

end.
