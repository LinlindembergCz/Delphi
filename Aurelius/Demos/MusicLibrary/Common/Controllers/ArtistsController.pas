
unit ArtistsController;

interface

uses
  Generics.Collections,
  Aurelius.Engine.ObjectManager,
  Artist;

type
  TArtistsController = class
  private
    FManager: TObjectManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeleteArtist(Artist: TArtist);
    function GetAllArtists: TList<TArtist>;
  end;

implementation

uses
  DBConnection;

{ TArtistController }

constructor TArtistsController.Create;
begin
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

procedure TArtistsController.DeleteArtist(Artist: TArtist);
begin
  if not FManager.IsAttached(Artist) then
    Artist := FManager.Find<TArtist>(Artist.Id);
  FManager.Remove(Artist);
end;

destructor TArtistsController.Destroy;
begin
  FManager.Free;
  inherited;
end;

function TArtistsController.GetAllArtists: TList<TArtist>;
begin
  FManager.Clear;
  Result := FManager.FindAll<TArtist>;
end;


end.
