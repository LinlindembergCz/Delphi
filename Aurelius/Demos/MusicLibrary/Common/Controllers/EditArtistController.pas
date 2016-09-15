unit EditArtistController;

interface
uses
  Generics.Collections,
  Aurelius.Engine.ObjectManager,
  Artist;

type
  TEditArtistController = class
  private
    FManager: TObjectManager;
    FArtist: TArtist;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveArtist(Artist: TArtist);
    procedure Load(ArtistId: Variant);
    property Artist: TArtist read FArtist;
  end;

implementation
uses
  DBConnection;

{ TEditArtistController }

constructor TEditArtistController.Create;
begin
  FArtist := TArtist.Create;
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

destructor TEditArtistController.Destroy;
begin
  if not FManager.IsAttached(FArtist) then
    FArtist.Free;
  FManager.Free;
  inherited;
end;

procedure TEditArtistController.Load(ArtistId: Variant);
begin
  if not FManager.IsAttached(FArtist) then
    FArtist.Free;
  FArtist := FManager.Find<TArtist>(ArtistId);
end;

procedure TEditArtistController.SaveArtist(Artist: TArtist);
begin
  if not FManager.IsAttached(Artist) then
    FManager.SaveOrUpdate(Artist);
  FManager.Flush;
end;

end.

