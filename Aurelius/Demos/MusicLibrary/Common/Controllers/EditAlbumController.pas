unit EditAlbumController;

interface
uses
  Generics.Collections,
  Aurelius.Engine.ObjectManager,
  MediaFile;

type
  TEditAlbumController = class
  private
    FManager: TObjectManager;
    FAlbum: TAlbum;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveAlbum(Album: TAlbum);
    procedure Load(AlbumId: Variant);
    property Album: TAlbum read FAlbum;
  end;

implementation
uses
  DBConnection;

{ TEditAlbumController }

constructor TEditAlbumController.Create;
begin
  FAlbum := TAlbum.Create;
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

destructor TEditAlbumController.Destroy;
begin
  if not FManager.IsAttached(FAlbum) then
    FAlbum.Free;
  FManager.Free;
  inherited;
end;

procedure TEditAlbumController.Load(AlbumId: Variant);
begin
  if not FManager.IsAttached(FAlbum) then
    FAlbum.Free;
  FAlbum := FManager.Find<TAlbum>(AlbumId);
end;

procedure TEditAlbumController.SaveAlbum(Album: TAlbum);
begin
  if not FManager.IsAttached(Album) then
    FManager.SaveOrUpdate(Album);
  FManager.Flush;
end;

end.
