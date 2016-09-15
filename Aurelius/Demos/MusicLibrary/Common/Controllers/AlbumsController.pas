
unit AlbumsController;

interface

uses
  MediaFile, Generics.Collections,
  Aurelius.Engine.ObjectManager;

type
  TAlbumsController = class
  private
    FManager: TObjectManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeleteAlbum(Album: TAlbum);
    function GetAllAlbums: TList<TAlbum>;
  end;

implementation

uses
  DBConnection;

{ TAlbunsController }

constructor TAlbumsController.Create;
begin
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

procedure TAlbumsController.DeleteAlbum(Album: TAlbum);
begin
  if not FManager.IsAttached(Album) then
    Album := FManager.Find<TAlbum>(Album.Id);
  FManager.Remove(Album);
end;

destructor TAlbumsController.Destroy;
begin
  FManager.Free;
  inherited;
end;

function TAlbumsController.GetAllAlbums: TList<TAlbum>;
begin
  FManager.Clear;
  Result := FManager.FindAll<TAlbum>;
end;

end.
