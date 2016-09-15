
unit EditSongController;

interface

uses
  Generics.Collections,
  Song, MediaFile, Artist, SongFormat,
  Aurelius.Engine.ObjectManager;

type
  TEditSongController = class
  private
    FManager: TObjectManager;
    FSong: TSong;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSong(Song: TSong);
    procedure Load(SongId: Variant);
    function GetAlbums: TList<TAlbum>;
    function GetArtists: TList<TArtist>;
    function GetSongFormats: TList<TSongFormat>;
    property Song: TSong read FSong;
  end;

implementation

uses
  DBConnection;

{ TMusicasController }

constructor TEditSongController.Create;
begin
  FSong := TSong.Create;
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

destructor TEditSongController.Destroy;
begin
  if not FManager.IsAttached(FSong) then
    FSong.Free;
  FManager.Free;
  inherited;
end;

function TEditSongController.GetAlbums: TList<TAlbum>;
begin
  Result := FManager.FindAll<TAlbum>;
end;

function TEditSongController.GetArtists: TList<TArtist>;
begin
  Result := FManager.FindAll<TArtist>;
end;

function TEditSongController.GetSongFormats: TList<TSongFormat>;
begin
  Result := FManager.FindAll<TSongFormat>;
end;

procedure TEditSongController.Load(SongId: Variant);
begin
  if not FManager.IsAttached(FSong) then
    FSong.Free;
  FSong := FManager.Find<TSong>(SongId);
end;

procedure TEditSongController.SaveSong(Song: TSong);
begin
  if not FManager.IsAttached(Song) then
    FManager.SaveOrUpdate(Song);
  FManager.Flush;
end;

end.
