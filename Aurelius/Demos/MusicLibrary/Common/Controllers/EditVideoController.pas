
unit EditVideoController;

interface

uses
  Generics.Collections,
  Video, MediaFile, Artist, VideoFormat,
  Aurelius.Engine.ObjectManager;

type
  TEditVideoController = class
  private
    FManager: TObjectManager;
    FVideo: TVideo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveVideo(Video: TVideo);
    procedure Load(VideoId: Variant);
    function GetAlbums: TList<TAlbum>;
    function GetArtists: TList<TArtist>;
    function GetVideoFormats: TList<TVideoFormat>;
    property Video: TVideo read FVideo;
  end;

implementation

uses
  DBConnection;

{ TMusicasController }

constructor TEditVideoController.Create;
begin
  FVideo := TVideo.Create;
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

destructor TEditVideoController.Destroy;
begin
  if not FManager.IsAttached(FVideo) then
    FVideo.Free;
  FManager.Free;
  inherited;
end;

function TEditVideoController.GetAlbums: TList<TAlbum>;
begin
  Result := FManager.FindAll<TAlbum>;
end;

function TEditVideoController.GetArtists: TList<TArtist>;
begin
  Result := FManager.FindAll<TArtist>;
end;

function TEditVideoController.GetVideoFormats: TList<TVideoFormat>;
begin
  Result := FManager.FindAll<TVideoFormat>;
end;

procedure TEditVideoController.Load(VideoId: Variant);
begin
  if not FManager.IsAttached(FVideo) then
    FVideo.Free;
  FVideo := FManager.Find<TVideo>(VideoId);
end;

procedure TEditVideoController.SaveVideo(Video: TVideo);
begin
  if not FManager.IsAttached(Video) then
    FManager.SaveOrUpdate(Video);
  FManager.Flush;
end;

end.
