
unit MediaFile;

interface

uses
  Generics.Collections,
  Artist,
  Aurelius.Mapping.Attributes,
  Aurelius.Types.Nullable,
  Aurelius.Types.Proxy;

type
  TAlbum = class;

  [Entity]
  [Table('MEDIA_FILES')]
  [Sequence('SEQ_MEDIA_FILES')]
  [Inheritance(TInheritanceStrategy.SingleTable)]
  [DiscriminatorColumn('MEDIA_TYPE', TDiscriminatorType.dtString)]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TMediaFile = class
  public
    [Column('ID', [TColumnProp.Unique, TColumnProp.Required, TColumnProp.NoUpdate])]
    FId: Integer;
    FMediaName: string;
    FFileLocation: string;
    FDuration: Nullable<integer>;

    [Association([TAssociationProp.Lazy], [])]
    [JoinColumn('ID_ALBUM', [])]
    FAlbum: Proxy<TAlbum>;

    [Association([TAssociationProp.Lazy], [])]
    [JoinColumn('ID_ARTIST', [])]
    FArtist: Proxy<TArtist>;

    function GetAlbum: TAlbum;
    function GetArtist: TArtist;
    procedure SetAlbum(const Value: TAlbum);
    procedure SetArtist(const Value: TArtist);
    function GetDurationAsString: string;
  public
    property Id: integer read FId;

    [Column('MEDIA_NAME', [TColumnProp.Required], 100)]
    property MediaName: string read FMediaName write FMediaName;

    [Column('FILE_LOCATION', [], 300)]
    property FileLocation: string read FFileLocation write FFileLocation;

    [Column('DURATION', [])]
    property Duration: Nullable<integer> read FDuration write FDuration;

    property Album: TAlbum read GetAlbum write SetAlbum;
    property Artist: TArtist read GetArtist write SetArtist;

    property DurationAsString: string read GetDurationAsString;
  end;

  [Entity]
  [Table('ALBUMS')]
  [Sequence('SEQ_ALBUMS')]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TAlbum = class
  private
    [Column('ID', [TColumnProp.Unique, TColumnProp.Required, TColumnProp.NoUpdate])]
    FId: Integer;
    FAlbumName: string;
    FReleaseYear: Nullable<Integer>;
    FMediaFiles: TList<TMediaFile>;
    function GetDuration: Nullable<integer>;
    function GetDurationAsString: string;
  public
    property Id: integer read FId;

    [Column('ALBUM_NAME', [TColumnProp.Required], 100)]
    property AlbumName: string read FAlbumName write FAlbumName;

    [Column('RELEASE_YEAR', [])]
    property ReleaseYear: Nullable<Integer> read FReleaseYear write FReleaseYear;

    [ManyValuedAssociation([], CascadeTypeAll, 'FAlbum')]
    property MediaFiles: TList<TMediaFile> read FMediaFiles write FMediaFiles;

    property Duration: Nullable<integer> read GetDuration;

    property DurationAsString: string read GetDurationAsString;

    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses
  SysUtils;

{ TMediaFile }

function TMediaFile.GetAlbum: TAlbum;
begin
  Result := FAlbum.Value;
end;

function TMediaFile.GetArtist: TArtist;
begin
  Result := FArtist.Value;
end;

function TMediaFile.GetDurationAsString: string;
begin
  if Duration.HasValue then
    Result := Format(
      '%s:%s',
      [FormatFloat('#0', Duration.Value div 60),
      FormatFloat('00', Duration.Value mod 60)
      ])
  else
    Result := '';
end;

procedure TMediaFile.SetAlbum(const Value: TAlbum);
begin
  FAlbum.Value := Value;
end;

procedure TMediaFile.SetArtist(const Value: TArtist);
begin
  FArtist.Value := Value;
end;

{ TAlbum }

constructor TAlbum.Create;
begin
  FMediaFiles := TList<TMediaFile>.Create;
end;

destructor TAlbum.Destroy;
begin
  FMediaFiles.Free;
  inherited;
end;

function TAlbum.GetDuration: Nullable<integer>;
var
  I: Integer;
begin
  if FMediaFiles.Count = 0 then
    Exit(SNULL);

  Result := 0;
  for I := 0 to FMediaFiles.Count - 1 do
  begin
    if FMediaFiles[I].Duration.IsNull then
      Exit(SNULL);
    Result := Result.Value + FMediaFiles[I].Duration.Value;
  end;
end;

function TAlbum.GetDurationAsString: string;
begin
  if Duration.HasValue then
    Result := Format(
      '%s:%s',
      [FormatFloat('#0', Duration.Value div 60),
      FormatFloat('00', Duration.Value mod 60)
      ])
  else
    Result := '';
end;

end.
