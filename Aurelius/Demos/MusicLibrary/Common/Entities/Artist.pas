unit Artist;

interface

uses
  Aurelius.Mapping.Attributes,
  Aurelius.Types.Nullable;

type
  [Entity]
  [Table('ARTISTS')]
  [Sequence('SEQ_ARTISTS')]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TArtist = class
  private
    [Column('ID', [TColumnProp.Unique, TColumnProp.Required, TColumnProp.NoUpdate])]
    FId: Integer;
    FArtistName: string;
    FGenre: Nullable<string>;
    function GetArtistName: string;
    procedure SetArtistName(const Value: string);
  public
    property Id: integer read FId;

    [Column('ARTIST_NAME', [TColumnProp.Required], 100)]
    property ArtistName: string read GetArtistName write SetArtistName;

    [Column('GENRE', [], 100)]
    property Genre: Nullable<string> read FGenre write FGenre;
  end;

implementation

{ TArtist }

function TArtist.GetArtistName: string;
begin
  Result := FArtistName;
end;

procedure TArtist.SetArtistName(const Value: string);
begin
  FArtistName := Value;
end;

end.
