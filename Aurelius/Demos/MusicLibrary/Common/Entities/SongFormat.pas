
unit SongFormat;

interface

uses
  Aurelius.Mapping.Attributes;

type
  [Entity]
  [Table('SONG_FORMATS')]
  [Sequence('SEQ_SONG_FORMATS')]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TSongFormat = class
  private
    [Column('ID', [TColumnProp.Unique, TColumnProp.Required, TColumnProp.NoUpdate])]
    FId: Integer;
    FFormatName: string;
  public
    property Id: integer read FId;

    [Column('FORMAT_NAME', [TColumnProp.Required], 100)]
    property FormatName: string read FFormatName write FFormatName;
  end;

implementation

end.
