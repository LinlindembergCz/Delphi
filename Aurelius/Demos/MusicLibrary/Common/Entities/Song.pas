
unit Song;

interface

uses
  Aurelius.Mapping.Attributes,
  MediaFile,
  SongFormat;

type
  [Entity]
  [DiscriminatorValue('SONG')]
  TSong = class(TMediaFile)
  private
    FSongFormat: TSongFormat;
  public
    [Association([], [])]
    [JoinColumn('ID_SONG_FORMAT', [])]
    property SongFormat: TSongFormat read FSongFormat write FSongFormat;
  end;

implementation

end.
