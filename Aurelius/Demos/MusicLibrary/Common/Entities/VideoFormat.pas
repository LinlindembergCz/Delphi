
unit VideoFormat;

interface

uses
  Aurelius.Mapping.Attributes;

type
  [Entity]
  [Table('VIDEO_FORMATS')]
  [Sequence('SEQ_VIDEO_FORMATS')]
  [Id('FId', TIdGenerator.IdentityOrSequence)]
  TVideoFormat = class
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

