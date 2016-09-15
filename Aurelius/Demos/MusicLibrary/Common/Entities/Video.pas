
unit Video;

interface

uses
  Aurelius.Mapping.Attributes,
  MediaFile,
  VideoFormat;

type
  [Entity]
  [DiscriminatorValue('VIDEO')]
  TVideo = class(TMediaFile)
  private
    FVideoFormat: TVideoFormat;
  public
    [Association([], [])]
    [JoinColumn('ID_VIDEO_FORMAT', [])]
    property VideoFormat: TVideoFormat read FVideoFormat write FVideoFormat;
  end;

implementation

end.
