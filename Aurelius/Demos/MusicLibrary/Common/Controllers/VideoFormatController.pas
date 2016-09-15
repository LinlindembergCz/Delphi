
unit VideoFormatController;

interface

uses
  DBConnection, VideoFormat;

type
  TVideoFormatController = class
  public
    procedure SaveFormat(VideoFormat: TVideoFormat);
  end;

implementation

uses
  Aurelius.Engine.ObjectManager;

{ TFormatosVideoController }

procedure TVideoFormatController.SaveFormat(VideoFormat: TVideoFormat);
var
  Manager: TObjectManager;
begin
  Manager := TDBConnection.GetInstance.CreateObjectManager;
  try
    Manager.SaveOrUpdate(VideoFormat);
    Manager.Flush;
  finally
    Manager.Free;
  end;
end;

end.
