
unit SongFormatController;

interface

uses
  SongFormat;

type
  TSongFormatController = class
  public
    procedure SaveFormat(SongFormat: TSongFormat);
  end;

implementation

uses
  DBConnection,
  Aurelius.Engine.ObjectManager;

{ TFormatosMusicaController }

procedure TSongFormatController.SaveFormat(SongFormat: TSongFormat);
var
  Manager: TObjectManager;
begin
  Manager := TDBConnection.GetInstance.CreateObjectManager;
  try
    Manager.SaveOrUpdate(SongFormat);
    Manager.Flush;
  finally
    Manager.Free;
  end;
end;

end.
