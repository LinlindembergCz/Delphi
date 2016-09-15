
unit MediaFilesController;

interface

uses
  Generics.Collections,
  MediaFile,
  Aurelius.Engine.ObjectManager;

type
  TMediaFilesController = class
  private
    FManager: TObjectManager;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAllMediaFiles: TList<TMediaFile>;
    procedure DeleteMediaFile(MediaFile: TMediaFile);
  end;

implementation

uses
  DBConnection;

{ TMediaFilesController }

constructor TMediaFilesController.Create;
begin
  FManager := TDBConnection.GetInstance.CreateObjectManager;
end;

procedure TMediaFilesController.DeleteMediaFile(MediaFile: TMediaFile);
begin
  if not FManager.IsAttached(MediaFile) then
    MediaFile := FManager.Find<TMediaFile>(MediaFile.Id);
  FManager.Remove(MediaFile);
end;

destructor TMediaFilesController.Destroy;
begin
  FManager.Free;
  inherited;
end;

function TMediaFilesController.GetAllMediaFiles: TList<TMediaFile>;
begin
  FManager.Clear;
  Result := FManager.FindAll<TMediaFile>;
end;

end.
