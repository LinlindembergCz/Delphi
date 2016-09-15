unit ListMediaFiles;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts,
  MediaFile, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, System.Bindings.Expression,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  GridController,
  MediaFilesController;

type
  TfrmListMediaFiles = class(TForm)
    TopLabel: TLabel;
    BottomLayout: TLayout;
    ButtonLayout: TLayout;
    btClose: TButton;
    btEdit: TButton;
    CenterLayout: TLayout;
    Grid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    btNewSong: TButton;
    btDelete: TButton;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    btNewVideo: TButton;
    procedure btEditClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewSongClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btNewVideoClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGridController: TGridController<TMediaFile>;
    FController: TMediaFilesController;
    procedure LoadMediaFiles;
    function GetSelectedMediaFile: TMediaFile;
  end;

implementation
uses
  Song, Video,
  EditAlbum, EditSong, EditVideo;

{$R *.fmx}

procedure TfrmListMediaFiles.btEditClick(Sender: TObject);
var
  MediaFile: TMediaFile;
  frmEditSong: TfrmEditSong;
  frmEditVideo: TfrmEditVideo;
begin
  MediaFile := GetSelectedMediaFile;
  if MediaFile = nil then Exit;

  if MediaFile is TSong then
  begin
    frmEditSong := TfrmEditSong.Create(Self);
    try
      frmEditSong.SetSong(TSong(MediaFile).Id);
      frmEditSong.ShowModal;
      if frmEditSong.ModalResult = mrOk then
        LoadMediaFiles;
    finally
      frmEditSong.Free;
    end;
  end
  else if MediaFile is TVideo then
  begin
    frmEditVideo := TfrmEditVideo.Create(Self);
    try
      frmEditVideo.SetVideo(TVideo(MediaFile).Id);
      frmEditVideo.ShowModal;
      if frmEditVideo.ModalResult = mrOk then
        LoadMediaFiles;
    finally
      frmEditVideo.Free;
    end;
  end;
end;

procedure TfrmListMediaFiles.btDeleteClick(Sender: TObject);
var
  MediaFile: TMediaFile;
  Msg: string;
begin
  MediaFile := GetSelectedMediaFile;
  if MediaFile = nil then Exit;

  Msg := 'Are you sure you want to delete Media File "' + MediaFile.MediaName + '"?';

  if MessageDlg(Msg, TMsgDlgType.mtWarning, mbYesNo, 0) = mrYes then
  begin
    FController.DeleteMediaFile(MediaFile);
    LoadMediaFiles;
  end;
end;

procedure TfrmListMediaFiles.btNewSongClick(Sender: TObject);
var
  frmEditSong: TfrmEditSong;
begin
  frmEditSong := TfrmEditSong.Create(Self);
  try
    frmEditSong.ShowModal;

    if frmEditSong.ModalResult = mrOk then
      LoadMediaFiles;
  finally
    frmEditSong.Free;
  end;
end;

procedure TfrmListMediaFiles.btNewVideoClick(Sender: TObject);
var
  frmEditVideo: TfrmEditVideo;
begin
  frmEditVideo := TfrmEditVideo.Create(Self);
  try
    frmEditVideo.ShowModal;

    if frmEditVideo.ModalResult = mrOk then
      LoadMediaFiles;
  finally
    frmEditVideo.Free;
  end;
end;

procedure TfrmListMediaFiles.LoadMediaFiles;
begin
//  BindScope1.Active := False;
//  FMediaFiles.Free;
//  FMediaFiles := FController.GetAllMediaFiles;
//  BindScope1.DataObject := FMediaFiles;
//  BindScope1.Active := True;

  if FGridController <> nil then FGridController.Free;
  FGridController := TGridController<TMediaFile>.Create(Grid, FController.GetAllMediaFiles,
    procedure(Obj: TMediaFile; Cols: TArray<string>)
    begin
      if Obj is TSong then
        Cols[0] := 'Song'
      else
        Cols[0] := 'Video';
      Cols[1] := Obj.MediaName;
      if Obj.Artist <> nil then
        Cols[2] := Obj.Artist.ArtistName;
      if Obj.Album <> nil then
        Cols[3] := Obj.Album.AlbumName;
      Cols[4] := Obj.DurationAsString;
      Cols[5] := Obj.FileLocation;
    end);
end;

procedure TfrmListMediaFiles.FormCreate(Sender: TObject);
begin
  FController := TMediaFilesController.Create;
  LoadMediaFiles;
end;

procedure TfrmListMediaFiles.FormDestroy(Sender: TObject);
begin
  FGridController.Free;
  FGridController := nil;
  FController.Free;
end;

function TfrmListMediaFiles.GetSelectedMediaFile: TMediaFile;
begin
  if FGridController = nil then Exit(nil);
  Result := FGridController.Selected;
end;

procedure TfrmListMediaFiles.GridDblClick(Sender: TObject);
begin
  btEditClick(Sender);
end;

end.
