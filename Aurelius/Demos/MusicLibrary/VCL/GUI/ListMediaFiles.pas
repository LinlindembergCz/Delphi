
unit ListMediaFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, Grids, MediaFile, BaseForm,
  MediaFilesController;

type
  TfrmListMediaFiles = class(TBaseForm)
    PanelTop: TPanel;
    BevelTop: TBevel;
    MainPanel: TPanel;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btNewSong: TButton;
    btExit: TButton;
    Grid: TStringGrid;
    btNewVideo: TButton;
    btEdit: TButton;
    btDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btNewSongClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewVideoClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TMediaFilesController;
    procedure LoadMediaFiles;
    function GetSelectedMediaFile: TMediaFile;
  end;

implementation

uses
  Generics.Collections,
  Song, Video,
  EditAlbum, EditSong, EditVideo;

{$R *.dfm}

procedure TfrmListMediaFiles.btEditClick(Sender: TObject);
var
  MediaFile: TMediaFile;
  frmEditSong: TfrmEditSong;
  frmEditVideo: TfrmEditVideo;
begin
  MediaFile := GetSelectedMediaFile;

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

  Msg := 'Are you sure you want to delete Media File "' + MediaFile.MediaName + '"?';

  if MessageDlg(Msg, mtWarning, mbYesNo, 0) = mrYes then
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
var
  MediaFiles: TList<TMediaFile>;
  MediaFile: TMediaFile;
  I, J: Integer;
begin
  for I := 0 to Grid.RowCount - 1 do
    for J := 0 to Grid.ColCount - 1 do
      Grid.Cells[J, I] := '';
  Grid.RowCount := 2;

  Grid.Cells[0, 0] := 'Type';
  Grid.Cells[1, 0] := 'Name';
  Grid.Cells[2, 0] := 'Artist';
  Grid.Cells[3, 0] := 'Album';
  Grid.Cells[4, 0] := 'Duration';
  Grid.Cells[5, 0] := 'File Location';

  MediaFiles := FController.GetAllMediaFiles;
  try
    if MediaFiles.Count > 0 then
    begin
      Grid.RowCount := 1 + MediaFiles.Count;

      for I := 0 to MediaFiles.Count - 1 do
      begin
        MediaFile := MediaFiles[I];

        Grid.Cols[0].Objects[I + 1] := MediaFile;

        if MediaFile is TSong then
          Grid.Cells[0, I + 1] := 'Song'
        else
          Grid.Cells[0, I + 1] := 'Video';

        Grid.Cells[1, I + 1] := MediaFile.MediaName;

        if MediaFile.Artist <> nil then
          Grid.Cells[2, I + 1] := MediaFile.Artist.ArtistName;

        if MediaFile.Album <> nil then
          Grid.Cells[3, I + 1] := MediaFile.Album.AlbumName;

        if MediaFile.Duration.HasValue then
          Grid.Cells[4, I + 1] := Format(
            '%s:%s',
            [FormatFloat('#0', MediaFile.Duration.Value div 60),
             FormatFloat('00', MediaFile.Duration.Value mod 60)
            ]);

        Grid.Cells[5, I + 1] := MediaFile.FileLocation;
      end;
    end;
  finally
    MediaFiles.Free;
  end;
end;

procedure TfrmListMediaFiles.FormCreate(Sender: TObject);
begin
  FController := TMediaFilesController.Create;
  LoadMediaFiles;
end;

procedure TfrmListMediaFiles.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

function TfrmListMediaFiles.GetSelectedMediaFile: TMediaFile;
begin
  Result := TMediaFile(Grid.Cols[0].Objects[Grid.Row]);
end;

procedure TfrmListMediaFiles.GridDblClick(Sender: TObject);
begin
  BtEditClick(Sender);
end;

end.
