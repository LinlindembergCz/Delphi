
unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, jpeg, ExtCtrls;

type
  TfrmMainForm = class(TForm)
    MainMenu: TMainMenu;
    mnDatabase: TMenuItem;
    mnCreate: TMenuItem;
    mnDestroy: TMenuItem;
    mnView: TMenuItem;
    mnMediaFiles: TMenuItem;
    mnArtists: TMenuItem;
    mnNew: TMenuItem;
    mbNewSong: TMenuItem;
    mnNewVideo: TMenuItem;
    mnNewArtist: TMenuItem;
    mnNewAlbum: TMenuItem;
    mnNewSongFormat: TMenuItem;
    mnNewVideoFormat: TMenuItem;
    Image1: TImage;
    mnHelp: TMenuItem;
    mnAbout: TMenuItem;
    mnTools: TMenuItem;
    mnSQLMonitor: TMenuItem;
    mnAlbums: TMenuItem;
    mnConfigure: TMenuItem;
    procedure mnCreateClick(Sender: TObject);
    procedure mnDestroyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnMediaFilesClick(Sender: TObject);
    procedure mnArtistsClick(Sender: TObject);
    procedure mbNewSongClick(Sender: TObject);
    procedure mnNewVideoClick(Sender: TObject);
    procedure mnNewArtistClick(Sender: TObject);
    procedure mnNewAlbumClick(Sender: TObject);
    procedure mnNewSongFormatClick(Sender: TObject);
    procedure mnNewVideoFormatClick(Sender: TObject);
    procedure mnAboutClick(Sender: TObject);
    procedure mnSQLMonitorClick(Sender: TObject);
    procedure mnAlbumsClick(Sender: TObject);
    procedure mnConfigureClick(Sender: TObject);
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses
  DBConnection,
  SqlMonitor, ConnectionDialog,
  Aurelius.Engine.DatabaseManager,
  Aurelius.Mapping.MappedClasses,
  Video, Song, MediaFile, SongFormat, VideoFormat, Artist,
  EditSong, EditArtist, EditSongFormat, EditAlbum, EditVideo, EditVideoFormat,
  ListMediaFiles, ListArtists, ListAlbums;

{$R *.dfm}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
  TDBConnection.GetInstance.AddCommandListener(TFrmSqlMonitor.GetInstance);

  TfrmConnectionDialog.CheckConnection;
end;

procedure TfrmMainForm.mnAboutClick(Sender: TObject);
begin
  MessageDlg(
    'Music Library '#13#10 +
    'Sample Application for TMS Aurelius Framework '#13#10 +
    'http://www.tmssoftware.com'
    ,
    mtInformation, [mbOk], 0);
end;

procedure TfrmMainForm.mnAlbumsClick(Sender: TObject);
var
  frmListAlbums: TfrmListAlbums;
begin
  frmListAlbums := TfrmListAlbums.Create(Self);
  try
    frmListAlbums.ShowModal;
  finally
    frmListAlbums.Free;
  end;
end;

procedure TfrmMainForm.mnArtistsClick(Sender: TObject);
var
  frmListArtists: TfrmListArtists;
begin
  frmListArtists := TfrmListArtists.Create(Self);
  try
    frmListArtists.ShowModal;
  finally
    frmListArtists.Free;
  end;
end;

procedure TfrmMainForm.mnConfigureClick(Sender: TObject);
begin
  TfrmConnectionDialog.ConfigureConnection;
end;

procedure TfrmMainForm.mnCreateClick(Sender: TObject);
var
  DatabaseManager: TDatabaseManager;
begin
  DatabaseManager := TDBConnection.GetInstance.GetNewDatabaseManager;
  try
    DatabaseManager.BuildDatabase;
    ShowMessage('Database created successfully.');
  finally
    DatabaseManager.Free;
  end;
end;

procedure TfrmMainForm.mnDestroyClick(Sender: TObject);
var
  DatabaseManager: TDatabaseManager;
  Msg: string;
begin
  Msg := 'Are you sure you want to destroy database?';

  if MessageDlg(Msg, mtWarning, mbYesNo, 0) = mrYes then
  begin
    DatabaseManager := TDBConnection.GetInstance.GetNewDatabaseManager;
    try
      DatabaseManager.DestroyDatabase;
      ShowMessage('Database destroyed successfully .');
    finally
      DatabaseManager.Free;
    end;
  end;
end;

procedure TfrmMainForm.mnMediaFilesClick(Sender: TObject);
var
  frmListMediaFiles: TfrmListMediaFiles;
begin
  frmListMediaFiles := TfrmListMediaFiles.Create(Self);
  try
    frmListMediaFiles.ShowModal;
  finally
    frmListMediaFiles.Free;
  end;
end;

procedure TfrmMainForm.mnNewAlbumClick(Sender: TObject);
var
  frmEditAlbum: TfrmEditAlbum;
begin
  frmEditAlbum := TfrmEditAlbum.Create(Self);
  try
    frmEditAlbum.ShowModal;
  finally
    frmEditAlbum.Free;
  end;
end;

procedure TfrmMainForm.mnNewArtistClick(Sender: TObject);
var
  frmEditArtist: TfrmEditArtist;
begin
  frmEditArtist := TfrmEditArtist.Create(Self);
  try
    frmEditArtist.ShowModal;
  finally
    frmEditArtist.Free;
  end;
end;

procedure TfrmMainForm.mbNewSongClick(Sender: TObject);
var
  frmEditSong: TfrmEditSong;
begin
  frmEditSong := TfrmEditSong.Create(Self);
  try
    frmEditSong.ShowModal;
  finally
    frmEditSong.Free;
  end;
end;

procedure TfrmMainForm.mnNewSongFormatClick(Sender: TObject);
var
  frmEditSongFormat: TfrmEditSongFormat;
begin
  frmEditSongFormat := TfrmEditSongFormat.Create(Self);
  try
    frmEditSongFormat.ShowModal;
  finally
    frmEditSongFormat.Free;
  end;
end;

procedure TfrmMainForm.mnNewVideoClick(Sender: TObject);
var
  frmEditVideo: TfrmEditVideo;
begin
  frmEditVideo := TfrmEditVideo.Create(Self);
  try
    frmEditVideo.ShowModal;
  finally
    frmEditVideo.Free;
  end;
end;

procedure TfrmMainForm.mnNewVideoFormatClick(Sender: TObject);
var
  frmEditVideoFormat: TfrmEditVideoFormat;
begin
  frmEditVideoFormat := TfrmEditVideoFormat.Create(Self);
  try
    frmEditVideoFormat.ShowModal;
  finally
    frmEditVideoFormat.Free;
  end;
end;

procedure TfrmMainForm.mnSQLMonitorClick(Sender: TObject);
begin
  TFrmSqlMonitor.GetInstance.Show;
end;

end.
