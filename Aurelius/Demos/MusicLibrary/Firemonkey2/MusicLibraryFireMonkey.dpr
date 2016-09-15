program MusicLibraryFireMonkey;

uses
  FMX.Forms,
  MainForm in 'GUI\MainForm.pas' {frmMainForm},
  Artist in '..\Common\Entities\Artist.pas',
  MediaFile in '..\Common\Entities\MediaFile.pas',
  Song in '..\Common\Entities\Song.pas',
  SongFormat in '..\Common\Entities\SongFormat.pas',
  Video in '..\Common\Entities\Video.pas',
  VideoFormat in '..\Common\Entities\VideoFormat.pas',
  SqlMonitor in 'GUI\SqlMonitor.pas' {frmSqlMonitor},
  EditVideo in 'GUI\EditVideo.pas' {frmEditVideo},
  ListArtists in 'GUI\ListArtists.pas' {frmListArtists},
  EditSongFormat in 'GUI\EditSongFormat.pas' {frmEditSongFormat},
  ListMediaFiles in 'GUI\ListMediaFiles.pas' {frmListMediaFiles},
  ListAlbums in 'GUI\ListAlbums.pas' {frmListAlbums},
  EditArtist in 'GUI\EditArtist.pas' {frmEditArtist},
  EditAlbum in 'GUI\EditAlbum.pas' {frmEditAlbum},
  EditVideoFormat in 'GUI\EditVideoFormat.pas' {frmEditVideoFormat},
  EditSong in 'GUI\EditSong.pas' {frmEditSong},
  AlbumsController in '..\Common\Controllers\AlbumsController.pas',
  ArtistsController in '..\Common\Controllers\ArtistsController.pas',
  EditAlbumController in '..\Common\Controllers\EditAlbumController.pas',
  EditArtistController in '..\Common\Controllers\EditArtistController.pas',
  EditSongController in '..\Common\Controllers\EditSongController.pas',
  EditVideoController in '..\Common\Controllers\EditVideoController.pas',
  MediaFilesController in '..\Common\Controllers\MediaFilesController.pas',
  SongFormatController in '..\Common\Controllers\SongFormatController.pas',
  VideoFormatController in '..\Common\Controllers\VideoFormatController.pas',
  DBConnection in '..\..\Common\DBConnection.pas',
  ConnectionDialog in '..\..\Common\FireMonkey\ConnectionDialog.pas' {frmConnectionDialog},
  GridController in 'GUI\GridController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
