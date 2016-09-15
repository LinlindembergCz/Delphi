program MusicLibraryVCL;

uses
  Forms,
  BaseForm in 'GUI\BaseForm.pas',
  EditAlbum in 'GUI\EditAlbum.pas' {frmEditAlbum},
  EditArtist in 'GUI\EditArtist.pas' {FrmEditArtist},
  EditSong in 'GUI\EditSong.pas' {frmEditSong},
  EditSongFormat in 'GUI\EditSongFormat.pas' {frmEditSongFormat},
  EditVideo in 'GUI\EditVideo.pas' {frmEditVideo},
  EditVideoFormat in 'GUI\EditVideoFormat.pas' {frmEditVideoFormat},
  ListAlbums in 'GUI\ListAlbums.pas' {frmListAlbums},
  ListArtists in 'GUI\ListArtists.pas' {frmListArtists},
  ListMediaFiles in 'GUI\ListMediaFiles.pas' {frmListMediaFiles},
  MainForm in 'GUI\MainForm.pas' {frmMainForm},
  SqlMonitor in 'GUI\SqlMonitor.pas' {FrmSqlMonitor},
  AlbumsController in '..\Common\Controllers\AlbumsController.pas',
  ArtistsController in '..\Common\Controllers\ArtistsController.pas',
  MediaFilesController in '..\Common\Controllers\MediaFilesController.pas',
  SongFormatController in '..\Common\Controllers\SongFormatController.pas',
  EditSongController in '..\Common\Controllers\EditSongController.pas',
  VideoFormatController in '..\Common\Controllers\VideoFormatController.pas',
  EditVideoController in '..\Common\Controllers\EditVideoController.pas',
  Artist in '..\Common\Entities\Artist.pas',
  MediaFile in '..\Common\Entities\MediaFile.pas',
  Song in '..\Common\Entities\Song.pas',
  SongFormat in '..\Common\Entities\SongFormat.pas',
  Video in '..\Common\Entities\Video.pas',
  VideoFormat in '..\Common\Entities\VideoFormat.pas',
  EditAlbumController in '..\Common\Controllers\EditAlbumController.pas',
  EditArtistController in '..\Common\Controllers\EditArtistController.pas',
  ConnectionDialog in '..\..\Common\VCL\ConnectionDialog.pas' {frmConnectionDialog},
  DBConnection in '..\..\Common\DBConnection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
