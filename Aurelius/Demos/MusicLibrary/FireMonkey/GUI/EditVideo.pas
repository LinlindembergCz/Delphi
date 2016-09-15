unit EditVideo;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Edit, FMX.ListBox,
  EditVideoController,
  Video;

type
  TfrmEditVideo = class(TForm)
    TopLabel: TLabel;
    ButtonLayout: TLayout;
    btCancel: TButton;
    btSave: TButton;
    BottomLayout: TLayout;
    CenterLayout: TLayout;
    NameLayout: TLayout;
    edName: TEdit;
    lbName: TLabel;
    CenterLayout2: TLayout;
    edFileLocation: TEdit;
    lbFileLocation: TLabel;
    btFile: TSpeedButton;
    Image1: TImage;
    Layout1: TLayout;
    cbFormat: TComboBox;
    lbFormat: TLabel;
    btNewFormat: TSpeedButton;
    Layout2: TLayout;
    cbAlbum: TComboBox;
    lbAlbum: TLabel;
    btNewAlbum: TSpeedButton;
    Layout3: TLayout;
    cbArtist: TComboBox;
    lbArtist: TLabel;
    btNewArtist: TSpeedButton;
    Layout4: TLayout;
    edDurationMinutes: TEdit;
    lbDuration: TLabel;
    edDurationSeconds: TEdit;
    Label1: TLabel;
    procedure btSaveClick(Sender: TObject);
    procedure btFileClick(Sender: TObject);
    procedure btNewFormatClick(Sender: TObject);
    procedure btNewArtistClick(Sender: TObject);
    procedure btNewAlbumClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    FController: TEditVideoController;
  public
    procedure LoadVideoFormats;
    procedure LoadArtists;
    procedure LoadAlbums;
    procedure SetVideo(VideoId: Variant);
  end;

implementation
uses
  Aurelius.Engine.ObjectManager,
  Artist, DBConnection, VideoFormat,
  EditVideoFormat, EditArtist, EditAlbum,
  MediaFile;

{$R *.fmx}

procedure TfrmEditVideo.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditVideo.btFileClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Options := [TOpenOption.ofFileMustExist];

    if OpenDlg.Execute then
      edFileLocation.Text := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

procedure TfrmEditVideo.btNewAlbumClick(Sender: TObject);
var
  frmEditAlbum: TfrmEditAlbum;
begin
  frmEditAlbum := TfrmEditAlbum.Create(Self);
  try
    frmEditAlbum.ShowModal;

    if frmEditAlbum.ModalResult = mrOk then
    begin
      LoadAlbums;
      CbAlbum.ItemIndex := CbAlbum.Items.Count - 1;
    end;
  finally
    frmEditAlbum.Free;
  end;
end;

procedure TfrmEditVideo.btNewArtistClick(Sender: TObject);
var
  frmEditArtist: TfrmEditArtist;
begin
  frmEditArtist := TfrmEditArtist.Create(Self);
  try
    frmEditArtist.ShowModal;

    if frmEditArtist.ModalResult = mrOk then
    begin
      LoadArtists;
      cbArtist.ItemIndex := cbArtist.Items.Count - 1;
    end;
  finally
    frmEditArtist.Free;
  end;
end;

procedure TfrmEditVideo.btNewFormatClick(Sender: TObject);
var
  frmEditVideoFormat: TfrmEditVideoFormat;
begin
  frmEditVideoFormat := TfrmEditVideoFormat.Create(Self);
  try
    frmEditVideoFormat.ShowModal;

    if frmEditVideoFormat.ModalResult = mrOk then
    begin
      LoadVideoFormats;
      cbFormat.ItemIndex := cbFormat.Items.Count - 1;
    end;
  finally
    frmEditVideoFormat.Free;
  end;
end;

procedure TfrmEditVideo.btSaveClick(Sender: TObject);
var
  Video: TVideo;
  Album: TAlbum;
  Min, Sec: Word;
begin
  Video := FController.Video;

  Video.MediaName := edName.Text;
  Video.FileLocation := edFileLocation.Text;

  if cbFormat.ItemIndex >= 0 then
    Video.VideoFormat := TVideoFormat(cbFormat.Items.Objects[cbFormat.ItemIndex]);

  if cbArtist.ItemIndex >= 0 then
    Video.Artist := TArtist(cbArtist.Items.Objects[cbArtist.ItemIndex]);

  if CbAlbum.ItemIndex >= 0 then
  begin
    Album := TAlbum(CbAlbum.Items.Objects[CbAlbum.ItemIndex]);
    Video.Album := Album;
  end;

  if (edDurationMinutes.Text <> '') and (edDurationSeconds.Text <> '') then
  begin
    Min := StrToInt(edDurationMinutes.Text);
    Sec := StrToInt(edDurationSeconds.Text);
    Video.Duration := Min * 60 + Sec;
  end;

  FController.SaveVideo(Video);

  ModalResult := mrOk;
end;

procedure TfrmEditVideo.FormCreate(Sender: TObject);
begin
  FController := TEditVideoController.Create;
  LoadVideoFormats;
  LoadArtists;
  LoadAlbums;
end;

procedure TfrmEditVideo.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TfrmEditVideo.LoadAlbums;
var
  Albums: TList<TAlbum>;
  Al: TAlbum;
begin
  cbAlbum.Items.Clear;
  Albums := FController.GetAlbums;
  try
    for Al in Albums do
      CbAlbum.Items.AddObject(Al.AlbumName, Al);
  finally
    Albums.Free;
  end;
end;

procedure TfrmEditVideo.LoadArtists;
var
  Artists: TList<TArtist>;
  A: TArtist;
begin
  cbArtist.Items.Clear;
  Artists := FController.GetArtists;
  try
    for A in Artists do
      cbArtist.Items.AddObject(A.ArtistName, A);
  finally
    Artists.Free;
  end;
end;

procedure TfrmEditVideo.LoadVideoFormats;
var
  VideoFormats: TList<TVideoFormat>;
  F: TVideoFormat;
begin
  cbFormat.Items.Clear;
  VideoFormats := FController.GetVideoFormats;
  try
    for F in VideoFormats do
      cbFormat.Items.AddObject(F.FormatName, F);
  finally
    VideoFormats.Free;
  end;
end;

procedure TfrmEditVideo.SetVideo(VideoId: Variant);
var
  Video: TVideo;
begin
  FController.Load(VideoId);
  Video := FController.Video;

  edName.Text := Video.MediaName;
  edFileLocation.Text := Video.FileLocation;

  cbFormat.ItemIndex := cbFormat.Items.IndexOfObject(Video.VideoFormat);
  cbArtist.ItemIndex := cbArtist.Items.IndexOfObject(Video.Artist);
  CbAlbum.ItemIndex := CbAlbum.Items.IndexOfObject(Video.Album);

  if Video.Duration.HasValue then
  begin
    edDurationMinutes.Text := FormatFloat('#0', Video.Duration.Value div 60);
    edDurationSeconds.Text := FormatFloat('00', Video.Duration.Value mod 60);
  end;
end;

end.
