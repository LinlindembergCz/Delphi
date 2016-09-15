
unit EditVideo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList, ComCtrls, Mask, Buttons,
  BaseForm,
  Aurelius.Engine.ObjectManager,
  EditVideoController,
  Video;

type
  TfrmEditVideo = class(TBaseForm)
    PanelTop: TPanel;
    PanelBottom: TPanel;
    BevelTop: TBevel;
    BevelBottom: TBevel;
    MainPanel: TPanel;
    lbName: TLabel;
    edName: TEdit;
    lbFileLocation: TLabel;
    lbFormat: TLabel;
    cbFormat: TComboBox;
    lbArtist: TLabel;
    LbAlbum: TLabel;
    cbArtist: TComboBox;
    CbAlbum: TComboBox;
    Label1: TLabel;
    edDuration: TMaskEdit;
    btSave: TButton;
    btCancel: TButton;
    btNewFormat: TSpeedButton;
    btNewArtist: TSpeedButton;
    btNewAlbum: TSpeedButton;
    btFile: TSpeedButton;
    edFileLocation: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btNewFormatClick(Sender: TObject);
    procedure btNewArtistClick(Sender: TObject);
    procedure btNewAlbumClick(Sender: TObject);
    procedure btFileClick(Sender: TObject);
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
  Artist, DBConnection, VideoFormat,
  Generics.Collections, EditVideoFormat, EditArtist, EditAlbum,
  MediaFile;

{$R *.dfm}

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
    OpenDlg.Options := [ofFileMustExist];

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

  if edDuration.Text <> '  :  ' then
  begin
    Min := StrToInt(Copy(edDuration.Text, 1, 2));
    Sec := StrToInt(Copy(edDuration.Text, 4, 2));
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
      CbAlbum.AddItem(Al.AlbumName, Al);
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
      cbArtist.AddItem(A.ArtistName, A);
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
      cbFormat.AddItem(F.FormatName, F);
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
    edDuration.Text := Format(
      '%s:%s',
      [FormatFloat('#0', Video.Duration.Value div 60),
      FormatFloat('00', Video.Duration.Value mod 60)
      ]);
end;

end.
