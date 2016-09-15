unit EditAlbum;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Edit, MediaFile,
  EditAlbumController;

type
  TfrmEditAlbum = class(TForm)
    TopLabel: TLabel;
    ButtonLayout: TLayout;
    btCancel: TButton;
    btSave: TButton;
    BottomLayout: TLayout;
    CenterLayout: TLayout;
    NameLayout: TLayout;
    edName: TEdit;
    lbName: TLabel;
    YearLayout: TLayout;
    edYear: TEdit;
    lbYear: TLabel;
    procedure btCancelClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TEditAlbumController;
  public
    procedure SetAlbum(AlbumId: Variant);
  end;

implementation

{$R *.fmx}

procedure TfrmEditAlbum.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEditAlbum.btSaveClick(Sender: TObject);
var
  Album: TAlbum;
begin
  Album := FController.Album;

  Album.AlbumName := edName.Text;
  if edYear.Text <> '' then
    Album.ReleaseYear := StrToInt(edYear.Text);

  FController.SaveAlbum(Album);

  ModalResult := mrOk;
end;

procedure TfrmEditAlbum.FormCreate(Sender: TObject);
begin
  FController := TEditAlbumController.Create;
end;

procedure TfrmEditAlbum.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TfrmEditAlbum.SetAlbum(AlbumId: Variant);
var
  Album: TAlbum;
begin
  FController.Load(AlbumId);
  Album := FController.Album;

  edName.Text := Album.AlbumName;

  if Album.ReleaseYear.HasValue then
    edYear.Text := IntToStr(Album.ReleaseYear);
end;

end.
