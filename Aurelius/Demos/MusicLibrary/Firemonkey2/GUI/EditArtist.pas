unit EditArtist;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Edit, Artist,
  EditArtistController;

type
  TfrmEditArtist = class(TForm)
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
    edGenre: TEdit;
    lbGenre: TLabel;
    procedure btCancelClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TEditArtistController;
  public
    procedure SetArtist(ArtistId: Variant);
  end;

implementation

{$R *.fmx}

procedure TFrmEditArtist.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmEditArtist.btSaveClick(Sender: TObject);
var
  Artist: TArtist;
begin
  Artist := FController.Artist;

  Artist.ArtistName := edName.Text;
  Artist.Genre := edGenre.Text;

  FController.SaveArtist(Artist);

  ModalResult := mrOk;
end;

procedure TfrmEditArtist.FormCreate(Sender: TObject);
begin
  FController := TEditArtistController.Create;
end;

procedure TfrmEditArtist.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TFrmEditArtist.SetArtist(ArtistId: Variant);
var
  Artist: TArtist;
begin
  FController.Load(ArtistId);
  Artist := FController.Artist;

  edName.Text := Artist.ArtistName;

  if Artist.Genre.HasValue then
    edGenre.Text := Artist.Genre;
end;

end.
