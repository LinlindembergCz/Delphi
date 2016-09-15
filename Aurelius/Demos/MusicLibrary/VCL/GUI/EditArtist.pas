
unit EditArtist;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, BaseForm, Artist,
  EditArtistController;

type
  TFrmEditArtist = class(TBaseForm)
    PanelTop: TPanel;
    BevelTop: TBevel;
    MainPanel: TPanel;
    lbName: TLabel;
    edName: TEdit;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btSave: TButton;
    btCancel: TButton;
    lbGenre: TLabel;
    edGenre: TEdit;
    procedure btSaveClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TEditArtistController;
  public
    procedure SetArtist(ArtistId: Variant);
  end;

implementation

{$R *.dfm}

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

procedure TFrmEditArtist.FormCreate(Sender: TObject);
begin
  FController := TEditArtistController.Create;
end;

procedure TFrmEditArtist.FormDestroy(Sender: TObject);
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
