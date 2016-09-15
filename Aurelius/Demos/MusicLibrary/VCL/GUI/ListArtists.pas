
unit ListArtists;

interface

uses
  Generics.Collections, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, Artist, BaseForm, ArtistsController;

type
  TfrmListArtists = class(TBaseForm)
    PanelTop: TPanel;
    BevelTop: TBevel;
    MainPanel: TPanel;
    Grid: TStringGrid;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btNew: TButton;
    btExit: TButton;
    btEdit: TButton;
    btDelete: TButton;
    procedure btEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TArtistsController;
    function GetSelectedArtist: TArtist;
    procedure LoadArtists;
  end;

implementation
uses
  EditArtist;

{$R *.dfm}

procedure TfrmListArtists.btEditClick(Sender: TObject);
var
  Artist: TArtist;
  frmEditArtist: TfrmEditArtist;
begin
  Artist := GetSelectedArtist;

  frmEditArtist := TfrmEditArtist.Create(Self);
  try
    frmEditArtist.SetArtist(Artist.Id);
    frmEditArtist.ShowModal;

    if frmEditArtist.ModalResult = mrOk then
      LoadArtists;
  finally
    frmEditArtist.Free;
  end;
end;

procedure TfrmListArtists.btDeleteClick(Sender: TObject);
var
  Artist: TArtist;
  Msg: string;
begin
  Artist := GetSelectedArtist;

  Msg := 'Are you sure you want to delete Artist "' + Artist.ArtistName + '"?';

  if MessageDlg(Msg, mtWarning, mbYesNo, 0) = mrYes then
  begin
    FController.DeleteArtist(Artist);
    LoadArtists;
  end;
end;

procedure TfrmListArtists.btNewClick(Sender: TObject);
var
  frmEditArtist: TfrmEditArtist;
begin
  frmEditArtist := TfrmEditArtist.Create(Self);
  try
    frmEditArtist.ShowModal;

    if frmEditArtist.ModalResult = mrOk then
      LoadArtists;
  finally
    frmEditArtist.Free;
  end;
end;

procedure TfrmListArtists.LoadArtists;
var
  Artists: TList<TArtist>;
  Artist: TArtist;
  I, J: Integer;
begin
  for I := 0 to Grid.RowCount - 1 do
    for J := 0 to Grid.ColCount - 1 do
      Grid.Cells[J, I] := '';
  Grid.RowCount := 2;

  Grid.Cells[0, 0] := 'Name';
  Grid.Cells[1, 0] := 'Genre';

  Artists := FController.GetAllArtists;
  try
    if Artists.Count > 0 then
    begin
      Grid.RowCount := 1 + Artists.Count;

      for I := 0 to Artists.Count - 1 do
      begin
        Artist := Artists[I];
        Grid.Cols[0].Objects[I + 1] := Artist;
        Grid.Cells[0, I + 1] := Artist.ArtistName;
        if Artist.Genre.HasValue then
          Grid.Cells[1, I + 1] := Artist.Genre;
      end;
    end;
  finally
    Artists.Free;
  end;
end;

procedure TfrmListArtists.FormCreate(Sender: TObject);
begin
  FController := TArtistsController.Create;
  LoadArtists;
end;

procedure TfrmListArtists.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

function TfrmListArtists.GetSelectedArtist: TArtist;
begin
  Result := TArtist(Grid.Cols[0].Objects[Grid.Row]);
end;

procedure TfrmListArtists.GridDblClick(Sender: TObject);
begin
  btEditClick(Sender);
end;

end.
