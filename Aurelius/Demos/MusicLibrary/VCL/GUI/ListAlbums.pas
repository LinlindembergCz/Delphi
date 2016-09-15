
unit ListAlbums;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, MediaFile, AlbumsController, BaseForm,
  Generics.Collections;

type
  TfrmListAlbums = class(TBaseForm)
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
    procedure FormCreate(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  private
    FController: TAlbumsController;
    function GetSelectedAlbum: TAlbum;
    procedure LoadAlbums;
  end;

implementation

uses
  EditAlbum;

{$R *.dfm}

procedure TfrmListAlbums.btEditClick(Sender: TObject);
var
  Album: TAlbum;
  frmEditAlbum: TfrmEditAlbum;
begin
  Album := GetSelectedAlbum;

  frmEditAlbum := TfrmEditAlbum.Create(Self);
  try
    frmEditAlbum.SetAlbum(Album.Id);
    frmEditAlbum.ShowModal;

    if frmEditAlbum.ModalResult = mrOk then
      LoadAlbums;
  finally
    frmEditAlbum.Free;
  end;
end;

procedure TfrmListAlbums.btDeleteClick(Sender: TObject);
var
  Album: TAlbum;
  Msg: string;
begin
  Album := GetSelectedAlbum;

  Msg := 'Are you sure you want to delete Album "' + Album.AlbumName + '"?';

  if MessageDlg(Msg, mtWarning, mbYesNo, 0) = mrYes then
  begin
    FController.DeleteAlbum(Album);
    LoadAlbums;
  end;
end;

procedure TfrmListAlbums.btNewClick(Sender: TObject);
var
  frmEditAlbum: TfrmEditAlbum;
begin
  frmEditAlbum := TfrmEditAlbum.Create(Self);
  try
    frmEditAlbum.ShowModal;

    if frmEditAlbum.ModalResult = mrOk then
      LoadAlbums;
  finally
    frmEditAlbum.Free;
  end;
end;

procedure TfrmListAlbums.LoadAlbums;
var
  Albums: TList<TAlbum>;
  Album: TAlbum;
  I, J: Integer;
begin
  for I := 0 to Grid.RowCount - 1 do
    for J := 0 to Grid.ColCount - 1 do
      Grid.Cells[J, I] := '';
  Grid.RowCount := 2;

  Grid.Cells[0, 0] := 'Name';
  Grid.Cells[1, 0] := 'Year';
  Grid.Cells[2, 0] := 'Duration';
  Grid.Cells[3, 0] := 'Total Files';

  Albums := FController.GetAllAlbums;
  try
    if Albums.Count > 0 then
    begin
      Grid.RowCount := 1 + Albums.Count;

      for I := 0 to Albums.Count - 1 do
      begin
        Album := Albums[I];
        Grid.Cols[0].Objects[I + 1] := Album;
        Grid.Cells[0, I + 1] := Album.AlbumName;

        if Album.ReleaseYear.HasValue then
          Grid.Cells[1, I + 1] := IntToStr(Album.ReleaseYear);

        if Album.Duration.HasValue then
          Grid.Cells[2, I + 1] := Format(
            '%s:%s',
            [FormatFloat('#0', Album.Duration.Value div 60),
             FormatFloat('00', Album.Duration.Value mod 60)
            ]);

        Grid.Cells[3, I + 1] := IntToStr(Album.MediaFiles.Count);
      end;
    end;
  finally
    Albums.Free;
  end;
end;

procedure TfrmListAlbums.FormCreate(Sender: TObject);
begin
  FController := TAlbumsController.Create;
  LoadAlbums;
end;

procedure TfrmListAlbums.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

function TfrmListAlbums.GetSelectedAlbum: TAlbum;
begin
  Result := TAlbum(Grid.Cols[0].Objects[Grid.Row]);
end;

procedure TfrmListAlbums.GridDblClick(Sender: TObject);
begin
  btEditClick(nil);
end;

end.
