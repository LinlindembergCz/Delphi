unit ListArtists;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts,
  Artist, Data.Bind.EngExt, Data.Bind.Components, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  ArtistsController;

type
  TfrmListArtists = class(TForm)
    TopLabel: TLabel;
    BottomLayout: TLayout;
    ButtonLayout: TLayout;
    btClose: TButton;
    btEdit: TButton;
    CenterLayout: TLayout;
    Grid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    btNew: TButton;
    btDelete: TButton;
    BindScope1: TBindScope;
    BindingsList1: TBindingsList;
    BindGridList1: TBindGridList;
    procedure btEditClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FArtists: TList<TArtist>;
    FController: TArtistsController;
    procedure DestroyArtistList;
    function GetSelectedArtist: TArtist;
    procedure LoadArtists;
  end;

implementation
uses
  EditArtist;

{$R *.fmx}

procedure TfrmListArtists.btEditClick(Sender: TObject);
var
  Artist: TArtist;
  frmEditArtist: TfrmEditArtist;
begin
  Artist := GetSelectedArtist;
  if Artist = nil then Exit;


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
  if Artist = nil then Exit;

  Msg := 'Are you sure you want to delete Artist "' + Artist.ArtistName + '"?';

  if MessageDlg(Msg, TMsgDlgType.mtWarning, mbYesNo, 0) = mrYes then
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

procedure TfrmListArtists.DestroyArtistList;
begin
  if FArtists = nil then Exit;
  FArtists.Free;
  FArtists := nil;
end;

procedure TfrmListArtists.LoadArtists;
begin
  BindScope1.Active := False;
  DestroyArtistList;
  FArtists := FController.GetAllArtists;
  BindScope1.DataObject := FArtists;
  BindScope1.Active := True;
end;

procedure TfrmListArtists.FormCreate(Sender: TObject);
begin
  FController := TArtistsController.Create;
  LoadArtists;
end;

procedure TfrmListArtists.FormDestroy(Sender: TObject);
begin
  BindScope1.Active := false;
  FController.Free;
  DestroyArtistList;
end;

function TfrmListArtists.GetSelectedArtist: TArtist;
begin
  if Grid.Selected < 0 then Exit(nil);
  Result := FArtists[Grid.Selected];
end;

procedure TfrmListArtists.GridDblClick(Sender: TObject);
begin
  btEditClick(Sender);
end;

end.
