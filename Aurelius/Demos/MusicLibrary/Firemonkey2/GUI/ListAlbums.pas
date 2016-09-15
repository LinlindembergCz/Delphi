unit ListAlbums;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts,
  MediaFile, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, System.Bindings.Expression,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  GridController,
  AlbumsController;

type
  TfrmListAlbums = class(TForm)
    TopLabel: TLabel;
    BottomLayout: TLayout;
    ButtonLayout: TLayout;
    btClose: TButton;
    btEdit: TButton;
    CenterLayout: TLayout;
    Grid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    btNew: TButton;
    btDelete: TButton;
    procedure btEditClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGridController: TGridController<TAlbum>;
    FController: TAlbumsController;
    procedure DestroyGridController;
    function GetSelectedAlbum: TAlbum;
    procedure LoadAlbums;
  end;

implementation
uses
  EditAlbum;

{$R *.fmx}

procedure TfrmListAlbums.btEditClick(Sender: TObject);
var
  Album: TAlbum;
  frmEditAlbum: TfrmEditAlbum;
begin
  Album := GetSelectedAlbum;
  if Album = nil then Exit;

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
  if Album = nil then Exit;

  Msg := 'Are you sure you want to delete Album "' + Album.AlbumName + '"?';

  if MessageDlg(Msg, TMsgDlgType.mtWarning, mbYesNo, 0) = mrYes then
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

procedure TfrmListAlbums.DestroyGridController;
begin
  if FGridController = nil then Exit;
  FGridController.Free;
  FGridController := nil;
end;

procedure TfrmListAlbums.LoadAlbums;
begin
//  BindScope1.Active := false;
//  DestroyAlbumList;
//  FAlbums := FController.GetAllAlbums;
//  BindScope1.DataObject := FAlbums;
//  BindScope1.Active := True;

  DestroyGridController;
  FGridController := TGridController<TAlbum>.Create(Grid, FController.GetAllAlbums,
    procedure(Obj: TAlbum; Cols: TArray<string>)
    begin
      Cols[0] := Obj.AlbumName;
      if Obj.ReleaseYear.HasValue then
        Cols[1] := IntToStr(Obj.ReleaseYear);
      if Obj.Duration.HasValue then
        Cols[2] := Obj.DurationAsString;
      Cols[3] := IntToStr(Obj.MediaFiles.Count);
    end);
end;

procedure TfrmListAlbums.FormCreate(Sender: TObject);
begin
  FController := TAlbumsController.Create;
  LoadAlbums;
end;

procedure TfrmListAlbums.FormDestroy(Sender: TObject);
begin
  FController.Free;
  DestroyGridController;
end;

function TfrmListAlbums.GetSelectedAlbum: TAlbum;
begin
  if FGridController = nil then Exit(nil);
  Result := FGridController.Selected;
end;

procedure TfrmListAlbums.GridDblClick(Sender: TObject);
begin
  btEditClick(Sender);
end;

end.
