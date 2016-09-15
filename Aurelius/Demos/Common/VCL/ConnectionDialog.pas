unit ConnectionDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmConnectionDialog = class(TForm)
    PanelTop: TPanel;
    PanelBottom: TPanel;
    btOk: TButton;
    btCancel: TButton;
    MainPanel: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    cbLibrary: TComboBox;
    PageControl1: TPageControl;
    tsdbExpress: TTabSheet;
    lbConnectionName: TLabel;
    cbConnectionName: TComboBox;
    pnInfo: TPanel;
    lbInfo: TLabel;
    tsdbGo: TTabSheet;
    Label2: TLabel;
    edConnectionString: TEdit;
    btEditConnectionString: TButton;
    Panel2: TPanel;
    Label3: TLabel;
    Panel3: TPanel;
    Label4: TLabel;
    tsSQLite: TTabSheet;
    tsNoSettings: TTabSheet;
    Panel4: TPanel;
    Memo1: TMemo;
    Label5: TLabel;
    edSQLiteFile: TEdit;
    Panel5: TPanel;
    Label6: TLabel;
    btOpenDialog: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure cbLibraryChange(Sender: TObject);
    procedure btEditConnectionStringClick(Sender: TObject);
    procedure btOpenDialogClick(Sender: TObject);
  private
    procedure PrepareDbExpressInterface;
    procedure SaveDbExpressSettings;

    procedure SaveDbGoSettings;

    procedure PrepareSQLiteInterface;
    procedure SaveSQLiteSettings;
  public
    class procedure ConfigureConnection;
    class procedure CheckConnection;
  end;

implementation
uses
  DBConnection;

{$R *.dfm}

{ TfrmConnectionDialog }

procedure TfrmConnectionDialog.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmConnectionDialog.btEditConnectionStringClick(Sender: TObject);
begin
  edConnectionString.Text := TDBConnection.GetInstance.EditDbGoConnectionString(edConnectionString.Text);
end;

procedure TfrmConnectionDialog.btOkClick(Sender: TObject);
var
  FirstConnection: boolean;
begin
  FirstConnection := not TDBConnection.GetInstance.HasConnection;

  case cbLibrary.ItemIndex of
    0: SaveSQLiteSettings;
    1: SaveDbGoSettings;
    2: SaveDbExpressSettings;
  end;

  TDBConnection.GetInstance.UnloadConnection;
  if TDBConnection.GetInstance.Connection.IsConnected then
  begin
    ModalResult := mrOk;
    if FirstConnection then
      ShowMessage('Connection successful. Database is empty, be sure to create tables and fields');
  end;
end;

procedure TfrmConnectionDialog.btOpenDialogClick(Sender: TObject);
begin
  OpenDialog1.FileName := edSQLiteFile.Text;
  if OpenDialog1.Execute then
    edSQLiteFile.Text := OpenDialog1.FileName;
end;

procedure TfrmConnectionDialog.cbLibraryChange(Sender: TObject);
var
  PageToActivate: TTabSheet;
  I: Integer;
begin
  PageToActivate := tsNoSettings;
  case cbLibrary.ItemIndex of
    0:
      if TDBConnection.GetInstance.IsSQLiteSupported then
        PageToActivate := tsSQLite;
    1:
      if TDBConnection.GetInstance.IsDbGoSupported then
        PageToActivate := tsDbGo;
    2:
      if TDBConnection.GetInstance.IsDbExpressSupported then
        PageToActivate := tsDBExpress;
  end;

  for I := 0 to PageControl1.PageCount - 1 do
    PageControl1.Pages[I].TabVisible := PageControl1.Pages[I] = PageToActivate;
end;

class procedure TfrmConnectionDialog.CheckConnection;
begin
  if not TDBConnection.GetInstance.HasConnection then
    ConfigureConnection;
end;

class procedure TfrmConnectionDialog.ConfigureConnection;
var
  frmConnectionDialog: TfrmConnectionDialog;
begin
  frmConnectionDialog := TfrmConnectionDialog.Create(nil);
  try
    frmConnectionDialog.ShowModal;
  finally
    frmConnectionDialog.Free;
  end;
end;

procedure TfrmConnectionDialog.FormCreate(Sender: TObject);
begin
  PrepareSQLiteInterface;
  PrepareDbExpressInterface;

  cbLibrary.ItemIndex := 0;
  cbLibraryChange(nil);
end;

procedure TfrmConnectionDialog.PrepareDbExpressInterface;
begin
  TDBConnection.GetInstance.GetDbExpressConnections(cbConnectionName.Items);
end;

procedure TfrmConnectionDialog.PrepareSQLiteInterface;
begin
  edSQLiteFile.Text := TDBConnection.GetInstance.DefaultSQLiteDatabase;
end;

procedure TfrmConnectionDialog.SaveDbExpressSettings;
var
  ConnectionName: string;
begin
  ConnectionName := cbConnectionName.Text;
  TDBConnection.GetInstance.SaveDbExpressSettings(ConnectionName);
end;

procedure TfrmConnectionDialog.SaveDbGoSettings;
var
  ConnectionString: string;
begin
  ConnectionString := edConnectionString.Text;
  TDBConnection.GetInstance.SaveDbGoSettings(ConnectionString);
end;

procedure TfrmConnectionDialog.SaveSQLiteSettings;
var
  SQLiteFile: string;
begin
  SQLiteFile := edSQLiteFile.Text;
  TDBConnection.GetInstance.SaveSQLiteSettings(SQLiteFile);
end;

end.
