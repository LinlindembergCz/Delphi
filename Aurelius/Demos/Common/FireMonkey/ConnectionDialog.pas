unit ConnectionDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.ListBox, FMX.TabControl, FMX.Memo;

type
  TfrmConnectionDialog = class(TForm)
    BottomLayout: TLayout;
    ButtonLayout: TLayout;
    btCancel: TButton;
    btOk: TButton;
    TopLabel: TLabel;
    CenterLayout: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    cbLibrary: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    TabControl1: TTabControl;
    tsDbGo: TTabItem;
    tsDbExpress: TTabItem;
    tsNoSettings: TTabItem;
    tsSQLite: TTabItem;
    lbInfo: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Layout2: TLayout;
    edConnectionString: TEdit;
    Label5: TLabel;
    btEditConnectionString: TButton;
    Layout3: TLayout;
    edSQLiteFile: TEdit;
    Label6: TLabel;
    btOpenDialog: TButton;
    ConnectionNameLayout: TLayout;
    cbConnectionName: TComboBox;
    lbConnectionName: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure btCancelClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btEditConnectionStringClick(Sender: TObject);
    procedure btOpenDialogClick(Sender: TObject);
    procedure cbLibraryChange(Sender: TObject);
  private
    procedure SaveDbExpressSettings;
    procedure PrepareDbExpressInterface;
    procedure PrepareSQLiteInterface;
    procedure SaveDbGoSettings;
    procedure SaveSQLiteSettings;
  public
    class procedure CheckConnection; static;
    class procedure ConfigureConnection; static;
  end;

implementation
uses
  DBConnection;

{$R *.fmx}

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
  PageToActivate: TTabItem;
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

  for I := 0 to TabControl1.ChildrenCount - 1 do
    if TabControl1.Children[I] is TTabItem then
      TTabItem(TabControl1.Children[I]).Visible := TabControl1.Children[I] = PageToActivate;
  TabControl1.ActiveTab := PageToActivate;
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
var
  Items: TStringList;
begin
  Items := TStringList.Create;
  try
    TDBConnection.GetInstance.GetDbExpressConnections(Items);
    Items.Sort;
    cbConnectionName.ListBox.Items.Assign(Items);
  finally
    Items.Free;
  end;
end;

procedure TfrmConnectionDialog.PrepareSQLiteInterface;
begin
  edSQLiteFile.Text := TDBConnection.GetInstance.DefaultSQLiteDatabase;
end;

procedure TfrmConnectionDialog.SaveDbExpressSettings;
var
  ConnectionName: string;
begin
  if cbConnectionName.Selected <> nil then
    ConnectionName := cbConnectionName.Selected.Text
  else
    ConnectionName := '';
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
