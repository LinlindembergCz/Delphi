unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Entities,
  Aurelius.Drivers.Interfaces;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    btConnection: TButton;
    lbConnection: TLabel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    btCreateSchema: TButton;
    brDestroySchema: TButton;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    Panel5: TPanel;
    Memo1: TMemo;
    TabSheet3: TTabSheet;
    Panel6: TPanel;
    Label1: TLabel;
    Panel7: TPanel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    edFirstName: TEdit;
    Label5: TLabel;
    edLastName: TEdit;
    Label6: TLabel;
    edEmail: TEdit;
    btSavePerson: TButton;
    Label7: TLabel;
    edLastId: TEdit;
    TabSheet4: TTabSheet;
    Panel8: TPanel;
    Label8: TLabel;
    Panel9: TPanel;
    GroupBox2: TGroupBox;
    lbFoundPerson: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edEmailToUpdate: TEdit;
    btUpdateEmail: TButton;
    edIdToFind: TEdit;
    btFindPerson: TButton;
    TabSheet5: TTabSheet;
    Panel10: TPanel;
    Label9: TLabel;
    Panel11: TPanel;
    GroupBox3: TGroupBox;
    Label14: TLabel;
    edNameToFind: TEdit;
    btListPeople: TButton;
    lbResults: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btConnectionClick(Sender: TObject);
    procedure btCreateSchemaClick(Sender: TObject);
    procedure brDestroySchemaClick(Sender: TObject);
    procedure btSavePersonClick(Sender: TObject);
    procedure btFindPersonClick(Sender: TObject);
    procedure btUpdateEmailClick(Sender: TObject);
    procedure btListPeopleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLastPersonFound: TPerson;
    procedure UpdateInterface;
    function Connection: IDBConnection;
  public
  end;

var
  Form1: TForm1;

implementation
uses
  Generics.Collections,
  DBConnection, ConnectionDialog,
  Aurelius.Criteria.Base,
  Aurelius.Criteria.Linq,
  Aurelius.Engine.DatabaseManager,
  Aurelius.Engine.ObjectManager;

{$R *.dfm}

procedure TForm1.brDestroySchemaClick(Sender: TObject);
var
  DatabaseManager: TDatabaseManager;
begin
  if MessageDlg('Confirm destroying database schema?', mtWarning, [mbYes, mbNo], 0, mbNo) <> mrYes then
    Exit;

  DatabaseManager := TDatabaseManager.Create(Connection);
  try
    DatabaseManager.DestroyDatabase;
    ShowMessage('Database schema destroyed successfully.');
  finally
    DatabaseManager.Free;
  end;
end;

procedure TForm1.btConnectionClick(Sender: TObject);
begin
  TfrmConnectionDialog.ConfigureConnection;
  UpdateInterface;
end;

procedure TForm1.btCreateSchemaClick(Sender: TObject);
var
  DatabaseManager: TDatabaseManager;
begin
  if MessageDlg('Confirm creating database schema?', mtWarning, [mbYes, mbNo], 0, mbYes) <> mrYes then
    Exit;

  DatabaseManager := TDatabaseManager.Create(Connection);
  try
    DatabaseManager.BuildDatabase;
    ShowMessage('Database schema created succesfully.');
  finally
    DatabaseManager.Free;
  end;
end;

procedure TForm1.btFindPersonClick(Sender: TObject);
var
  Manager: TObjectManager;
begin
  Manager := TObjectManager.Create(Connection);
  try
    Manager.OwnsObjects := false; // Make FLastPersonFound to exist after manager is destroyed
    if FLastPersonFound <> nil then
      FLastPersonFound.Free;
    FLastPersonFound := Manager.Find<TPerson>(StrToInt(edIdToFind.Text));
    if FLastPersonFound <> nil then
    begin
      lbFoundPerson.Caption := FLastPersonFound.FirstName + ' ' + FLastPersonFound.LastName;
      edEmailToUpdate.Text := FLastPersonFound.Email;
      btUpdateEmail.Enabled := True;
    end else
    begin
      lbFoundPerson.Caption := '(no person found)';
      edEmailToUpdate.Text := '';
      btUpdateEmail.Enabled := False;
    end;
  finally
    Manager.Free;
  end;
end;

procedure TForm1.btListPeopleClick(Sender: TObject);
var
  PersonName: string;
  Manager: TObjectManager;
  PersonList: TObjectList<TPerson>;
  Person: TPerson;
begin
  PersonName := Trim(edNameToFind.Text);
  if PersonName = '' then
  begin
    ShowMessage('Please specify a criteria.');
    Exit;
  end;

  PersonName := '%' + PersonName + '%'; // create a mask
  Manager := TObjectManager.Create(Connection);
  try
    PersonList := Manager.CreateCriteria<TPerson>
      .Where(TLinq.Like('FirstName', PersonName) or (TLinq.Like('LastName', PersonName)))
      .AddOrder(TOrder.Asc('FirstName'))
      .List;
    try
      lbResults.Items.Clear;
      for Person in PersonList do
        lbResults.Items.Add(Format('(%d) %s %s - %s',
          [Person.Id,
           Person.FirstName,
           Person.LastName,
           Person.Email
          ]));
    finally
      PersonList.Free;
    end;
  finally
    Manager.Free;
  end;
end;

procedure TForm1.btSavePersonClick(Sender: TObject);
var
  Person: TPerson;
  Manager: TObjectManager;
begin
  Person := TPerson.Create;
  Person.LastName := edLastName.Text;
  Person.FirstName := edFirstName.Text;
  Person.Email := edEmail.Text;
  Manager := TObjectManager.Create(Connection);
  try
    Manager.Save(Person);
    Manager.Flush;
    edLastId.Text := IntToStr(Person.Id);
  finally
    Manager.Free;
  end;
end;

procedure TForm1.btUpdateEmailClick(Sender: TObject);
var
  Manager: TObjectManager;
begin
  FLastPersonFound.Email := edEmailToUpdate.Text;
  Manager := TObjectManager.Create(Connection);
  try
    Manager.OwnsObjects := false;
    Manager.Update(FLastPersonFound);
    Manager.Flush;
  finally
    Manager.Free;
  end;
end;

function TForm1.Connection: IDBConnection;
begin
  Result := TDBConnection.GetInstance.Connection;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FLastPersonFound <> nil then
    FLastPersonFound.Free;
end;

procedure TForm1.UpdateInterface;
begin
  if TDBConnection.GetInstance.HasConnection then
    lbConnection.Caption := 'Connection configured.';
  PageControl1.Visible := TDBConnection.GetInstance.HasConnection;
  PageControl1.ActivePageIndex := 0;
end;

end.


