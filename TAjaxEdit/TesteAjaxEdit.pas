unit TesteAjaxEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AjaxEdit,
  Vcl.Imaging.GIFImg, Vcl.ExtCtrls, IdHTTP, Data.DB, Data.Win.ADODB, Vcl.Grids,
  Vcl.DBGrids;

type
  TForm1 = class(TForm)
    AjaxEdit1: TAjaxEdit;
    Image1: TImage;
    ADOConnection1: TADOConnection;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ADOQuery1: TADOQuery;
    Button1: TButton;
    procedure AjaxEdit1ShowAjax(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
     ADOConnection1.ConnectionString :=
         'Provider=Microsoft.Jet.OLEDB.4.0;' +
         'Data Source=' + ExtractFilePath(Application.ExeName) + 'ADDemo.mdb;' +
         'Mode=ReadWrite|Share Deny None;Persist Security Info=False;';
end;

procedure TForm1.AjaxEdit1ShowAjax(Sender: TObject);
Var
     I : Integer;
begin
     ADOQuery1.SQL.Text := 'SELECT NOME_Fantasia FROM T_Clientes ' +
                           ' WHERE NOME_Fantasia LIKE '+
                           QUOTEDSTR('%'+ AjaxEdit1.Text + '%') +
                           ' ORDER BY NOME_Fantasia ASC';
     for I := 0 to 15 do
     Begin
        ADOConnection1.Connected := True;
        ADOQuery1.Active := True;
        ADOConnection1.Connected := False;
        Application.ProcessMessages;
     End;
     ADOConnection1.Connected := True;
     ADOQuery1.Active := True;
  // Terminou a consulta do banco, Escondo a imagem Ajax
     AjaxEdit1.HideAjax;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     ADOConnection1.Connected := False;
end;


end.
