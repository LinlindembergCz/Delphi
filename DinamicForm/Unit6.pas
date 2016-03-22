unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TForm6 = class(TForm)
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Nome: TStringField;
    ClientDataSet1Endereco: TStringField;
    ClientDataSet1Status: TStringField;
    ClientDataSet1DataNacimento: TDateField;
    procedure FormActivate(Sender: TObject);
  private
    procedure CreateDinamicView;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.CreateDinamicView;
var
  i,c, altura: integer;
  Textos: Array[1..10] of TLabel;
  Campos: Array[1..10] of TEdit;
  Datas: Array[1..10] of TDateTimePicker;
begin
  c := 0;
  altura := 10;
  for i := 0 to ClientDataSet1.Fields.Count-1 do
  begin
    Textos[c] := TLabel.Create(Form6);
    Textos[c].Parent := Form6;
    Textos[c].Left := 40;
    Textos[c].Top := altura;
    Textos[c].Width := 50;
    Textos[c].Height := 13;
    Textos[c].Caption := ClientDataSet1.Fields[c].DisplayLabel;

    if ClientDataSet1.Fields[c].DataType = ftDate then
    begin
      Altura := altura + 15;
      Datas[c] := TDateTimePicker.Create(Form6);
      Datas[c].Parent := Form6;
      Datas[c].Left := 40;
      Datas[c].Top := altura;
      Datas[c].Width := 100;
      Datas[c].Height := 21;
      Datas[c].TabOrder := c;
    end
    else
    begin
      Altura := altura + 15;
      Campos[c] := TEdit.Create(Form6);
      Campos[c].Parent := Form6;
      Campos[c].Left := 40;
      Campos[c].Top := altura;
      Campos[c].Width := 100;
      Campos[c].Height := 21;
      Campos[c].TabOrder := c;
    end;

    Altura := altura + 35;
    c := c+1;
  end;
end;

procedure TForm6.FormActivate(Sender: TObject);
begin
  ClientDataSet1.open;
  CreateDinamicView;
end;

end.
