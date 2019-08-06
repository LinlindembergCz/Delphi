unit Unit35;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Strutils, Math;

type
  TForm35 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form35: TForm35;

implementation

uses
ACBrDFeUtil,ACBrUtil,ACBrValidador;

{$R *.dfm}

procedure TForm35.Button1Click(Sender: TObject);
var
  chaveNFe:string;
  vUF :string;
  vDataEmissao :string;
  vCNPJ :string;
  vModelo :string;
  vSerie :string;
  vNumero :string;
  vtpEmi :string;
  vCodigo:string;
  I: Integer;
  OldchaveNFe:string;

  function soNumero(valor: string):string;
  var
    j:integer;
    aux:string;
  begin
    aux:= '';
    for j := 0 to length(valor)-1 do
    begin
      if strtointDef(valor[j],-1) > -1 then
      begin
        aux:= aux + valor[j];
      end;
    end;
    result:= aux;
  end;
begin
  OldchaveNFe:= '';
  for I := 0 to Memo1.Lines.Count -1 do
  begin
    chaveNFe:= '';
    chaveNFe:= SoNumero(Memo1.lines[ I ]);

    if chaveNFe[1] = '0' then
    begin
      chaveNFe:= copy(chaveNFe,2, length(chaveNFe));
      OldchaveNFe:= chaveNFe;
    end
    else
      OldchaveNFe:= chaveNFe;

    vUF          := Copy(chaveNFe, 1, 2);
    vDataEmissao := Copy(chaveNFe, 3, 2)+Copy(chaveNFe, 5, 2);
    vCNPJ        := Copy(chaveNFe, 7, 14);
    vModelo      := Copy(chaveNFe, 21, 2);
    vSerie       := Copy(chaveNFe, 23, 3);
    vNumero      := Copy(chaveNFe, 26, 9);
    vtpEmi       := Copy(chaveNFe, 35, 1);
    vCodigo      := Copy(chaveNFe, 36, 8);

    chaveNFe := vUF + vDataEmissao + vCNPJ + vModelo + vSerie + vNumero + vtpEmi + vCodigo;
    chaveNFe := chaveNFe + Modulo11(chaveNFe);

    if not CheckBox1.Checked then
    begin
        Memo2.Lines.Add(chaveNFe);
        Memo2.Lines.Add('vUF ='+ vUF);
        Memo2.Lines.Add('vDataEmissao ='+vDataEmissao);
        Memo2.Lines.Add('vCNPJ ='+ vCNPJ);
        Memo2.Lines.Add('vModelo ='+vModelo );
        Memo2.Lines.Add('vSerie ='+ vSerie );
        Memo2.Lines.Add('vNumero ='+ vNumero );
        Memo2.Lines.Add('vtpEmi ='+ vtpEmi );
        Memo2.Lines.Add('vCodigo ='+vCodigo );
        Memo2.Lines.Add('--------------------------------------------');
    end
    else
        Memo2.Lines.Add( format('Update "Vendas" Set "ChaveNFE" = ''%s'' where "ChaveNFE"=''%s'';',
       [chaveNFe, OldchaveNFe] ) );
  end;

end;

procedure TForm35.Button2Click(Sender: TObject);
begin
  Memo2.Lines.Clear;
end;

end.
