unit Unit37;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm37 = class(TForm)
    btn1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Itens: TLabel;
    Label1: TLabel;
    FileOpenDialog1: TFileOpenDialog;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

uses uClassNFeImportacao;

{$R *.dfm}

procedure TForm37.btn1Click(Sender: TObject);
var
  loNFeImportacao: TNFeImportacao;
  liCont: Integer;
  valorBase, valortotal, valorTotalProdutos, valorDesconto, valorFCP, valorFCPST, valorICMS : Double;
begin
  if FileOpenDialog1.Execute then
  begin
    if FileOpenDialog1.Files.Count > 0 then
    begin
      loNFeImportacao := TNFeImportacao.Create;
      loNFeImportacao.LerXml(FileOpenDialog1.FileName);
      valorBase := 0;
      valortotal := 0;
      valorTotalProdutos := 0;
      valorFCP   := 0;
      valorFCPST := 0;
      valorICMS:= 0;


      With loNFeImportacao Do
      Begin
        For liCont := 1 To NFeImportacao_Itens.Count Do
        Begin
          With NFeImportacao_Itens.Items[liCont - 1] Do
          Begin
            valorBase := valorBase + NFeImportacao_Item_ICMS.Vlr_BaseCalculo;
            valortotal := valortotal + Vlr_Total - Vlr_Desc;
            valorDesconto := valorDesconto + Vlr_Desc;
            valorICMS:= valorICMS + NFeImportacao_Item_ICMS.Vlr_ICMS;
            valorFCP:= valorICMS + NFeImportacao_Item_ICMS.Vlr_ICMS;
            valorFCPST:= valorICMS + NFeImportacao_Item_ICMS.Vlr_ICMS;
          End;

          valorTotalProdutos := valorTotalProdutos +
              (NFeImportacao_Itens.Items[liCont - 1].Pco_Liquido *
              NFeImportacao_Itens.Items[liCont - 1].Qtde_Atendida)
        End;
        Memo1.Clear;
        Memo2.Clear;

        Memo1.Lines.Add('  Total :' + floattostr(valortotal));
        Memo1.Lines.Add('  Base Calculo :' + floattostr(valorBase));
        Memo1.Lines.Add('  Total Desconto :' + floattostr(valorDesconto));
        Memo1.Lines.Add('  Total Produtos :' + floattostr(valorTotalProdutos));
        Memo1.Lines.Add('  Total ICMS :' + floattostr(valorICMS));
        Memo1.Lines.Add('  Total FCP :' + floattostr(valorFCP));
        Memo1.Lines.Add('  Total FCPST :' + floattostr(valorFCPST));

        Memo2.Lines.Add('  Total :' + floattostr(Vlr_Total));
        Memo2.Lines.Add('  Base Calculo  :' + floattostr(Vlr_BaseICMS));
        Memo2.Lines.Add('  Total Desconto :' + floattostr(Desc_Comercial));
        Memo2.Lines.Add('  Total Produtos  :' + floattostr(Vlr_Produtos));
        Memo2.Lines.Add('  Total ICMS :' + floattostr(valorICMS));
        Memo2.Lines.Add('  Total FCP :' + floattostr(valorFCP));
        Memo2.Lines.Add('  Total FCPST :' + floattostr(valorFCPST));
      End;
    End;
  End;
end;

end.
