unit uClassNFeImportacao;

interface

uses
  SysUtils, SqlExpr, Provider, Forms, Contnrs, Windows, DateUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor,  uClassNFe;

Type
  TPessoaNFe = Class
  private
    FCod_MunicipioIBGE: Integer;
    FBairro: String;
    FRazao_Social: String;
    FInscEst: String;
    FUF: String;
    FCEP: String;
    FCNPJCPF: String;
    FNome_Fantasia: String;
    FCidade: String;
    FEndereco: String;
    FTelefone: String;
    FNumero: String;
    procedure SetBairro(const Value: String);
    procedure SetCEP(const Value: String);
    procedure SetCidade(const Value: String);
    procedure SetCNPJCPF(const Value: String);
    procedure SetCod_MunicipioIBGE(const Value: Integer);
    procedure SetEndereco(const Value: String);
    procedure SetInscEst(const Value: String);
    procedure SetNome_Fantasia(const Value: String);
    procedure SetRazao_Social(const Value: String);
    procedure SetTelefone(const Value: String);
    procedure SetUF(const Value: String);
    procedure SetNumero(const Value: String);
  public
     property Razao_Social : String read FRazao_Social write SetRazao_Social;
     property Nome_Fantasia : String read FNome_Fantasia write SetNome_Fantasia;
     property CNPJCPF : String read FCNPJCPF write SetCNPJCPF;
     property InscEst : String read FInscEst write SetInscEst;
     property Endereco : String read FEndereco write SetEndereco;
     property Numero : String read FNumero write SetNumero;
     property Bairro : String read FBairro write SetBairro;
     property CEP : String read FCEP write SetCEP;
     property Cod_MunicipioIBGE : Integer read FCod_MunicipioIBGE write SetCod_MunicipioIBGE;
     property Cidade : String read FCidade write SetCidade;
     property UF : String read FUF write SetUF;
     property Telefone : String read FTelefone write SetTelefone;
  end;

Type
  TDestinatarioNFe = Class( TPessoaNFe )
  end;

Type
  TFornecedorNFe = Class( TPessoaNFe )
  end;

Type
  {M+}
  TChaveAcessoNFe = Class(TPersistent)
  private
    FCodUF: String;
    FCNPJ: String;
    FValor: String;
    FDigVerif: String;
    FSerie: String;
    FCodDocFiscal: String;
    FModelo: String;
    FNumDocFiscal: String;
    FMesAno: String;
    procedure SetCNPJ(const Value: String);
    procedure SetCodDocFiscal(const Value: String);
    procedure SetCodUF(const Value: String);
    procedure SetDigVerif(const Value: String);
    procedure SetMesAno(const Value: String);
    procedure SetModelo(const Value: String);
    procedure SetNumDocFiscal(const Value: String);
    procedure SetSerie(const Value: String);
    procedure SetValor(const Value: String);
  published
    property Valor: String read FValor write SetValor;
    property CodUF: String read FCodUF write SetCodUF;
    property MesAno: String read FMesAno write SetMesAno;
    property CNPJ: String read FCNPJ write SetCNPJ;
    property Modelo: String read FModelo write SetModelo;
    property Serie: String read FSerie write SetSerie;
    property NumDocFiscal: String read FNumDocFiscal write SetNumDocFiscal;
    property CodDocFiscal: String read FCodDocFiscal write SetCodDocFiscal;
    property DigVerif: String read FDigVerif write SetDigVerif;
  End;
  {M-}
Type
  TNFeImportacao_Item_ICMS = Class
  private
    FSitA: Integer;
    FPerc_ICMS: Real;
    FPerc_MVA: Real;
    FPerc_Reducao_BaseCalculo_Substituido: Real;
    FVlr_BaseCalculo: Real;
    FVlr_ICMS_Substituido: Real;
    FModalidadeBaseCalculo_Substituido: Integer;
    FVlr_ICMS: Real;
    FModalidadeBaseCalculo: Integer;
    FVlr_BaseCalculo_Substituido: Real;
    FSitB: Integer;
    FPerc_ICMS_Substituido: Real;
    FVlr_FCP : Real;
    FVlr_FCPST : Real;
    procedure SetModalidadeBaseCalculo(const Value: Integer);
    procedure SetModalidadeBaseCalculo_Substituido(const Value: Integer);
    procedure SetPerc_ICMS(const Value: Real);
    procedure SetPerc_MVA(const Value: Real);
    procedure SetPerc_Reducao_BaseCalculo_Substituido(const Value: Real);
    procedure SetSitA(const Value: Integer);
    procedure SetSitB(const Value: Integer);
    procedure SetVlr_BaseCalculo(const Value: Real);
    procedure SetVlr_BaseCalculo_Substituido(const Value: Real);
    procedure SetVlr_ICMS(const Value: Real);
    procedure SetVlr_ICMS_Substituido(const Value: Real);
    procedure SetPerc_ICMS_Substituido(const Value: Real);
  public
     property SitA : Integer read FSitA write SetSitA;
     property SitB : Integer read FSitB write SetSitB;
     property ModalidadeBaseCalculo : Integer read FModalidadeBaseCalculo write SetModalidadeBaseCalculo;
     property Vlr_BaseCalculo : Real read FVlr_BaseCalculo write SetVlr_BaseCalculo;
     property Perc_ICMS : Real read FPerc_ICMS write SetPerc_ICMS;
     property Vlr_ICMS : Real read FVlr_ICMS write SetVlr_ICMS;
     property ModalidadeBaseCalculo_Substituido : Integer read FModalidadeBaseCalculo_Substituido write SetModalidadeBaseCalculo_Substituido;
     property Perc_MVA : Real read FPerc_MVA write SetPerc_MVA;
     property Perc_Reducao_BaseCalculo_Substituido : Real read FPerc_Reducao_BaseCalculo_Substituido write SetPerc_Reducao_BaseCalculo_Substituido;
     property Vlr_BaseCalculo_Substituido : Real read FVlr_BaseCalculo_Substituido write SetVlr_BaseCalculo_Substituido;
     property Perc_ICMS_Substituido : Real read FPerc_ICMS_Substituido write SetPerc_ICMS_Substituido;
     property Vlr_ICMS_Substituido : Real read FVlr_ICMS_Substituido write SetVlr_ICMS_Substituido;
     property Vlr_FCP : Real   read FVlr_FCP write FVlr_FCP;
     property Vlr_FCPST : Real read FVlr_FCPST write FVlr_FCPST;
  end;

Type
  TNFeImportacao_Item_IPI = Class
  private
    FSitTribitaria: Integer;
    FVlr_IPI: Real;
    FVlr_BaseCalculo: Real;
    FVlr_Unitario: Real;
    FPerc_IPI: Real;
    FQtdeTotal: Real;
    procedure SetPerc_IPI(const Value: Real);
    procedure SetQtdeTotal(const Value: Real);
    procedure SetSitTribitaria(const Value: Integer);
    procedure SetVlr_BaseCalculo(const Value: Real);
    procedure SetVlr_IPI(const Value: Real);
    procedure SetVlr_Unitario(const Value: Real);
  public
     property SitTribitaria : Integer read FSitTribitaria write SetSitTribitaria;
     property Vlr_BaseCalculo : Real read FVlr_BaseCalculo write SetVlr_BaseCalculo;
     property QtdeTotal : Real read FQtdeTotal write SetQtdeTotal;
     property Vlr_Unitario : Real read FVlr_Unitario write SetVlr_Unitario;
     property Perc_IPI : Real read FPerc_IPI write SetPerc_IPI;
     property Vlr_IPI : Real read FVlr_IPI write SetVlr_IPI;
  end;

Type
   {M+}
   TNFeImportacao_Item = Class(TPersistent)
   private
      FPerc_ICMS: Real;
      FPerc_RedBase: Real;
      FQtde_Atendida : Real;
      FVlr_ICMSSubs: Real;
      FDescricao: String;
      FPco_Liquido: Real;
      FCod_Produto: String;
      FVlr_ICMS: Real;
      FVlr_RedBase: Real;
      FCod_Barra: String;
      FPerc_ICMSSubs: Real;
      FVlr_Total: Real;
      FSitB: String;
      FCod_CFOP: String;
      FUnid: String;
      FNFeImportacao_Item_ICMS: TNFeImportacao_Item_ICMS;
      FNFeImportacao_Item_IPI: TNFeImportacao_Item_IPI;
      FCod_NCM: String;
      FCod_ANP: Integer;
      FFCI: string;
      FVlr_Desc: Real;
      procedure SetCod_Barra(const Value: String);
      procedure SetCod_Produto(const Value: String);
      procedure SetDescricao(const Value: String);
      procedure SetPco_Liquido(const Value: Real);
      procedure SetPerc_ICMS(const Value: Real);
      procedure SetPerc_ICMSSubs(const Value: Real);
      procedure SetPerc_RedBase(const Value: Real);
      procedure SetQtde_Atendida(const Value: Real);
      procedure SetSitB(const Value: String);
      procedure SetVlr_ICMS(const Value: Real);
      procedure SetVlr_ICMSSubs(const Value: Real);
      procedure SetVlr_RedBase(const Value: Real);
      procedure SetVlr_Total(const Value: Real);
      procedure SetCod_CFOP(const Value: String);
      procedure SetUnid(const Value: String);
      procedure SetNFeImportacao_Item_ICMS(const Value: TNFeImportacao_Item_ICMS);
      procedure SetNFeImportacao_Item_IPI(const Value: TNFeImportacao_Item_IPI);
      procedure SetCod_NCM(const Value: String);
      procedure SetCod_ANP(const Value: Integer);
      procedure SetFCI(const Value: string);
      procedure SetVlr_Desc(const Value: Real);
   public
      property NFeImportacao_Item_ICMS : TNFeImportacao_Item_ICMS read FNFeImportacao_Item_ICMS write SetNFeImportacao_Item_ICMS;
      property NFeImportacao_Item_IPI : TNFeImportacao_Item_IPI read FNFeImportacao_Item_IPI write SetNFeImportacao_Item_IPI;
   published
      property Cod_Produto : String read FCod_Produto write SetCod_Produto;
      property Cod_Barra : String read FCod_Barra write SetCod_Barra;
      property Cod_ANP : Integer read FCod_ANP write SetCod_ANP;
      property Descricao : String read FDescricao write SetDescricao;
      property Cod_CFOP : String read FCod_CFOP write SetCod_CFOP;
      property Cod_NCM : String read FCod_NCM write SetCod_NCM;
      property Unid : String read FUnid write SetUnid;
      property SitB : String read FSitB write SetSitB;
      property Perc_ICMS : Real read FPerc_ICMS write SetPerc_ICMS;
      property Perc_ICMSSubs : Real read FPerc_ICMSSubs write SetPerc_ICMSSubs;
      property Perc_RedBase : Real read FPerc_RedBase write SetPerc_RedBase;
      property Vlr_ICMS : Real read FVlr_ICMS write SetVlr_ICMS;
      property Vlr_ICMSSubs : Real read FVlr_ICMSSubs write SetVlr_ICMSSubs;
      property Vlr_RedBase : Real read FVlr_RedBase write SetVlr_RedBase;
      property Qtde_Atendida : Real read FQtde_Atendida write SetQtde_Atendida;
      property Pco_Liquido : Real read FPco_Liquido write SetPco_Liquido;
      property Vlr_Total : Real read FVlr_Total write SetVlr_Total;
      property Vlr_Desc : Real read FVlr_Desc write SetVlr_Desc;

      property FCI: string read FFCI write SetFCI;
      constructor Create;
      destructor Destroy; override;
   end;
   {M-}
Type
  TNFeImportacao_Itens = Class( TObjectList )
  private
     function GetItem(Index: Integer): TNFeImportacao_Item;
     procedure SetItem(Index: Integer; AObject: TNFeImportacao_Item);
  public
     function Add(AObject: TNFeImportacao_Item): Integer;
     property Items[Index: Integer]: TNFeImportacao_Item read GetItem write SetItem; default;
  End;

Type
   TNFeImportacao_Fatura = Class
   private
    FVlr_Bruto: Real;
    FVlr_Desconto: Real;
    FDocumento: String;
    FVlr_Liquido: Real;
    procedure SetDocumento(const Value: String);
    procedure SetVlr_Bruto(const Value: Real);
    procedure SetVlr_Desconto(const Value: Real);
    procedure SetVlr_Liquido(const Value: Real);
   public
      property Documento : String read FDocumento write SetDocumento;
      property Vlr_Bruto : Real read FVlr_Bruto write SetVlr_Bruto;
      property Vlr_Desconto : Real read FVlr_Desconto write SetVlr_Desconto;
      property Vlr_Liquido : Real read FVlr_Liquido write SetVlr_Liquido;
   end;

Type
   TNFeImportacao_Duplicata = Class
   private
      FVencimento: TDateTime;
      FDocumento: String;
      FVlr_APagar: Real;
      procedure SetDocumento(const Value: String);
      procedure SetVencimento(const Value: TDateTime);
      procedure SetVlr_APagar(const Value: Real);
   public
      property Documento : String read FDocumento write SetDocumento;
      property Vencimento : TDateTime read FVencimento write SetVencimento;
      property Vlr_APagar : Real read FVlr_APagar write SetVlr_APagar;
   end;

Type
  TNFeImportacao_Duplicatas = Class( TObjectList )
  private
     FOwnsObjects : Boolean;
     function GetItem(Index: Integer): TNFeImportacao_Duplicata;
     procedure SetItem(Index: Integer; AObject: TNFeImportacao_Duplicata);
  public
     function Add(AObject: TNFeImportacao_Duplicata): Integer;
     function Remove(AObject: TNFeImportacao_Duplicata): Integer;
     function IndexOf(AObject: TNFeImportacao_Duplicata): Integer;
     procedure Insert(Index: Integer; AObject: TNFeImportacao_Duplicata);
     property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
     property Items[Index: Integer]: TNFeImportacao_Duplicata read GetItem write SetItem; default;
  End;

Type
   TNFeImportacao = Class
   private
      FVlr_Frete: Real;
      FVlr_IPI: Real;
      FVlr_ICMSSubs: Real;
      FArredondamento: Real;
      FVlr_BaseICMS: Real;
      FSeqPedVendaDevolvida: String;
      FSeqNotaFiscal: String;
      FData_Cad: TDateTime;
      FStatus: String;
      FVlr_ICMS: Real;
      FVlr_Seguro: Real;
      FSeqPedVenda: String;
      FVlr_Total: Real;
      FCod_Cliente: String;
      FCod_Fornecedor_Sai: String;
      FE_S: String;
      FDesc_Comercial: Real;
      FVlr_Despesas: Real;
      FVlr_BaseICMSSubs: Real;
      FNFeImportacao_Itens: TNFeImportacao_Itens;
      FFornecedorNFe: TFornecedorNFe;
      FRetornoNFe: TNFe;
      FSerie: String;
      FEspecieNF: String;
      FNFeImportacao_Duplicatas: TNFeImportacao_Duplicatas;
      FDestinatarioNFe: TDestinatarioNFe;
      FVlr_Produtos: Real;
      FNFeImportacao_Fatura: TNFeImportacao_Fatura;
      FSimplesNacional: Boolean;
      FNFE_Chave_referente: String;
      FInformacaoAdicional: String;
      FFinalidade: String;
      FVlr_FCP:  Real;
      FVlr_FCPST:  Real;
      procedure SetArredondamento(const Value: Real);
      procedure SetCod_Cliente(const Value: String);
      procedure SetCod_Fornecedor_Sai(const Value: String);
      procedure SetData_Cad(const Value: TDateTime);
      procedure SetDesc_Comercial(const Value: Real);
      procedure SetE_S(const Value: String);
      procedure SetSeqNotaFiscal(const Value: String);
      procedure SetSeqPedVenda(const Value: String);
      procedure SetSeqPedVendaDevolvida(const Value: String);
      procedure SetStatus(const Value: String);
      procedure SetVlr_BaseICMS(const Value: Real);
      procedure SetVlr_BaseICMSSubs(const Value: Real);
      procedure SetVlr_Despesas(const Value: Real);
      procedure SetVlr_Frete(const Value: Real);
      procedure SetVlr_ICMS(const Value: Real);
      procedure SetVlr_ICMSSubs(const Value: Real);
      procedure SetVlr_IPI(const Value: Real);
      procedure SetVlr_Seguro(const Value: Real);
      procedure SetVlr_Total(const Value: Real);
      procedure SetNFeImportacao_Itens(const Value: TNFeImportacao_Itens);
      procedure SetFornecedorNFe(const Value: TFornecedorNFe);
      procedure SetRetornoNFe(const Value: TNFe);
      procedure SetSerie(const Value: String);
      procedure SetEspecieNF(const Value: String);
      procedure SetNFeImportacao_Duplicatas(const Value: TNFeImportacao_Duplicatas);
      procedure SetDestinatarioNFe(const Value: TDestinatarioNFe);
      procedure SetVlr_Produtos(const Value: Real);
      procedure SetNFeImportacao_Fatura(const Value: TNFeImportacao_Fatura);
      procedure SetSimplesNacional(const Value: Boolean);
      procedure SetNFE_Chave_referente(const Value: String);
    procedure SetInformacaoAdicional(const Value: String);
    procedure SetFinalidade(const Value: String);

   public
      property SeqNotaFiscal : String read FSeqNotaFiscal write SetSeqNotaFiscal;
      property Serie : String read FSerie write SetSerie;
      property EspecieNF : String read FEspecieNF write SetEspecieNF;
      property SimplesNacional : Boolean read FSimplesNacional write SetSimplesNacional;
      property SeqPedVenda : String read FSeqPedVenda write SetSeqPedVenda;
      property E_S : String read FE_S write SetE_S;
      property Status : String read FStatus write SetStatus;
      property SeqPedVendaDevolvida : String read FSeqPedVendaDevolvida write SetSeqPedVendaDevolvida;
      property Cod_Cliente : String read FCod_Cliente write SetCod_Cliente;
      property Cod_Fornecedor_Sai : String read FCod_Fornecedor_Sai write SetCod_Fornecedor_Sai;
      property Data_Cad : TDateTime read FData_Cad write SetData_Cad;
      property Vlr_BaseICMS : Real read FVlr_BaseICMS write SetVlr_BaseICMS;
      property Vlr_ICMS : Real read FVlr_ICMS write SetVlr_ICMS;
      property Vlr_BaseICMSSubs : Real read FVlr_BaseICMSSubs write SetVlr_BaseICMSSubs;
      property Vlr_ICMSSubs : Real read FVlr_ICMSSubs write SetVlr_ICMSSubs;
      property Vlr_Frete : Real read FVlr_Frete write SetVlr_Frete;
      property Vlr_Seguro : Real read FVlr_Seguro write SetVlr_Seguro;
      property Vlr_Despesas : Real read FVlr_Despesas write SetVlr_Despesas;
      property Vlr_IPI : Real read FVlr_IPI write SetVlr_IPI;
      property Desc_Comercial : Real read FDesc_Comercial write SetDesc_Comercial;
      property Arredondamento : Real read FArredondamento write SetArredondamento;
      property Vlr_Produtos : Real read FVlr_Produtos write SetVlr_Produtos;
      property Vlr_Total : Real read FVlr_Total write SetVlr_Total;
      property Vlr_FCP:  Real read FVlr_FCP write FVlr_FCP;
      property Vlr_FCPST:  Real read FVlr_FCPST write FVlr_FCPST;
      //public List<string> NFE_Chaves_Devolvidas { get; set; }
      property DestinatarioNFe : TDestinatarioNFe read FDestinatarioNFe write SetDestinatarioNFe;
      property FornecedorNFe : TFornecedorNFe read FFornecedorNFe write SetFornecedorNFe;
      property NFeImportacao_Itens : TNFeImportacao_Itens read FNFeImportacao_Itens write SetNFeImportacao_Itens;
      property RetornoNFe : TNFe read FRetornoNFe write SetRetornoNFe;
      property NFeImportacao_Fatura : TNFeImportacao_Fatura read FNFeImportacao_Fatura write SetNFeImportacao_Fatura;
      property NFeImportacao_Duplicatas : TNFeImportacao_Duplicatas read FNFeImportacao_Duplicatas write SetNFeImportacao_Duplicatas;
      //public string NFe_Motivo_Retorno { get; set; }
      procedure LerXml( XMLNFe: String );
      property NFE_Chave_referente : String read FNFE_Chave_referente write SetNFE_Chave_referente;
      property Finalidade : String read FFinalidade write SetFinalidade;
      function DesmembrarChaveAcesso( prsChaveAcesso: String ): TChaveAcessoNFe;
      property InformacaoAdicional : String read FInformacaoAdicional write SetInformacaoAdicional;

      constructor Create;
      Destructor Destroy; override;
   end;


implementation

{ NFeImportacao }

constructor TNFeImportacao.Create;
begin
   inherited;

   FNFeImportacao_Itens       := TNFeImportacao_Itens.Create;
   FNFeImportacao_Fatura      := TNFeImportacao_Fatura.Create;
   FNFeImportacao_Duplicatas  := TNFeImportacao_Duplicatas.Create;

end;

function TNFeImportacao.DesmembrarChaveAcesso( prsChaveAcesso: String ): TChaveAcessoNFe;
begin
   Result := TChaveAcessoNFe.Create;
   with Result do
   begin
      FValor        := prsChaveAcesso;
      FCodUF        := Copy( prsChaveAcesso, 1, 2 );
      FMesAno       := Copy( prsChaveAcesso, 3, 4 );
      FCNPJ         := Copy( prsChaveAcesso, 7, 14 );
      FModelo       := Copy( prsChaveAcesso, 21, 2 );
      FSerie        := Copy( prsChaveAcesso, 23, 3 );
      FNumDocFiscal := Copy( prsChaveAcesso, 26, 9 );
      FCodDocFiscal := Copy( prsChaveAcesso, 35, 9 );
      FCodDocFiscal := Copy( prsChaveAcesso, 44, 1 );
   end;
end;

destructor TNFeImportacao.Destroy;
begin
   If Assigned( FDestinatarioNFe ) Then
      FreeAndNil( FDestinatarioNFe );
   If Assigned( FFornecedorNFe ) Then
      FreeAndNil( FFornecedorNFe );
   If Assigned( FNFeImportacao_Itens ) Then
      FreeAndNil( FNFeImportacao_Itens );
   If Assigned( FNFeImportacao_Fatura ) Then
      FreeAndNil( FNFeImportacao_Fatura );
   If Assigned( FNFeImportacao_Duplicatas ) Then
      FreeAndNil( FNFeImportacao_Duplicatas );

   inherited;
end;

procedure TNFeImportacao.SetArredondamento(const Value: Real);
begin
  FArredondamento := Value;
end;

procedure TNFeImportacao.SetCod_Cliente(const Value: String);
begin
  FCod_Cliente := Value;
end;

procedure TNFeImportacao.SetCod_Fornecedor_Sai(const Value: String);
begin
  FCod_Fornecedor_Sai := Value;
end;

procedure TNFeImportacao.SetData_Cad(const Value: TDateTime);
begin
  FData_Cad := Value;
end;

procedure TNFeImportacao.SetDesc_Comercial(const Value: Real);
begin
  FDesc_Comercial := Value;
end;

procedure TNFeImportacao.SetDestinatarioNFe(const Value: TDestinatarioNFe);
begin
  FDestinatarioNFe := Value;
end;

procedure TNFeImportacao.SetEspecieNF(const Value: String);
begin
  FEspecieNF := Value;
end;

procedure TNFeImportacao.SetE_S(const Value: String);
begin
  FE_S := Value;
end;

procedure TNFeImportacao.SetFinalidade(const Value: String);
begin
  FFinalidade := Value;
end;

procedure TNFeImportacao.SetFornecedorNFe(const Value: TFornecedorNFe);
begin
  FFornecedorNFe := Value;
end;

procedure TNFeImportacao.SetInformacaoAdicional(const Value: String);
begin
  FInformacaoAdicional := Value;
end;

procedure TNFeImportacao.SetNFeImportacao_Duplicatas(const Value: TNFeImportacao_Duplicatas);
begin
  FNFeImportacao_Duplicatas := Value;
end;

procedure TNFeImportacao.SetNFeImportacao_Fatura(const Value: TNFeImportacao_Fatura);
begin
  FNFeImportacao_Fatura := Value;
end;

procedure TNFeImportacao.SetNFeImportacao_Itens(const Value: TNFeImportacao_Itens);
begin
  FNFeImportacao_Itens := Value;
end;

procedure TNFeImportacao.SetNFE_Chave_referente(const Value: String);
begin
  FNFE_Chave_referente := Value;
end;

procedure TNFeImportacao.SetRetornoNFe(const Value: TNFe);
begin
  FRetornoNFe := Value;
end;

procedure TNFeImportacao.SetSeqNotaFiscal(const Value: String);
begin
  FSeqNotaFiscal := Value;
end;

procedure TNFeImportacao.SetSeqPedVenda(const Value: String);
begin
  FSeqPedVenda := Value;
end;

procedure TNFeImportacao.SetSeqPedVendaDevolvida(const Value: String);
begin
  FSeqPedVendaDevolvida := Value;
end;

procedure TNFeImportacao.SetSerie(const Value: String);
begin
  FSerie := Value;
end;

procedure TNFeImportacao.SetSimplesNacional(const Value: Boolean);
begin
  FSimplesNacional := Value;
end;

procedure TNFeImportacao.SetStatus(const Value: String);
begin
  FStatus := Value;
end;

procedure TNFeImportacao.SetVlr_BaseICMS(const Value: Real);
begin
  FVlr_BaseICMS := Value;
end;

procedure TNFeImportacao.SetVlr_BaseICMSSubs(const Value: Real);
begin
  FVlr_BaseICMSSubs := Value;
end;

procedure TNFeImportacao.SetVlr_Despesas(const Value: Real);
begin
  FVlr_Despesas := Value;
end;

procedure TNFeImportacao.SetVlr_Frete(const Value: Real);
begin
  FVlr_Frete := Value;
end;

procedure TNFeImportacao.SetVlr_ICMS(const Value: Real);
begin
  FVlr_ICMS := Value;
end;

procedure TNFeImportacao.SetVlr_ICMSSubs(const Value: Real);
begin
  FVlr_ICMSSubs := Value;
end;

procedure TNFeImportacao.SetVlr_IPI(const Value: Real);
begin
  FVlr_IPI := Value;
end;

procedure TNFeImportacao.SetVlr_Produtos(const Value: Real);
begin
  FVlr_Produtos := Value;
end;

procedure TNFeImportacao.SetVlr_Seguro(const Value: Real);
begin
  FVlr_Seguro := Value;
end;

procedure TNFeImportacao.SetVlr_Total(const Value: Real);
begin
  FVlr_Total := Value;
end;

{ NFeImportacao_Item }

constructor TNFeImportacao_Item.Create;
begin
   Cod_ANP := 0;
end;

destructor TNFeImportacao_Item.Destroy;
begin
   If Assigned( FNFeImportacao_Item_ICMS ) Then
      FreeAndNil( FNFeImportacao_Item_ICMS );
   If Assigned( FNFeImportacao_Item_IPI ) Then
      FreeAndNil( FNFeImportacao_Item_IPI );

  inherited;
end;

procedure TNFeImportacao_Item.SetCod_ANP(const Value: Integer);
begin
  FCod_ANP := Value;
end;

procedure TNFeImportacao_Item.SetCod_Barra(const Value: String);
begin
  FCod_Barra := Value;
end;

procedure TNFeImportacao_Item.SetCod_CFOP(const Value: String);
begin
  FCod_CFOP := Value;
end;

procedure TNFeImportacao_Item.SetCod_NCM(const Value: String);
begin
  FCod_NCM := Value;
end;

procedure TNFeImportacao_Item.SetCod_Produto(const Value: String);
begin
  FCod_Produto := Value;
end;

procedure TNFeImportacao_Item.SetDescricao(const Value: String);
begin
  FDescricao := Value;
end;

procedure TNFeImportacao_Item.SetFCI(const Value: string);
begin
  FFCI := Value;
end;

procedure TNFeImportacao_Item.SetNFeImportacao_Item_ICMS(
  const Value: TNFeImportacao_Item_ICMS);
begin
  FNFeImportacao_Item_ICMS := Value;
end;

procedure TNFeImportacao_Item.SetNFeImportacao_Item_IPI(const Value: TNFeImportacao_Item_IPI);
begin
  FNFeImportacao_Item_IPI := Value;
end;

procedure TNFeImportacao_Item.SetPco_Liquido(const Value: Real);
begin
  FPco_Liquido := Value;
end;

procedure TNFeImportacao_Item.SetPerc_ICMS(const Value: Real);
begin
  FPerc_ICMS := Value;
end;

procedure TNFeImportacao_Item.SetPerc_ICMSSubs(const Value: Real);
begin
  FPerc_ICMSSubs := Value;
end;

procedure TNFeImportacao_Item.SetPerc_RedBase(const Value: Real);
begin
  FPerc_RedBase := Value;
end;

procedure TNFeImportacao_Item.SetQtde_Atendida(const Value: Real);
begin
  FQtde_Atendida := Value;
end;

procedure TNFeImportacao_Item.SetSitB(const Value: String);
begin
  FSitB := Value;
end;

procedure TNFeImportacao_Item.SetUnid(const Value: String);
begin
  FUnid := Value;
end;

procedure TNFeImportacao_Item.SetVlr_Desc(const Value: Real);
begin
  FVlr_Desc := Value;
end;

procedure TNFeImportacao_Item.SetVlr_ICMS(const Value: Real);
begin
  FVlr_ICMS := Value;
end;

procedure TNFeImportacao_Item.SetVlr_ICMSSubs(const Value: Real);
begin
  FVlr_ICMSSubs := Value;
end;

procedure TNFeImportacao_Item.SetVlr_RedBase(const Value: Real);
begin
  FVlr_RedBase := Value;
end;

procedure TNFeImportacao_Item.SetVlr_Total(const Value: Real);
begin
  FVlr_Total := Value;
end;

{ TNFeImportacao_Duplicata }

procedure TNFeImportacao_Duplicata.SetDocumento(const Value: String);
begin
  FDocumento := Value;
end;

procedure TNFeImportacao_Duplicata.SetVencimento(const Value: TDateTime);
begin
  FVencimento := Value;
end;

procedure TNFeImportacao_Duplicata.SetVlr_APagar(const Value: Real);
begin
  FVlr_APagar := Value;
end;

procedure TNFeImportacao.LerXml( XMLNFe: String );
var //Ok             : Boolean;
    i, j, l, k         : Integer;
    lNFeImportacao_Item     : TNFeImportacao_Item;
    lNFeImportacao_Duplicata : TNFeImportacao_Duplicata;
    lsCaracter : String;
    Leitor : TLeitor;
begin
  Leitor := TLeitor.Create;
  Try
     Leitor.CarregarArquivo( XMLNFe );

     lsCaracter := '"';

     I := 0;
     I := RetornarPosEx( 'Id=', Leitor.Arquivo, I + 6 );
     if I = 0 then
       raise Exception.Create( 'Não encontrei inicio do URI: Id=' );
     L := RetornarPosEx( '"', Leitor.Arquivo, I + 2 );
     if L = 0 then
     Begin
        lsCaracter := #39;
        L := RetornarPosEx( lsCaracter, Leitor.Arquivo, I + 2 );
     End;
     if L = 0 then
       raise Exception.Create( 'Não encontrei inicio do URI: aspas inicial' );
     J := RetornarPosEx( lsCaracter, Leitor.Arquivo, L + 1 );
     if J = 0 then
       raise Exception.Create( 'Não encontrei inicio do URI: aspas final' );

     Self.FornecedorNFe   := TFornecedorNFe.Create;
     Self.DestinatarioNFe := TDestinatarioNFe.Create;

     Self.RetornoNFe := TNFe.Create;
     Self.RetornoNFe.ChaveAcesso := Copy( Leitor.Arquivo, L + 4, J - L - 4 );

     //NFe.infNFe.ID := Copy( Leitor.Arquivo, I + 4, J - I - 4 );

     (* Grupo da TAG <ide> *******************************************************)
     if Leitor.rExtrai(1, 'ide') <> '' then
     begin
        Self.SeqNotaFiscal := IntToStr( Leitor.rCampo( tcInt, 'nNF' ) );
        Self.Data_Cad      := Leitor.rCampo( tcDat, 'dhEmi' );
        Self.EspecieNF     := Leitor.rCampo( tcStr, 'mod' );
        Self.Serie         := IntToStr( Leitor.rCampo( tcInt, 'serie' ) );
        Case StrToIntDef( Leitor.rCampo( tcStr, 'tpNF' ), 0 ) Of
           0 : Self.E_S := 'E'; // Entrada
           1 : Self.E_S := 'S'; // Saída
        End;
        Case StrToIntDef( Leitor.rCampo(tcStr, 'indPag'), 0 ) Of
           0 : ; // pagamento à vista;
           1 : ; // pagamento à prazo;
           2 : ; // outros.
        End;

        If Leitor.rCampo(tcStr, 'finNFe') = '2' then
        Begin
           Self.Finalidade := Leitor.rCampo(tcStr, 'finNFe');
           Self.NFE_Chave_referente := Leitor.rCampo(tcStr, 'refNFe');
        End;

       (*B02*)//NFe.ide.cUF := Leitor.rCampo(tcInt, 'cUF');
       (*B03*)//NFe.ide.cNF := Leitor.rCampo(tcInt, 'cNF');
       (*B04*)//NFe.ide.natOp := Leitor.rCampo(tcStr, 'natOp');
       (*B05*)//NFe.ide.indPag := StrToIndpag(ok, Leitor.rCampo(tcStr, 'indPag'));
       (*B06*)//NFe.ide.modelo := Leitor.rCampo(tcStr, 'mod');
       (*B07*)//NFe.ide.serie := Leitor.rCampo(tcInt, 'serie');
       (*B08*)//NFe.ide.nNF := Leitor.rCampo(tcInt, 'nNF');
       (*B09*)//NFe.ide.dEmi := Leitor.rCampo(tcDat, 'dEmi');
       (*B10*)//NFe.ide.dSaiEnt := Leitor.rCampo(tcDat, 'dSaiEnt');
       (*B11*)//NFe.ide.tpNF := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
       (*B12*)//NFe.ide.cMunFG := Leitor.rCampo(tcInt, 'cMunFG');

       (*B21*)//NFe.Ide.tpImp := StrToTpImp(ok, Leitor.rCampo(tcStr, 'tpImp'));
       (*B22*)//NFe.Ide.tpEmis := StrToTpEmis(ok, Leitor.rCampo(tcStr, 'tpEmis'));
       (*B23*)//NFe.Ide.cDV := Leitor.rCampo(tcInt, 'cDV');
       (*B24*)//NFe.Ide.tpAmb := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
       (*B25*)//NFe.Ide.finNFe := StrToFinNFe(ok, Leitor.rCampo(tcStr, 'finNFe'));
       (*B26*)//NFe.Ide.procEmi := StrToProcEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
       (*B27*)//NFe.Ide.verProc := Leitor.rCampo(tcStr, 'verProc');

       (* Grupo da TAG <ide><NFref> *)
       i := 0;
       Leitor.rExtrai(1, 'ide');
       while Leitor.rExtrai(2, 'NFref', '', i + 1) <> '' do
       begin
         //NFe.Ide.NFref.Add;
         (*B13*)//NFe.ide.NFref[i].refNFe := Leitor.rCampo(tcEsp, 'refNFe');
         (*B15*)//NFe.Ide.NFref[i].RefNF.cUF := Leitor.rCampo(tcInt, 'cUF');
         (*B16*)//NFe.Ide.NFref[i].RefNF.AAMM := Leitor.rCampo(tcEsp, 'AAMM');
         (*B17*)//NFe.Ide.NFref[i].RefNF.CNPJ := Leitor.rCampo(tcEsp, 'CNPJ');
         (*B18*)//NFe.Ide.NFref[i].RefNF.Modelo := StrToInt(Leitor.rCampo(tcInt, 'mod'));
         (*B19*)//NFe.ide.NFref[i].RefNF.serie := Leitor.rCampo(tcInt, 'serie');
         (*B20*)//NFe.Ide.NFref[i].RefNF.nNF := Leitor.rCampo(tcInt, 'nNF');
         inc(i);
       end;

     end;

     (* Grupo da TAG <emit> ******************************************************)
     if Leitor.rExtrai(1, 'emit') <> '' then
     begin
       Self.FornecedorNFe.CNPJCPF       := Leitor.rCampoCNPJCPF;
       Self.FornecedorNFe.Razao_Social  := Leitor.rCampo( tcStr, 'xNome' );
       Self.FornecedorNFe.Nome_Fantasia := Leitor.rCampo( tcStr, 'xFant' );
       Self.FornecedorNFe.InscEst       := Leitor.rCampo( tcStr, 'IE' );
       Self.SimplesNacional             := (Leitor.rCampo( tcStr, 'CRT' ) = '1');

       (*C02/C02a*)//NFe.Emit.CNPJCPF := Leitor.rCampoCNPJCPF;
       (*C03*)//NFe.Emit.xNome := Leitor.rCampo(tcStr, 'xNome');
       (*C04*)//NFe.Emit.xFant := Leitor.rCampo(tcStr, 'xFant');
       (*C17*)//NFe.Emit.IE := Leitor.rCampo(tcStr, 'IE');
       (*C18*)//NFe.Emit.IEST := Leitor.rCampo(tcStr, 'IEST');
       (*C19*)//NFe.Emit.IM := Leitor.rCampo(tcStr, 'IM');
       (*C20*)//NFe.Emit.CNAE := Leitor.rCampo(tcStr, 'CNAE');
     end;

     (* Grupo da TAG <emit><EnderEmit> *)
     if Leitor.rExtrai(1, 'emit') <> '' then
     begin
       if Leitor.rExtrai(2, 'enderEmit') <> '' then
       begin
          Self.FornecedorNFe.Endereco          := Leitor.rCampo( tcStr, 'xLgr' );
          Self.FornecedorNFe.Numero            := Leitor.rCampo( tcStr, 'nro' );
          Self.FornecedorNFe.Bairro            := Leitor.rCampo( tcStr, 'xBairro' );
          Self.FornecedorNFe.Cod_MunicipioIBGE := Leitor.rCampo( tcInt, 'cMun' );
          Self.FornecedorNFe.Cidade            := Leitor.rCampo( tcStr, 'xMun' );
          Self.FornecedorNFe.UF                := Leitor.rCampo( tcStr, 'UF' );
          Self.FornecedorNFe.CEP               := Leitor.rCampo( tcStr, 'CEP' );
          Self.FornecedorNFe.Telefone          := Leitor.rCampo( tcStr, 'fone' );

         (*C06*)//NFe.Emit.enderEmit.xLgr := Leitor.rCampo(tcStr, 'xLgr');
         (*C07*)//NFe.Emit.enderEmit.nro := Leitor.rCampo(tcStr, 'nro');
         (*C08*)//NFe.Emit.enderEmit.xCpl := Leitor.rCampo(tcStr, 'xCpl');
         (*C09*)//NFe.Emit.enderEmit.xBairro := Leitor.rCampo(tcStr, 'xBairro');
         (*C10*)//NFe.Emit.EnderEmit.cMun := Leitor.rCampo(tcInt, 'cMun');
         (*C11*)//NFe.Emit.enderEmit.xMun := Leitor.rCampo(tcStr, 'xMun');
         (*C12*)//NFe.Emit.enderEmit.UF := Leitor.rCampo(tcStr, 'UF');
         (*C13*)//NFe.Emit.enderEmit.CEP := Leitor.rCampo(tcInt, 'CEP');
         (*C14*)//NFe.Emit.enderEmit.cPais := Leitor.rCampo(tcInt, 'cPais');
         (*C15*)//NFe.Emit.enderEmit.xPais := Leitor.rCampo(tcStr, 'xPais');
         (*C16*)//NFe.Emit.enderEmit.fone := Leitor.rCampo(tcStr, 'fone');
       end;
     end;

     (* Grupo da TAG <avulsa> ****************************************************)
     if Leitor.rExtrai(1, 'avulsa') <> '' then
     begin
       (*D02*)//NFe.Avulsa.CNPJ := Leitor.rCampo(tcStr, 'CNPJ');
       (*D03*)//NFe.Avulsa.xOrgao := Leitor.rCampo(tcStr, 'xOrgao');
       (*D04*)//NFe.Avulsa.matr := Leitor.rCampo(tcStr, 'matr');
       (*D05*)//NFe.Avulsa.xAgente := Leitor.rCampo(tcStr, 'xAgente');
       (*D06*)//NFe.Avulsa.fone := Leitor.rCampo(tcStr, 'fone');
       (*D07*)//NFe.Avulsa.UF := Leitor.rCampo(tcStr, 'UF');
       (*D08*)//NFe.Avulsa.nDAR := Leitor.rCampo(tcStr, 'nDAR');
       (*D09*)//NFe.Avulsa.dEmi := Leitor.rCampo(tcDat, 'dEmi');
       (*D10*)//NFe.Avulsa.vDAR := Leitor.rCampo(tcDe2, 'vDAR');
       (*D11*)//NFe.Avulsa.repEmi := Leitor.rCampo(tcStr, 'repEmi');
       (*D12*)//NFe.Avulsa.dPag := Leitor.rCampo(tcDat, 'dPag');
     end;

     (* Grupo da TAG <dest> ******************************************************)
     if Leitor.rExtrai(1, 'dest') <> '' then
     begin
       Self.DestinatarioNFe.CNPJCPF      := Leitor.rCampoCNPJCPF;
       Self.DestinatarioNFe.Razao_Social := Leitor.rCampo( tcStr, 'xNome' );
       Self.DestinatarioNFe.InscEst      := Leitor.rCampo( tcStr, 'IE' );

       (*E02/E03*)//NFe.Dest.CNPJCPF := Leitor.rCampoCNPJCPF;
       (*E04*)//NFe.Dest.xNome := Leitor.rCampo(tcStr, 'xNome');
       (*E17*)//NFe.Dest.IE := Leitor.rCampo(tcStr, 'IE');
       (*E18*)//NFe.Dest.ISUF := Leitor.rCampo(tcStr, 'ISUF');
     end;

     (* Grupo da TAG <dest> <EnderDest> *)
     if Leitor.rExtrai(1, 'dest') <> '' then
     begin
       if Leitor.rExtrai(2, 'enderDest') <> '' then
       begin
         (*E06*)//NFe.Dest.enderDest.xLgr := Leitor.rCampo(tcStr, 'xLgr');
         (*E07*)//NFe.Dest.enderDest.nro := Leitor.rCampo(tcStr, 'nro');
         (*E08*)//NFe.Dest.enderDest.xCpl := Leitor.rCampo(tcStr, 'xCpl');
         (*E09*)//NFe.Dest.enderDest.xBairro := Leitor.rCampo(tcStr, 'xBairro');
         (*E10*)//NFe.Dest.enderDest.cMun := Leitor.rCampo(tcInt, 'cMun');
         (*E11*)//NFe.Dest.enderDest.xMun := Leitor.rCampo(tcStr, 'xMun');
         (*E12*)//NFe.Dest.enderDest.UF := Leitor.rCampo(tcStr, 'UF');
         (*E13*)//NFe.Dest.enderDest.CEP := Leitor.rCampo(tcInt, 'CEP');
         (*E14*)//NFe.Dest.enderDest.cPais := Leitor.rCampo(tcInt, 'cPais');
         (*E15*)//NFe.Dest.enderDest.xPais := Leitor.rCampo(tcStr, 'xPais');
         (*E16*)//NFe.Dest.enderDest.fone := Leitor.rCampo(tcStr, 'fone');
       end;
     end;

     (* Grupo da TAG <retirada> **************************************************)
     if Leitor.rExtrai(1, 'retirada') <> '' then
     begin
       (*F02*)//NFe.Retirada.CNPJ := Leitor.rCampo(tcStr, 'CNPJ');
       (*F03*)//NFe.Retirada.xLgr := Leitor.rCampo(tcStr, 'xLgr');
       (*F04*)//NFe.Retirada.nro := Leitor.rCampo(tcStr, 'nro');
       (*F05*)//NFe.Retirada.xCpl := Leitor.rCampo(tcStr, 'xCpl');
       (*F06*)//NFe.Retirada.xBairro := Leitor.rCampo(tcStr, 'xBairro');
       (*F07*)//NFe.Retirada.cMun := Leitor.rCampo(tcInt, 'cMun');
       (*F08*)//NFe.Retirada.xMun := Leitor.rCampo(tcStr, 'xMun');
       (*F09*)//NFe.Retirada.UF := Leitor.rCampo(tcStr, 'UF');
     end;

     (* Grupo da TAG <entrega> ***************************************************)
     if Leitor.rExtrai(1, 'entrega') <> '' then
     begin
       (*G***)//NFe.Entrega.CNPJ := Leitor.rCampo(tcStr, 'CNPJ');
       (*G03*)//NFe.Entrega.xLgr := Leitor.rCampo(tcStr, 'xLgr');
       (*G04*)//NFe.Entrega.nro := Leitor.rCampo(tcStr, 'nro');
       (*G05*)//NFe.Entrega.xCpl := Leitor.rCampo(tcStr, 'xCpl');
       (*G06*)//NFe.Entrega.xBairro := Leitor.rCampo(tcStr, 'xBairro');
       (*G07*)//NFe.Entrega.cMun := Leitor.rCampo(tcInt, 'cMun');
       (*G08*)//NFe.Entrega.xMun := Leitor.rCampo(tcStr, 'xMun');
       (*G09*)//NFe.Entrega.UF := Leitor.rCampo(tcStr, 'UF');
     end;

     (* Grupo da TAG <det> *******************************************************)
     i := 0;
     while Leitor.rExtrai(1, 'det nItem=' + lsCaracter + IntToStr(i + 1) + lsCaracter, 'det', i + 1) <> '' do
     begin
       lNFeImportacao_Item := TNFeImportacao_Item.Create;
       //NFe.Det.Add;
       (*   *)//NFe.Det[i].prod.nItem := i + 1;
       (*V01*)//NFe.Det[i].infAdProd := Leitor.rCampo(tcStr, 'infAdProd');

       (* Grupo da TAG <det><prod> *)
       Leitor.rExtrai(2, 'prod');

       lNFeImportacao_Item.Cod_CFOP      := Leitor.rCampo( tcEsp, 'CFOP' );
       lNFeImportacao_Item.Cod_Produto   := Leitor.rCampo( tcStr, 'cProd' );
       lNFeImportacao_Item.Cod_Barra     := Leitor.rCampo( tcStr, 'cEAN' );
       lNFeImportacao_Item.Descricao     := Leitor.rCampo( tcStr, 'xProd' );
       lNFeImportacao_Item.Cod_NCM       := Leitor.rCampo( tcStr, 'NCM' );
       lNFeImportacao_Item.Unid          := Leitor.rCampo( tcEsp, 'uCom' );
       lNFeImportacao_Item.Qtde_Atendida := Leitor.rCampo( tcDe4, 'qCom' );
       lNFeImportacao_Item.Pco_Liquido   := Leitor.rCampo( tcDe4, 'vUnCom' ); // ** Manual_Integração_Contribuinte_versão_4.01-NT2009.006 **
                                                                         //Informar o valor unitário de comercialização do produto,
                                                                         //campo meramente informativo, o contribuinte pode utilizar a
                                                                         //precisão desejada (0-10 decimais). Para efeitos de cálculo,
                                                                         //o valor unitário será obtido pela divisão do valor do produto
                                                                         //pela quantidade comercial. (v2.0)
       lNFeImportacao_Item.Vlr_Total     := Leitor.rCampo( tcDe2, 'vProd' );
       lNFeImportacao_Item.FCI           := Leitor.rCampo( tcStr, 'nFCI' );
       {TODO: Verificar o porquê do valor do desconto dos itens não estão sendo carregados do XML}
       lNFeImportacao_Item.Vlr_Desc      := Leitor.rCampo( tcDe2, 'vDesc' );


       (*I02*)//NFe.Det[i].Prod.cProd := Leitor.rCampo(tcStr, 'cProd');
       (*I03*)//NFe.Det[i].Prod.cEAN := Leitor.rCampo(tcStr, 'cEAN');
       (*I04*)//NFe.Det[i].Prod.xProd := Leitor.rCampo(tcStr, 'xProd');
       (*I05*)//NFe.Det[i].Prod.NCM := Leitor.rCampo(tcStr, 'NCM');
       (*I06*)//NFe.Det[i].Prod.EXTIPI := Leitor.rCampo(tcStr, 'EXTIPI');
       (*I07*)//NFe.Det[i].Prod.genero := Leitor.rCampo(tcInt, 'genero');
       (*I08*)//NFe.Det[i].Prod.CFOP := Leitor.rCampo(tcEsp, 'CFOP');
       (*I09*)//NFe.Det[i].Prod.uCom := Leitor.rCampo(tcStr, 'uCom');
       (*I10*)//NFe.Det[i].Prod.qCom := Leitor.rCampo(tcDe4, 'qCom');
       (*I10a*)//NFe.Det[i].Prod.vUnCom := Leitor.rCampo(tcDe4, 'vUnCom');
       (*I11*)//NFe.Det[i].Prod.vProd := Leitor.rCampo(tcDe2, 'vProd');
       (*I12*)//NFe.Det[i].Prod.cEANTrib := Leitor.rCampo(tcStr, 'cEANTrib');
       (*I13*)//NFe.Det[i].Prod.uTrib := Leitor.rCampo(tcStr, 'uTrib');
       (*I14*)//NFe.Det[i].Prod.qTrib := Leitor.rCampo(tcDe4, 'qTrib');
       (*I14a*)//NFe.Det[i].Prod.vUnTrib := Leitor.rCampo(tcDe4, 'vUnTrib');
       (*I15*)//NFe.Det[i].Prod.vFrete := Leitor.rCampo(tcDe2, 'vFrete');
       (*I16*)//NFe.Det[i].Prod.vSeg := Leitor.rCampo(tcDe2, 'vSeg');
       (*I17*)//NFe.Det[i].Prod.vDesc := Leitor.rCampo(tcDe2, 'vDesc');

       (* Grupo da TAG <det><prod><DI> *)
       j := 0;
       while Leitor.rExtrai(3, 'DI', '', j + 1) <> '' do
       begin
         //NFe.Det[i].Prod.DI.Add;
         (*I19*)//NFe.Det[i].Prod.DI[j].nDI := Leitor.rCampo(tcStr, 'nDI');
         (*I20*)//NFe.Det[i].Prod.DI[j].dDI := Leitor.rCampo(tcDat, 'dDI');
         (*I21*)//NFe.Det[i].Prod.DI[j].xLocDesemb := Leitor.rCampo(tcStr, 'xLocDesemb');
         (*I22*)//NFe.Det[i].Prod.DI[j].UFDesemb := Leitor.rCampo(tcStr, 'UFDesemb');
         (*I23*)//NFe.Det[i].Prod.DI[j].dDesemb := Leitor.rCampo(tcDat, 'dDesemb');
         (*I24*)//NFe.Det[i].Prod.DI[j].cExportador := Leitor.rCampo(tcStr, 'cExportador');

         (* Grupo da TAG <det><prod><DI><adi> *)
         k := 0;
         while Leitor.rExtrai(4, 'adi', '', k + 1) <> '' do
         begin
           //NFe.Det[i].Prod.DI[j].adi.Add;
           (*I26*)//NFe.Det[i].Prod.DI[j].adi[k].nAdicao := Leitor.rCampo(tcInt, 'nAdicao');
           (*I27*)//NFe.Det[i].Prod.DI[j].adi[k].nSeqAdi := Leitor.rCampo(tcInt, 'nSeqAdic');
           (*I28*)//NFe.Det[i].Prod.DI[j].adi[k].cFabricante := Leitor.rCampo(tcStr, 'cFabricante');
           (*I29*)//NFe.Det[i].Prod.DI[j].adi[k].vDescDI := Leitor.rCampo(tcDe2, 'vDescDI');
           inc(k);
         end;

         inc(j);
       end;

       (* Grupo da TAG <det><prod><veicProd> *)
       if Leitor.rExtrai(3, 'veicProd') <> '' then
       begin

         (*J02*)//NFe.Det[i].Prod.veicProd.tpOP := StrToTpOP(ok, Leitor.rCampo(tcStr, 'tpOp'));
         (*J03*)//NFe.Det[i].Prod.veicProd.chassi := Leitor.rCampo(tcStr, 'chassi');
         (*J04*)//NFe.Det[i].Prod.veicProd.cCor := Leitor.rCampo(tcStr, 'cCor');
         (*J05*)//NFe.Det[i].Prod.veicProd.xCor := Leitor.rCampo(tcStr, 'xCor');
         (*J06*)//NFe.Det[i].Prod.veicProd.pot := Leitor.rCampo(tcStr, 'pot');
         (*J07*)//NFe.Det[i].Prod.veicProd.CM3 := Leitor.rCampo(tcStr, 'CM3');
         (*J08*)//NFe.Det[i].Prod.veicProd.pesoL := Leitor.rCampo(tcStr, 'pesoL');
         (*J09*)//NFe.Det[i].Prod.veicProd.pesoB := Leitor.rCampo(tcStr, 'pesoB');
         (*J10*)//NFe.Det[i].Prod.veicProd.nSerie := Leitor.rCampo(tcStr, 'nSerie');
         (*J11*)//NFe.Det[i].Prod.veicProd.tpComb := Leitor.rCampo(tcStr, 'tpComb');
         (*J12*)//NFe.Det[i].Prod.veicProd.nMotor := Leitor.rCampo(tcStr, 'nMotor');
         (*J13*)//NFe.Det[i].Prod.veicProd.CMKG := Leitor.rCampo(tcStr, 'CMKG');
         (*J14*)//NFe.Det[i].Prod.veicProd.dist := Leitor.rCampo(tcStr, 'dist');
         (*J15*)//NFe.Det[i].Prod.veicProd.RENAVAM := Leitor.rCampo(tcEsp, 'RENAVAM');
         (*J16*)//NFe.Det[i].Prod.veicProd.anoMod := Leitor.rCampo(tcInt, 'anoMod');
         (*J17*)//NFe.Det[i].Prod.veicProd.anoFab := Leitor.rCampo(tcInt, 'anoFab');
         (*J18*)//NFe.Det[i].Prod.veicProd.tpPint := Leitor.rCampo(tcStr, 'tpPint');
         (*J19*)//NFe.Det[i].Prod.veicProd.tpVeic := Leitor.rCampo(tcInt, 'tpVeic');
         (*J20*)//NFe.Det[i].Prod.veicProd.espVeic := Leitor.rCampo(tcInt, 'espVeic');
         (*J21*)//NFe.Det[i].Prod.veicProd.VIN := Leitor.rCampo(tcStr, 'VIN');
         (*J22*)//NFe.Det[i].Prod.veicProd.condVeic := StrToCondVeic(ok, Leitor.rCampo(tcStr, 'condVeic'));
         (*J23*)//NFe.Det[i].Prod.veicProd.cMod := Leitor.rCampo(tcStr, 'cMod');
       end;

       (* Grupo da TAG <det><prod><med> *)
       j := 0;
       while Leitor.rExtrai(3, 'med', '', j + 1) <> '' do
       begin
         //NFe.Det[i].Prod.med.Add;
         (*K02*)//NFe.Det[i].Prod.med[j].nLote := Leitor.rCampo(tcStr, 'nLote');
         (*K03*)//NFe.Det[i].Prod.med[j].qLote := Leitor.rCampo(tcDe3, 'qLote');
         (*K04*)//NFe.Det[i].Prod.med[j].dFab := Leitor.rCampo(tcDat, 'dFab ');
         (*K05*)//NFe.Det[i].Prod.med[j].dVal := Leitor.rCampo(tcDat, 'dVal ');
         (*K06*)//NFe.Det[i].Prod.med[j].vPMC := Leitor.rCampo(tcDe2, 'vPMC ');
         inc(j);
       end;

       (* Grupo da TAG <det><prod><arma> *)
       j := 0;
       while Leitor.rExtrai(3, 'arma', '', j + 1) <> '' do
       begin
         //NFe.Det[i].Prod.arma.add;
         (*L02*)//NFe.Det[i].Prod.arma[j].tpArma := StrToTpArma(ok, Leitor.rCampo(tcStr, 'tpArma'));
         (*L03*)//NFe.Det[i].Prod.arma[j].nSerie := Leitor.rCampo(tcInt, 'nSerie');
         (*L04*)//NFe.Det[i].Prod.arma[j].nCano := Leitor.rCampo(tcInt, 'nCano');
         (*L05*)//NFe.Det[i].Prod.arma[j].descr := Leitor.rCampo(tcStr, 'descr');
         inc(j);
       end;

       (* Grupo da TAG <det><prod><comb> *)
       if Leitor.rExtrai(3, 'comb') <> '' then
       begin
         lNFeImportacao_Item.Cod_ANP := Leitor.rCampo(tcInt, 'cProdANP');
      
         (*L102*)//NFe.Det[i].Prod.comb.cProdANP := Leitor.rCampo(tcInt, 'cProdANP');
         (*L103*)//NFe.Det[i].Prod.comb.CODIF := Leitor.rCampo(tcEsp, 'CODIF');
         (*L104*)//NFe.Det[i].Prod.comb.qTemp := Leitor.rCampo(tcDe4, 'qTemp');
         if Leitor.rExtrai(4, 'CIDE') <> '' then
         begin
           (*L106*)//NFe.Det[i].Prod.comb.CIDE.qBCprod := Leitor.rCampo(tcDe4, 'qBCprod');
           (*L107*)//NFe.Det[i].Prod.comb.CIDE.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
           (*L108*)//NFe.Det[i].Prod.comb.CIDE.vCIDE := Leitor.rCampo(tcDe2, 'vCIDE');
         end;
         if Leitor.rExtrai(4, 'ICMSComb') <> '' then
         begin
           (*L110*)//NFe.Det[i].Prod.comb.ICMS.vBCICMS := Leitor.rCampo(tcDe2, 'vBCICMS');
           (*L111*)//NFe.Det[i].Prod.comb.ICMS.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
           (*L112*)//NFe.Det[i].Prod.comb.ICMS.vBCICMSST := Leitor.rCampo(tcDe2, 'vBCICMSST');
           (*L113*)//NFe.Det[i].Prod.comb.ICMS.vICMSST := Leitor.rCampo(tcDe2, 'vICMSST');
         end;
         if Leitor.rExtrai(4, 'ICMSInter') <> '' then
         begin
           (*L115*)//NFe.Det[i].Prod.comb.ICMSInter.vBCICMSSTDest := Leitor.rCampo(tcDe2, 'vBCICMSSTDest');
           (*L116*)//NFe.Det[i].Prod.comb.ICMSInter.vICMSSTDest := Leitor.rCampo(tcDe2, 'vICMSSTDest');
         end;
         if Leitor.rExtrai(4, 'ICMSCons') <> '' then
         begin
           (*L118*)//NFe.Det[i].Prod.comb.ICMSCons.vBCICMSSTCons := Leitor.rCampo(tcDe2, 'vBCICMSSTCons');
           (*L119*)//NFe.Det[i].Prod.comb.ICMSCons.vICMSSTCons := Leitor.rCampo(tcDe2, 'vICMSSTCons');
           (*L120*)//NFe.Det[i].Prod.comb.ICMSCons.UFcons := Leitor.rCampo(tcStr, 'UFcons');
         end;
       end;

       (* Grupo da TAG <det><imposto> ********************************************)
       Leitor.rExtrai(2, 'imposto');
       if Leitor.rExtrai(3, 'ICMS') <> '' then
       begin
         lNFeImportacao_Item.NFeImportacao_Item_ICMS := TNFeImportacao_Item_ICMS.Create;
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.SitA                                 := Leitor.rCampo( tcInt, 'orig' );
         if not Self.SimplesNacional then
         begin
           lNFeImportacao_Item.NFeImportacao_Item_ICMS.SitB                                 := Leitor.rCampo( tcInt, 'CST' );
         end
         else
         begin
           lNFeImportacao_Item.NFeImportacao_Item_ICMS.SitB                                 := Leitor.rCampo( tcInt, 'CSOSN' );
         end;
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.ModalidadeBaseCalculo                := Leitor.rCampo( tcInt, 'modBC' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_BaseCalculo                      := Leitor.rCampo( tcDe2, 'vBC' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Perc_ICMS                            := Leitor.rCampo( tcDe2, 'pICMS' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_ICMS                             := Leitor.rCampo( tcDe2, 'vICMS' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.ModalidadeBaseCalculo_Substituido    := Leitor.rCampo( tcInt, 'modBCST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Perc_MVA                             := Leitor.rCampo( tcDe2, 'pMVAST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Perc_Reducao_BaseCalculo_Substituido := Leitor.rCampo( tcDe2, 'pRedBCST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_BaseCalculo_Substituido          := Leitor.rCampo( tcDe2, 'vBCST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Perc_ICMS_Substituido                := Leitor.rCampo( tcDe2, 'pICMSST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_ICMS_Substituido                 := Leitor.rCampo( tcDe2, 'vICMSST' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_FCP                              := Leitor.rCampo( tcDe2, 'vFCP' );
         lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_FCPST                            := Leitor.rCampo( tcDe2, 'vFCPST' );



         lNFeImportacao_Item.Perc_ICMS := lNFeImportacao_Item.NFeImportacao_Item_ICMS.Perc_ICMS;
         lNFeImportacao_Item.Vlr_ICMS  := lNFeImportacao_Item.NFeImportacao_Item_ICMS.Vlr_ICMS;

         (*N11*)//NFe.Det[i].Imposto.ICMS.orig := StrToOrig(ok, Leitor.rCampo(tcStr, 'orig'));
         (*N12*)//NFe.Det[i].Imposto.ICMS.CST := StrToCSTICMS(ok, Leitor.rCampo(tcStr, 'CST'));
         (*N13*)//NFe.Det[i].Imposto.ICMS.modBC := StrToModBC(ok, Leitor.rCampo(tcStr, 'modBC'));
         (*N14*)//NFe.Det[i].Imposto.ICMS.pRedBC := Leitor.rCampo(tcDe2, 'pRedBC');
         (*N15*)//NFe.Det[i].Imposto.ICMS.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*N16*)//NFe.Det[i].Imposto.ICMS.pICMS := Leitor.rCampo(tcDe2, 'pICMS');
         (*N17*)//NFe.Det[i].Imposto.ICMS.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
         (*N18*)//NFe.Det[i].Imposto.ICMS.modBCST := StrToModBCST(ok, Leitor.rCampo(tcStr, 'modBCST'));
         (*N19*)//NFe.Det[i].Imposto.ICMS.pMVAST := Leitor.rCampo(tcDe2, 'pMVAST');
         (*N20*)//NFe.Det[i].Imposto.ICMS.pRedBCST := Leitor.rCampo(tcDe2, 'pRedBCST');
         (*N21*)//NFe.Det[i].Imposto.ICMS.vBCST := Leitor.rCampo(tcDe2, 'vBCST');
         (*N22*)//NFe.Det[i].Imposto.ICMS.pICMSST := Leitor.rCampo(tcDe2, 'pICMSST');
         (*N23*)//NFe.Det[i].Imposto.ICMS.vICMSST := Leitor.rCampo(tcDe2, 'vICMSST');
       end;
       if Leitor.rExtrai(3, 'IPI') <> '' then
       begin
         (*O02*)//NFe.Det[i].Imposto.IPI.clEnq := Leitor.rCampo(tcStr, 'clEnq');
         (*O03*)//NFe.Det[i].Imposto.IPI.CNPJProd := Leitor.rCampo(tcStr, 'CNPJProd');
         (*O04*)//NFe.Det[i].Imposto.IPI.cSelo := Leitor.rCampo(tcStr, 'cSelo');
         (*O05*)//NFe.Det[i].Imposto.IPI.qSelo := Leitor.rCampo(tcInt, 'qSelo');
         (*O06*)//NFe.Det[i].Imposto.IPI.cEnq := Leitor.rCampo(tcStr, 'cEnq');


         // Inicializa CST com sendo Não tributada e conforme o TIPO entrada ou saida
         // Caso a Tag não seja informada sera gravada com sendo não tributada
         //if NFe.ide.tpNF = tnEntrada then
         //  NFe.Det[i].Imposto.IPI.CST := ipi53;
         //if NFe.ide.tpNF = tnSaida then
         //  NFe.Det[i].Imposto.IPI.CST := ipi03;

         if Leitor.rExtrai(3, 'IPITrib') <> '' then
         begin
            lNFeImportacao_Item.NFeImportacao_Item_IPI := TNFeImportacao_Item_IPI.Create;
            lNFeImportacao_Item.NFeImportacao_Item_IPI.SitTribitaria   := Leitor.rCampo( tcInt, 'CST' );
            lNFeImportacao_Item.NFeImportacao_Item_IPI.Vlr_BaseCalculo := Leitor.rCampo( tcDe2, 'vBC' );
            lNFeImportacao_Item.NFeImportacao_Item_IPI.QtdeTotal       := Leitor.rCampo( tcDe4, 'qUnid' );
            lNFeImportacao_Item.NFeImportacao_Item_IPI.Vlr_Unitario    := Leitor.rCampo( tcDe4, 'vUnid' );
            lNFeImportacao_Item.NFeImportacao_Item_IPI.Perc_IPI        := Leitor.rCampo( tcDe2, 'pIPI' );
            lNFeImportacao_Item.NFeImportacao_Item_IPI.Vlr_IPI         := Leitor.rCampo( tcDe2, 'vIPI' );

           (*O09*)//NFe.Det[i].Imposto.IPI.CST := StrToCSTIPI(ok, Leitor.rCampo(tcStr, 'CST'));
           (*O10*)//NFe.Det[i].Imposto.IPI.vBC := Leitor.rCampo(tcDe2, 'vBC');
           (*O11*)//NFe.Det[i].Imposto.IPI.qUnid := Leitor.rCampo(tcDe4, 'qUnid');
           (*O12*)//NFe.Det[i].Imposto.IPI.vUnid := Leitor.rCampo(tcDe4, 'vUnid');
           (*O13*)//NFe.Det[i].Imposto.IPI.pIPI := Leitor.rCampo(tcDe2, 'pIPI');
           (*O14*)//NFe.Det[i].Imposto.IPI.vIPI := Leitor.rCampo(tcDe2, 'vIPI');
         end;
         if Leitor.rExtrai(3, 'IPINT') <> '' then
         begin
           (*O09*)//NFe.Det[i].Imposto.IPI.CST := StrToCSTIPI(ok, Leitor.rCampo(tcStr, 'CST'));
         end;
       end;
       if Leitor.rExtrai(3, 'II') <> '' then
       begin
         (*P02*)//NFe.Det[i].Imposto.II.vBc := Leitor.rCampo(tcDe2, 'vBC');
         (*P03*)//NFe.Det[i].Imposto.II.vDespAdu := Leitor.rCampo(tcDe2, 'vDespAdu');
         (*P04*)//NFe.Det[i].Imposto.II.vII := Leitor.rCampo(tcDe2, 'vII');
         (*P05*)//NFe.Det[i].Imposto.II.vIOF := Leitor.rCampo(tcDe2, 'vIOF');
       end;
       if Leitor.rExtrai(3, 'PIS') <> '' then
       begin
         (*Q06*)//NFe.Det[i].Imposto.PIS.CST := StrToCSTPIS(ok, Leitor.rCampo(tcStr, 'CST'));
         (*Q07*)//NFe.Det[i].Imposto.PIS.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*Q08*)//NFe.Det[i].Imposto.PIS.pPIS := Leitor.rCampo(tcDe2, 'pPIS');
         (*Q09*)//NFe.Det[i].Imposto.PIS.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
         (*Q10*)//NFe.Det[i].Imposto.PIS.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
         (*Q11*)//NFe.Det[i].Imposto.PIS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
       end;
       if Leitor.rExtrai(3, 'PISST') <> '' then
       begin
         (*R02*)//NFe.Det[i].Imposto.PISST.vBc := Leitor.rCampo(tcDe2, 'vBC');
         (*R03*)//NFe.Det[i].Imposto.PISST.pPis := Leitor.rCampo(tcDe2, 'pPIS');
         (*R04*)//NFe.Det[i].Imposto.PISST.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
         (*R05*)//NFe.Det[i].Imposto.PISST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
         (*R06*)//NFe.Det[i].Imposto.PISST.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
       end;
       if Leitor.rExtrai(3, 'COFINS') <> '' then
       begin
         (*S06*)//NFe.Det[i].Imposto.COFINS.CST := StrToCSTCOFINS(ok, Leitor.rCampo(tcStr, 'CST'));
         (*S07*)//NFe.Det[i].Imposto.COFINS.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*S08*)//NFe.Det[i].Imposto.COFINS.pCOFINS := Leitor.rCampo(tcDe2, 'pCOFINS');
         (*S09*)//NFe.Det[i].Imposto.COFINS.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
         (*S10*)//NFe.Det[i].Imposto.COFINS.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
         (*S11*)//NFe.Det[i].Imposto.COFINS.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
       end;
       if Leitor.rExtrai(3, 'COFINSST') <> '' then
       begin
         (*T02*)//NFe.Det[i].Imposto.COFINSST.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*T03*)//NFe.Det[i].Imposto.COFINSST.pCOFINS := Leitor.rCampo(tcDe2, 'pCOFINS');
         (*T04*)//NFe.Det[i].Imposto.COFINSST.qBCProd := Leitor.rCampo(tcDe4, 'qBCProd');
         (*T05*)//NFe.Det[i].Imposto.COFINSST.vAliqProd := Leitor.rCampo(tcDe4, 'vAliqProd');
         (*T06*)//NFe.Det[i].Imposto.COFINSST.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
       end;
       if Leitor.rExtrai(3, 'ISSQN') <> '' then
       begin
         (*U02*)//NFe.Det[i].Imposto.ISSQN.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*U03*)//NFe.Det[i].Imposto.ISSQN.vAliq := Leitor.rCampo(tcDe2, 'vAliq');
         (*U04*)//NFe.Det[i].Imposto.ISSQN.vISSQN := Leitor.rCampo(tcDe2, 'vISSQN');
         (*U05*)//NFe.Det[i].Imposto.ISSQN.cMunFG := Leitor.rCampo(tcInt, 'cMunFG');
         (*U06*)//NFe.Det[i].Imposto.ISSQN.cListServ := Leitor.rCampo(tcInt, 'cListServ');
       end;

       inc(i);

       Self.NFeImportacao_Itens.Add( lNFeImportacao_Item );
     end;

     (* Grupo da TAG <total> *****************************************************)
     if Leitor.rExtrai(1, 'total') <> '' then
     begin
       if Leitor.rExtrai(2, 'ICMSTot') <> '' then
       begin
          Self.Vlr_BaseICMS     := Leitor.rCampo( tcDe2, 'vBC' );
          Self.Vlr_ICMS         := Leitor.rCampo( tcDe2, 'vICMS' );
          Self.Vlr_BaseICMSSubs := Leitor.rCampo( tcDe2, 'vBCST' );
          Self.Vlr_ICMSSubs     := Leitor.rCampo( tcDe2, 'vST' );
          Self.Vlr_Frete        := Leitor.rCampo( tcDe2, 'vFrete' );
          Self.Vlr_Seguro       := Leitor.rCampo( tcDe2, 'vSeg' );
          Self.Vlr_Despesas     := Leitor.rCampo( tcDe2, 'vOutro' );
          Self.Vlr_IPI          := Leitor.rCampo( tcDe2, 'vIPI' );
          Self.Vlr_Produtos     := Leitor.rCampo( tcDe2, 'vProd' );
          Self.Vlr_Total        := Leitor.rCampo( tcDe2, 'vNF' );
          Self.Desc_Comercial   := Leitor.rCampo( tcDe2, 'vDesc' );
          Self.Vlr_FCP          := Leitor.rCampo( tcDe2, 'vFCP' );
          Self.Vlr_FCPST          := Leitor.rCampo( tcDe2, 'vFCPST' );

         (*W03*)//NFe.Total.ICMSTot.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*W04*)//NFe.Total.ICMSTot.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
         (*W05*)//NFe.Total.ICMSTot.vBCST := Leitor.rCampo(tcDe2, 'vBCST');
         (*W06*)//NFe.Total.ICMSTot.vST := Leitor.rCampo(tcDe2, 'vST');
         (*W07*)//NFe.Total.ICMSTot.vProd := Leitor.rCampo(tcDe2, 'vProd');
         (*W08*)//NFe.Total.ICMSTot.vFrete := Leitor.rCampo(tcDe2, 'vFrete');
         (*W09*)//NFe.Total.ICMSTot.vSeg := Leitor.rCampo(tcDe2, 'vSeg');
         (*W10*)//NFe.Total.ICMSTot.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
         (*W11*)//NFe.Total.ICMSTot.vII := Leitor.rCampo(tcDe2, 'vII');
         (*W12*)//NFe.Total.ICMSTot.vIPI := Leitor.rCampo(tcDe2, 'vIPI');
         (*W13*)//NFe.Total.ICMSTot.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
         (*W14*)//NFe.Total.ICMSTot.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
         (*W15*)//NFe.Total.ICMSTot.vOutro := Leitor.rCampo(tcDe2, 'vOutro');
         (*W16*)//NFe.Total.ICMSTot.vNF := Leitor.rCampo(tcDe2, 'vNF');
       end;
       if Leitor.rExtrai(2, 'ISSQNtot') <> '' then
       begin
         (*W18*)//NFe.Total.ISSQNtot.vServ := Leitor.rCampo(tcDe2, 'vServ');
         (*W19*)//NFe.Total.ISSQNtot.vBC := Leitor.rCampo(tcDe2, 'vBC');
         (*W20*)//NFe.Total.ISSQNtot.vISS := Leitor.rCampo(tcDe2, 'vISS');
         (*W21*)//NFe.Total.ISSQNtot.vPIS := Leitor.rCampo(tcDe2, 'vPIS');
         (*W22*)//NFe.Total.ISSQNtot.vCOFINS := Leitor.rCampo(tcDe2, 'vCOFINS');
       end;
       if Leitor.rExtrai(2, 'retTrib') <> '' then
       begin
         (*W24*)//NFe.Total.retTrib.vRetPIS := Leitor.rCampo(tcDe2, 'vRetPIS');
         (*W25*)//NFe.Total.retTrib.vRetCOFINS := Leitor.rCampo(tcDe2, 'vRetCOFINS');
         (*W26*)//NFe.Total.retTrib.vRetCSLL := Leitor.rCampo(tcDe2, 'vRetCSLL');
         (*W27*)//NFe.Total.retTrib.vBCIRRF := Leitor.rCampo(tcDe2, 'vBCIRRF');
         (*W28*)//NFe.Total.retTrib.vIRRF := Leitor.rCampo(tcDe2, 'vIRRF');
         (*W29*)//NFe.Total.retTrib.vBCRetPrev := Leitor.rCampo(tcDe2, 'vBCRetPrev');
         (*W30*)//NFe.Total.retTrib.vRetPrev := Leitor.rCampo(tcDe2, 'vRetPrev');
       end;
     end;

     (* Grupo da TAG <transp> ****************************************************)
     if Leitor.rExtrai(1, 'transp') <> '' then
     begin
       (*X02*)//NFe.Transp.modFrete := StrToModFrete(ok, Leitor.rCampo(tcStr, 'modFrete'));
       if Leitor.rExtrai(2, 'transporta') <> '' then
       begin
         (*X04/X05*)//NFe.Transp.Transporta.CNPJCPF := Leitor.rCampoCNPJCPF;
         (*X06*)//NFe.Transp.Transporta.xNome := Leitor.rCampo(tcStr, 'xNome');
         (*X07*)//NFe.Transp.Transporta.IE := Leitor.rCampo(tcStr, 'IE');
         (*X08*)//NFe.Transp.Transporta.xEnder := Leitor.rCampo(tcStr, 'xEnder');
         (*X09*)//NFe.Transp.Transporta.xMun := Leitor.rCampo(tcStr, 'xMun');
         (*X10*)//NFe.Transp.Transporta.UF := Leitor.rCampo(tcStr, 'UF');
       end;
       if Leitor.rExtrai(2, 'retTransp') <> '' then
       begin
         (*X12*)//NFe.Transp.retTransp.vServ := Leitor.rCampo(tcDe2, 'vServ');
         (*X13*)//NFe.Transp.retTransp.vBCRet := Leitor.rCampo(tcDe2, 'vBCRet');
         (*X14*)//NFe.Transp.retTransp.pICMSRet := Leitor.rCampo(tcDe2, 'pICMSRet');
         (*X15*)//NFe.Transp.retTransp.vICMSRet := Leitor.rCampo(tcDe2, 'vICMSRet');
         (*X16*)//NFe.Transp.retTransp.CFOP := Leitor.rCampo(tcEsp, 'CFOP');
         (*X17*)//NFe.Transp.retTransp.cMunFG := Leitor.rCampo(tcStr, 'cMunFG');
       end;
       if Leitor.rExtrai(2, 'veicTransp') <> '' then
       begin
         (*X19*)//NFe.Transp.veicTransp.placa := Leitor.rCampo(tcStr, 'placa');
         (*X20*)//NFe.Transp.veicTransp.UF := Leitor.rCampo(tcStr, 'UF');
         (*X21*)//NFe.Transp.veicTransp.RNTC := Leitor.rCampo(tcStr, 'RNTC');
       end;

       i := 0;
       while Leitor.rExtrai(2, 'reboque', '', i + 1) <> '' do
       begin
         //NFe.Transp.Reboque.add;
         (*X23*)//NFe.Transp.Reboque[i].placa := Leitor.rCampo(tcStr, 'placa');
         (*X24*)//NFe.Transp.Reboque[i].UF := Leitor.rCampo(tcStr, 'UF');
         (*X25*)//NFe.Transp.Reboque[i].RNTC := Leitor.rCampo(tcStr, 'RNTC');
         inc(i);
       end;

       i := 0;
       while Leitor.rExtrai(2, 'vol', '', i + 1) <> '' do
       begin
         //NFe.Transp.Vol.add;
         (*X27*)//NFe.Transp.Vol[i].qVol := Leitor.rCampo(tcInt, 'qVol');
         (*X28*)//NFe.Transp.vol[i].esp := Leitor.rCampo(tcStr, 'esp');
         (*X29*)//NFe.Transp.Vol[i].marca := Leitor.rCampo(tcStr, 'marca');
         (*X30*)//NFe.Transp.Vol[i].nVol := Leitor.rCampo(tcStr, 'nVol');
         (*X31*)//NFe.Transp.Vol[i].pesoL := Leitor.rCampo(tcDe3, 'pesoL');
         (*X32*)//NFe.Transp.Vol[i].pesoB := Leitor.rCampo(tcDe3, 'pesoB');
         j := 0;
         while Leitor.rExtrai(3, 'lacres', '', j + 1) <> '' do
         begin
           //NFe.transp.Vol[i].lacres.add;
           (*X34*)//NFe.transp.Vol[i].lacres[j].nLacre := Leitor.rCampo(tcStr, 'nLacre');
           inc(j);
         end;
         inc(i);
       end;

     end;

     (* Grupo da TAG <cobr> ******************************************************)
     if Leitor.rExtrai(1, 'cobr') <> '' then
     begin
       if Leitor.rExtrai(1, 'fat') <> '' then
       begin
         Self.FNFeImportacao_Fatura.Documento    := Leitor.rCampo( tcStr, 'nFat' );
         Self.FNFeImportacao_Fatura.Vlr_Bruto    := Leitor.rCampo( tcDe2, 'vOrig' );
         Self.FNFeImportacao_Fatura.Vlr_Desconto := Leitor.rCampo( tcDe2, 'vDesc' );
         Self.FNFeImportacao_Fatura.Vlr_Liquido  := Leitor.rCampo( tcDe2, 'vLiq' );

         (*Y03*)//NFe.Cobr.Fat.nFat := Leitor.rCampo(tcStr, 'nFat');
         (*Y04*)//NFe.Cobr.Fat.vOrig := Leitor.rCampo(tcDe2, 'vOrig');
         (*Y05*)//NFe.Cobr.Fat.vDesc := Leitor.rCampo(tcDe2, 'vDesc');
         (*Y06*)//NFe.Cobr.Fat.vLiq := Leitor.rCampo(tcDe2, 'vLiq');
       end;
       i := 0;
       while Leitor.rExtrai(1, 'dup', '', i + 1) <> '' do
       begin
         lNFeImportacao_Duplicata := TNFeImportacao_Duplicata.Create;
         With lNFeImportacao_Duplicata Do
         Begin
            Documento  := Leitor.rCampo( tcStr, 'nDup' );
            Vencimento := Leitor.rCampo( tcDat, 'dVenc' );
            Vlr_APagar := Leitor.rCampo( tcDe2, 'vDup' );
         End;
         Self.NFeImportacao_Duplicatas.Add( lNFeImportacao_Duplicata );

         //NFe.Cobr.Dup.Add;
         (*Y08*)//NFe.Cobr.Dup[i].nDup := Leitor.rCampo(tcStr, 'nDup');
         (*Y09*)//NFe.Cobr.Dup[i].dVenc := Leitor.rCampo(tcDat, 'dVenc');
         (*Y10*)//NFe.Cobr.Dup[i].vDup := Leitor.rCampo(tcDe2, 'vDup');
         inc(i);
       end;
     end;

     (* Grupo da TAG <InfAdic> ***************************************************)

     if Leitor.rExtrai(1, 'infAdic') <> '' then
     begin
       if Self.NFE_Chave_referente <> '' then
       Begin
         Self.InformacaoAdicional := Leitor.rCampo(tcStr, 'infCpl');
       End;
       (*Z02*)//NFe.InfAdic.infAdFisco := Leitor.rCampo(tcStr, 'infAdFisco');
       (*Z03*)//NFe.InfAdic.infCpl := Leitor.rCampo(tcStr, 'infCpl');
       i := 0;
       while Leitor.rExtrai(2, 'obsCont', '', i + 1) <> '' do
       begin
         //NFe.InfAdic.obsCont.Add;
         (*Z05*)//NFe.InfAdic.obsCont[i].xCampo := Leitor.rAtributo('xCampo');
         (*Z06*)//NFe.InfAdic.obsCont[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');
         inc(i);
       end;
       i := 0;
       while Leitor.rExtrai(2, 'obsFisco', '', i + 1) <> '' do
       begin
         //NFe.InfAdic.obsFisco.Add;
         (*Z08*)//NFe.InfAdic.obsFisco[i].xCampo := Leitor.rAtributo('xCampo');
         (*Z09*)//NFe.InfAdic.obsFisco[i].xTexto := Leitor.rCampo(tcStr, 'xTexto');
         inc(i)
       end;
       i := 0;
       while Leitor.rExtrai(2, 'procRef', '', i + 1) <> '' do
       begin
         //NFe.InfAdic.procRef.Add;
         (*Z11*)//NFe.InfAdic.procRef[i].nProc := Leitor.rCampo(tcStr, 'nProc');
         (*Z12*)//NFe.InfAdic.procRef[i].indProc := StrToIndProc(ok, Leitor.rCampo(tcStr, 'indProc'));
         inc(i);
       end;
     end;



     (* Grupo da TAG <exporta> ***************************************************)
     if Leitor.rExtrai(1, 'exporta') <> '' then
     begin
       (*ZA02*)//NFe.exporta.UFembarq := Leitor.rCampo(tcStr, 'UFEmbarq');
       (*ZA03*)//NFe.exporta.xLocEmbarq := Leitor.rCampo(tcStr, 'xLocEmbarq');
     end;

     (* Grupo da TAG <compra> ****************************************************)
     if Leitor.rExtrai(1, 'compra') <> '' then
     begin
       (*ZB02*)//NFe.compra.xNEmp := Leitor.rCampo(tcStr, 'xNEmp');
       (*ZB03*)//NFe.compra.xPed := Leitor.rCampo(tcStr, 'xPed');
       (*ZB04*)//NFe.compra.xCont := Leitor.rCampo(tcStr, 'xCont');
     end;

     (* Grupo da TAG <signature> *************************************************)

     leitor.Grupo := Leitor.Arquivo;

     //NFe.signature.URI := Leitor.rAtributo('Reference URI=');
     //NFE.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
     //NFE.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
     //NFE.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');

     (* Grupo da TAG <protNFe> ****************************************************)
     if Leitor.rExtrai(1, 'protNFe') <> '' then
     begin
        Self.RetornoNFe.Status := NFeGerada;
        If ( Leitor.rCampo( tcStr, 'tpAmb' ) = '1' ) And // Ambiente : Produção
           ( Leitor.rCampo( tcInt, 'cStat' ) = 100 ) Then // Autorizado o uso da NF-e
        Begin
           Self.RetornoNFe.Status := NFeAutorizada;
        End;
        Case StrToIntDef( Leitor.rCampo( tcStr, 'tpAmb' ), -1 ) Of
           0 : Self.RetornoNFe.Ambiente := NFeambHomologacao;
           1 : Self.RetornoNFe.Ambiente := NFeambProducao;
        End;
        Self.RetornoNFe.Motivo        := Leitor.rCampo( tcStr, 'xMotivo' );
        Self.RetornoNFe.Protocolo     := Leitor.rCampo( tcStr, 'nProt' );
        Self.RetornoNFe.DataProtocolo := Leitor.rCampo( tcDatHor, 'dhRecbto' );
        Self.RetornoNFe.digVal        := Leitor.rCampo( tcStr, 'digVal' );
        Self.RetornoNFe.Recibo        := Leitor.rCampo( tcStr, 'dhRecbto' );
        Self.RetornoNFe.verAplic      := Leitor.rCampo( tcStr, 'verAplic' );

        //NFe.procNFe.tpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        //NFe.procNFe.verAplic := Leitor.rCampo(tcStr, 'verAplic');
        //NFe.procNFe.chNFe    := Leitor.rCampo(tcStr, 'chNFe');
        //NFe.procNFe.dhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
        //NFe.procNFe.nProt    := Leitor.rCampo(tcStr, 'nProt');
        //NFe.procNFe.digVal   := Leitor.rCampo(tcStr, 'digVal');
        //NFe.procNFe.cStat    := Leitor.rCampo(tcInt, 'cStat');
        //NFe.procNFe.xMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
     end;
  Finally
     FreeAndNil( Leitor );
  End;
end;

{ TNFeImportacao_Item_ICMS }

procedure TNFeImportacao_Item_ICMS.SetModalidadeBaseCalculo(const Value: Integer);
begin
  FModalidadeBaseCalculo := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetModalidadeBaseCalculo_Substituido(
  const Value: Integer);
begin
  FModalidadeBaseCalculo_Substituido := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetPerc_ICMS(const Value: Real);
begin
  FPerc_ICMS := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetPerc_ICMS_Substituido(const Value: Real);
begin
  FPerc_ICMS_Substituido := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetPerc_MVA(const Value: Real);
begin
  FPerc_MVA := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetPerc_Reducao_BaseCalculo_Substituido(
  const Value: Real);
begin
  FPerc_Reducao_BaseCalculo_Substituido := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetSitA(const Value: Integer);
begin
  FSitA := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetSitB(const Value: Integer);
begin
  FSitB := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetVlr_BaseCalculo(const Value: Real);
begin
  FVlr_BaseCalculo := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetVlr_BaseCalculo_Substituido(const Value: Real);
begin
  FVlr_BaseCalculo_Substituido := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetVlr_ICMS(const Value: Real);
begin
  FVlr_ICMS := Value;
end;

procedure TNFeImportacao_Item_ICMS.SetVlr_ICMS_Substituido(const Value: Real);
begin
  FVlr_ICMS_Substituido := Value;
end;

{ TNFeImportacao_Item_IPI }

procedure TNFeImportacao_Item_IPI.SetPerc_IPI(const Value: Real);
begin
  FPerc_IPI := Value;
end;

procedure TNFeImportacao_Item_IPI.SetQtdeTotal(const Value: Real);
begin
  FQtdeTotal := Value;
end;

procedure TNFeImportacao_Item_IPI.SetSitTribitaria(const Value: Integer);
begin
  FSitTribitaria := Value;
end;

procedure TNFeImportacao_Item_IPI.SetVlr_BaseCalculo(const Value: Real);
begin
  FVlr_BaseCalculo := Value;
end;

procedure TNFeImportacao_Item_IPI.SetVlr_IPI(const Value: Real);
begin
  FVlr_IPI := Value;
end;

procedure TNFeImportacao_Item_IPI.SetVlr_Unitario(const Value: Real);
begin
  FVlr_Unitario := Value;
end;

{ TPessoaNFe }

procedure TPessoaNFe.SetBairro(const Value: String);
begin
  FBairro := Value;
end;

procedure TPessoaNFe.SetCEP(const Value: String);
begin
  FCEP := Value;
end;

procedure TPessoaNFe.SetCidade(const Value: String);
begin
  FCidade := Value;
end;

procedure TPessoaNFe.SetCNPJCPF(const Value: String);
begin
  FCNPJCPF := Value;
end;

procedure TPessoaNFe.SetCod_MunicipioIBGE(const Value: Integer);
begin
  FCod_MunicipioIBGE := Value;
end;

procedure TPessoaNFe.SetEndereco(const Value: String);
begin
  FEndereco := Value;
end;

procedure TPessoaNFe.SetInscEst(const Value: String);
begin
  FInscEst := Value;
end;

procedure TPessoaNFe.SetNome_Fantasia(const Value: String);
begin
  FNome_Fantasia := Value;
end;

procedure TPessoaNFe.SetNumero(const Value: String);
begin
  FNumero := Value;
end;

procedure TPessoaNFe.SetRazao_Social(const Value: String);
begin
  FRazao_Social := Value;
end;

procedure TPessoaNFe.SetTelefone(const Value: String);
begin
  FTelefone := Value;
end;

procedure TPessoaNFe.SetUF(const Value: String);
begin
  FUF := Value;
end;

{ TNFeImportacao_Duplicatas }

function TNFeImportacao_Duplicatas.Add(AObject: TNFeImportacao_Duplicata): Integer;
begin
  Result := inherited Add(AObject);
end;

function TNFeImportacao_Duplicatas.GetItem(Index: Integer): TNFeImportacao_Duplicata;
begin
  Result := TNFeImportacao_Duplicata(inherited Items[Index]);
end;

function TNFeImportacao_Duplicatas.IndexOf(AObject: TNFeImportacao_Duplicata): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TNFeImportacao_Duplicatas.Insert(Index: Integer; AObject: TNFeImportacao_Duplicata);
begin
  inherited Insert(Index, AObject);
end;

function TNFeImportacao_Duplicatas.Remove(AObject: TNFeImportacao_Duplicata): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TNFeImportacao_Duplicatas.SetItem(Index: Integer; AObject: TNFeImportacao_Duplicata);
begin
  inherited Items[Index] := AObject;
end;

{ TNFeImportacao_Itens }

function TNFeImportacao_Itens.Add(AObject: TNFeImportacao_Item): Integer;
begin
  Result := inherited Add(AObject);
end;

function TNFeImportacao_Itens.GetItem(Index: Integer): TNFeImportacao_Item;
begin
  Result := TNFeImportacao_Item(inherited Items[Index]);
end;

procedure TNFeImportacao_Itens.SetItem(Index: Integer; AObject: TNFeImportacao_Item);
begin
  inherited Items[Index] := AObject;
end;

{ TNFeImportacao_Fatura }

procedure TNFeImportacao_Fatura.SetDocumento(const Value: String);
begin
  FDocumento := Value;
end;

procedure TNFeImportacao_Fatura.SetVlr_Bruto(const Value: Real);
begin
  FVlr_Bruto := Value;
end;

procedure TNFeImportacao_Fatura.SetVlr_Desconto(const Value: Real);
begin
  FVlr_Desconto := Value;
end;

procedure TNFeImportacao_Fatura.SetVlr_Liquido(const Value: Real);
begin
  FVlr_Liquido := Value;
end;

{ TChaveAcessoNFe }

procedure TChaveAcessoNFe.SetCNPJ(const Value: String);
begin
  FCNPJ := Value;
end;

procedure TChaveAcessoNFe.SetCodDocFiscal(const Value: String);
begin
  FCodDocFiscal := Value;
end;

procedure TChaveAcessoNFe.SetCodUF(const Value: String);
begin
  FCodUF := Value;
end;

procedure TChaveAcessoNFe.SetDigVerif(const Value: String);
begin
  FDigVerif := Value;
end;

procedure TChaveAcessoNFe.SetMesAno(const Value: String);
begin
  FMesAno := Value;
end;

procedure TChaveAcessoNFe.SetModelo(const Value: String);
begin
  FModelo := Value;
end;

procedure TChaveAcessoNFe.SetNumDocFiscal(const Value: String);
begin
  FNumDocFiscal := Value;
end;

procedure TChaveAcessoNFe.SetSerie(const Value: String);
begin
  FSerie := Value;
end;

procedure TChaveAcessoNFe.SetValor(const Value: String);
begin
  FValor := Value;
end;

end.
