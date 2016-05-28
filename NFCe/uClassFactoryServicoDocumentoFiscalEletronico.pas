unit uClassFactoryServicoDocumentoFiscalEletronico;

interface

uses
  Forms, Dialogs, Sysutils,strUtils, Math, pcnNFe, DB,ACBrNFe, pcnConversao,Variants,
  ACBrUtil, ACBrNFeDANFEClass, ACBrNFeDANFeESCPOS, ACBrBase, ACBrDFe, XMLIntf,
  XMLDoc, zlib, ACBrMail, ACBrNFeDANFeRLClass, ACBrDANFCeFortesFr, pcnConversaoNFe,
  ACBrDFeSSL, ACBrNFeConfiguracoes, uClassServicoDocumentoFiscalEletronico,
  uClassNotaFiscal, uClassItemNotaFiscal , pcnAuxiliar, IniFiles, DBClient;

type
  TFactoryServicoDocumentoFiscalEletronico = class
  strict private
    class var ConfiguracoesDFe: TConfiguracoesNFe;
    class function LoadConfiguracoesDFe(ACBR:TACBrNFe; prsPathNFE: string; prbVisualizar:boolean): TConfiguracoesNFe; static;
    class function GetCST(prsSitB: string): TpcnCSTIcms; static;
    class function GetCSOSN(prsSitB: string): TpcnCSOSNIcms; static;
    class function GetCstPis(CodigoCst: string): TpcnCstPis; static;
    class function GetCstCofins(CodigoCst: string): TpcnCstCofins; static;    
  private
    class procedure PreencherIde(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);static;
    class procedure PreencherEmitente(Servico: TServicoDocumentoFiscalEletronico;  proNotaFiscal: TNotaFiscal);static;
    class procedure PreencherDest(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal); static;
    class procedure PreencherItens(Servico: TServicoDocumentoFiscalEletronico;  proNotaFiscal: TNotaFiscal);static;
    class procedure PreencherTotal(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal); static;
    class procedure PreencherPag(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal); static;
    class procedure PreencherInfAdic(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal); static;
  public
    class function FactoryServicoNFC( proNotaFiscal: TNotaFiscal; prsPathNFE: string; prbVisualizar:boolean = true ):TServicoDocumentoFiscalNFC;
    class function FactoryServicoNFE( proNotaFiscal: TNotaFiscal; prsPathNFE: string; prbVisualizar:boolean = true): TServicoDocumentoFiscalNFE;
  end;

implementation

uses  uClassEmpresa, uMenu, UFuncoes;

{ TFactoryDocumentoFiscal }

class function TFactoryServicoDocumentoFiscalEletronico.LoadConfiguracoesDFe(ACBR:TACBrNFe;
prsPathNFE : string;
prbVisualizar:boolean ): TConfiguracoesNFe;
var
   pastaSchemas: string;
   loParamEmailConfig: TIniFile;
begin
   ConfiguracoesDFe := TConfiguracoesNFe.create( ACBR );
   with ConfiguracoesDFe do
   begin
      Geral.FormaEmissao         := teNormal;
      Geral.VersaoDF             := ve310;
      Geral.Salvar               := true;
      Geral.ExibirErroSchema     := true;
      Geral.RetirarAcentos       := true;
      Geral.IncluirQRCodeXMLNFCe := true;
      Geral.FormatoAlerta        := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';
      Geral.SSLLib               := libCapicom;//(libNone, libOpenSSL, libCapicom, libCapicomDelphiSoap);
      Geral.IdCSC                := goEmpresa.IDTokenDFe;
      Geral.CSC                  := goEmpresa.TokenDFe;
      Certificados.ArquivoPFX    := goEmpresa.CertificadoDFe;
    //Certificados.Senha         := goEmpresa.SenhaDFe;
      Certificados.NumeroSerie   := uppercase( goEmpresa.NumeroSerieDFe);

      if not DirectoryExists( prsPathNFE ) then
         CreateDir( prsPathNFE );
      Arquivos.PathSalvar        := prsPathNFE;
      Arquivos.PathInu           := prsPathNFE;
      Arquivos.PathEvento        := prsPathNFE;

      pastaSchemas := gsPath+ 'SchemasXML\NFe';
      if not DirectoryExists( pastaSchemas ) then
         CreateDir( pastaSchemas );

      Arquivos.PathSchemas       := pastaSchemas;
      Arquivos.SalvarApenasNFeProcessadas  := True;

      WebServices.Salvar         := true;
      WebServices.UF             := goEmpresa.Endereco.Uf.Descricao;
      WebServices.Ambiente       := taHomologacao;
      if goEmpresa.AmbienteDFe = 1 then
      WebServices.Ambiente       :=  taProducao;

      WebServices.Visualizar     :=  prbVisualizar;
   end;

   try
    loParamEmailConfig:= TIniFile.Create( gspath+'config.ini');
     with (ConfiguracoesDFe.Owner as TACBrNFe) do
     begin
        MAIL := TACBRMail.create(ConfiguracoesDFe.Owner);
        MAIL.from     := loParamEmailConfig.ReadString('SMTP', 'Email', '');
        MAIL.host     := loParamEmailConfig.ReadString('SMTP', 'Servidor', '');
        MAIL.Username := loParamEmailConfig.ReadString('SMTP', 'Usuario', '');
        MAIL.Password := loParamEmailConfig.ReadString('SMTP', 'Senha', '');
        MAIL.IsHTML   := True;
        MAIL.port     := inttostr(loParamEmailConfig.ReadInteger('Proxy', 'Porta', 587));
        MAIL.SetSSL   := loParamEmailConfig.ReadBool('SMTP', 'SSL', False);
     end;
   finally
     loParamEmailConfig.free;
   end;
   result := ConfiguracoesDFe;
end;

class function TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFE( proNotaFiscal: TNotaFiscal;
                                                                           prsPathNFE: string;
                                                                           prbVisualizar:boolean = true  ): TServicoDocumentoFiscalNFE;
var
  Servico : TServicoDocumentoFiscalNFE;
begin
  showmessage('Recurso ainda não implementado para NFE!');
  //Criar um serviço NFE
  Servico  := TServicoDocumentoFiscalNFE.create( LoadConfiguracoesDFe(TACBrNFe.create(application), prsPathNFE, prbVisualizar) );
  //Preencher os Objetos do ABCBR conforme as diretrizes do serviço NFE
  PreencherIde( Servico, proNotaFiscal);
  PreencherEmitente( Servico, proNotaFiscal);
  PreencherDest( Servico, proNotaFiscal);
  PreencherItens( Servico, proNotaFiscal);
  PreencherTotal( Servico, proNotaFiscal);
//PreencherFatu( Servico, proNotaFiscal);
//PreencherPag( Servico, proNotaFiscal);
//PreencherTransp( Servico, proNotaFiscal);
  PreencherInfAdic( Servico, proNotaFiscal);
//Retornar o serviço NFE já preparado para ser enviar a NotaFiscal
  result      := Servico;
end;

class function TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( proNotaFiscal: TNotaFiscal;
                                                                           prsPathNFE: string;
                                                                           prbVisualizar:boolean = true ):TServicoDocumentoFiscalNFC;
var
  Servico : TServicoDocumentoFiscalNFC;
begin
//Criar um serviço NFC
  Servico  := TServicoDocumentoFiscalNFC.create( LoadConfiguracoesDFe( TACBrNFe.create(application), prsPathNFE, prbVisualizar ) );
  if proNotaFiscal <> nil then
  begin
  //Preencher os Objetos do ABCBR conforme as diretrizes do serviço NFC
    PreencherIde( Servico, proNotaFiscal);
    PreencherEmitente( Servico, proNotaFiscal);
    if proNotaFiscal.COD_CLIENTE <> '00001' then
       PreencherDest( Servico, proNotaFiscal);
    PreencherItens( Servico, proNotaFiscal);
    PreencherTotal( Servico, proNotaFiscal);
    PreencherPag( Servico, proNotaFiscal);
    PreencherInfAdic( Servico, proNotaFiscal);
  end;
//Retornar o serviço NFE já preparado para ser enviar a NotaFiscal
  result      := Servico;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherIde(Servico: TServicoDocumentoFiscalEletronico;
                                                                      proNotaFiscal: TNotaFiscal);
begin
   with Servico.DocumentoFiscal.Ide do
   begin
      cNF       := StrToInt( proNotaFiscal.SeqNotaFiscal );
      indPag    := ipVista;
      modelo    := ifthen( proNotaFiscal.Especie = 'NFE',55,65); //<<---
      serie     := strtoint(proNotaFiscal.Serie);//<<-- Não pode ser 1 caracter
      nNF       := StrToInt( proNotaFiscal.SeqNotaFiscal );
      dEmi      := StrToDatetime( FormatDatetime('DD/MM/YYYY ',proNotaFiscal.Data_Emissao)+ FormatDatetime('HH:MM:SS ', Time) );
      //Atribuir valores ao Atributos abaixo ocasionará rejeição Conforme a validação do ACBR:
      //if (NFe.Ide.dSaiEnt <> 0) then  //B10-10
      //   AdicionaErro('705-Rejeição: NFC-e com data de entrada/saída');
      //dSaiEnt := StrToDatetime( FormatDatetime('DD/MM/YYYY ', proNotaFiscal.Data_Emissao)+ FormatDatetime('HH:MM:SS ', Time) );
      //hSaiEnt := Time;
      //É por isso que esses dois atributos estão desabilitados!
      natOp     := 'VENDAS';//<<---
      tpNF      := tnSaida;
      tpEmis    := teNormal;
      tpAmb     := taHomologacao;
      if goEmpresa.AmbienteDFe = 1 then
      tpAmb     :=  taProducao;
      cUF       := UFtoCUF(goEmpresa.Endereco.Uf.Descricao);
      cMunFG    := StrToInt(goEmpresa.Endereco.MunicipioIbge.Codigo);
      finNFe    := fnNormal;
      if proNotaFiscal.Especie = 'NFE' then
         tpImp  := tiRetrato
      else
         tpImp  := tiNFCe;
      indFinal  := cfConsumidorFinal;
      indPres   := pcPresencial;
   end;
end;


class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherEmitente(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);
begin
   with Servico.DocumentoFiscal.Emit do
   begin
      CNPJCPF           := goEmpresa.CNPJ_CPF;
      IE                := goEmpresa.InscricaoEstadual;
      xNome             := goEmpresa.RazaoSocial;
      xFant             := goEmpresa.DESCRICAO;
      IEST              := '';
      if goEmpresa.OptanteSimples = 'S' then
         CRT  := crtSimplesNacional
      else
         CRT  := crtRegimeNormal;
      EnderEmit.fone    := goEmpresa.Telefone;
      EnderEmit.CEP     := StrToInt(goEmpresa.Endereco.CEP);
      EnderEmit.xLgr    := goEmpresa.Endereco.Logradouro;
      EnderEmit.nro     := ifthen( goEmpresa.Endereco.Numero <> '',goEmpresa.Endereco.Numero ,'0');
      EnderEmit.xCpl    := goEmpresa.Endereco.Complemento;
      EnderEmit.xBairro := goEmpresa.Endereco.Bairro;
      EnderEmit.cMun    := StrToInt(goEmpresa.Endereco.MunicipioIbge.Codigo);
      EnderEmit.xMun    := goEmpresa.Endereco.Cidade;
      EnderEmit.UF      := goEmpresa.Endereco.Uf.Descricao;
      enderEmit.cPais   := 1058;
      enderEmit.xPais   := 'BRASIL';
    end;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherDest(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);
begin
    with Servico.DocumentoFiscal.Dest do
    begin
      CNPJCPF           := proNotaFiscal.Cliente.CNPJ_CPF;
      xNome             := proNotaFiscal.Cliente.NomeFantasia;
      ISUF              := '';
      EnderDest.Fone    := proNotaFiscal.Cliente.Telefone;
      EnderDest.CEP     := strtoint( proNotaFiscal.Cliente.Endereco.CEP);
      EnderDest.xLgr    := proNotaFiscal.Cliente.Endereco.Logradouro;
      EnderDest.nro     := ifthen( goEmpresa.Endereco.Numero <> '',
                                   goEmpresa.Endereco.Numero ,'0');
      EnderDest.xCpl    := proNotaFiscal.Cliente.Endereco.Complemento;
      EnderDest.xBairro := proNotaFiscal.Cliente.Endereco.Bairro;
      EnderDest.cMun    := strtoint( proNotaFiscal.Cliente.Endereco.MunicipioIbge.Codigo);
      EnderDest.xMun    := proNotaFiscal.Cliente.Endereco.Cidade;
      EnderDest.UF      := proNotaFiscal.Cliente.Endereco.Uf.Descricao;
      EnderDest.cPais   := 1058;
      EnderDest.xPais   := 'BRASIL';
      indIEDest         := inNaoContribuinte;
    end;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherPag(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);
var
   cds:TClientDataSet;
begin
   with Servico.DocumentoFiscal.Pag.Add do
   begin
      try
         {cds  := gConexao.GetDataSet('T_TPPGTOS','TIPO_COBRANCA','Codigo='+
                                      quotedstr(proNotaFiscal.Cod_TpPgto),
                                      nil,
                                      '' );}
         tPag := fpDinheiro;
         {case cds.fieldbyname('TIPO_COBRANCA').AsInteger of
           2 : tPag := fpCartaoCredito;
           3 : tPag := fpCheque;
           4 : tPag := fpCartaoDebito;
           //: tPag := fpOutro;
         end;}
         vPag := proNotaFiscal.VLR_TOTAL;
      finally
        //cds.free;
      end;
   end;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherItens(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);
var
  I:integer;
  Item: TItemNotaFiscal;
  lrTotalValorICMS, lrTotalValorPis, lrTotalValorCOFINS: real;
begin
    lrTotalValorICMS   := 0;
    lrTotalValorPis    := 0;
    lrTotalValorCOFINS := 0;
    for I := 0 to proNotaFiscal.Itens.Count - 1 do
    begin
      with Servico.DocumentoFiscal.Det.Add do
      begin
        Item := TItemNotaFiscal( proNotaFiscal.Itens[ I ] );

        Prod.nItem     := i + 1; // Número sequencial, para cada item deve ser incrementado
        Prod.cProd     := Item.Cod_Produto;
        Prod.xProd     := Item.DESCRICAO;
        if Servico.DocumentoFiscal.Ide.tpAmb = taHomologacao then
           Prod.xProd  := 'NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
        Prod.NCM       := Item.Produto.CodigoNCM;
        Prod.CFOP      := Item.CFOP.Codigo;
        Prod.cEAN      := Item.Produto.CodigoDeBarras;
        Prod.uCom      := Item.Produto.UNID;
        Prod.qCom      := Item.Quantidade ;
        Prod.vUnCom    := Item.PCO_VENDA;
        Prod.cEANTrib  := Item.Produto.CodigoDeBarras;
        Prod.uTrib     := Item.Produto.UNID;
        Prod.qTrib     := Item.Quantidade;
        Prod.vUnTrib   := Item.PCO_VENDA;
        Prod.vProd     := Item.ValorTotal;
        Prod.vOutro    := 0;
        Prod.vFrete    := 0;
        Prod.vSeg      := 0;
        Prod.vDesc     := Item.ValorDesconto;
        Prod.CEST      := Item.CEST;
        Prod.EXTIPI    := '';

        with Imposto do
        begin
          // lei da transparencia nos impostos
          vTotTrib := 0;
          with ICMS do
          begin
             if goEmpresa.OptanteSimples = 'S' then
             begin
                 CST   := GetCST( Item.SitB );
             end
             else
             begin
                 CSOSN := GetCSOSN( Item.SitB );
             end;
             ICMS.orig    := oeNacional;
             ICMS.modBC   := dbiValorOperacao;
             ICMS.vBC     := Item.ICMS.ValorBaseCalculo;
             ICMS.pICMS   := Item.ICMS.Percentual;
             ICMS.vICMS   := Item.ICMS.Valor;
             ICMS.modBCST := dbisMargemValorAgregado;
             ICMS.pMVAST  := Item.PercentualIcmsSubs;
             ICMS.pRedBCST:= Item.ICMS.PercentualReducao;
             ICMS.vBCST   := Item.Vlr_BaseIcmsSubs;
             ICMS.pICMSST := Item.PercentualIcmsSubs;
             ICMS.vICMSST := Item.Vlr_IcmsSubs;
             ICMS.pRedBC  := Item.ICMS.ValorReducaoBaseCalculo;
          end;

          With PIS do
          begin
             CST  := GetCSTPIS( Item.Pis.CodigoSituacaoTributaria );
             vBC  := Item.Pis.ValorBaseCalculo;
             pPIS := Item.Pis.Percentual;
             vPis := Item.Pis.Valor;
          end;

          With COFINS do
          begin
             CST  := GetCSTCOFINS( Item.COFINS.CodigoSituacaoTributaria );
             vBC  := Item.COFINS.ValorBaseCalculo;
             pCOFINS := Item.COFINS.Percentual;
             vCOFINS := Item.COFINS.Valor;
          end;

          lrTotalValorPis   := lrTotalValorPis + Item.Pis.Valor;
          lrTotalValorCOFINS := lrTotalValorCOFINS + Item.COFINS.Valor;
          lrTotalValorICMS  := lrTotalValorICMS +  Item.ICMS.Valor;
          
        end;
      end;
    end;
    proNotaFiscal.ValorCofins := lrTotalValorCOFINS;
    proNotaFiscal.ValorPis    := lrTotalValorPis;
    proNotaFiscal.VLR_ICMS    := lrTotalValorICMS;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherTotal(Servico: TServicoDocumentoFiscalEletronico;
                                                                        proNotaFiscal: TNotaFiscal);
begin
   with Servico.DocumentoFiscal.Total do
   begin
      ICMSTot.vBC          := proNotaFiscal.VLR_BASEICMS;
      ICMSTot.vICMS        := proNotaFiscal.VLR_ICMS;
      ICMSTot.vBCST        := proNotaFiscal.VLR_BASEICMSSUBS;
      ICMSTot.vST          := proNotaFiscal.VLR_ICMSSUBS;
      ICMSTot.vProd        := proNotaFiscal.Vlr_TotalProd;
      ICMSTot.vFrete       := proNotaFiscal.VLR_FRETE;
      ICMSTot.vSeg         := proNotaFiscal.VLR_SEGURO;
      ICMSTot.vDesc        := proNotaFiscal.DESC_COMERCIAL;
      ICMSTot.vII          := 0;
      ICMSTot.vIPI         := proNotaFiscal.VLR_IPI;
      ICMSTot.vPIS         := proNotaFiscal.ValorPis;
      ICMSTot.vCOFINS      := proNotaFiscal.ValorCofins;
      ICMSTot.vOutro       := proNotaFiscal.VLR_DESPESAS;
      ICMSTot.vNF          := proNotaFiscal.VLR_TOTAL;
      ICMSTot.vFCPUFDest   := 0.00;
      ICMSTot.vICMSUFDest  := 0.00;
      ICMSTot.vICMSUFRemet := 0.00;
    //ICMSTot.vTotTrib     :=
   end;
end;

class procedure TFactoryServicoDocumentoFiscalEletronico.PreencherInfAdic(Servico: TServicoDocumentoFiscalEletronico; proNotaFiscal: TNotaFiscal);
begin
   with Servico.DocumentoFiscal.InfAdic do
   begin
      infCpl     := '';
      infAdFisco := '';
   end;
end;

class function TFactoryServicoDocumentoFiscalEletronico.GetCST(prsSitB: string): TpcnCSTIcms;
begin
  if prsSitB = '00' then result:= cst00 else
  if prsSitB = '10' then result:= cst10 else
  if prsSitB = '20' then result:= cst20 else
  if prsSitB = '30' then result:= cst30 else
  if prsSitB = '40' then result:= cst40 else
  if prsSitB = '41' then result:= cst41 else
  if prsSitB = '45' then result:= cst45 else
  if prsSitB = '50' then result:= cst50 else
  if prsSitB = '51' then result:= cst51 else
  if prsSitB = '60' then result:= cst60 else
  if prsSitB = '70' then result:= cst70 else
  if prsSitB = '80' then result:= cst80 else
  if prsSitB = '81' then result:= cst81 else
  if prsSitB = '90' then result:= cst90;
end;

class function TFactoryServicoDocumentoFiscalEletronico.GetCSOSN(prsSitB: string): TpcnCSOSNIcms;
begin
  if prsSitB = '101' then result:= csosn101 else
  if prsSitB = '102' then result:= csosn102 else
  if prsSitB = '103' then result:= csosn103 else
  if prsSitB = '201' then result:= csosn201 else
  if prsSitB = '202' then result:= csosn202 else
  if prsSitB = '203' then result:= csosn203 else
  if prsSitB = '300' then result:= csosn300 else
  if prsSitB = '400' then result:= csosn400 else
  if prsSitB = '500' then result:= csosn500 else
  if prsSitB = '900' then result:= csosn900 else
  if prsSitB = ''    then result:= csosnVazio;
end;

class function TFactoryServicoDocumentoFiscalEletronico.GetCstPis( CodigoCst: string): TpcnCstPis;
begin
  if CodigoCst = '01' then result:= pis01 else
  if CodigoCst = '02' then result:= pis02 else
  if CodigoCst = '03' then result:= pis03 else
  if CodigoCst = '04' then result:= pis04 else
  if CodigoCst = '05' then result:= pis05 else
  if CodigoCst = '06' then result:= pis06 else
  if CodigoCst = '07' then result:= pis07 else
  if CodigoCst = '08' then result:= pis08 else
  if CodigoCst = '09' then result:= pis09 else
  if CodigoCst = '49' then result:= pis49 else
  if CodigoCst = '50' then result:= pis50 else
  if CodigoCst = '51' then result:= pis51 else
  if CodigoCst = '52' then result:= pis52 else
  if CodigoCst = '53' then result:= pis53 else
  if CodigoCst = '54' then result:= pis54 else
  if CodigoCst = '55' then result:= pis55 else
  if CodigoCst = '56' then result:= pis56 else
  if CodigoCst = '60' then result:= pis60 else
  if CodigoCst = '61' then result:= pis61 else
  if CodigoCst = '62' then result:= pis62 else
  if CodigoCst = '63' then result:= pis63 else
  if CodigoCst = '64' then result:= pis64 else
  if CodigoCst = '65' then result:= pis65 else
  if CodigoCst = '66' then result:= pis66 else
  if CodigoCst = '67' then result:= pis67 else
  if CodigoCst = '70' then result:= pis70 else
  if CodigoCst = '71' then result:= pis71 else
  if CodigoCst = '72' then result:= pis72 else
  if CodigoCst = '73' then result:= pis73 else
  if CodigoCst = '74' then result:= pis74 else
  if CodigoCst = '75' then result:= pis75 else
  if CodigoCst = '98' then result:= pis98 else
  if CodigoCst = '99' then result:= pis99;
end;

class function TFactoryServicoDocumentoFiscalEletronico.GetCstCofins( CodigoCst: string): TpcnCstCofins;
begin
   if CodigoCst = '01' then result:= cof01  else
   if CodigoCst = '02' then result:= cof02  else
   if CodigoCst = '03' then result:= cof03  else
   if CodigoCst = '04' then result:= cof04  else
   if CodigoCst = '05' then result:= cof05  else
   if CodigoCst = '06' then result:= cof06  else
   if CodigoCst = '07' then result:= cof07  else
   if CodigoCst = '08' then result:= cof08  else
   if CodigoCst = '09' then result:= cof09  else
   if CodigoCst = '49' then result:= cof49  else
   if CodigoCst = '50' then result:= cof50  else
   if CodigoCst = '51' then result:= cof51  else
   if CodigoCst = '52' then result:= cof52  else
   if CodigoCst = '53' then result:= cof53  else
   if CodigoCst = '54' then result:= cof54  else
   if CodigoCst = '55' then result:= cof55  else
   if CodigoCst = '56' then result:= cof56  else
   if CodigoCst = '60' then result:= cof60  else
   if CodigoCst = '61' then result:= cof61  else
   if CodigoCst = '62' then result:= cof62  else
   if CodigoCst = '63' then result:= cof63  else
   if CodigoCst = '64' then result:= cof64  else
   if CodigoCst = '65' then result:= cof65  else
   if CodigoCst = '66' then result:= cof66  else
   if CodigoCst = '67' then result:= cof67  else
   if CodigoCst = '70' then result:= cof70  else
   if CodigoCst = '71' then result:= cof71  else
   if CodigoCst = '72' then result:= cof72  else
   if CodigoCst = '73' then result:= cof73  else
   if CodigoCst = '74' then result:= cof74  else
   if CodigoCst = '75' then result:= cof75  else
   if CodigoCst = '98' then result:= cof98  else
   if CodigoCst = '99' then result:= cof99;
end;

end.

