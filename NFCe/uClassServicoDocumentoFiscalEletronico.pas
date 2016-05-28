unit uClassServicoDocumentoFiscalEletronico;

interface

uses IniFiles, ShellAPI, pcnRetConsReciNFe, pcnNFe, pcnAuxiliar,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ACBrNFe, pcnConversao, ACBrUtil, ACBrNFeDANFEClass, ACBrNFeDANFeESCPOS,
  ACBrBase, ACBrDFe, XMLIntf, XMLDoc, zlib, ACBrMail, ACBrNFeDANFeRLClass,
  ACBrDANFCeFortesFr, pcnConversaoNFe,  ACBrDFeSSL, ACBrNFeConfiguracoes,
  ACBrNFeDANFEFR, ACBrNFeWebServices, pcnProcNFe;

type
  //Não coloque regra de negocio aqui!
  TServicoDocumentoFiscalEletronico = class
  private
    FErros  : string;
    FDocumentoFiscal  : TNFE;
    procedure IncializarComponenteACBR( ConfiguracoesNFe: TConfiguracoesNFe );
    procedure InicializarDocumentoFiscal;
  protected
    FACBrNFe: TACBrNFe;
    FACBrNFeDANFEFR : TACBrNFeDANFEFR;
    procedure GerarDFe( var Erros: string );
  public
    destructor Destroy; override;
    property Erros: string read  FErros;
    property DocumentoFiscal  : TNFE read FDocumentoFiscal write FDocumentoFiscal;
    constructor Create( ConfiguracoesNFe: TConfiguracoesNFe );virtual;

    procedure Gerar(prbAssinar:boolean =false );virtual; abstract;
    function Enviar(Lote: string):boolean;virtual; abstract;
    function GetRetorno: TProcNFe;
    function Inutilizar( psCNPJEmit,
                          psModelo,
                          psSerie,
                          psAno,
                          psNumeroInicial,
                          psNumeroFinal,
                          psJustificativa : String):boolean;
    function EnviarEmailNF( prsEmailDestinatario: String): Boolean;
    procedure LoadFromFile(Arquivo: string);
    function ExtractPathNFe: string;
    function GetChaveAcesso: string;
    function GetFileNameXML(prsChaveAcesso: string): string;
    function VerificarStatusServico: boolean;
    procedure ConfigurarContingencia(Data: TDatetime);
    function Cancelar(SeqNotaFiscal, Especie, Serie, prsChaveAcesso, Justificativa, Protocolo, LoteNFe: String): Boolean;
    procedure Imprimir( prsChaveAcesso : String );
    function AmbienteHomologacao: boolean;
    function AmbienteProducao: boolean;
  end;


  TServicoDocumentoFiscalNFE = class(TServicoDocumentoFiscalEletronico)
  public
    constructor Create( ConfiguracoesNFe: TConfiguracoesNFe );override;
    procedure Gerar( var Erros: string );
    function Enviar(Lote: string):boolean;override;
  end;


  TServicoDocumentoFiscalNFC = class(TServicoDocumentoFiscalEletronico)
  strict private
    function GetQrCode: string;
  public
    constructor Create( ConfiguracoesNFe: TConfiguracoesNFe );override;
    procedure Gerar( var Erros: string);
    function Enviar(Lote: string):boolean;override;
  end;

implementation

{ TControllerNFCe }



constructor TServicoDocumentoFiscalEletronico.Create( ConfiguracoesNFe: TConfiguracoesNFe );
begin
   IncializarComponenteACBR(ConfiguracoesNFe);
end;

procedure TServicoDocumentoFiscalEletronico.Imprimir( prsChaveAcesso : String );
begin
   FACBrNFe.NotasFiscais.LoadFromFile( GetFileNameXML( prsChaveAcesso ), False );
   FACBrNFeDANFEFR.FastFile := ExtractFilePath(Application.ExeName) + 'Relatorios\DANFeNFCe.fr3';
   FACBrNFe.DANFE := FACBrNFeDANFEFR;
   FACBrNFe.DANFE.ImprimirDANFE( FACBrNFe.NotasFiscais[0].NFe );
end;

procedure TServicoDocumentoFiscalEletronico.IncializarComponenteACBR( ConfiguracoesNFe: TConfiguracoesNFe );
begin
   FACBrNFe   := (ConfiguracoesNFe.Owner as TACBrNFe);
   with FACBrNFe.Configuracoes do
   begin
      Geral.FormaEmissao         := ConfiguracoesNFe.Geral.FormaEmissao;//teNormal;
      Geral.VersaoDF             := ConfiguracoesNFe.Geral.VersaoDF;//ve310;
      Geral.Salvar               := ConfiguracoesNFe.Geral.Salvar;//true;
      Geral.ExibirErroSchema     := ConfiguracoesNFe.Geral.ExibirErroSchema;//true;
      Geral.RetirarAcentos       := ConfiguracoesNFe.Geral.RetirarAcentos;//true;
      Geral.FormatoAlerta        := ConfiguracoesNFe.Geral.FormatoAlerta;//'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';
      Geral.SSLLib               := ConfiguracoesNFe.Geral.SSLLib;//libCapicom;//(libNone, libOpenSSL, libCapicom, libCapicomDelphiSoap);
      Geral.ModeloDF             := ConfiguracoesNFe.Geral.ModeloDF;//moNFCe;
      Geral.IncluirQRCodeXMLNFCe := ConfiguracoesNFe.Geral.IncluirQRCodeXMLNFCe;//true;
      Arquivos.PathSalvar        := ConfiguracoesNFe.Arquivos.PathSalvar;//'D:\Lindemberg\Delphi\NFCe\';
      Arquivos.PathSchemas       := ConfiguracoesNFe.Arquivos.PathSchemas;//'D:\Producao\Componentes\D2007\ACBR\Exemplos\ACBrDFe\Schemas\NFe\';
      Certificados.ArquivoPFX    := ConfiguracoesNFe.Certificados.ArquivoPFX;//'D:\Producao\Postos\Programa\certificado\Posto Santa Rita BA = SENHA 123mudar - Expira em 19-05-2016.pfx';
      Geral.CSC                  := ConfiguracoesNFe.Geral.CSC;
      Geral.IdCSC                := ConfiguracoesNFe.Geral.IdCSC;
    //Certificados.Senha         := ConfiguracoesNFe.Certificados.Senha;//'123mudar';
      Certificados.NumeroSerie   := ConfiguracoesNFe.Certificados.NumeroSerie;//'32144a65a088464ba8861893c72082f9';
  {   Arquivos.PathSalvar        := 'D:\Lindemberg\Delphi\NFCe\';
      Arquivos.PathSchemas       := 'D:\Producao\Componentes\D2007\ACBR\Exemplos\ACBrDFe\Schemas\NFe\';
      Certificados.ArquivoPFX    := 'D:\Producao\Postos\Programa\certificado\Posto Santa Rita BA = SENHA 123mudar - Expira em 19-05-2016.pfx';
      Certificados.Senha         := '123mudar';
      Certificados.NumeroSerie   := '32144a65a088464ba8861893c72082f9';   }
      WebServices.Tentativas     := 2;
      WebServices.Salvar         := ConfiguracoesNFe.WebServices.Salvar;//true;
      WebServices.UF             := ConfiguracoesNFe.WebServices.UF;//'BA';
      WebServices.Ambiente       := ConfiguracoesNFe.WebServices.Ambiente;
      WebServices.AguardarConsultaRet:= 2000; // aguardar 2 segundos antes de consultar
      WebServices.Visualizar     := ConfiguracoesNFe.WebServices.Visualizar;

   end;
end;

constructor TServicoDocumentoFiscalNFC.Create( ConfiguracoesNFe: TConfiguracoesNFe );
begin
  ConfiguracoesNFe.Geral.ModeloDF  := moNFCe;
  inherited Create(ConfiguracoesNFe);
  InicializarDocumentoFiscal;
end;

constructor TServicoDocumentoFiscalNFE.Create( ConfiguracoesNFe: TConfiguracoesNFe );
begin
  ConfiguracoesNFe.Geral.ModeloDF  := moNFe;
  inherited Create(ConfiguracoesNFe);
  InicializarDocumentoFiscal;
end;

procedure TServicoDocumentoFiscalEletronico.GerarDFe( var Erros: string);
begin
   FACBrNFe.NotasFiscais.Validar;
   FACBrNFe.NotasFiscais.ValidarRegrasdeNegocios( Erros );
   if Erros = '' then
   begin
     FACBrNFe.NotasFiscais.GerarNFe;
   end;
end;

function TServicoDocumentoFiscalEletronico.GetChaveAcesso: string;
begin
  result:= Copy( stringReplace( DocumentoFiscal.infNFE.ID ,'NFe', '',[rfReplaceAll]) , 1, 43) ;
end;

function TServicoDocumentoFiscalEletronico.GetRetorno: TProcNFe;
begin
  result:= FACBrNFe.NotasFiscais.Items[0].NFe.procNFe;
end;

function TServicoDocumentoFiscalEletronico.GetFileNameXML(prsChaveAcesso: string ): string;
var
  digito: Integer;
begin
   GerarDigito( digito, prsChaveAcesso );
   result:= ExtractPathNFe + prsChaveAcesso + inttostr( digito ) +'-nfe.xml';
end;

procedure TServicoDocumentoFiscalEletronico.InicializarDocumentoFiscal;
begin
   FACBrNFe.NotasFiscais.Clear;
   DocumentoFiscal:= FACBrNFe.NotasFiscais.Add.NFe;
   FACBrNFeDANFEFR := TACBrNFeDANFEFR.Create(Nil);
end;

function TServicoDocumentoFiscalEletronico.Inutilizar( psCNPJEmit,
                                                        psModelo,
                                                        psSerie,
                                                        psAno,
                                                        psNumeroInicial,
                                                        psNumeroFinal,
                                                        psJustificativa : String):Boolean;
begin
    result := false;
    Try
        FACBrNFe.WebServices.Inutiliza(psCNPJEmit,
                                       psJustificativa,
                                       StrToInt(psAno),
                                       StrToInt(psModelo),
                                       StrToInt(psSerie),
                                       StrToInt(psNumeroInicial),
                                       StrToInt(psNumeroFinal) );
       result := true;
    Except
        On E: Exception Do
        Begin
          showmessage('Erro ao inutilizar! ' + E.Message);
        End;
    End;
end;

procedure TServicoDocumentoFiscalEletronico.LoadFromFile(Arquivo: string);
begin
   FACBrNFe.NotasFiscais.Clear;
   FACBrNFe.NotasFiscais.LoadFromFile( Arquivo , false);
end;

function TServicoDocumentoFiscalNFC.GetQrCode: string;
begin
  result:= FACBrNFe.GetURLQRCode( DocumentoFiscal.Ide.cUF,
                                  DocumentoFiscal.Ide.tpAmb,
                                  DocumentoFiscal.infNFe.ID,
                                  DocumentoFiscal.Dest.CNPJCPF,
                                  DocumentoFiscal.Ide.dEmi,
                                  DocumentoFiscal.Total.ICMSTot.vNF,
                                  DocumentoFiscal.Total.ICMSTot.vBC,
                                  DocumentoFiscal.signature.DigestValue );
end;

procedure TServicoDocumentoFiscalNFC.Gerar( var Erros: string);
begin
   DocumentoFiscal.Transp.modFrete   := mfSemFrete;// NFC-e não tem FRETE
   DocumentoFiscal.infNFeSupl.qrCode := GetQrCode;
   GerarDFe( Erros );
end;

function TServicoDocumentoFiscalNFC.Enviar(Lote: string):boolean;
begin
   result:= FACBrNFe.Enviar( Lote, false, true);
end;

procedure TServicoDocumentoFiscalNFE.Gerar(var Erros: string );
begin
   GerarDFe(Erros )
end;

function TServicoDocumentoFiscalNFE.Enviar(Lote: string):boolean;
begin
  result := FACBrNFe.Enviar(Lote);
end;

function TServicoDocumentoFiscalEletronico.EnviarEmailNF( prsEmailDestinatario : String): Boolean;

  function HTMLTag(prsTag, prsConteudo : String) : String;
  Begin
     if LowerCase(prsTag) = 'br' then
     Begin
        Result := '<br>';
        Exit;
     End;
     Result := Format('<%s>%s</%s>', [prsTag, prsConteudo, prsTag]);
  end;

Var
  lslMensagens : TStringList;
  lstAnexos:TStringList;
  lsArquivo:string;
begin
   If prsEmailDestinatario = '' Then
   Begin
      Result := True;
      Exit;
   End;

   Try
      Try
         //showmessage( 'Host '+ FACBrNFe.MAIL.Host+'  Email '+prsEmailDestinatario );
         if (FACBrNFe.MAIL.Host <> '') and (prsEmailDestinatario <> '') then
         begin
            lslMensagens := TStringList.Create;
            lslMensagens.Add( Format('URI da nota fiscal: %s%s', [DocumentoFiscal.signature.URI, HTMLTag('br','')]) );
            lslMensagens.Add( Format('Empresa: %s%s', [DocumentoFiscal.Emit.xNome, HTMLTag('br','')]) );
            lslMensagens.Add( Format('CNPJ: %s', [DocumentoFiscal.Emit.CNPJCPF]) );
            lsArquivo:= GetFileNameXML( GetChaveAcesso );

            if FileExists( lsArquivo ) then
            begin
               lstAnexos:= TStringList.create;
               lstAnexos.Add( GetFileNameXML( GetChaveAcesso ) );
            end;

            FACBrNFe.EnviarEmail( prsEmailDestinatario,
                                  Format('NFE - %s', [DocumentoFiscal.Emit.xNome]),
                                  lslMensagens,
                                  nil,
                                  lstAnexos ,
                                  nil );

         end;
         Result := True;
      Except
         on E: Exception do
         begin
            showmessage(E.message );
            Result := False;
         end;
      End;
   Finally
      FreeAndNil( lslMensagens );
      if lstAnexos <> nil then
      lstAnexos.free;
   End;
end;

function TServicoDocumentoFiscalEletronico.ExtractPathNFe: string;
begin
   result:= FACBrNFe.Configuracoes.Arquivos.PathSalvar;
end;

function TServicoDocumentoFiscalEletronico.VerificarStatusServico:boolean;
begin
  result := false;
  try
    result := FACBrNFe.WebServices.StatusServico.Executar;
  except end;
end;

procedure TServicoDocumentoFiscalEletronico.ConfigurarContingencia(Data:TDatetime);
begin
  with DocumentoFiscal.Ide do
  begin
     tpEmis := teOffLine;
     dhCont := StrToDatetime( FormatDatetime('DD/MM/YYYY ',Data )+ FormatDatetime('HH:MM:SS ', Time) );
     xJust  := 'COMUNICAÇÃO INDISPONIVEL COM SERVIDOR DO SEFAZ';
  end;
  DocumentoFiscal.InfAdic.infAdFisco := 'EMITIDA EM CONTINGÊNCIA';
end;

function TServicoDocumentoFiscalEletronico.AmbienteHomologacao: boolean;
begin
   result:= FACBrNFe.Configuracoes.WebServices.AmbienteCodigo = 2;
end;

function TServicoDocumentoFiscalEletronico.AmbienteProducao: boolean;
begin
   result:= FACBrNFe.Configuracoes.WebServices.AmbienteCodigo = 1;
end;

function TServicoDocumentoFiscalEletronico.Cancelar( SeqNotaFiscal,
                                                     Especie,
                                                     Serie,
                                                     prsChaveAcesso,
                                                     Justificativa,
                                                     Protocolo,
                                                     LoteNFe: String ): Boolean;
var
  digito: Integer;
  lsArquivo: string;
begin
    Result := False;
    try
       lsArquivo             :=  GetFileNameXML(prsChaveAcesso);
       if FileExists( lsArquivo ) then
          LoadFromFile(lsArquivo)
       else
       begin
          raise Exception.Create('O Arquivo XML não foi localizado '+ lsArquivo );
       end;

       GerarDigito( digito, prsChaveAcesso );
       FACBrNFe.EventoNFe.Evento.Clear;
       with FACBrNFe.EventoNFe.Evento.Add do
       begin
          infEvento.chNFe    := prsChaveAcesso+inttostr(digito);
          infEvento.CNPJ     := copy(prsChaveAcesso,7,14);
          infEvento.dhEvento := now;
          infEvento.tpAmb    := FACBrNFe.Configuracoes.WebServices.Ambiente;
          infEvento.cOrgao   := StrToInt(copy(prsChaveAcesso,1,2));
          infEvento.tpEvento := teCancelamento;
          infEvento.detEvento.xJust := Justificativa;
          infEvento.detEvento.nProt := Protocolo;
       end;

       if FACBrNFe.EnviarEvento(StrToInt(LoteNFe)) then
       begin
          with FACBrNFe.WebServices.EnvEvento do
          begin
            if EventoRetorno.retEvento.Items[0].RetInfEvento.cStat <> 135 then
            begin
              raise Exception.CreateFmt(
                'Ocorreu o seguinte erro ao cancelar a NFCe:'  + sLineBreak +
                'Código:%d' + sLineBreak +
                'Motivo: %s', [
                  EventoRetorno.retEvento.Items[0].RetInfEvento.cStat,
                  EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo
              ]);
            end;
          end;
          result := true;
       end
       else
       begin
            with FACBrNFe.WebServices.EnvEvento do
            begin
              raise Exception.Create(
                'Ocorreram erros ao tentar efetuar o cancelamento da NFCe:' + sLineBreak +
                'Lote: '     + IntToStr(EventoRetorno.idLote) + sLineBreak +
                'Ambiente: ' + TpAmbToStr(EventoRetorno.tpAmb) + sLineBreak +
                'Orgao: '    + IntToStr(EventoRetorno.cOrgao) + sLineBreak +
                sLineBreak +
                'Status: '   + IntToStr(EventoRetorno.cStat) + sLineBreak +
                'Motivo: '   + EventoRetorno.xMotivo
              );
            end;
       end;
    except on E: Exception do
        begin
           raise Exception.Create('Erro: ' + E.Message);
           exit;
        end;
    end;       
end;

destructor TServicoDocumentoFiscalEletronico.Destroy;
begin
  {if FACBrNFe <> nil then
     FACBrNFe.free;
  if FACBrNFeDANFEFR <> nil then
     FACBrNFeDANFEFR.free;
  if FDocumentoFiscal <> nil then
     FDocumentoFiscal.free;}
end;

end.

