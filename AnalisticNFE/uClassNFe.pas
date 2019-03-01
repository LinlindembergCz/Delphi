unit uClassNFe;

interface

uses
  SysUtils, SqlExpr, Provider, Forms, Contnrs, Windows, Classes, DateUtils,
  StrUtils;

type
   TNFeStatus = ( NFeNone, NFeEnviada, NFeGerada, NFeCancelada, NFeAutorizada,
                  NFeImpressa, NFeNaoGerada, NFeInutilizada, NFeContingencia, NFeContingenciaOffLine );
   TNFeAmbiente = (NFeambProducao, NFeambHomologacao);
   TNFe = class
   private
      FNFe_ChaveAcesso: string;
      FNFe_Status: TNFeStatus;
      FNFe_Recibo: string;
      FNFe_DataProtocolo: TDateTime;
      FNFe_Protocolo: string;
      FDigVal: string;
      FCodigoCliente: string;
      FMotivo: String;
      FverAplic: String;
      FAmbiente: TNFeAmbiente;
    FDataContingencia: TDateTime;
    FReciboContigencia: string;
      procedure SetNFe_ChaveAcesso(const Value: string);
      procedure SetNFe_DataProtocolo(const Value: TDateTime);
      procedure SetNFe_Protocolo(const Value: string);
      procedure SetNFe_Recibo(const Value: string);
      procedure SetNFe_Status(const Value: TNFeStatus);
      procedure SetDigVal(const Value: string);
      procedure SetCodigoCliente(const Value: string);
      procedure SetMotivo(const Value: String);
      procedure SetverAplic(const Value: String);
      procedure SetAmbiente(const Value: TNFeAmbiente);
      procedure SetDataContingencia(const Value: TDateTime);
      procedure SetReciboContigencia(const Value: string);
   protected
    { protected declarations }
   public
      class function CalculaDigitoNfe(Valor: String; Base: Integer = 9; Resto : boolean = false): string;
      property CodigoCliente: string read FCodigoCliente write SetCodigoCliente;
      property ChaveAcesso : string read FNFe_ChaveAcesso write SetNFe_ChaveAcesso;
      property Ambiente : TNFeAmbiente read FAmbiente write SetAmbiente;
      property Status : TNFeStatus read FNFe_Status write SetNFe_Status;
      property Motivo : String read FMotivo write SetMotivo;
      property Protocolo: string read FNFe_Protocolo write SetNFe_Protocolo;
      property DataProtocolo: TDateTime read FNFe_DataProtocolo write SetNFe_DataProtocolo;
      property ReciboContigencia: string read FReciboContigencia write SetReciboContigencia;
      property DataContingencia: TDateTime read FDataContingencia write SetDataContingencia;
      property Recibo: string read FNFe_Recibo write SetNFe_Recibo;
      property DigVal: string read FDigVal write SetDigVal;
      property verAplic : String read FverAplic write SetverAplic;
   end;

const STATUS_OK: string = 'OK';
      RETORNO_LOTE_PROCESSAMENTO     : string = '105';
      RETORNO_CHAVE_DE_ACESSO_DIFERE : string = '613';
      RETORNO_NOTA_JA_EXISTE         : string = '217';
      RETORNO_ERRO_GENERICO          : string = '000';
      RETORNO_LOTE_RECEBIDO          : String = '103';
      RETORNO_LOTE_PROCESSADO        : string = '104';
      RETORNO_LOTE_AUTORIZADO        : string = '100';
      STATUS_ERRO_DE_COMUNICACAO     : string = 'ERROCOMUNICACAOSEFAZ';
      STATUS_USO_DENEGADO            : string = '302';
      SISTEMA: string = 'posto';
      AMBIENTEHOMOLOGACAO: string = 'homologacao';
      AMBIENTEPRODUCAO: string = 'Producao';   

implementation

{ TNFe }

class function TNFe.CalculaDigitoNfe(Valor: String; Base: Integer = 9; Resto : boolean = false) : string;
{
   Rotina muito usada para calcular dígitos verificadores
   Pega-se cada um dos dígitos contidos no parâmetro VALOR, da direita para a
   esquerda e multiplica-se pela seqüência de pesos 2, 3, 4 ... até BASE.
   Por exemplo: se a base for 9, os pesos serão 2,3,4,5,6,7,8,9,2,3,4,5...
   Se a base for 7, os pesos serão 2,3,4,5,6,7,2,3,4...
   Soma-se cada um dos subprodutos.
   Divide-se a soma por 11.
   Faz-se a operação 11-Resto da divisão e devolve-se o resultado dessa operação
   como resultado da função Modulo11.
   Obs.: Caso o resultado seja maior que 9, deverá ser substituído por 0 (ZERO).
}
var
   Soma : integer;
   Contador, Peso, Digito : integer;
begin
   Soma := 0;
   Peso := 2;
   for Contador := Length(Valor) downto 1 do
   begin
      Soma := Soma + (StrToInt(Valor[Contador]) * Peso);
      if Peso < Base then
         Peso := Peso + 1
      else
         Peso := 2;
   end;

   if Resto then
      Result := IntToStr(Soma mod 11)
   else
   begin
      Digito := 11 - (Soma mod 11);
      if (Digito > 9) then
         Digito := 0;
      Result := IntToStr(Digito);
   end
end;

procedure TNFe.SetAmbiente(const Value: TNFeAmbiente);
begin
  FAmbiente := Value;
end;

procedure TNFe.SetCodigoCliente(const Value: string);
begin
  FCodigoCliente := Value;
end;

procedure TNFe.SetDataContingencia(const Value: TDateTime);
begin
  FDataContingencia := Value;
end;

procedure TNFe.SetDigVal(const Value: string);
begin
  FDigVal := Value;
end;

procedure TNFe.SetMotivo(const Value: String);
begin
  FMotivo := Value;
end;

procedure TNFe.SetNFe_ChaveAcesso(const Value: string);
begin
  FNFe_ChaveAcesso := Value;
end;

procedure TNFe.SetNFe_DataProtocolo(const Value: TDateTime);
begin
  FNFe_DataProtocolo := Value;
end;

procedure TNFe.SetNFe_Protocolo(const Value: string);
begin
  FNFe_Protocolo := Value;
end;

procedure TNFe.SetNFe_Recibo(const Value: string);
begin
  FNFe_Recibo := Value;
end;

procedure TNFe.SetNFe_Status(const Value: TNFeStatus);
begin
  FNFe_Status := Value;
end;

procedure TNFe.SetReciboContigencia(const Value: string);
begin
  FReciboContigencia := Value;
end;

procedure TNFe.SetverAplic(const Value: String);
begin
  FverAplic := Value;
end;

end.
