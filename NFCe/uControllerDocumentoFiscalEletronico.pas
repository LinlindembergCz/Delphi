unit uControllerDocumentoFiscalEletronico;

interface

uses uClassNotaFiscal, DB, Dialogs, SysUtils, pcnProcNFe, pcnNFe, DateUtils,
     uClassServicoDocumentoFiscalEletronico, strUtils, pcnAuxiliar,
     ACBrNFe, uInterfaceDAONFe;

type
   TControllerDocumentoFiscalEletronico = class
   private
     pvoDAONfe: IDAONFe;
     FPastaXML: string;
     gACBrNFe : TACBrNFe;
     FServicoNFC : TServicoDocumentoFiscalNFC;
     function ImprimirDanfe( proNotaFiscal : TNotaFiscal ): Boolean;
     function Enviar( seqNotaFiscal, especie, serie: string  ):boolean; overload;//function
     function Enviar( proNotaFiscal : TNotaFiscal ):boolean; overload;//function
   public
     constructor Create(proDAONFe : IDAONFe);
     destructor Destroy;override;
     procedure GerarNFC( proNotaFiscal: TNotaFiscal );
     procedure GerarNFE( proNotaFiscal: TNotaFiscal );
     procedure EnviarContingencia;
     procedure Cancelar(prsSeqNotaFiscal,
                        prsEspecie,
                        prsSerie,
                        prsChaveAcesso,
                        prsProtocolo: String);
     function Inutilizar(Seqnotafiscal1,
                          Seqnotafiscal2,
                          Especie,
                          Serie,
                          Mensagem: string;
                          UpdateStatus : boolean = true):boolean;

   end;

implementation

uses
uClassFactoryServicoDocumentoFiscalEletronico, uMenu, uClassConexao, UFuncoes, 
  Classes, DBClient, ACBrNFeDANFEFR, Windows, ACBrUtil ;

{ TControllerDocumentoFiscalEletronico }

constructor TControllerDocumentoFiscalEletronico.Create(proDAONFe : IDAONFe);
begin
   FPastaXML := gsPath+ 'Enviar\NFe\Individuais\'+gsCNPJCPFEmp+'\'+FormatDatetime('MM-YYYY', date);
   pvoDAONfe := proDAONFe;
end;

destructor TControllerDocumentoFiscalEletronico.Destroy;
begin
  if FServicoNFC <> nil then
  FServicoNFC.free;
  //if pvoDAONfe <> nil then pvoDAONfe.free;
end;

procedure TControllerDocumentoFiscalEletronico.GerarNFC( proNotaFiscal: TNotaFiscal );
var
  lsErros    : string;
  lbContingencia : boolean;
begin
   try

     //prepara um serviço que implmenta o ACBR
     FServicoNFC := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( proNotaFiscal, FPastaXML, true );
     //Se não houver conexao com a internet ou o serviço da sefaz não responder gerar NFC em modo contingencia
     lbContingencia := not VerificaConexaoInternet;
     if lbContingencia then
     begin
        FServicoNFC.ConfigurarContingencia(proNotaFiscal.Data_Emissao);
     end;
     //Geração do XML na pasta Enviar
     try
        FServicoNFC.Gerar(lsErros);

        if lsErros <> '' then
           raise Exception.create('Problema(s) na Pre-Validãcao: '+ lsErros );

        if ( not lbContingencia ) then//Só envia a nota se não estiver em contigencia, ou seja, on-line.
        begin//Enviar a Nota Fiscal
           if Enviar( proNotaFiscal ) then
              ImprimirDanfe( proNotaFiscal );
        end
        else
        begin
           pvoDAONfe.AlterarNFE_Status( gsCod_Emp,
                                       proNotaFiscal.SeqNotaFiscal,
                                       proNotaFiscal.Especie,
                                       proNotaFiscal.Serie,
                                       '',
                                       '',
                                       FServicoNFC.GetChaveAcesso,//<<Captura a chave de acesso gerado pelo ACBR
                                       'G',
                                       '9');
           ImprimirDanfe( proNotaFiscal );
        end;
     Except
        On E: Exception Do
        Begin
           CaixaMensagem( E.message, ctAviso, [cbOk], 0 );
        end;
     end;
  finally
     if FServicoNFC <> nil then FServicoNFC.free;
  end;
end;

function TControllerDocumentoFiscalEletronico.Enviar( proNotaFiscal : TNotaFiscal ):boolean;
begin
  result:= false;
  if Enviar( proNotaFiscal.SeqNotaFiscal, proNotaFiscal.ESPECIE, proNotaFiscal.SERIE)then
  begin
     result:= true;
     if proNotaFiscal.Cliente.EmailArquivoXML <> '' then
     begin
        FServicoNFC.EnviarEmailNF( proNotaFiscal.Cliente.EmailArquivoXML );
     end;
  end;
end;

function TControllerDocumentoFiscalEletronico.Enviar( seqNotaFiscal, especie, serie: string ):boolean;
var
  Retorno    : TProcNFe;

begin
   result := false;
   try
      try
         if FServicoNFC.Enviar( pvoDAONfe.GetSeqLoteNFe(gsCod_Emp) ) then
         begin
            result := true;
            //Obter o retorno da Nota
            Retorno  := FServicoNFC.GetRetorno;
            //Enviar email
            pvoDAONfe.AlterarNFE_Status( gsCod_Emp,
                                        seqNotaFiscal,
                                        especie,
                                        serie,
                                        Retorno.nProt,
                                        datetimetostr(Retorno.dhRecbto),
                                        FServicoNFC.GetChaveAcesso, //<<Captura a chave de acesso gerado pelo ACBR
                                        '',
                                        ifthen( especie = 'NFC','X' ,'F') );
         end;
      Except
         On E: Exception Do
         Begin
           Retorno  := FServicoNFC.GetRetorno;
           if ( Pos('Inativo ou Inoperante', E.message) > 0 ) then
           begin
              pvoDAONfe.AlterarSequenciaLoteNFe( gsCod_Emp );

              pvoDAONfe.AlterarNFE_Status( gsCod_Emp,
                                          seqNotaFiscal,
                                          especie,
                                          serie,
                                          '',
                                          '',
                                          FServicoNFC.GetChaveAcesso,//<<Captura a chave de acesso gerado pelo ACBR
                                          'G',
                                          '9');
           end
           else
           if ( Pos('Rejeicao', E.message) > 0 ) or ( Not ( Retorno.cStat in [100, 150]) ) then
           begin
             Inutilizar( seqNotaFiscal,seqNotaFiscal, especie, serie, E.message );
           end;
           CaixaMensagem( E.message, ctAviso, [cbOk], 0 );
         end;
      end;
      //realizar impressao danfe

   finally
      if Retorno     <> nil then
         Retorno.free;
   end;
end;

function TControllerDocumentoFiscalEletronico.ImprimirDanfe(  proNotaFiscal : TNotaFiscal ): Boolean;
const
    DANFENFCRC : string = 'DANFE_NFC_ACBRFR';
Var ACBrNFcDANFEFR : TACBrNFeDANFEFR;
    lsChaveNfe, lsAnoMes, lsCNPJ, lsArquivo: string;
    lsCaminhoDanfe: string;
    loResourceStream: TResourceStream;
begin
   Result := False;
   loResourceStream := nil;
   gACBrNFe         := nil;
   ACBrNFcDANFEFR   := nil;
   try
      try
         if proNotaFiscal = nil then
         begin
             CaixaMensagem('Não encontrada nota fiscal', ctErro, [ cbOk ], 0);
             Exit;
         end;
       
         lsAnoMes := FormatDatetime('mm-yyyy', date );
         lsChaveNfe := FServicoNFC.GetChaveAcesso;
         lsChaveNfe := lsChaveNfe + Modulo11( lsChaveNfe );
         lsCNPJ := gsCNPJCPFEmp;

         gACBrNFe := TACBrNFe.Create(nil);
         gACBrNFe.NotasFiscais.LoadFromFile(gsPath + 'Enviar\NFe\Individuais\'+lsCNPJ+'\'+lsAnoMes+'\'+lsChaveNfe+'-nfe.xml', false);

         if gACBrNFe.NotasFiscais.Count = 0 then
             raise Exception.Create('Não foi carregado nenhum xml para impressão');

         lsCaminhoDanfe := gsPath + 'Relatorios\DANFeNFCe.fr3';
         If Not FilesExists(lsCaminhoDanfe) Then
         Begin
            if NOT DirectoryExists(gsPath + 'Relatorios') then
               CreateDir( gsPath + 'Relatorios' );
            If FindResource(HInstance, PAnsiChar( DANFENFCRC ), 'TEXT') <> 0 then
            begin
               loResourceStream := TResourceStream.Create( Hinstance, DANFENFCRC, PAnsiChar( 'TEXT' ) );
               loResourceStream.SavetoFile( lsCaminhoDanfe );
            end;
         End;

         ACBrNFcDANFEFR := TACBrNFeDANFEFR.Create(nil);
         ACBrNFcDANFEFR.FastFile := lsCaminhoDanfe;
         ACBrNFcDANFEFR.MostrarPreview := False;
         gACBrNFe.DANFE := ACBrNFcDANFEFR;
         gACBrNFe.NotasFiscais.Imprimir;


         //FServicoNFC.ImprimirDanfe;

         Result := True;

      except
         on E: Exception do
         begin
            E.Message := Format('Falha ao imprimir o DANFe: %s', [E.Message]);
            raise;
         end;
      end;
   finally
      if Assigned( loResourceStream ) then FreeAndNil( loResourceStream );
      If Assigned( ACBrNFcDANFEFR ) then FreeAndNil(ACBrNFcDANFEFR);
      If Assigned( gACBrNFe ) then FreeAndNil(gACBrNFe);            
   end;
end;

function TControllerDocumentoFiscalEletronico.Inutilizar(Seqnotafiscal1,
                                                         Seqnotafiscal2,
                                                         Especie,
                                                         Serie,
                                                         Mensagem: string;
                                                         UpdateStatus : boolean = true):boolean;
var
   Retorno    : TProcNFe;
begin
   Try
     Try//Inutilizar a nota fiscal
        Retorno  := FServicoNFC.GetRetorno;
        if FServicoNFC.Inutilizar( gsCNPJCPFEmp,
                                         ifthen( Especie = 'NFC','65' ,'55'),
                                         Serie,
                                         inttostr( yearOf(Date)),
                                         Seqnotafiscal1,
                                         Seqnotafiscal2,
                                         Mensagem) then
         begin
            result := true;
            if UpdateStatus then
            begin
              pvoDAONfe.AlterarNFE_Status( gsCod_Emp,
              seqNotaFiscal1,
              especie,
              serie,
              Retorno.nProt,
              FServicoNFC.GetChaveAcesso,//<<Captura a chave de acesso gerado pelo ACBR
              Retorno.xMotivo ,
              'C',
              'Z' );
            end;
         end;
     except
        On E: Exception Do
        Begin
           CaixaMensagem('Erro: ' + E.Message, ctErro, [ cbOk ], 0);
        End;
     End;
   finally

   end;
end;

procedure TControllerDocumentoFiscalEletronico.EnviarContingencia;
var
    lcdsNotasFiscaisContingencia : TDataSet;
    lsSequencia, lsEspecieNF , lsSerieNF, lsNFEChaveAcesso: String;
    lsArquivo : string;
begin
   try
      FServicoNFC    := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( nil , FPastaXML, false );
      lcdsNotasFiscaisContingencia := pvoDAONfe.RetornarNotasFiscaisContingencia(gsCod_Emp);
      with lcdsNotasFiscaisContingencia do
      begin
         first;
         while Not Eof do
         begin
            try
               lsSequencia      := FieldByName( 'SeqNotaFiscal' ).AsString;
               lsEspecieNF      := FieldByName( 'Especie' ).AsString;
               lsSerieNF        := FieldByName( 'Serie' ).AsString;
               lsNFEChaveAcesso := FieldByName( 'NFE_ChaveAcesso' ).AsString;
               lsArquivo        :=  FServicoNFC.GetFileNameXML(lsNFEChaveAcesso);
               if FileExists( lsArquivo ) then
                   FServicoNFC.LoadFromFile(lsArquivo)//Carregar o Arquivo XML
               else
                   Raise Exception.Create('Arquivo de Contingência não existe: '+ lsArquivo);
               Enviar( lsSequencia, lsEspecieNF, lsSerieNF );
            finally
               Next;
            end;
         end;
      end;
   finally
      lcdsNotasFiscaisContingencia.free;
      FServicoNFC.free;
   end;
end;

procedure TControllerDocumentoFiscalEletronico.Cancelar(prsSeqNotaFiscal,
                                                        prsEspecie,
                                                        prsSerie,
                                                        prsChaveAcesso,
                                                        prsProtocolo: String);
var
  lsLoteNFe,
  Justificativa: string;
begin
   try
      FServicoNFC   := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( nil , FPastaXML, TRUE );
      lsLoteNFe     := pvoDAONfe.GetSeqLoteNFe(gsCod_Emp);

      Justificativa := 'Cancelamento de Nota Fiscal Eletronica ao consumidor';

      if FServicoNFC.Cancelar( prsSeqNotaFiscal,
                            prsEspecie,
                            prsSerie ,
                            prsChaveAcesso,
                            Justificativa,
                            prsProtocolo,
                            lsLoteNFe ) then
       begin
           pvoDAONfe.AlterarNFE_Status( gsCod_Emp,
                                       prsSeqNotaFiscal,
                                       prsEspecie,
                                       prsSerie,
                                       '',
                                       '',
                                       '',
                                       'C',
                                       'C');
       end;

   finally
     FServicoNFC.free;
   end;
end;

procedure TControllerDocumentoFiscalEletronico.GerarNFE(
  proNotaFiscal: TNotaFiscal);
var
  ServicoNFE : TServicoDocumentoFiscalNFE;
begin
  {try
    ServicoNFE := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFE( proNotaFiscal );
  //ServicoNFC.Gerar;
    ServicoNFC.Enviar;
  finally
    ServicoNFC.free;
  end;}
end;



end.


