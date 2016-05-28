unit uControllerDocumentoFiscalEletronico;

interface

uses uClassNotaFiscal, DB, Dialogs, SysUtils, pcnProcNFe, pcnNFe, DateUtils,
     uClassServicoDocumentoFiscalEletronico, strUtils, pcnAuxiliar,
     ACBrNFe, uInterfaceDAONFe;

type
   TControllerDocumentoFiscalEletronico = class
   private
     FCodigoEmpresa: string;
     FCNPJCPFEmp:string;
     FSeqNotaFiscal: string;
     FSerie: string;
     FEspecie: string;
     FDataEmissao:TDatetime;
     FEmailArquivoXML:string;
     FSenhaMaster : Boolean;
     FDataHoraContingencia:TDatetime;
     pvoDAONfe: IDAONFe;
     FPastaXML: string;
     gACBrNFe : TACBrNFe;
     FServicoNFC : TServicoDocumentoFiscalNFC;
     function ImprimirDanfe( proNotaFiscal : TNotaFiscal ): Boolean;
     function Enviar( prsSeqNotaFiscal,
                      prsEspecie,
                      prsSerie: string;
                      var prbContingencia: boolean ):boolean; overload;//function
     function Enviar( proNotaFiscal : TNotaFiscal ):boolean; overload;//function
   public
     constructor Create(proDAONFe : IDAONFe;
                        prsCodigoEmpresa: string;
                        prsCNPJCPFEmp:string);overload;
     constructor Create(proDAONFe : IDAONFe;
                        prsCodigoEmpresa: string;
                        prsCNPJCPFEmp:string;
                        prsSeqNotaFiscal: string;
                        prsSerie: string;
                        prsEspecie: string;
                        prdDataEmissao:TDatetime;
                        prsEmailArquivoXML:string;
                        prbSenhaMaster : Boolean);overload;
     destructor Destroy;override;
     procedure GerarNFC( proNotaFiscal: TNotaFiscal;var prbContingencia:boolean   );
     procedure GerarNFE( proNotaFiscal: TNotaFiscal;var prbContingencia:boolean  );
     function EnviarContingencia:boolean;
     procedure Cancelar(prsSeqNotaFiscal,
                        prsEspecie,
                        prsSerie,
                        prsChaveAcesso,
                        prsProtocolo: String);
     function Inutilizar(prsSeqnotafiscal1,
                          prsSeqnotafiscal2,
                          prsEspecie,
                          prsSerie,
                          Mensagem: string;
                          UpdateStatus : boolean = true):boolean;
   end;

implementation

uses
uClassFactoryServicoDocumentoFiscalEletronico, uMenu, uClassConexao, UFuncoes,
  Classes, DBClient, ACBrNFeDANFEFR, Windows, ACBrUtil;

{ TControllerDocumentoFiscalEletronico }

constructor TControllerDocumentoFiscalEletronico.Create(proDAONFe : IDAONFe;
                                                        prsCodigoEmpresa: string;
                                                        prsCNPJCPFEmp:string;
                                                        prsSeqNotaFiscal: string;
                                                        prsSerie: string;
                                                        prsEspecie: string;
                                                        prdDataEmissao:TDatetime;
                                                        prsEmailArquivoXML:string;
                                                        prbSenhaMaster : Boolean);
begin
   pvoDAONfe := proDAONFe;
   FCodigoEmpresa:= prsCodigoEmpresa;
   FCNPJCPFEmp:=prsCNPJCPFEmp;
   FSeqNotaFiscal:= prsSeqNotaFiscal;
   FSerie:= prsSerie;
   FEspecie:= prsEspecie;
   FDataEmissao:=prdDataEmissao;
   FEmailArquivoXML := prsEmailArquivoXML;
   FSenhaMaster := prbSenhaMaster;
   FPastaXML := gsPath+ 'Enviar\NFe\Individuais\'+FCNPJCPFEmp+'\'+FormatDatetime('MM-YYYY', date);
end;

constructor TControllerDocumentoFiscalEletronico.Create(proDAONFe : IDAONFe;
                                                        prsCodigoEmpresa: string;
                                                        prsCNPJCPFEmp:string);
begin
   pvoDAONfe := proDAONFe;
   FCodigoEmpresa:= prsCodigoEmpresa;
   FCNPJCPFEmp:=prsCNPJCPFEmp;
   FPastaXML := gsPath+ 'Enviar\NFe\Individuais\'+FCNPJCPFEmp+'\'+FormatDatetime('MM-YYYY', date);
end;

destructor TControllerDocumentoFiscalEletronico.Destroy;
begin
  //if FServicoNFC <> nil then FServicoNFC.free;
  //if pvoDAONfe <> nil then pvoDAONfe.free;
end;

procedure TControllerDocumentoFiscalEletronico.GerarNFC( proNotaFiscal: TNotaFiscal ; var prbContingencia:boolean );
var
  lsErros    : string;
  lbContingencia : boolean;
begin
   try
     //prepara um serviço que implmenta o ACBR
     FServicoNFC := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( proNotaFiscal, FPastaXML, true );
     //Se não houver conexao com a internet ou o serviço da sefaz não responder gerar NFC em modo contingencia
     lbContingencia := (not VerificaConexaoInternet) or (prbContingencia);
     if lbContingencia then
     begin
        if FDataHoraContingencia = 0 then
           FDataHoraContingencia:= now;
        FServicoNFC.ConfigurarContingencia(FDataHoraContingencia);
     end
     else
     begin
        FDataHoraContingencia := 0;
     end;
     //Geração do XML na pasta Enviar
     try
        FServicoNFC.Gerar(lsErros);

        if lsErros <> '' then
           raise Exception.create('Problema(s) na Pre-Validação: '+ lsErros );

        if ( not lbContingencia ) then//Só envia a nota se não estiver em contigencia, ou seja, on-line.
        begin//Enviar a Nota Fiscal
           if Enviar( proNotaFiscal ) then
              ImprimirDanfe( proNotaFiscal );
           if ( FEmailArquivoXML <> '' ) then
           begin
             FServicoNFC.EnviarEmailNF( FEmailArquivoXML );
           end;
        end
        else
        begin
           pvoDAONfe.AlterarNFE_Status( FCodigoEmpresa,
                                        FSeqNotaFiscal,
                                        FEspecie,
                                        FSerie,
                                        '',
                                        '',
                                        FServicoNFC.GetChaveAcesso,//<<Captura a chave de acesso gerado pelo ACBR
                                        'G',
                                        '9');
        end;
     Except
        On E: Exception Do
        Begin
           CaixaMensagem( E.message, ctAviso, [cbOk], 0 );
        end;
     end;
  finally
     //if FServicoNFC <> nil then FServicoNFC.free;
  end;
end;

function TControllerDocumentoFiscalEletronico.Enviar( proNotaFiscal : TNotaFiscal):boolean;
var
  lbContingencia:boolean;
begin
  result:= false;
  if Enviar( FSeqNotaFiscal, FESPECIE, FSERIE, lbContingencia )then
  begin
     result:= true;
     if lbContingencia then
     begin
       FServicoNFC.free;
       GerarNFC(proNotaFiscal, lbContingencia );
     end;
   end;
end;

function TControllerDocumentoFiscalEletronico.Enviar( prsSeqNotaFiscal,
                                                      prsEspecie,
                                                      prsSerie: string;
                                                      var prbContingencia:boolean ):boolean;
var
  Retorno    : TProcNFe;
  lsErros    : string;
  lsArquivoXML: string;
begin
   result := false;
    try
       if FServicoNFC.Enviar( pvoDAONfe.GetSeqLoteNFe(FCodigoEmpresa) ) then
       begin
          result := true;
          //Obter o retorno da Nota
          Retorno  := FServicoNFC.GetRetorno;
          //Enviar email
          pvoDAONfe.AlterarNFE_Status( FCodigoEmpresa,
                                      prsSeqNotaFiscal,
                                      prsEspecie,
                                      prsSerie,
                                      Retorno.nProt,
                                      datetimetostr(Retorno.dhRecbto),
                                      FServicoNFC.GetChaveAcesso, //<<Captura a chave de acesso gerado pelo ACBR
                                      '',
                                      ifthen( prsEspecie = 'NFC','X' ,'F') );
       end;
    Except
       On E: Exception Do
       Begin
         if ( Pos('Inativo ou Inoperante', E.message) > 0 )  or  ( Pos('Requisição não enviada', E.message) > 0 ) then
         begin
            //Apaga o XML gerado e retornará True para gerar o XML em modo contingencia (9)
            lsArquivoXML:= FServicoNFC.GetFileNameXML(FServicoNFC.GetChaveAcesso);
            if FileExists( lsArquivoXML )  then
               DeleteFile( Pchar( lsArquivoXML ) );
            prbContingencia:= true;
            result := true;
         end
         else
         begin
           Retorno  := FServicoNFC.GetRetorno;
           if ( Pos('Rejeicao', E.message) > 0 ) or ( Not ( Retorno.cStat in [100, 150]) ) then
           begin
              Inutilizar( prsSeqNotaFiscal, prsSeqNotaFiscal, prsEspecie, prsSerie, E.message );
           end
           else
              CaixaMensagem( E.message, ctAviso, [cbOk], 0 );
         end;
       end;
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

         lsAnoMes := FormatDatetime('mm-yyyy', FDataEmissao );
         lsChaveNfe := FServicoNFC.GetChaveAcesso;
         lsChaveNfe := lsChaveNfe + Modulo11( lsChaveNfe );
         lsCNPJ := FCNPJCPFEmp;

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
         If FSenhaMaster Then
            ACBrNFcDANFEFR.MostrarPreview := True
         else
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

function TControllerDocumentoFiscalEletronico.Inutilizar(prsSeqnotafiscal1,
                                                         prsSeqnotafiscal2,
                                                         prsEspecie,
                                                         prsSerie,
                                                         Mensagem: string;
                                                         UpdateStatus : boolean = true):boolean;
var
   Retorno    : TProcNFe;
begin
   Try//Inutilizar a nota fiscal
      if FServicoNFC = nil then      
         FServicoNFC   := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( nil , FPastaXML, TRUE );
         
      if FServicoNFC.Inutilizar( FCNPJCPFEmp,
                                 ifthen( prsEspecie = 'NFC','65' ,'55'),
                                 prsSerie,
                                 inttostr( yearOf(Date)),
                                 prsSeqnotafiscal1,
                                 prsSeqnotafiscal2,
                                 Mensagem) then
       begin
          result := true;
          if UpdateStatus then
          begin
            Retorno  := FServicoNFC.GetRetorno;

            pvoDAONfe.AlterarNFE_Status( FCodigoEmpresa,
            prsSeqNotaFiscal1,
            prsEspecie,
            prsSerie,
            Retorno.nProt,
            FServicoNFC.GetChaveAcesso,//<<Captura a chave de acesso gerado pelo ACBR
            '' ,
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
end;

function TControllerDocumentoFiscalEletronico.EnviarContingencia:boolean;
var
    lcdsNotasFiscaisContingencia : TDataSet;
    lsSequencia, lsEspecieNF , lsSerieNF, lsNFEChaveAcesso: String;
    lsArquivo : string;
    lbContingencia:boolean;
begin
  result := false;
  FServicoNFC    := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( nil , FPastaXML, false );
  lcdsNotasFiscaisContingencia := pvoDAONfe.RetornarNotasFiscaisContingencia(FCodigoEmpresa);
  with lcdsNotasFiscaisContingencia do
  begin
     first;
     //Inicilizar Data para todas as contingencia
     FDataEmissao     := FieldByName( 'Data_Cad' ).AsDateTime;
     while Not Eof do
     begin
        try
           try
             FSeqNotaFiscal   := FieldByName( 'SeqNotaFiscal' ).AsString;
             FEspecie         := FieldByName( 'Especie' ).AsString;
             FSerie           := FieldByName( 'Serie' ).AsString;

             lsNFEChaveAcesso := FieldByName( 'NFE_ChaveAcesso' ).AsString;
             lsArquivo        :=  FServicoNFC.GetFileNameXML(lsNFEChaveAcesso);
             if FileExists( lsArquivo ) then
                 FServicoNFC.LoadFromFile(lsArquivo)//Carregar o Arquivo XML
             else
                 Raise Exception.Create('Arquivo de Contingência não existe: '+ lsArquivo);
             result := Enviar( FSeqNotaFiscal, FEspecie, FSerie, lbContingencia );
           except
              On E: Exception Do
              Begin
                 CaixaMensagem('Erro: ' + E.Message, ctErro, [ cbOk ], 0);
              End;
           end;
        finally
           Next;
        end;
     end;
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
  FServicoNFC   := TFactoryServicoDocumentoFiscalEletronico.FactoryServicoNFC( nil , FPastaXML, TRUE );
  lsLoteNFe     := pvoDAONfe.GetSeqLoteNFe(FCodigoEmpresa);
  Justificativa := 'Cancelamento de Nota Fiscal Eletronica ao consumidor';
  try
     if FServicoNFC.Cancelar( prsSeqNotaFiscal,
                              prsEspecie,
                              prsSerie ,
                              prsChaveAcesso,
                              Justificativa,
                              prsProtocolo,
                              lsLoteNFe ) then
     begin
        pvoDAONfe.AlterarNFE_Status( FCodigoEmpresa,
                                      prsSeqNotaFiscal,
                                      prsEspecie,
                                      prsSerie,
                                      '',
                                      '',
                                      '',
                                      'C',
                                      'C');
     end;
   except
      On E: Exception Do
      Begin
         CaixaMensagem('Erro: ' + E.Message, ctErro, [ cbOk ], 0);
      End;
   end;
end;

procedure TControllerDocumentoFiscalEletronico.GerarNFE( proNotaFiscal: TNotaFiscal;
                                                         var prbContingencia:boolean );
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


