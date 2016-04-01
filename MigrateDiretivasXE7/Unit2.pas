unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, Vcl.Buttons,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, Data.DB, FireDAC.Comp.Client, System.threading,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet;
type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    chkSave: TCheckBox;
    lbcount: TLabel;
    Memo3: TMemo;
    Button4: TButton;
    Button5: TButton;
    lbReplaces: TLabel;
    FileListBox100: TFileListBox;
    FDQuery1: TFDQuery;
    procedure Button1Click(Sender: TObject);
    procedure FileListBox1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    procedure AjustarUses;
    procedure DBXpressToFireDAC(Arquivo: String;FileListBox:TFileListBox);
    { Private declarations }
  public
    { Public declarations }
    contador, countreplace :integer;
  end;

var
  Form2: TForm2;

const
diretorios: Array[0..11] of string = ( 'D:\Producao\PostoMultiEmpresa',
                                      'D:\Producao\PostoMultiEmpresa\DAO',
                                      'D:\Producao\PostoMultiEmpresa\Controller',
                                      'D:\Producao\PostoMultiEmpresa\Modelos',
                                      'D:\Producao\Compartilhados\Modelos',
                                      'D:\Producao\Compartilhados',
                                      'D:\Producao\Compartilhados\DAO',
                                      'D:\Producao\Compartilhados\Modelos',
                                      'D:\Producao\Compartilhados\Controller',
                                      'D:\Producao\PostoPDVMultiEmpresa',
                                      'D:\Producao\PostoPDVMultiEmpresa\DAO',
                                      'D:\Producao\PostoPDVMultiEmpresa\Controller'
                                    );
const
Units : Array [0..26]of string= ( (*'{$IF CompilerVersion >= 23.0}',
                                 '{$IF CompilerVersion <= 23.0}'*)
                                ' DBXpress,',
                                ' DBTables,',
                                ' IBTable,',
                                ' IBQuery,',
                                ' IBDatabase,',
                                ' Sockets,',
                                ' DBIProcs,',
                                ' TeeProcs,',
                                ' DBChart,',
                                ' Chart,',
                                ' TeEngine,',
                                ' TeeNumericGauge,',
                                ' TeeLinearGauge,',
                                ' TeCanvas,',
                                ' TeeCircularGauge,',
                                ' xmldom,',
                                ' XMLIntf,',
                                ' msxmldom,',
                                ' XMLDoc,',
                                ' oxmldom,',
                                ' TeeEdit,',
                                ' Series,',
                                ' ColorGrd,',
                                ' TeeBrazil,',
                                ' TeePageNumTool,',
                                ' TeeTools,',
                                ' IBCustomDataSet' );




Diretivas : Array [0..26]of string= ((* '{$IF CompilerVersion >= 25.0}',
                                     '{$IF CompilerVersion <= 25.0}'*)
            '',
            '{$IF CompilerVersion < 23.0}DBTables,{$IFEND} ',
            '{$IF CompilerVersion < 23.0}IBTable, {$ELSE}IBX.IBTable,{$IFEND}',
            '{$IF CompilerVersion < 23.0}IBQuery, {$ELSE}IBX.IBQuery,{$IFEND}',
            '{$IF CompilerVersion < 23.0}IBDatabase, {$ELSE}IBX.IBDatabase,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Web.Win.Sockets,{$ELSE}Sockets,{$IFEND}',
            '{$IF CompilerVersion < 23.0}DBIProcs,{$IFEND} ',
            '{$IF CompilerVersion < 23.0}TeeProcs,{$ELSE}VCLTee.TeeProcs,{$IFEND}',
            '{$IF CompilerVersion < 23.0}DBChart,{$ELSE}VCLTee.DBChart,{$IFEND}',
            '{$IF CompilerVersion < 23.0}Chart,{$ELSE}VCLTee.Chart,{$IFEND}',
            '{$IF CompilerVersion < 23.0}TeEngine,{$ELSE}VCLTee.TeEngine,{$IFEND}',
            '{$IF CompilerVersion < 23.0}TeeNumericGauge,{$ELSE}VCLTee.TeeNumericGauge,{$IFEND}',
            '{$IF CompilerVersion < 23.0}TeeLinearGauge,{$ELSE}VCLTee.TeeLinearGauge,{$IFEND}',
            '{$IF CompilerVersion < 23.0}TeCanvas,{$ELSE}VCLTee.TeCanvas,{$IFEND}',
            '{$IF CompilerVersion < 23.0}TeeCircularGauge,{$ELSE}VCLTee.TeeCircularGauge,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Xml.xmldom,{$ELSE}xmldom,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Xml.XMLIntf,{$ELSE}XMLIntf,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Xml.win.msxmldom,{$ELSE}msxmldom,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Xml.XMLDoc,{$ELSE}XMLDoc,{$IFEND}',
            '{$IF CompilerVersion < 23.0}oxmldom,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}VCLTee.TeeEdit,{$ELSE} TeeEdit,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}VCLTee.Series,{$ELSE} Series,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}Vcl.ColorGrd,{$ELSE} ColorGrd,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}VCLTee.TeeBrazil,{$ELSE} TeeBrazil,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}VCLTee.TeePageNumTool,{$ELSE} TeePageNumTool,{$IFEND}',
            '{$IF CompilerVersion >= 23.0}VCLTee.TeeTools,{$ELSE}TeeTools,{$IFEND}',
            '{$IF CompilerVersion < 23.0}IBCustomDataSet,{$ELSE}IBX.IBCustomDataSet,{$IFEND}' );

ConstDBXpress : Array [0..53]of string = ('TSQLConnection', 'TSQLQuery','TSQLDataSet',
                                         'TSQLCommand','TSQLStoredProc','SqlExpr,',
                                         'SqlExpr;', 'SqlExpr ;',
                                         '.SQLConnection', 'TParams', 'TParam', 'PrepareStatement',
                                         'TSimpleDataSet', 'DataSet.CommandText','TSQLTransactionDesc',
                                         'dtm1.dbxPrincipal.GetTableNames( chkTabelas.Items, False )',
                                         'dtm1.dbxPrincipal.GetTableNames( chkTabelas.Items, False )',
                                         'dtm1.dbxPrincipal.GetTableNames( lllstTabelas, False )',
                                         'dtm1.dbxPrincipal.GetTableNames( llstTemp, False )',
                                         'dtm1.dbxPrincipal.GetTableNames( llstTemp, False )',
                                         'dtm1.dbxPrincipal.GetTableNames( llstTemporarias, False )',
                                         'fSQLConnection.GetTableNames( llstTemp, False )',
                                         'fSQLConnection.GetTableNames( llstExtras, False )',
                                         'Fconection.GetTableNames( List )',
                                         'pSqlConnectionMRT.GetTableNames( llstTabelas, False )',
                                         'pSqlConnection.GetTableNames( llstTabelas, False )',
                                        'trdNrTransacao.IsolationLevel := xilREADCOMMITTED;',
                                        'dtm1.dbxPrincipal.BeginTransaction( TDBXIsolations.ReadCommitted )',
                                        'dtm1.dbxPrincipal.RollBackFreeAndNil( lDBXTransaction )',
                                        'loDBXTransaction := gConexao.Conection.BeginTransaction( TDBXIsolations.ReadCommitted )',
                                        'lDBXTransaction := dtm1.dbxPrincipal.BeginTransaction( TDBXIsolations.ReadCommitted )',
                                        'lDBXTransaction := FConexao.Conection.BeginTransaction( TDBXIsolations.ReadCommitted )',
                                        'FConexao.Conection.StartTransaction( trdNrTransacao )',
                                        'dtm1.dbxPrincipal.CommitFreeAndNil( lDBXTransaction )',
                                        'gConexao.Conection.CommitFreeAndNil( loDBXTransaction )',
                                        'FConexao.Conection.CommitFreeAndNil( lDBXTransaction )',
                                        'dtm1.dbxPrincipal.Commit( trdNrTransacao )',
                                        'FConexao.Conection.Commit( trdNrTransacao )',
                                        'FConexao.Conection.RollBack( trdNrTransacao )',
                                        'FConexao.Conection.RollbackFreeAndNil( lDBXTransaction )',
                                        'trdNrTransacao.TransactionID  := Random( 255 )',
                                        'If trdNrTransacao.TransactionID > 0 Then',
                                        'dtm1.dbxPrincipal.StartTransaction( trdNrTransacao )',
                                        'dtm1.dbxPrincipal.RollBack( trdNrTransacao )',
                                        'var ptrdNrTransacao: TTransactionDesc',
                                        'ptrdNrTransacao.TransactionID  := Random( 255 )',
                                        'If ptrdNrTransacao.TransactionID > 0 Then',
                                        'proConnection.RollBack( ptrdNrTransacao )',
                                        'proConnection.StartTransaction( ptrdNrTransacao)',
                                        'gConexao.Conection.StartTransaction( trdNrTransacao )',
                                        'gConexao.Conection.RollBack( trdNrTransacao )',
                                        'gConexao.Conection.Commit( trdNrTransacao )',
                                        'ParamCheck',
                                        'TTransactionDesc'

                                          );

ConstFireDac : Array [0..53]of string= ( 'TFDConnection','TFDQuery' ,'TFDDataSet' ,'TFDCommand' , 'TFDStoredProc',
                                        'FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, '+
                                        'FireDAC.Stan.Param, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, '+
                                        'FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, '+
                                        'FireDAC.Comp.Client,'
                                        ,
                                        'FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, '+
                                        'FireDAC.Stan.Param, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, '+
                                        'FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, '+
                                        'FireDAC.Comp.Client;'
                                        ,
                                        'FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, '+
                                        'FireDAC.Stan.Param, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, '+
                                        'FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, '+
                                        'FireDAC.Comp.Client;',
                                        '.Connection',
                                        'TFDParams' , 'TFDParam', 'Prepared', 'TFDQuery', 'SQL.Text','TFDTransaction',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', chkTabelas.Items,[], [tkTable] )',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', chkTabelas.Items,[], [tkTable] )',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', lllstTabelas,[], [tkTable] )',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', llstTemp,[], [tkTable] )',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', llstTemp,[], [tkTable] )',
                                        'dtm1.dbxPrincipal.GetTableNames('''','''','''', llstTemporarias,[], [tkTable] )',
                                        'fSQLConnection.GetTableNames('''','''','''', llstTemp,[], [tkTable])',
                                        'fSQLConnection.GetTableNames('''','''','''', llstExtras,[], [tkTable] )',
                                        'Fconection.GetTableNames('''','''','''', List )',
                                        'pSqlConnectionMRT.GetTableNames('''','''','''', llstTabelas,[], [tkTable])',
                                        'pSqlConnection.GetTableNames('''','''','''', llstTabelas,[], [tkTable] )',

                                        '//trdNrTransacao.IsolationLevel := xilREADCOMMITTED;',
                                        'dtm1.dbxPrincipal.Transactions.StartTransaction',
                                        'dtm1.dbxPrincipal.Transactions.Rollback',
                                        'gConexao.Conection.Transactions.StartTransaction',
                                        'dtm1.dbxPrincipal.Transactions.StartTransaction',
                                        'FConexao.Conection.Transactions.StartTransaction',
                                        'FConexao.Conection.Transactions.StartTransaction',
                                        'dtm1.dbxPrincipal.Transactions.Commit',
                                        'gConexao.Conection.Transactions.Commit',
                                        'FConexao.Conection.Transactions.Commit',
                                        'dtm1.dbxPrincipal.Transactions.Commit',
                                        'FConexao.Conection.Transactions.Commit',
                                        'FConexao.Conection.Transactions.Rollback',
                                        'FConexao.Conection.Transactions.Rollback',
                                        '//trdNrTransacao.TransactionID  := Random( 255 )',
                                        '//If trdNrTransacao.TransactionID > 0 Then',

                                        'dtm1.dbxPrincipal.Transactions.StartTransaction',
                                        'dtm1.dbxPrincipal.Transactions.Rollback',
                                        '',
                                        '//ptrdNrTransacao.TransactionID  := Random( 255 );',
                                        '//If ptrdNrTransacao.TransactionID > 0 Then',
                                        'proConnection.Transactions.Rollback',
                                        'proConnection.Transactions.StartTransaction',
                                        'gConexao.Conection.Transactions.StartTransaction',
                                        'gConexao.Conection.Transactions.Rollback',
                                        'gConexao.Conection.Transactions.Commit',
                                        'Prepared',
                                        'TFDTransaction'
                                        );


implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  J:integer;
  I: Integer;
begin
  contador:= 0;
  for I := 0 to 0 do //length(diretorios)-1 do
  begin
    FileListBox100.Directory:= diretorios[i];
    for j := 0 to FileListBox100.Count -1 do
    begin
      FileListBox100.ItemIndex := J;
      Memo1.Lines.LoadFromFile(FileListBox100.FileName);
      application.ProcessMessages;
      AjustarUses;
    end;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  tasks: array of ITask;
  cont, I :integer;
  J:integer;
begin
  FileListBox100.mask:= '*.dfm';
  for I := 0 to length(diretorios)-1 do
  begin
    FileListBox100.Directory:= diretorios[i];
    for j := 0 to FileListBox100.Count -1 do
    begin
      FileListBox100.ItemIndex := J;
      Memo1.Lines.LoadFromFile(FileListBox100.FileName);
      DBXpressToFireDAC(Memo1.text,FileListBox100);
    end;
  end;

  FileListBox100.mask:= '*.pas';
  for I := 0 to length(diretorios)-1 do
  begin
    FileListBox100.Directory:= diretorios[i];
    for j := 0 to FileListBox100.Count -1 do
    begin
      FileListBox100.ItemIndex := J;
      Memo1.Lines.LoadFromFile(FileListBox100.FileName);
      DBXpressToFireDAC(Memo1.text,FileListBox100);
    end;
  end;

end;

procedure TForm2.DBXpressToFireDAC(Arquivo: string;FileListBox:TFileListBox);
var
  texto: string;
  textoReplace: string;
  replace: Boolean;
  I : Integer;
  ArquivoReplace: TStringList;
begin
  ArquivoReplace:= TStringList.create;

  Texto := copy(Arquivo, pos('interface', Arquivo),
                            length( Arquivo) );
  textoReplace := Texto;
  replace := false;
  for I := 0 to length(ConstDBXpress) - 1 do
  begin
    if Pos( upperCase(ConstDBXpress[i]), upperCase(textoReplace) ) > 0 then
    begin
      textoReplace := StringReplace(textoReplace, ConstDBXpress[i], ConstFireDac[i], [rfReplaceAll,rfIgnoreCase]);
      replace := true;
      inc(countreplace);
      lbReplaces.Caption:= inttostr( countreplace );
    end;
  end;
  if replace then
  begin
    ArquivoReplace.Add(  StringReplace(Arquivo, Texto, textoReplace, [rfReplaceAll,rfIgnoreCase]) );
    if chkSave.Checked then
       ArquivoReplace.SaveToFile(FileListBox.FileName);
    inc(contador);
    lbcount.Caption:= inttostr( contador );
    lbcount.Repaint;
  end;

  ArquivoReplace.free;
end;

procedure TForm2.FileListBox1Click(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile(FileListBox100.FileName);
end;

procedure TForm2.AjustarUses;
var
  texto: string;
  textoReplace: string;
  replace: Boolean;
  I : Integer;
begin
  Texto := copy(Memo1.text, pos('uses', Memo1.text) + 4,
                            pos('type', Memo1.text)-4 -
                            pos('uses', Memo1.text) + 4 );
  textoReplace := Texto;
  replace := false;
  for I := 0 to length(Units) - 1 do
  begin
    if Pos( upperCase(Units[i]), upperCase(textoReplace) ) > 0 then
    begin
      textoReplace := StringReplace(textoReplace, Units[i], Diretivas[i], [rfReplaceAll,rfIgnoreCase]);
      replace := true;
    end;
  end;
  if replace then
  begin
    Memo2.text := StringReplace(Memo1.text, Texto, textoReplace, [rfReplaceAll,rfIgnoreCase]);
    if chkSave.Checked then
       Memo2.Lines.SaveToFile(FileListBox100.FileName);
    inc(contador);
    lbcount.Caption:= inttostr( contador );
    lbcount.Repaint;
  end;
end;


end.

