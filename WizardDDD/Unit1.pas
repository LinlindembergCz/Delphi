unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit,
  Vcl.FileCtrl, Vcl.Buttons, strutils, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  Data.DB, Data.SqlExpr;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Button2: TButton;
    MemoEntity: TMemo;
    MemoRepository: TMemo;
    MemoService: TMemo;
    MemoController: TMemo;
    ValueListEditor1: TValueListEditor;
    Panel2: TPanel;
    edtEntity: TEdit;
    edtField: TEdit;
    cboType: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    MemoInterfaceRepository: TMemo;
    MemoInterfaceService: TMemo;
    SaveDialog1: TSaveDialog;
    edtProject: TEdit;
    Label4: TLabel;
    FileOpenDialog1: TFileOpenDialog;
    SpeedButton1: TSpeedButton;
    edtLength: TEdit;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    chkEntity: TCheckBox;
    chkRepository: TCheckBox;
    chkService: TCheckBox;
    chkController: TCheckBox;
    chkNotNull: TCheckBox;
    chkSave: TCheckBox;
    Button3: TButton;
    SQLConnection1: TSQLConnection;
    FDConnection1: TFDConnection;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function ExtractFileProject: string;
    procedure GenerateEntity(Entity:string);
    procedure GenerateInterfaceRepository(Entity:string);
    procedure GenerateRepository(Entity:string);
    procedure GenerateInterfaceService(Entity:string);
    procedure GenerateService(Entity:string);
    procedure GenerateController(Entity: string);
    procedure GenerateFactoryController(Entity: string;save:boolean = false);
    procedure GenerateFactoryEntity(Entity: string;save:boolean = false);
    procedure GenerateFactoryRepository(Entity: string;save:boolean = false);
    procedure GenerateFactoryService(Entity: string;save:boolean = false);
    procedure AddFileProject(FileName, path : string; Entity : string = '' );
    procedure RegisterEntity(Entity: string;save:boolean = false);
    procedure GenerateFactory(Arquivo, MarcacaoUses, DeclaracaoEntity, MarcacaoFactory,DeclaracaoFactory : string; save:boolean );
    procedure SaveController(Entity:string; save: boolean = false);
    procedure SaveService(Entity:string; save: boolean = false);
    procedure SaveRepository(Entity:string; save: boolean = false);
    procedure SaveEntity(Entity:string; save: boolean = false);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
   if (ValueListEditor1.RowCount > 0) and (edtProject.Text  <> '') then
   begin
      if chkEntity.Checked then
      begin
        GenerateEntity(edtEntity.Text);
        GenerateFactoryEntity(edtEntity.Text,chkSave.Checked);
      end;

      if chkRepository.Checked then
      begin
        GenerateInterfaceRepository(edtEntity.Text);
        GenerateRepository(edtEntity.Text);
        GenerateFactoryRepository(edtEntity.Text,chkSave.Checked);
      end;

      if chkService.Checked then
      begin
        GenerateInterfaceService(edtEntity.Text);
        GenerateService(edtEntity.Text);
        GenerateFactoryService(edtEntity.Text,chkSave.Checked);
      end;

      if chkController.Checked then
      begin
        GenerateController(edtEntity.Text);
        GenerateFactoryController(edtEntity.Text,chkSave.Checked);
      end;
   end;
   RegisterEntity(edtEntity.Text);
   showmessage('Operação finalizada!');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SaveEntity(edtEntity.Text,true);
  SaveRepository(edtEntity.Text,true);
  SaveService(edtEntity.Text,true);
  SaveController(edtEntity.Text,true);

  GenerateFactoryEntity(edtEntity.Text,true);
  GenerateFactoryRepository(edtEntity.Text,true);
  GenerateFactoryController(edtEntity.Text,true);
  GenerateFactoryService(edtEntity.Text,true);

end;

function TForm1.ExtractFileProject:string;
begin
  result := ExtractFilePath( edtProject.Text );
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  notnull: string;
begin
  if (edtField.Text <> '') and (cboType.Text <> '') then
  begin
    notnull:= ifthen( chkNotNull.Checked,'not null','');
    if cboType.Text = 'TString' then
       ValueListEditor1.InsertRow(edtField.Text, cboType.Text+'('+edtLength.text+')'+' '+ notnull, true )
    else
       ValueListEditor1.InsertRow(edtField.Text, cboType.Text +' '+ notnull, true  );
    edtField.Text  := '';
  end;
end;

procedure TForm1.AddFileProject( FileName, path : string; Entity : string = '');
var
  arquivo:TStringList;
begin
  arquivo:=TStringList.Create;
  Try
    arquivo.LoadFromFile( edtProject.text );
    if Pos( FileName+' in '+ quotedstr( path ), arquivo.Text ) = 0  then
    begin
      arquivo.Text := stringReplace( arquivo.Text, '//unit in Caminho.pas' ,
                                                  '  ,'+FileName+' in '+ quotedstr( path )+
                                                  #13+
                                                  '//unit in Caminho.pas'  ,[rfReplaceAll] );
      if Entity <> '' then
         arquivo.Text := stringReplace( arquivo.Text, '(*Entity*)',', '+Entity+' (*Entity*)',[rfReplaceAll] );
      arquivo.SaveToFile( edtProject.text );
    end;
    //unit in Caminho.pas
    //unitRepository in Caminho.pas
    //unitInterfaceRepository in Caminho.pas
    //unitInterfaceService in Caminho.pas
    //unitService in Caminho.pas
    //unitController in Caminho.pas
  finally
    arquivo.Free;
  end;
end;

procedure TForm1.GenerateEntity(Entity:string);
var
  I: Integer;
  properties: string;
  types: string;
begin
  MemoEntity.lines.clear;
  MemoEntity.lines.Add('unit Class' + Entity+';');
  MemoEntity.lines.Add('');
  MemoEntity.lines.Add('interface');
  MemoEntity.lines.Add('');
  MemoEntity.lines.Add('uses');
  MemoEntity.lines.Add(' System.Classes, Dialogs, SysUtils, EntityBase, EntityTypes, Atributies;');
  MemoEntity.lines.Add('');
  MemoEntity.lines.Add('type');
  MemoEntity.lines.Add('  [EntityTable(' + quotedstr( Entity ) + ')]');
  MemoEntity.lines.Add('  ' + Entity + ' = class( TEntityBase )');
  MemoEntity.lines.Add('  private');
  for I := 1 to ValueListEditor1.RowCount -1 do
  begin
    types := ValueListEditor1.Values[ValueListEditor1.Keys[i]];
    if  pos( 'TString',types) > -0 then
      MemoEntity.lines.Add('    F' + ValueListEditor1.Keys[i] + ': ' + copy( types, 0 ,Pos('(',types)-1 ) + ';')
    else
    if Pos('not null',types) > 0 then
       MemoEntity.lines.Add('    F' + ValueListEditor1.Keys[i] + ': ' + copy( types, 0 ,Pos(' ',types)-1 ) + ';')
    else
       MemoEntity.lines.Add('    F' + ValueListEditor1.Keys[i] + ': ' + types + ';')

  end;
  MemoEntity.lines.Add('  public');
  for I := 1 to ValueListEditor1.RowCount - 1 do
  begin
    types := ValueListEditor1.Values[ValueListEditor1.Keys[i]];
    if  pos( 'TString',types) > -0 then
    begin
        properties := '[EntityField('+ quotedstr( ValueListEditor1.Keys[i] ) +','+
                                       quotedstr('varchar'+ copy(types,Pos('(',types),length(types)))+','+  'true'+')]';
        MemoEntity.lines.Add('     ' + properties);
        properties :=  'property ' + ValueListEditor1.Keys[i] + ': ' +
                                         copy( types, 0 ,Pos('(',types)-1 ) +
                                        ' read F' + ValueListEditor1.Keys[i] +
                                        ' write F' + ValueListEditor1.Keys[i] + ';';
        MemoEntity.lines.Add('     ' + properties);
    end
    else
    if  pos( 'TInteger',types) > -0 then
    begin
       properties := '[EntityField('+ quotedstr( ValueListEditor1.Keys[i] ) +','+
        quotedstr('Integer'+' '+ifthen( pos( 'not null',types) > -0,'not null',''))  +','+ 'true' +')]';
       MemoEntity.lines.Add('     ' + properties);
       properties := 'property ' + ValueListEditor1.Keys[i] + ': ' + 'TInteger' +
                                  ' read F' + ValueListEditor1.Keys[i] +
                                  ' write F' + ValueListEditor1.Keys[i] + ';';
       MemoEntity.lines.Add('     ' + properties);
    end
    else
    if  pos( 'TFloat',types) > -0 then
    begin
       properties := '[EntityField('+ quotedstr( ValueListEditor1.Keys[i] ) +','+
       quotedstr(' NUMERIC(10,3)'+' '+ifthen( pos( 'not null',types) > -0,'not null',''))+','+ 'true' +')]';
       MemoEntity.lines.Add('     ' + properties);
       properties := 'property ' + ValueListEditor1.Keys[i] + ': ' + 'TFloat' +
                                  ' read F' + ValueListEditor1.Keys[i] +
                                  ' write F' + ValueListEditor1.Keys[i] + ';';
       MemoEntity.lines.Add('     ' + properties);
    end;
  end;
  MemoEntity.lines.Add('  end;');
  MemoEntity.lines.Add('');
  MemoEntity.lines.Add('implementation');
  MemoEntity.lines.Add('');

  MemoEntity.lines.Add('initialization RegisterClass('+Entity+');');
  MemoEntity.lines.Add('finalization UnRegisterClass('+Entity+');');

  MemoEntity.lines.Add('end.');
  SaveEntity(Entity,chkSave.Checked);

end;

procedure TForm1.GenerateInterfaceRepository(Entity:string);
begin
  MemoInterfaceRepository.lines.clear;
  MemoInterfaceRepository.lines.Add('unit InterfaceRepository' + Entity + ';');
  MemoInterfaceRepository.lines.Add('');
  MemoInterfaceRepository.lines.Add('interface');
  MemoInterfaceRepository.lines.Add('');
  MemoInterfaceRepository.lines.Add('uses');
  MemoInterfaceRepository.lines.Add('InterfaceRepository, Class' + Entity + ';');
  MemoInterfaceRepository.lines.Add('');
  MemoInterfaceRepository.lines.Add('type');
  MemoInterfaceRepository.lines.Add('  IRepository' + Entity + ' = interface(IRepositoryBase)');
  MemoInterfaceRepository.lines.Add('    function GetEntity: ' + Entity + ';');
  MemoInterfaceRepository.lines.Add('  end;');
  MemoInterfaceRepository.lines.Add('');
  MemoInterfaceRepository.lines.Add('implementation');
  MemoInterfaceRepository.lines.Add('');
  MemoInterfaceRepository.lines.Add('end.');
//SaveRepository(Entity);

end;

procedure TForm1.GenerateRepository(Entity:string);
begin
  MemoRepository.lines.clear;
  MemoRepository.lines.Add('unit Repository' + Entity + ';');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('interface');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('uses');
  MemoRepository.lines.Add('System.Classes, Repository, class' + Entity + ', RepositoryBase, InterfaceRepository'+Entity+', InterfaceRepository,  Context, EntityBase;');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('type');
  MemoRepository.lines.Add('  TRepository' + Entity + ' = class(TRepositoryBase ,IRepository' + Entity + ')');
  MemoRepository.lines.Add('  private');
  MemoRepository.lines.Add('    _Repository' + Entity + ':IRepository<' + Entity + '>;');
  MemoRepository.lines.Add('  public');
  MemoRepository.lines.Add('    Constructor Create(dbContext:TContext);');
  MemoRepository.lines.Add('    function GetEntity: ' + Entity + ';');
  MemoRepository.lines.Add('  end;');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('implementation');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('{ TRepository' + Entity + ' }');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('constructor TRepository' + Entity + '.Create(dbContext:TContext);');
  MemoRepository.lines.Add('begin');
  MemoRepository.lines.Add('  _Repository' + Entity + ' := TRepository<' + Entity + '>.create(dbContext) ;');
  MemoRepository.lines.Add('  _RepositoryBase    := _Repository' + Entity + ' As IRepository<TEntityBase>;');
  MemoRepository.lines.Add('  //_RepositoryBase    :=  TRepository<' + Entity + '>.create(dbContext) As IRepository<TEntityBase>;');
  MemoRepository.lines.Add('end;');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('function TRepository' + Entity + '.GetEntity: ' + Entity + ';');
  MemoRepository.lines.Add('begin');
  MemoRepository.lines.Add('  result:= _Repository' + Entity + '.Entity;');
  MemoRepository.lines.Add('end;');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('initialization RegisterClass('+'TRepository' +Entity+');');
  MemoRepository.lines.Add('finalization UnRegisterClass('+'TRepository' +Entity+');');
  MemoRepository.lines.Add('');
  MemoRepository.lines.Add('End.');


  SaveRepository(Entity,chkSave.Checked);
end;

procedure TForm1.GenerateInterfaceService(Entity:string);
begin
  MemoInterfaceService.lines.clear;
  MemoInterfaceService.lines.Add('unit InterfaceService' +Entity + ';');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add('interface');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add('uses');
  MemoInterfaceService.lines.Add('InterfaceService;');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add('type');
  MemoInterfaceService.lines.Add('  IService' +Entity + ' = interface(IServiceBase)');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add(' end;');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add('implementation');
  MemoInterfaceService.lines.Add('');
  MemoInterfaceService.lines.Add('end.');
  //SaveService(Entity);
end;

procedure TForm1.GenerateService(Entity:string);
begin
  MemoService.lines.clear;
  MemoService.lines.Add('unit Service' + Entity + ';');
  MemoService.lines.Add('');
  MemoService.lines.Add('interface');
  MemoService.lines.Add('');
  MemoService.lines.Add('uses');
  MemoService.lines.Add('System.Classes, ServiceBase, InterfaceService' + Entity + ', EnumEntity;');
  MemoService.lines.Add('');
  MemoService.lines.Add('type');
  MemoService.lines.Add('  TService' + Entity + '=class( TServiceBase , IService' + Entity + ')');
  MemoService.lines.Add('  public');
  MemoService.lines.Add('');
  MemoService.lines.Add('  end;');
  MemoService.lines.Add('');
  MemoService.lines.Add('implementation');
  MemoService.lines.Add('');
  MemoService.lines.Add('{ Service' + Entity + ' }');
  MemoService.lines.Add('');
  MemoService.lines.Add('initialization RegisterClass('+'TService' +Entity+');');
  MemoService.lines.Add('finalization UnRegisterClass('+'TService' +Entity+');');
  MemoService.lines.Add('');
  MemoService.lines.Add('end.');
  SaveService(Entity,chkSave.Checked);
end;

procedure TForm1.GenerateController(Entity:string);
begin
  MemoController.lines.clear;
  MemoController.lines.Add('unit Controller'+Entity+';');
  MemoController.lines.Add('');
  MemoController.lines.Add('interface');
  MemoController.lines.Add('');
  MemoController.lines.Add('uses');
  MemoController.lines.Add('System.Classes, DB, DBClient, ControllerBase,  EnumEntity;');
  MemoController.lines.Add('');
  MemoController.lines.Add('type');
  MemoController.lines.Add('  TController'+Entity+' = class(TControllerBase)');
  MemoController.lines.Add('  public');
  MemoController.lines.Add('');
  MemoController.lines.Add('  end;');
  MemoController.lines.Add('');
  MemoController.lines.Add('implementation');
  MemoController.lines.Add('');
  MemoController.lines.Add('initialization RegisterClass('+'TController' +Entity+');');
  MemoController.lines.Add('finalization UnRegisterClass('+'TController' +Entity+');');
  MemoController.lines.Add('');
  MemoController.lines.Add('end.');
  SaveController(Entity,chkSave.Checked);
end;

procedure TForm1.GenerateFactory(Arquivo, MarcacaoUses, DeclaracaoEntity, MarcacaoFactory,DeclaracaoFactory : string; save:boolean );
var
  arquivoList: TStringList;
begin
  if save then
  begin
    try
      arquivoList:= TStringList.Create;
      arquivoList.LoadFromFile( ExtractFileProject +Arquivo );

      arquivoList.Text := stringreplace( arquivoList.text, MarcacaoUses,DeclaracaoEntity+MarcacaoUses,[rfReplaceAll]);

      arquivoList.Text := stringreplace( arquivoList.text,
                                     MarcacaoFactory,
                                     DeclaracaoFactory+#13+
                                     MarcacaoFactory,
                                     [rfReplaceAll]);
      arquivoList.SaveToFile( ExtractFileProject + Arquivo );
    finally
       arquivoList.Free;
    end;
  end;
end;

procedure TForm1.SaveController(Entity:string; save: boolean = false);
begin
  if save then
  begin
     SaveDialog1.InitialDir := ExtractFileProject+'UI\Controllers';
    SaveDialog1.FileName := 'Controller' + Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
      MemoController.Lines.SaveToFile(SaveDialog1.FileName);
    AddFileProject('Controller' + Entity, SaveDialog1.FileName);
  end;
end;

procedure TForm1.SaveService(Entity:string; save: boolean = false);
begin
  if save then
  begin
    SaveDialog1.InitialDir := ExtractFileProject+'Domain\IService';
    SaveDialog1.FileName := 'InterfaceService' +Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
    MemoInterfaceService.Lines.SaveToFile(SaveDialog1.FileName );

    AddFileProject('InterfaceService' +Entity, SaveDialog1.FileName );

    SaveDialog1.InitialDir := ExtractFileProject+'Service';
    SaveDialog1.FileName := 'Service' + Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
      MemoService.Lines.SaveToFile(SaveDialog1.FileName);
    AddFileProject('Service' + Entity, SaveDialog1.FileName);
  end;
end;

procedure TForm1.SaveRepository(Entity:string; save: boolean = false);
begin
  if save then
  begin
    SaveDialog1.InitialDir := ExtractFileProject+'Domain\IRepositories';
    SaveDialog1.FileName := 'InterfaceRepository' + Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
    MemoInterfaceRepository.Lines.SaveToFile(SaveDialog1.FileName );
    AddFileProject('InterfaceRepository' + Entity, SaveDialog1.FileName );

    SaveDialog1.InitialDir := ExtractFileProject+'Infra\Repositories';
    SaveDialog1.FileName := 'Repository' + Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
      MemoRepository.Lines.SaveToFile(SaveDialog1.FileName);
    AddFileProject('Repository' + Entity, SaveDialog1.FileName);
  end;
end;

procedure TForm1.SaveEntity(Entity:string; save: boolean = false);
begin
  if save then
  begin
    SaveDialog1.InitialDir := ExtractFileProject+'Domain\Entities';
    SaveDialog1.FileName := 'Class' + Entity;
    SaveDialog1.Execute;
    if SaveDialog1.FileName <> '' then
      MemoEntity.Lines.SaveToFile(SaveDialog1.FileName);
    AddFileProject('Class' + Entity, SaveDialog1.FileName, Entity);
  end;
end;

procedure TForm1.GenerateFactoryEntity( Entity: string;save:boolean = false);
begin
   GenerateFactory( 'Domain\Factories\FactoryEntity.pas',
                    '',
                    '',
                    '//tpEntity: result := TEntityBase( TAutoMapper.GetInstance( ''classEntity.TEntity'' )).Create;',
                    '    tp'+Entity+' : result := TEntityBase( TAutoMapper.GetInstance( '+ quotedstr( 'Class'+Entity+'.'+Entity) +' )).Create;',
                    save );
end;

procedure TForm1.GenerateFactoryRepository(Entity: string;save:boolean = false);
begin
   GenerateFactory( 'Domain\Factories\FactoryRepository.pas',
                    '',
                    '',
                    '//tpEntity: result:= RepositoryEntity.TRepositoryEntity;',
                    '    tp'+Entity+' : result:= '+ quotedstr( 'Repository' + Entity+'.TRepository'+Entity)+';',
                    save );
end;

procedure TForm1.GenerateFactoryService(Entity: string;save:boolean = false);
begin
   GenerateFactory( 'Domain\Factories\FactoryService.pas',
                    '',
                    '',
                    '//tpEntity: result:= ServiceEntity.TServiceEntity;',
                    '    tp'+Entity+' : result:= '+ quotedstr( 'Service' + Entity+'.TService'+Entity)+';',
                    save );
end;

procedure TForm1.GenerateFactoryController(Entity: string;save:boolean = false);
begin
   GenerateFactory( 'UI\Factories\FactoryController.pas',
                    '',
                    '',
                    '//tpEntity: result:= ControllerEntity.TControllerEntity;',
                     '    tp'+Entity+' : result:= '+ quotedstr( 'Controller' + Entity+'.TController'+Entity)+';',
                    save );
end;

procedure TForm1.RegisterEntity(Entity: string;save:boolean = false);
var
  arquivo:TStringList;
begin
  if save then
  begin
    try
      arquivo:=TStringList.Create;
      arquivo.LoadFromFile(ExtractFilepath(edtProject.text)+'EnumEntity.pas');
      if Pos( 'tp'+Entity, arquivo.text) = 0 then
      begin
        arquivo.text := stringreplace(  arquivo.text, 'tpEntidade',' tp'+Entity+' , tpEntidade',[rfReplaceAll] );
        arquivo.SaveToFile( ExtractFilepath(edtProject.text)+'EnumEntity.pas' );
      end;
    finally
      arquivo.Free;
    end;
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FileOpenDialog1.Execute;
  if FileOpenDialog1.FileName <> '' then
     edtProject.Text:= FileOpenDialog1.FileName;
end;

end.
