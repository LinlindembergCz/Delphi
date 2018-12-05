unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.StdCtrls, System.Generics.Collections;

type
  TForm6 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  //Pra não criar outros arquivos para cada classe usei essa tecnica bem legal do Delphi!
  TPessoaFisica = class;
  TPessoaJuridica = class;
  TCliente = class;
  TFornecedor = class;
  TFabricante = class;
  TEnderecoCobranca =class;
  TEnderecoEntrega =class;
  TMunicipio= class;
  TBairro= class;

  //Esta é classe Super todos Herdam direta ou indiretamente de TEntidade
  TEntidade = class
  private
    FId: integer;
  public
    property Id: integer read FId write FId;
  end;
  //A classe Base de todas as pessoas representada no seu sistema
  TPessoa  = class (TEntidade)
  private
    FCliente : TCliente;
    FFornecedor: TFornecedor;
    FFabricante: TFabricante;
  public
    constructor Create;virtual;
    destructor Destroy;override;
    // Uma pessoa pode ser, se necessário, uma coposicao de Cliente, Fornecedor , fabricante...
    property Cliente: TCliente  read FCliente write FCliente;
    property Fornecedor:TFornecedor  read FFornecedor write FFornecedor;
    property Fabricante: TFabricante read FFabricante write FFabricante;
  end;

  //Este é o objeto de valor que representara o endereço dos respectivos Esteriotipo!
  TUF =class
  private
    FDescricao: string;
    FMunicipio: TList<TMunicipio>;
  public
    property Descricao: string read FDescricao write FDescricao ;
    property Municipio: TList<TMunicipio> read FMunicipio write FMunicipio ;
  end;

  TMunicipio =class
  private
    FDescricao: string;
    FBairro:TList<TBairro>;
  public
    property Descricao: string  read FDescricao write FDescricao ;
    property Bairro: TList<TBairro> read FBairro write FBairro ;
  end;

  TBairro =class
  private
    FDescricao: string;
  public
    property  Descricao: string  read FDescricao write FDescricao ;
  end;

  TLocalizacao = class(TEntidade)
  private
    Frua: string;
    FNumero: string;
    FBairro: TBairro;
  public
     property Bairro: TBairro read FBairro write FBairro;
     property Rua: string read Frua write Frua;
     property Numero: string  read FNumero write FNumero;
  end;

  //aqui estará os atributos inerentes a pessoa fisica
  TPessoaFisica = class(TPessoa)
  private
    FCPF: string;
  public
    property CPF: string read FCPF write FCPF;
    constructor Create;override;
  end;

  //aqui estará os atributos inerentes a pessoa juridica
  TPessoaJuridica = class(TPessoa)
  private
    FCNPJ: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    constructor Create;override;
  end;

  //Esta é a classe que representará o Esteriotipo da Pessoa, pode se chamado
  //tambem de resposabilidade , Titulo, Papel..
  TEsteriotipo = class(TEntidade)
  private
    FLocalizacao: TLocalizacao;
    FEnderecosCobranca: TList<TEnderecoCobranca>;
    FEnderecosEntrega: TList<TEnderecoEntrega>;
    FPessoaFisica: TPessoaFisica;
    /// <link>association</link>
    FPessoaJuridica: TPessoaJuridica;
  public
    property Localizacao: TLocalizacao read FLocalizacao write FLocalizacao;
    property EnderecosCobranca: TList<TEnderecoCobranca> read FEnderecosCobranca write FEnderecosCobranca;
    property EnderecosEntrega: TList<TEnderecoEntrega> read FEnderecosEntrega write FEnderecosEntrega;
   //Preferi referenciar em TEsteriotipo a composicao PessoaFisica ou PessoaJuridica,
   //mas poderia ser referenciado em cada esteriotipo descendente se necessário.
    property PessoaFisica: TPessoaFisica      read FPessoaFisica write FPessoaFisica;
    property PessoaJuridica: TPessoaJuridica  read FPessoaJuridica write FPessoaJuridica;
    constructor Create;virtual;
    destructor Destroy;override;
  end;

  TEnderecoEntrega = class
  private
    FEsteriotipo: TEsteriotipo;
    FLocalizacao: TLocalizacao;
  public
    property  Esteriotipo: TEsteriotipo read FEsteriotipo write FEsteriotipo;
    property  Localizacao: TLocalizacao read FLocalizacao write FLocalizacao;
  end;

  TEnderecoCobranca = class
  private
    FEsteriotipo: TEsteriotipo;
    FLocalizacao: TLocalizacao;
  public
    property  Esteriotipo: TEsteriotipo read FEsteriotipo write FEsteriotipo;
    property  Localizacao: TLocalizacao read FLocalizacao write FLocalizacao;
  end;

  //Agora o pulo do GATO!
  TCliente = class(TEsteriotipo)
  private
    Fidade: integer;
  public
    property  idade:integer read Fidade write Fidade;
  end;

  TFornecedor = class(TEsteriotipo)
  private
    FSegmento: string;
  public
    property  Segmento:string read FSegmento write FSegmento;
    //constructor Create;
  end;

  TFabricante =class(TEsteriotipo)
  private
    FValidade: TDatetime;
  public
    property Validade: TDatetime read FValidade write FValidade;
    //constructor Create;
  end;

  var
   Form6: TForm6;

implementation

{$R *.dfm}


procedure TForm6.Button1Click(Sender: TObject);
var
  Pessoa: TPessoa;
  Cobranca: TEnderecoCobranca;
  Localizacao:TLocalizacao;
begin
  //Não precisarei fazer uses de cada classe aumentando o acoplamento.
  //Uma pessoa pode assumir os varios esteriotipos
  Pessoa:=TPessoa.Create;

  Pessoa.Cliente.idade:= 40;
  Pessoa.Cliente.PessoaFisica.CPF:='000000000000';
  Pessoa.Cliente.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Cliente.Localizacao.Rua:='xxxxxxxxxxxxxxx';
  Pessoa.Fornecedor.Segmento:='Informatica';
  Pessoa.Fornecedor.PessoaFisica.CPF:='000000000000';
  Pessoa.Fornecedor.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Fornecedor.Localizacao.Rua:='yyyyyyyyyyy';
  Pessoa.Fabricante.Validade:= now + 360;
  Pessoa.Fabricante.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Fabricante.Localizacao.Rua:='zzzzzzzzzzzzzz';

  Cobranca:= TEnderecoCobranca.Create;
  Localizacao:= TLocalizacao.Create;
  Localizacao.Rua:='YYYYYYYYYY';
  Cobranca.Localizacao := Localizacao;
  Cobranca.Esteriotipo      := Pessoa.Cliente;

  Pessoa.Cliente.EnderecosCobranca.Add( Cobranca );

  showmessage(
    'Endereco do Cliente:'+ Pessoa.Cliente.localizacao.Rua +#13+
    'Endereco de Cobranca do Cliente:'+ Pessoa.Cliente.EnderecosCobranca.Items[0].Localizacao.Rua );

  Pessoa.Free;

end;

{ TPessoa }

constructor TPessoa.Create;
begin
  FCliente    := TCliente.Create;
  FFornecedor := TFornecedor.Create;
  FFabricante := TFabricante.Create;
end;

destructor TPessoa.Destroy;
begin
  inherited;
  FCliente.free;
  FFornecedor.free;
  FFabricante.free;
end;

{ TEsteriotipo }

constructor TEsteriotipo.Create;
begin
  Localizacao:= TLocalizacao.create;
  EnderecosCobranca:= TList<TEnderecoCobranca>.create;
  EnderecosEntrega:= TList<TEnderecoEntrega>.create;
  PessoaFisica:= TPessoaFisica.create;
  PessoaJuridica:= TPessoaJuridica.create;
end;

destructor TEsteriotipo.Destroy;
begin
  inherited;
  Localizacao.free;
  EnderecosCobranca.free;
  EnderecosEntrega.free;
  FPessoaFisica.free;
  FPessoaJuridica.free;
end;

{ TPessoaFisica }

constructor TPessoaFisica.Create;
begin

end;

{ TPessoaJuridica }

constructor TPessoaJuridica.Create;
begin

end;

end.

