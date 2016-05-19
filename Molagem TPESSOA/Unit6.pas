unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.StdCtrls;

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

  //Esta é classe Super todos Herdam direta ou indiretamente de TEntidade
  TEntidade = class
  private
    FId: integer;
  public
    property Id: integer read FId write FId;
  end;

  //Este é o objeto de valor que representara o endereço dos respectivos Esteriotipo!
  TEndereco = class(TEntidade)
  private
    Frua: string;
  public
     property  Rua: string read Frua write Frua;
     //property Bairro
     //property Cidade
     //....
  end;

  //Esta é a classe que representará o Esteriotipo da Pessoa, pode se chamado
  //tambem de resposabilidade , Titulo, Papel..
  TEsteriotipo = class(TEntidade)
  private
    FEndereco: TEndereco;
    FPessoaFisica: TPessoaFisica;
    FPessoaJuridica: TPessoaJuridica;
  public
    property  Endereco:TEndereco read FEndereco write FEndereco;
   //Preferi referenciar em TEsteriotipo a composicao PessoaFisica ou PessoaJuridica,
   //mas poderia ser referenciado em cada esteriotipo descendente se necessário.
    property PessoaFisica: TPessoaFisica      read FPessoaFisica write FPessoaFisica;
    property PessoaJuridica: TPessoaJuridica  read FPessoaJuridica write FPessoaJuridica;
    constructor Create;
  end;

  //A classe Base de todas as pessoas representada no seu sistema
  TPessoa  = class (TEntidade)
  private
    FCliente : TCliente;
    FFornecedor: TFornecedor;
    FFabricante: TFabricante;
  public
    // Uma pessoa pode ser, se necessário, uma coposicao de Cliente, Fornecedor , fabricante...
    property Cliente: TCliente  read FCliente write FCliente;
    property Fornecedor:TFornecedor  read FFornecedor write FFornecedor;
    property Fabricante: TFabricante read FFabricante write FFabricante;
    constructor Create;
  end;

  //aqui estará os atributos inerentes a pessoa fisica
  TPessoaFisica = class(TPessoa)
  private
    FCPF: string;
  public
    property CPF: string read FCPF write FCPF;
  end;
  //aqui estará os atributos inerentes a pessoa juridica
  TPessoaJuridica = class(TPessoa)
  private
    FCNPJ: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
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
  end;

  TFabricante =class(TEsteriotipo)
  private
    FValidade: TDatetime;
  public
    property Validade: TDatetime read FValidade write FValidade;
  end;

  var
   Form6: TForm6;

implementation

{$R *.dfm}


procedure TForm6.Button1Click(Sender: TObject);
var
  Pessoa: TPessoa;
begin
  Pessoa:=TPessoa.Create;

  Pessoa.Cliente.idade:= 40;
  Pessoa.Cliente.PessoaFisica.CPF:='000000000000';
  Pessoa.Cliente.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Cliente.Endereco.rua:='xxxxxxxxxxxxxxx';

  Pessoa.Fornecedor.Segmento:='Informatica';
  Pessoa.Fornecedor.PessoaFisica.CPF:='000000000000';
  Pessoa.Fornecedor.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Fornecedor.Endereco.rua:='yyyyyyyyyyy';


  Pessoa.Fabricante.Validade:= now + 360;
  Pessoa.Fabricante.PessoaJuridica.CNPJ:='000000000000';
  Pessoa.Fabricante.Endereco.rua:='zzzzzzzzzzzzzz';

  Pessoa.Free;

end;


{ TPessoa }

constructor TPessoa.Create;
begin
  FCliente := TCliente.Create;
  FFornecedor:= TFornecedor.Create;
  FFabricante:= TFabricante.Create;
end;

{ TEsteriotipo }

constructor TEsteriotipo.Create;
begin

end;

end.

