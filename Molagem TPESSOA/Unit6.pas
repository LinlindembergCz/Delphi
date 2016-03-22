unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm6 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TEntidade = class
  end;

  TPessoa  = class (TEntidade)
  private
    FId: integer;
    procedure SetId(const Value: integer);
  published
  public
    property Id:integer read FId write SetId;
  end;

  TPessoaFisica = class(TPessoa)
  private
    FCPF: string;
    procedure SetCPF(const Value: string);
  published
  public
    property CPF: string read FCPF write SetCPF;
  end;

  TPessoaJuridica = class(TPessoa)
  private
    FCNPJ: string;
    procedure SetCNPJ(const Value: string);
  published
  public
    property CNPJ: string read FCNPJ write SetCNPJ;
  end;

  TPapel = class
  public
    Papel:TPessoa;
  end;

  TEndereco = class
  private
    FEndereco: string;
    procedure SetEndereco(const Value: string);
  published
  public
    property Descricao: string read FEndereco write SetEndereco;
  end;

  TCliente = class(TPapel)
  private
    FTipoCliente: string;
    FEndereco: TEndereco;
    procedure SetEndereco(const Value: TEndereco);
    procedure SetTipoCliente(const Value: string);
  published
  public
    property TipoCliente: string read FTipoCliente write SetTipoCliente;
    property Endereco: TEndereco read FEndereco write SetEndereco;
  end;

  TFornecedor = class(TPapel)
  private
    FEndereco: TEndereco;
    procedure SetEndereco(const Value: TEndereco);
  published
  public
    property Endereco: TEndereco read FEndereco write SetEndereco;
  end;

  TFuncionario = class(TPapel)

  end;



var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.FormCreate(Sender: TObject);
var
   Cliente:TCliente;
   Fornecedor:TFornecedor;
begin
   cliente.Endereco.Descricao;
   cliente.Papel.Id;

   TPessoaFisica(cliente.Papel).CPF;

   Fornecedor.Endereco.Descricao;
   TPessoaJuridica(Fornecedor.Papel).CNPJ;
end;

{ TPessoa }

procedure TPessoa.SetId(const Value: integer);
begin
  FId := Value;
end;

{ TCliente }

procedure TCliente.SetEndereco(const Value: TEndereco);
begin
  FEndereco := Value;
end;

procedure TCliente.SetTipoCliente(const Value: string);
begin
  FTipoCliente := Value;
end;

{ TEndereco }

procedure TEndereco.SetEndereco(const Value: string);
begin
  FEndereco := Value;
end;

{ TPessoaFisica }

procedure TPessoaFisica.SetCPF(const Value: string);
begin
  FCPF := Value;
end;

{ TPessoaJuridica }

procedure TPessoaJuridica.SetCNPJ(const Value: string);
begin
  FCNPJ := Value;
end;

{ TFornecedor }

procedure TFornecedor.SetEndereco(const Value: TEndereco);
begin
  FEndereco := Value;
end;

end.
