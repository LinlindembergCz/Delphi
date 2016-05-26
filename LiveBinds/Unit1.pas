unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.Bind.EngExt, Vcl.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components,
  Data.Bind.ObjectScope, Vcl.StdCtrls, System.Generics.Collections,
  Data.Bind.Controls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Bind.Navigator;

type
  TPessoa =class
  private
    FEmail: string;
    FIdade: integer;
    FNome: string;
    procedure SetEmail(const Value: string);
    procedure SetIdade(const Value: integer);
    procedure SetNome(const Value: string);
  public
    constructor Create(pNome:string; pIdade:integer; pEmail: string);
  published
    property Nome: string read FNome write SetNome;
    property Idade: integer read FIdade write SetIdade;
    property Email: string read FEmail write SetEmail;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DataGeneratorAdapter1: TDataGeneratorAdapter;
    AdapterBindSource1: TAdapterBindSource;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    BindNavigator1: TBindNavigator;
    Button1: TButton;
    procedure AdapterBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FLista : TObjectList<TPessoa>;
  end;



var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TPessoa }

constructor TPessoa.Create(pNome: string; pIdade: integer; pEmail: string);
begin
  Nome:= pNome;
  Idade:= pIdade;
  Email:= pEmail;
end;

procedure TPessoa.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TPessoa.SetIdade(const Value: integer);
begin
  FIdade := Value;
end;

procedure TPessoa.SetNome(const Value: string);
begin
  FNome := Value;
end;

procedure TForm1.AdapterBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  FLista := TObjectList<TPessoa>.Create;
  FLista.Add(TPessoa.Create('Filipe', 25, 'filipe@email.com'));
  FLista.Add(TPessoa.Create('Cíntia', 27, 'cintia@email.com'));
  FLista.Add(TPessoa.Create('Julio', 32, 'julio@email.com'));
  ABindSourceAdapter := TListBindSourceAdapter<TPessoa>.Create(Self, FLista);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  showmessage( FLista.Items[0].Nome);
end;

end.
