(*
   ------------------------------------
   TAjaxEdit  - Ver�o 1.0

   Componente que traz para o Delphi, numa aplica��o Desktop,
   o efeito de uma requisi��o Ajax numa p�gina de internet.
   Possibilita uma consulta em banco de dados no meio da edi��o
   de um campo qualquer.

   Exemplo : Uma busca muito comum feita nas
   p�ginas da internet onde o usu�rio digita parte de um nome e a p�gina
   faz uma requisi��o ajax (sem recarregar a p�gina)
   e retorna uma lista de nomes que
   cont�m a palavra digitada, o usu�rio ent�o escolhe a palavra
   da lista retornada e o sistema preenche o edit.

   TAjaxEdit simula essa situa��o.

   -----------------------------------
   Feito e testado no XE2, funciona em vers�es superiores e anteriores
   ao XE2

   -----------------------------------
   Autor : Claudio Ferreira

   Licen�a :
     Livre para copiar, usar e melhorar, mas mantenha este cabe�alho

     ** Vers�o 1.0 - Claudio Ferreira
        primeira vers�o 20/12/2015

   Meu contato : cldmag@gmail.com
   Facebook : claudio.ferreira.cma
   ------------------------------------

*)
unit AjaxEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.GIFImg, Vcl.Forms;

type
   TAjaxEdit = Class(TEdit)
   private
       fContAjax         : Integer;
       fSecondsToShow    : Integer;
       fAjaxImage        : TImage;
       FOnShowAjax       : TNotifyEvent;
       fActiveOnCaracter : Integer;
       fEnableAjax       : Boolean;

       procedure AjaxOnTimer(Sender: TObject);
       procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
       procedure CMExit(var Message: TCMExit); message CM_EXIT;
   protected
       AjaxTimer : TTimer;
       procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   Public
       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
       procedure HideAjax;
   published
       property SecondsToShow    : Integer Read fSecondsToShow write fSecondsToShow;
       Property AjaxImage        : TImage read fAjaxImage write fAjaxImage;
       property OnShowAjax       : TNotifyEvent read FOnShowAjax write FOnShowAjax;
       property ActiveOnCaracter : Integer read fActiveOnCaracter write fActiveOnCaracter;
       property EnableAjax       : Boolean read fEnableAjax write fEnableAjax;
   End;

procedure Register;

implementation

constructor TAjaxEdit.Create(AOwner: TComponent);
begin
     Inherited Create(AOwner);
     AjaxTimer          := TTimer.Create (nil);
     AjaxTimer.Interval := 250;
     AjaxTimer.OnTimer  := AjaxOnTimer;
     AjaxTimer.Enabled  := False;

     fContAjax          := 0;
     fSecondsToShow     := 1500;
     fActiveOnCaracter  := 3;
     fEnableAjax        := True;
end;

destructor TAjaxEdit.Destroy;
begin
     AjaxTimer.Free;
     inherited;
end;

procedure TAjaxEdit.CMEnter(var Message: TCMEnter);
begin
     inherited;
     fContAjax := 0;
end;

procedure TAjaxEdit.CMExit(var Message: TCMExit);
begin
     AjaxTimer.Enabled := False;
     inherited;
end;

procedure TAjaxEdit.KeyDown(var Key: Word; Shift: TShiftState);
Begin
     inherited;
     fContAjax := 0;

     if (Length (Text + Chr(Key)) >= fActiveOnCaracter) and
        (fEnableAjax) then
         AjaxTimer.Enabled := True
     Else
         AjaxTimer.Enabled := False;
End;

procedure TAjaxEdit.AjaxOnTimer(Sender: TObject);
begin
     Inc (fContAjax);
     if fContAjax * 250 = fSecondsToShow then begin
        AjaxImage.Visible := True;
        TGIFImage(AjaxImage.Picture.Graphic).Animate := True;
     end;

     if fContAjax * 250 = fSecondsToShow + AjaxTimer.Interval then begin
        if Assigned(FOnShowAjax) then
           FOnShowAjax(Self);
     end;
end;

procedure TAjaxEdit.HideAjax;
Begin
     AjaxImage.Visible := False;
End;

procedure Register;
begin
     RegisterComponents('Ferreira Components', [TAjaxEdit]);
end;

end.


