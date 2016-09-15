
unit EditSongFormat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseForm, SongFormat;

type
  TfrmEditSongFormat = class(TBaseForm)
    PanelTop: TPanel;
    BevelTop: TBevel;
    PanelPrincipal: TPanel;
    lbName: TLabel;
    edName: TEdit;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btSave: TButton;
    btCancel: TButton;
    procedure btSaveClick(Sender: TObject);
  private
    FFormat: TSongFormat;
  public
    property SongFormat: TSongFormat read FFormat write FFormat;
  end;

implementation

uses
  SongFormatController;

{$R *.dfm}

procedure TfrmEditSongFormat.btSaveClick(Sender: TObject);
var
  Controller: TSongFormatController;
begin
  FFormat := TSongFormat.Create;
  FFormat.FormatName := edName.Text;

  Controller := TSongFormatController.Create;
  try
    Controller.SaveFormat(FFormat);
  finally
    Controller.Free;
  end;

  ModalResult := mrOk;
end;

end.
