
unit EditVideoFormat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseForm, VideoFormat;

type
  TfrmEditVideoFormat = class(TBaseForm)
    PanelTop: TPanel;
    BevelTop: TBevel;
    MainPanel: TPanel;
    lbName: TLabel;
    edName: TEdit;
    BevelBottom: TBevel;
    PanelBottom: TPanel;
    btSave: TButton;
    btCancel: TButton;
    procedure btSaveClick(Sender: TObject);
  private
    FFormat: TVideoFormat;
  public
    property VideoFormat: TVideoFormat read FFormat write FFormat;
  end;

implementation

uses
  VideoFormatController;

{$R *.dfm}

procedure TfrmEditVideoFormat.btSaveClick(Sender: TObject);
var
  Controller: TVideoFormatController;
begin
  FFormat := TVideoFormat.Create;
  FFormat.FormatName := edName.Text;

  Controller := TVideoFormatController.Create;
  try
    Controller.SaveFormat(FFormat);
  finally
    Controller.Free;
  end;

  ModalResult := mrOk;
end;

end.
