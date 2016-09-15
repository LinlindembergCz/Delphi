unit EditVideoFormat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Edit, VideoFormat;

type
  TfrmEditVideoFormat = class(TForm)
    TopLabel: TLabel;
    ButtonLayout: TLayout;
    btCancel: TButton;
    btSave: TButton;
    BottomLayout: TLayout;
    CenterLayout: TLayout;
    NameLayout: TLayout;
    edName: TEdit;
    lbName: TLabel;
    procedure btSaveClick(Sender: TObject);
  private
    FFormat: TVideoFormat;
  public
    property VideoFormat: TVideoFormat read FFormat write FFormat;
  end;

implementation
uses
  VideoFormatController;

{$R *.fmx}

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
