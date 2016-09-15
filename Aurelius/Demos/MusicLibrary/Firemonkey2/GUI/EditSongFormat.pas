unit EditSongFormat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.Edit, SongFormat;

type
  TfrmEditSongFormat = class(TForm)
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
    FFormat: TSongFormat;
  public
    property SongFormat: TSongFormat read FFormat write FFormat;
  end;

implementation
uses
  SongFormatController;

{$R *.fmx}

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
