unit Unit30;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, System.threading;

type
  TForm30 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    ImgMercurio: TImage;
    imgVenus: TImage;
    imgTerra: TImage;
    imgMarte: TImage;
    procedure Button1Click(Sender: TObject);
  private
    procedure InitTask(i: integer;iSleep:integer; img:TImage);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form30: TForm30;
  tasks: array of ITask;

implementation

{$R *.fmx}

procedure TForm30.InitTask(i:integer; iSleep:integer; img:TImage);
begin
   tasks[i] := TTask.Create( procedure ()
                             var
                                a : Integer;
                             begin
                                a:= 0;
                                while CheckBox1.isChecked do
                                begin
                                   application.ProcessMessages;
                                   img.RotationAngle:= a;
                                   img.Repaint;
                                   inc(a);
                                   if a = 360 then
                                      a:= 0;
                                   sleep( iSleep );
                                end;
                             end );
   tasks[i].Start;
end;

procedure TForm30.Button1Click(Sender: TObject);
begin
   Setlength(tasks ,3);

   InitTask(0, 21//6
   , ImgMercurio );
   InitTask(1, 43//2
   , imgVenus );
   InitTask(2, 86//4
   , imgTerra );
   InitTask(3, 129//6
   , imgMarte );

   TTask.CurrentTask.ExecuteWork;
end;

end.
