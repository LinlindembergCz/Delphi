unit Unit27;

interface

uses FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  System.Classes, FMX.Types, FMX.Ani, FMX.Forms;

type
  TForm27 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

{$R *.fmx}

procedure TForm27.Button1Click(Sender: TObject);
begin
  Image1.AnimateFloat('Position.y', - Image1.Height, 1.5);
  Image1.AnimateFloat('Position.X',  Image1.width, 1.5);
end;

procedure TForm27.Button2Click(Sender: TObject);
begin
   Image2.AnimateFloat('Position.y', - Image2.Height, 1.5, TAnimationType.In,TInterpolationType.Circular);
   Image2.AnimateFloat('Position.X', Image2.width, 1.5, TAnimationType.In,TInterpolationType.Circular);
end;

procedure TForm27.Button3Click(Sender: TObject);
begin
 Image3.AnimateFloat('Position.y', - Image3.Height,1.5, TAnimationType.In,TInterpolationType.Cubic);
  Image3.AnimateFloat('Position.X',  Image3.width,1.5, TAnimationType.In,TInterpolationType.Cubic);
end;

procedure TForm27.Button4Click(Sender: TObject);
begin
 Image4.AnimateFloat('Position.y', - Image4.Height, 1.5, TAnimationType.In,TInterpolationType.Bounce);
  Image4.AnimateFloat('Position.X',  Image4.width, 1.5, TAnimationType.In,TInterpolationType.Bounce);
end;

procedure TForm27.Button5Click(Sender: TObject);
begin
 Image5.AnimateFloat('Position.y', - Image5.Height,1.5, TAnimationType.In,TInterpolationType.Exponential);
 Image5.AnimateFloat('Position.X',  Image5.width,1.5, TAnimationType.In,TInterpolationType.Exponential);
end;

procedure TForm27.Button6Click(Sender: TObject);
begin
 Image6.AnimateFloat('Position.y', - Image6.Height, 1.5, TAnimationType.In,TInterpolationType.Sinusoidal);
 Image6.AnimateFloat('Position.X',  Image6.width, 1.5, TAnimationType.In,TInterpolationType.Sinusoidal);
end;

procedure TForm27.Button7Click(Sender: TObject);
begin
 Image7.AnimateFloat('Position.y', - Image7.Height, 1.5, TAnimationType.In,TInterpolationType.Elastic);
 Image7.AnimateFloat('Position.X',  Image7.width, 1.5, TAnimationType.out,TInterpolationType.Elastic);
end;

end.
