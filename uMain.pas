unit uMain;

interface

uses
  Winapi.Messages,
  System.SysUtils, System.Variants,
  System.Classes,System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  Math,
  uSierpinskiTriangle;

  type
  TForm2 = class(TForm)
    btnReset: TButton;
    btnInitialPicker: TButton;
    btnPointPicker: TButton;
    edtPointCount: TEdit;
    pnControls: TPanel;
    imSierpinski: TImage;
    procedure btnResetClick(Sender: TObject);
    procedure btnInitialPickerClick(Sender: TObject);
    procedure btnPointPickerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fFormRatio: Double;
    IsInitialized: Boolean;

    fSierpinskiTriangle: TSierpinskiTriangle;

    procedure DrawPoint(point: TPoint);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnInitialPickerClick(Sender: TObject);
var
  brush: TBrush;
  Point: TPoint;
begin
  if not IsInitialized then
  begin
    IsInitialized := True;

    brush := TBrush.Create;
    brush.Color := clBlue;
    brush.Style := bsSolid;

    imSierpinski.Canvas.Brush := brush;

    fSierpinskiTriangle.PickThreeRandomInitialPoints;
    for Point in fSierpinskiTriangle.InitialPoints do
    begin
       DrawPoint(point);
    end;
  end
  else
  begin
    self.btnReset.Click;
    Self.btnInitialPicker.Click;
  end;
end;

procedure TForm2.btnPointPickerClick(Sender: TObject);
var
  point: TPoint;
  Amount: Integer;
  i: Integer;
begin
  if not IsInitialized then
  begin
    btnInitialPickerClick(nil);
  end;

  Amount := StrToInt(edtPointCount.Text);
  for i := 1 to Amount do
  begin
    if fSierpinskiTriangle.ExtraPoints.Count = 0 then
    begin
      point := fSierpinskiTriangle.PickRandomPointInTriangle;
      point := fSierpinskiTriangle.PickPointHalfwayToRandomVertexFrom(point);
    end
    else
    begin
      point := fSierpinskiTriangle.PickPointHalfwayToRandomVertexFrom(fSierpinskiTriangle.ExtraPoints.Last);
    end;
    DrawPoint(point);
  end;
end;

procedure TForm2.btnResetClick(Sender: TObject);
var
  brush: TBrush;
begin
  IsInitialized := False;
  fSierpinskiTriangle.InitialPoints.Clear;
  fSierpinskiTriangle.ExtraPoints.Clear;

  brush := TBrush.Create;
  brush.Color := clWebOrange;
  brush.Style := bsSolid;

  imSierpinski.Canvas.Brush := brush;
  imSierpinski.Canvas.Rectangle(0, 0, imSierpinski.Width ,imSierpinski.Height);
end;

procedure TForm2.DrawPoint(point: TPoint);
var
  size: Integer;
begin
  size := 2;
  imSierpinski.Canvas.Rectangle(Point.X-size, Point.Y-size, Point.X+size, Point.Y+size); // Draw a square of 2x2 pixels
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fFormRatio := 2 / 3;
  fSierpinskiTriangle := TSierpinskiTriangle.Create;
  fSierpinskiTriangle.ImageWidth := imSierpinski.Width;
  fSierpinskiTriangle.ImageHeight := imSierpinski.Height;
  btnResetClick(nil);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSierpinskiTriangle);
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  imSierpinski.Width := Floor(Self.Width * fFormRatio);
  pnControls.Width := Floor(Self.Width * (1 - fFormRatio));
end;

end.
