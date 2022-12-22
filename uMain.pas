unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants,
  System.Classes,System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  Math;

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
    fPointSize: Integer;

    fInitialPoints: TList<TPoint>;
    fExtraPoints: TList<TPoint>;

    procedure DrawPoint(point: TPoint);
    function RandomPointWithinTriangle: TPoint;
    function FindMidwayPointFromRandomVertex(point: TPoint): TPoint;
    procedure Pick3SemiRandomInitialPoints;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnInitialPickerClick(Sender: TObject);
var
  brush: TBrush;
begin
  if not IsInitialized then
  begin
    IsInitialized := True;

    brush := TBrush.Create;
    brush.Color := clBlue;
    brush.Style := bsSolid;
    imSierpinski.Canvas.Brush := brush;
    fPointSize := 5;

    Pick3SemiRandomInitialPoints;
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
  Amount := StrToInt(edtPointCount.Text);
  for i := 1 to Amount do
  begin
    if fExtraPoints.Count = 0 then
    begin
      point := RandomPointWithinTriangle;
    end
    else
    begin
      point := FindMidwayPointFromRandomVertex(fExtraPoints.Last);
    end;
    DrawPoint(point);
    fExtraPoints.Add(point);
  end;
end;

procedure TForm2.btnResetClick(Sender: TObject);
var
  brush: TBrush;
begin
  IsInitialized := False;
  fInitialPoints.Clear;


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
  fInitialPoints := TList<TPoint>.Create;
  fExtraPoints := TList<TPoint>.Create;
  btnResetClick(nil);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fInitialPoints);
  FreeAndNil(fExtraPoints);
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  imSierpinski.Width := Floor(Self.Width * fFormRatio);
  pnControls.Width := Floor(Self.Width * (1 - fFormRatio));
end;

function TForm2.FindMidwayPointFromRandomVertex(point: TPoint): TPoint;
var
  vertex: TPoint;
begin
  // Pick a random triangle vertex from the initial 3 points
  vertex := fInitialPoints[Random(3)];

  // Create a point midway
  Result := TPoint.Create((point.X + vertex.X) div 2,(point.Y + vertex.Y) div 2);
end;

procedure TForm2.Pick3SemiRandomInitialPoints;
var
  X: Integer;
  Y: Integer;
  Point: TPoint;
begin
  X := Random(imSierpinski.Width - 2 * fPointSize) + fPointSize;
  Y := fPointSize;
  Point := TPoint.Create(X, Y);
  fInitialPoints.Add(Point);
  DrawPoint(Point);

  X := Random(20 * fPointSize) + fPointSize;
  Y := imSierpinski.Height - 2 * fPointSize;
  Point := TPoint.Create(X, Y);
  fInitialPoints.Add(Point);
  DrawPoint(Point);

  X := imSierpinski.Width - 21 * fPointSize + Random(20 * fPointSize);
  Y := imSierpinski.Height - 2 * fPointSize;
  Point := TPoint.Create(X, Y);
  fInitialPoints.Add(Point);
  DrawPoint(Point);
end;

function TForm2.RandomPointWithinTriangle: TPoint;
var
  a, b: TPoint; // Vectors
  P1, P2, P3: TPoint;
  u1, u2: Double;
  w: TPoint;
begin
  P1 := fInitialPoints[0];
  P2 := fInitialPoints[1];
  P3 := fInitialPoints[2];

  a := P2 - P1;
  b := P3 - P1;

  // Generate random value between 0 and 1
  u1 := Random;
  u2 := Random;

  if u1 + u2 > 1.0 then
  begin
    u1 := 1 - u1;
    u2 := 1 - u2;
  end;


  w := TPoint.Create(P1.X + Floor(u1 * a.X + u2 * b.X),
                     P1.Y + Floor(u1 * a.Y + u2 * b.Y));
  Result := w;
end;

end.
