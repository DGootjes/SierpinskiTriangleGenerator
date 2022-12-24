unit uSierpinskiTriangle;

interface
  uses
  System.SysUtils,
  System.Generics.Collections,
  Math;

  type
  TPoint = record
    X: Integer;
    Y: Integer;
    class operator Subtract(a: TPoint; b: TPoint) : TPoint;
    constructor Create(X, Y: Integer);
  end;

  TSierpinskiTriangle = class(TObject)
  public
    InitialPoints: TList<TPoint>;
    ExtraPoints: TList<TPoint>;
    PointSize: Integer;
    ImageWidth: Integer;
    ImageHeight: Integer;

    procedure PickThreeRandomInitialPoints;
    function PickRandomPointInTriangle: TPoint;
    function PickPointHalfwayToRandomVertexFrom(point: TPoint): TPoint;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPoint }

constructor TPoint.Create(X, Y: Integer);
begin
  self.X := X;
  Self.Y := Y;
end;

class operator TPoint.Subtract(a, b: TPoint): TPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

{ TSierpinskiTriangle }

constructor TSierpinskiTriangle.Create;
begin
  InitialPoints := TList<TPoint>.Create;
  ExtraPoints := TList<TPoint>.Create;
  PointSize := 5;
end;

destructor TSierpinskiTriangle.Destroy;
begin
  FreeAndNil(InitialPoints);
  FreeAndNil(ExtraPoints);
  inherited;
end;

function TSierpinskiTriangle.PickPointHalfwayToRandomVertexFrom(
  point: TPoint): TPoint;
var
  vertex: TPoint;
begin
  // Pick a random triangle vertex
  vertex := InitialPoints[Random(3)];

  // Create a point midway
  Result := TPoint.Create((point.X + vertex.X) div 2,
                          (point.Y + vertex.Y) div 2);
  ExtraPoints.Add(Result);
end;

function TSierpinskiTriangle.PickRandomPointInTriangle: TPoint;
var
  a, b: TPoint; // Vectors
  P1, P2, P3: TPoint;
  u1, u2: Double;
  w: TPoint;
begin
  // A random point within the triangle can be found by a
  // convex combination of the three vertices
  P1 := InitialPoints[0];
  P2 := InitialPoints[1];
  P3 := InitialPoints[2];

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

procedure TSierpinskiTriangle.PickThreeRandomInitialPoints;
var
  X, Y: Integer;
  Point: TPoint;
begin
  X := Random(ImageWidth - 2 * PointSize) + PointSize;
  Y := PointSize;
  Point:= TPoint.Create(X, Y);
  InitialPoints.Add(Point);
  //DrawPoint(Point);

  X := Random(20 * PointSize) + PointSize;
  Y := ImageHeight - 2*PointSize;
  Point:= TPoint.Create(X, Y);
  InitialPoints.Add(Point);
  //DrawPoint(Point);

  X := ImageWidth - 21 * PointSize + Random(20 * PointSize);
  Y := ImageHeight - 2*PointSize;
  Point:= TPoint.Create(X, Y);
  InitialPoints.Add(Point);
  // DrawPoint(Point);
end;

end.

