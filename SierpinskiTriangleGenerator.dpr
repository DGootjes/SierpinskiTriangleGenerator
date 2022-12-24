program SierpinskiTriangleGenerator;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form2},
  uSierpinskiTriangle in 'uSierpinskiTriangle.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
