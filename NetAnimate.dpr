program NetAnimate;

uses
  System.StartUpCopy,
  FMX.Forms,
  NetAnimate.Main in 'NetAnimate.Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
