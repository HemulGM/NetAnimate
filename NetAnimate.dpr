program NetAnimate;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  NetAnimate.Main in 'NetAnimate.Main.pas' {FormMain};

{$R *.res}

begin
  GlobalUseDirect2D := False;
  GlobalUseGPUCanvas := True;
  Application.Initialize;
  //ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
