program MbScanTcp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UnitMain, synacode, unitMbWrite, unitabout;// uthreadtimer;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMBScanTcp, FormMBScanTcp);
  Application.CreateForm(TFormMbWrite, FormMbWrite);
  Application.CreateForm(TFormabout, Formabout);
  Application.Run;
end.

