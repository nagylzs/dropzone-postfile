program PostFile;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, indylaz, ProgressFileStream, CustomMultiPartDataStream, uHelp,
  streamuploader
  { you can add units after this }
  ,DefaultTranslator;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.Run;
end.

