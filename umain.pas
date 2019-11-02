unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, IdHTTP, IdSSLOpenSSL, DateUtils, ProgressFileStream, IdComponent;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCancel: TButton;
    btnSelectFile: TButton;
    btnStartUpload: TButton;
    Button1: TButton;
    btnHelp: TButton;
    eFileName: TEdit;
    eURL: TEdit;
    IdHTTP: TIdHTTP;
    ioSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
    Label1: TLabel;
    lblProgress: TLabel;
    lblProgress1: TLabel;
    lblURL: TLabel;
    mHeaders: TMemo;
    od: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    progressBar: TProgressBar;
    rgTLS: TRadioGroup;
    tsTLS: TTabSheet;
    Timer: TTimer;
    tsBasic: TTabSheet;
    tsHeaders: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnStartUploadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgTLSClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FStarted: TDateTime;
    FStartedYear: word;
    FStartedDoy: word;
    FLastUpdated: TDateTime;
    FLastUploaded: longint;
    FTotalSize: longint;
    FCancel: boolean;

    procedure OnReadProgress(Sender: TProgressFileStream; Size: longint);
    function FormatSize(ASize: double): string;

    procedure SetUploading(const AValue: boolean);

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  IdMultipartFormData, CustomMultipartDataStream, uHelp;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnStartUploadClick(Sender: TObject);
var
  response: TStringStream;
  form: TCustomMultipartDataStream;
  src: TProgressFileStream;
  totalElapsed: TDateTime;
  avgSpeed: double;
begin
  if Trim(eURL.Text) = '' then
  begin
    ShowMessage('Invalid URL');
    exit;
  end;

  if not FileExists(eFileName.Text) then
  begin
    ShowMessage('Invalid filename');
    exit;
  end;

  with ioSocketOpenSSL.SSLOptions do
  begin
    case rgTLS.ItemIndex of
      0: SSLVersions:= [sslvTLSv1_2];
      1: SSLVersions:= [sslvTLSv1_2, sslvTLSv1_1];
      2: SSLVersions:= [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1 ];
    end;
  end;

  try
    src := TProgressFileStream.Create(eFileName.Text);
    try
      FTotalSize := src.Size;
      FLastUpdated := 0;
      FLastUploaded := 0;
      FStarted := Now;
      DecodeDateDay(FStarted, FStartedYear, FStartedDoy);
      FCancel := False;
      SetUploading(True);

      progressBar.Max := FTotalSize;
      progressBar.Position := 0;
      lblProgress.Font.Color := clDefault;
      src.OnReadProgress := @OnReadProgress;
      response := TStringStream.Create('');
      try
        try
          form := TCustomMultipartDataStream.Create;
          try
            form.AddFileStream('file', eFileName.Text, src);
            with IdHttp.Request.CustomHeaders do
            begin
              Clear;
              AddStrings(mHeaders.Lines);
            end;
            IdHTTP.Post(eURL.Text, form, response);
          finally
            form.Free;
          end;
        except
          on e: EAbort do
          begin
            ShowMessage('User aborted upload.');
            FCancel:= true;
          end;
          on e :Exception do
          begin
            lblProgress.Caption:= 'UPLOAD ERROR!';
            lblProgress.Font.Color:= clRed;
            ShowMessage(e.Message);
            FCancel:= true;
          end;
        end;
      finally
        response.Free;
      end;

    finally
      src.Free;
    end;

    totalElapsed := Now - FStarted;
    avgSpeed := progressBar.Max / totalElapsed * OneSecond;

    if FCancel then
    begin
      lblProgress.Font.Color := clRed;
      lblProgress.Caption := 'ABORTED! ' + FormatSize(progressBar.Max) +
        ' in ' + TimeToStr(totalElapsed) + ' Avg. speed= ' + FormatSize(avgSpeed) + '/s';
    end
    else
    begin
      lblProgress.Font.Color := clGreen;
      progressBar.Position := progressBar.Max;
      lblProgress.Caption := 'Uploaded ' + FormatSize(progressBar.Max) +
        ' in ' + TimeToStr(totalElapsed) + ' Avg. speed= ' + FormatSize(avgSpeed) + '/s';
    end;

    SetUploading(False);

  except
    on e: Exception do
    begin
      ExitCode := -1;
      raise e;
    end;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if od.Execute then
    mHeaders.Lines.LoadFromFile(od.FileName);
end;

procedure TfrmMain.eFileNameChange(Sender: TObject);
begin
  lblProgress.Font.Color := clDefault;
  lblProgress.Caption := 'Press the "Start upload" button to upload your file...';
end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  FCancel := True;
  btnCancel.Enabled := False;
end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

procedure TfrmMain.btnSelectFileClick(Sender: TObject);
begin
  if od.Execute then
    eFileName.Text := od.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
end;

procedure TfrmMain.rgTLSClick(Sender: TObject);
begin
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
begin
  if Visible then
  begin
    Timer.Enabled := False;

    if Application.HasOption('h', 'help') then
    begin
      frmHelp.ShowModal;
      Application.Terminate;
    end;
    if Application.HasOption('title') then
    begin
      Caption := Application.GetOptionValue('title');
    end;

    if Application.HasOption('u', 'url') then
    begin
      eURL.Text := Application.GetOptionValue('u', 'url');
    end;
    if Application.HasOption('f', 'file') then
    begin
      eFilename.Text := Application.GetOptionValue('f', 'file');
    end;
    if Application.HasOption('d', 'headers') then
    begin
      mHeaders.Lines.LoadFromFile(Application.GetOptionValue('d', 'headers'));
    end;
    if Application.HasOption('allow-tls-1.0') then
    begin
      rgTLS.ItemIndex:= 2;
    end else if Application.HasOption('allow-tls-1.1') then
    begin
      rgTLS.ItemIndex:= 1;
    end else begin
      rgTLS.ItemIndex:= 0;
    end;

    if Application.HasOption('auto-start') then
    begin
      btnStartUpload.Click;
    end;
  end;
end;

procedure TfrmMain.OnReadProgress(Sender: TProgressFileStream; Size: longint);
var
  totalElapsed: double;
  totalUploaded: longint;
  percent: double;

  batchElapsed: double;
  batchUploaded: double;
  batchSpeed: double;
  sBatchSpeed: string;

  remaining: longint;
  eta: TDateTime;
  sEta: string;

  year, doy: word;
begin
  if FCancel then
    Abort;

  batchElapsed := Now - FLastUpdated;
  if batchElapsed > 100 * OneMillisecond then
  begin
    totalUploaded := Sender.Position;
    batchUploaded := totalUploaded - FLastUploaded;
    batchSpeed := batchUploaded / batchElapsed;

    progressBar.Position := totalUploaded;
    percent := 100.0 * totalUploaded / FTotalSize;

    remaining := FTotalSize - totalUploaded;
    if batchSpeed > 0 then
    begin
      sBatchSpeed := FormatSize(batchSpeed * OneSecond) + '/s';
      eta := Now + remaining / batchSpeed;
      DecodeDateDay(eta, year, doy);
      if (year <> FStartedYear) or (doy <> FStartedDoy) then
      begin
        sEta := DateTimeToStr(eta);
      end
      else
      begin
        sEta := FormatDateTime('t', eta);
      end;
    end
    else
    begin
      sBatchSpeed := 'Stalled';
      sEta := 'N/A';
    end;

    totalElapsed := Now - FStarted;

    lblProgress.Caption :=
      FormatFloat('0.0 %', percent) + ' Uploaded: ' + FormatSize(totalUploaded) +
      ' Remaining: ' + FormatSize(remaining) + ' Speed: ' + sBatchSpeed +
      ' Elapsed: ' + TimeToStr(totalElapsed) + ' ETA: ' + sEta;
    Application.ProcessMessages;

    FLastUpdated := Now;
    FLastUploaded := totalUploaded;

    if Application.HasOption('auto-close') then
    begin
      Close;
    end;
  end;
end;

function TfrmMain.FormatSize(ASize: double): string;
begin
  if (ASize > 1024 * 1024 * 1024) then
  begin
    Result := FormatFloat('0.0 G', ASize / 1024 / 1024 / 1024);
  end
  else if (ASize > 1024 * 1024) then
  begin
    Result := FormatFloat('0.0 M', ASize / 1024 / 1024);
  end
  else if (ASize > 1024) then
  begin
    Result := FormatFloat('0.0 K', ASize / 1024);
  end
  else
  begin
    Result := FormatFloat('0.0 B', ASize);
  end;
end;

procedure TfrmMain.SetUploading(const AValue: boolean);
begin
  btnStartUpload.Enabled := not AValue;
  eURL.Enabled := not AValue;
  eFileName.Enabled := not AValue;
  btnSelectFile.Enabled := not AValue;
  btnCancel.Enabled := AValue;
end;

end.
