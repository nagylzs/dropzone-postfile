unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, IdHTTP, IdSSLOpenSSL, DateUtils, IdComponent,
  StreamUploader;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCancel: TButton;
    btnSelectFile: TButton;
    btnStartUpload: TButton;
    Button1: TButton;
    btnHelp: TButton;
    cbAutoClose: TCheckBox;
    eFileName: TEdit;
    eURL: TEdit;
    Label1: TLabel;
    lblProgress: TLabel;
    lblProgress1: TLabel;
    lblUploaded: TLabel;
    lblRemaining: TLabel;
    lblURL: TLabel;
    mHeaders: TMemo;
    od: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    progressBar: TProgressBar;
    rgTLS: TRadioGroup;
    tmProgress: TTimer;
    tsTLS: TTabSheet;
    tmStarted: TTimer;
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
    procedure tmProgressTimer(Sender: TObject);
    procedure tmStartedTimer(Sender: TObject);
  private
    FStreamUploader: TStreamUploader;


    procedure SetUploading(const AValue: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  frmMain: TfrmMain;

implementation

uses
  uHelp;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnStartUploadClick(Sender: TObject);
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

  if Assigned(FStreamUploader) then
  begin
    if FStreamUploader.Finished then
    begin
      FreeAndNil(FStreamUploader);
    end
    else
    begin
      ShowMessage('Previous upload is still running.');
      exit;
    end;
  end;

  try

    FStreamUploader := TStreamUploader.Create(eURL.Text, eFileName.Text);
    with FStreamUploader.IOHandler.SSLOptions do
    begin
      case rgTLS.ItemIndex of
        0: SSLVersions := [sslvTLSv1_2];
        1: SSLVersions := [sslvTLSv1_2, sslvTLSv1_1];
        2: SSLVersions := [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1];
      end;
    end;
    FStreamUploader.Headers.Clear;
    FStreamUploader.Headers.AddStrings(mHeaders.Lines);
    SetUploading(True);
    FStreamUploader.Start;

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

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

end;

procedure TfrmMain.rgTLSClick(Sender: TObject);
begin

end;

procedure TfrmMain.tmProgressTimer(Sender: TObject);
var
  AvgSpeed: double;
begin
  if Assigned(FStreamUploader) then
  begin
    with FStreamUploader, Status do
    begin
      if Cancelled then
      begin
        lblProgress.Caption := 'Upload aborted.';
        lblProgress.Font.Color := clMaroon;
        SetUploading(False);
        lblRemaining.Caption := '';
      end
      else if WasError then
      begin
        lblProgress.Caption := ErrorMessage;
        lblProgress.Font.Color := clRed;
        SetUploading(False);
        lblRemaining.Caption := '';
      end
      else if Finished then
      begin
        lblUploaded.Caption :=
          ' Uploaded: ' + FormatSize(totalUploaded) + ' in ' + TimeToStr(totalElapsed);
        lblRemaining.Caption := '';

        if (TotalSize > 0) and (TotalElapsed > 0) then
          AvgSpeed := TotalSize / (TotalElapsed / OneSecond)
        else
          AvgSpeed := -1;

        lblProgress.Caption := 'Upload finished, avg speed =' + FormatSpeed(AvgSpeed);
        lblProgress.Font.Color := clGreen;

        SetUploading(False);

        if cbAutoClose.Checked then
           Application.Terminate;
      end
      else
      begin
        lblProgress.Caption :=
          FormatFloat('0.0 %', PercentDone) + ' Speed: ' +
          FormatSpeed(BatchSpeed) + ' ETA: ' + FormatEta(Eta, Started);
        lblProgress.Font.Color := clBlue;
        lblUploaded.Caption :=
          ' Uploaded: ' + FormatSize(totalUploaded) + ' in ' + TimeToStr(totalElapsed);
        lblRemaining.Caption :=
          ' Remaining: ' + FormatSize(Remaining) + ' in ' +
          FormatRemainingTime(RemainingTime);
      end;
      progressBar.Position := Trunc(100 * PercentDone);
    end;
  end
  else
  begin
    lblProgress.Caption := 'Press the "Start upload" button to upload your file...';
    lblProgress.Font.Color := clGray;

    lblUploaded.Caption := '';
    lblRemaining.Caption := '';
    progressBar.Position := 0;
  end;
end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  btnCancel.Enabled := False;
  if Assigned(FStreamUploader) then
    FStreamUploader.Cancel;
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

procedure TfrmMain.tmStartedTimer(Sender: TObject);
var
  values : TStringArray;
  index : integer;
  value : string;
begin
  if Visible then
  begin
    tmStarted.Enabled := False;

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
    if Application.HasOption('a', 'add-header') then
    begin
      values := Application.GetOptionValues('a', 'add-header');
      for index := Low(values) to High(values) do
      begin
        value := values[index];
        mHeaders.Lines.Add(value);
      end;
    end;
    if Application.HasOption('allow-tls-1.0') then
    begin
      rgTLS.ItemIndex := 2;
    end
    else if Application.HasOption('allow-tls-1.1') then
    begin
      rgTLS.ItemIndex := 1;
    end
    else
    begin
      rgTLS.ItemIndex := 0;
    end;

    if Application.HasOption('auto-close') then
    begin
      cbAutoClose.Checked := true;
    end;

    if Application.HasOption('auto-start') then
    begin
      btnStartUpload.Click;
    end;
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

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStreamUploader := nil;
  SetUploading(False);
end;

destructor TfrmMain.Destroy;
begin
  try
    if Assigned(FStreamUploader) then
      FreeAndNil(FStreamUploader);
  finally
    inherited Destroy;
  end;
end;

end.
