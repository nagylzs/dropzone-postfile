unit StreamUploader;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, IdSSLOpenSSL, ProgressFileStream, IdComponent, IdHTTP,
  CustomMultiPartDataStream, DateUtils;

type

  { TStreamUploader }

  TStreamUploaderStatus = record
    Started: TDateTime;
    LastUpdated: TDateTime;
    LastUploaded: longint;
    TotalSize: longint;
    Cancelled: boolean;
    Finished: boolean;

    TotalElapsed: double;
    TotalUploaded: longint;
    PercentDone: double;

    BatchElapsed: double;
    BatchUploaded: double;
    BatchSpeed: double;

    Remaining: longint;
    Eta: TDateTime;
    RemainingTime: double;
  end;

  TStreamUploader = class(TThread)
  private
    FIdHTTP: TIdHTTP;
    FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FHeaders: TStrings;
    FForm: TCustomMultipartDataStream;
    FOnCompleted: TNotifyEvent;

    FURL: string;
    FFileName: string;

    FSrc: TProgressFileStream;

    FInternalStatus: TStreamUploaderStatus;
    FLock: TRTLCriticalSection;
    FCancelRequested: boolean;

    function GetStatus: TStreamUploaderStatus;
    procedure OnReadProgress(Sender: TProgressFileStream; Size: longint);
    procedure SetStatus(AValue: TStreamUploaderStatus);

  public
    constructor Create(const AURL: string; const AFileName: string);
    destructor Destroy; override;

    procedure Execute; override;
    procedure Cancel;

    class function FormatSize(const ASize: double): string;
    class function FormatSpeed(const ABytesPersSec: double): string;
    class function FormatEta(const aEta, aStarted: TDateTime): string;
    class function FormatRemainingTime(const aRemaining: double): string;

    property Headers: TStrings read FHeaders;
    property IOHandler: TIdSSLIOHandlerSocketOpenSSL read FIOHandler;
    property Status: TStreamUploaderStatus read GetStatus write SetStatus;
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
  end;

implementation

{ TStreamUploader }

procedure TStreamUploader.OnReadProgress(Sender: TProgressFileStream; Size: longint);
var
  nextStatus: TStreamUploaderStatus;
begin
  if FCancelRequested then
    Abort;

  nextStatus := GetStatus;
  with nextStatus do
  begin
    TotalUploaded := Sender.Position;
    TotalElapsed := Now - Started;
    Remaining := TotalSize - TotalUploaded;

    BatchElapsed := Now - LastUpdated;
    BatchUploaded := TotalUploaded - LastUploaded;

    if BatchElapsed > 200 * OneMillisecond then
    begin
      BatchSpeed := BatchUploaded / (BatchElapsed / OneSecond);
      LastUpdated := Now;
      LastUploaded := Sender.Position;

      if TotalSize > 0 then
        PercentDone := 100 * TotalUploaded / TotalSize
      else
        PercentDone := 100;

      if BatchSpeed > 0 then
      begin
        RemainingTime := Remaining / (BatchSpeed / OneSecond);
        eta := Now + remainingTime;
      end else begin
        RemainingTime := -1;
        eta := -1;
      end;

      SetStatus(nextStatus);
    end;
  end;
end;

function TStreamUploader.GetStatus: TStreamUploaderStatus;
begin
  EnterCriticalSection(FLock);
  try
    Move(FInternalStatus, Result, sizeOf(FInternalStatus));
  finally
    LeaveCriticalsection(FLock)
  end;
end;

class function TStreamUploader.FormatSize(const ASize: double): string;
begin
  if (ASize > 1024 * 1024 * 1024) then
  begin
    Result := FormatFloat('0.00 G', ASize / 1024 / 1024 / 1024);
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

class function TStreamUploader.FormatSpeed(const ABytesPersSec: double): string;
begin
  if ABytesPersSec <= 0 then
    Result := '(stalled)'
  else
    Result := FormatSize(ABytesPersSec) + '/s';
end;

class function TStreamUploader.FormatEta(const aEta, aStarted: TDateTime): string;
var
  eYear, eDoy: word;
  sYear, sDoy: word;
begin
  if (aEta <= 0) then
  begin
    Result := 'N/A';
  end
  else
  begin
    DecodeDateDay(aStarted, sYear, sDoy);
    DecodeDateDay(aEta, eYear, eDoy);
    if (eYear <> sYear) or (eDoy <> sDoy) then
      Result := DateTimeToStr(aEta);
    Result := FormatDateTime('t', aEta);
  end;
end;

class function TStreamUploader.FormatRemainingTime(const aRemaining: double): string;
begin
  if (aRemaining <= 0) then
    Result := 'N/A'
  else
    Result := TimeToStr(aRemaining);
end;

procedure TStreamUploader.SetStatus(AValue: TStreamUploaderStatus);
begin
  EnterCriticalSection(FLock);
  try
    FInternalStatus := AValue;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TStreamUploader.Create(const AURL: string; const AFileName: string);
begin
  inherited Create(True);
  FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FIdHTTP := TIdHTTP.Create;
  FIdHTTP.IOHandler := FIOHandler;
  FHeaders := TStringList.Create;
  FForm := nil;
  FSrc := nil;
  FURL := AUrl;
  FFileName := AFileName;
  FreeOnTerminate:=false;

  InitCriticalSection(FLock);
end;

destructor TStreamUploader.Destroy;
begin
  try
    FIdHTTP.IOHandler := nil;
    FreeAndNil(FIdHTTP);
    FreeAndNil(FIOHandler);
    FreeAndNil(FHeaders);
    DoneCriticalsection(FLock);
  finally
    inherited Destroy;
  end;
end;

procedure TStreamUploader.Execute;
var
  response: TStringStream;
  nextStatus: TStreamUploaderStatus;
  wasError: boolean;
begin
  FSrc := TProgressFileStream.Create(FFileName);
  wasError := False;
  try
    // Update starting status
    with nextStatus do
    begin
      Started := Now;
      LastUpdated := Started;
      LastUploaded := 0;

      TotalSize := FSrc.Size;
      Cancelled := False;
      Finished := False;

      TotalElapsed := 0;
      TotalUploaded := 0;
      PercentDone := 0;
      BatchElapsed := -1;
      BatchUploaded := -1;
      BatchSpeed := 0;
      Remaining := -1;
      Eta := -1;
      RemainingTime := -1;
    end;
    SetStatus(nextStatus);

    FCancelRequested := False;
    FSrc.OnReadProgress := @OnReadProgress;
    response := TStringStream.Create('');
    try
      try
        FForm := TCustomMultipartDataStream.Create;
        try
          FForm.AddFileStream('file', FFileName, FSrc);
          with FIdHTTP.Request.CustomHeaders do
          begin
            Clear;
            AddStrings(FHeaders);
          end;
          FIdHTTP.Post(FUrl, FForm, response);
          // Update finishing status
          nextStatus := GetStatus;
          with nextStatus do
          begin
            TotalUploaded := TotalSize;
            TotalElapsed := Now - Started;
            Remaining := 0;

            BatchElapsed := Now - LastUpdated;
            BatchUploaded := TotalUploaded - LastUploaded;

            LastUpdated := Now;
            LastUploaded := 0;
            PercentDone := 100;
            RemainingTime := 0;
            Eta := -1;
          end;
          SetStatus(nextStatus);
        finally
          FForm.Free;
        end;
      except
        on e: EAbort do
        begin
          nextStatus.LastUploaded := FSrc.Position;
          nextStatus.Cancelled := True;
          nextStatus.Finished := False;
          SetStatus(nextStatus);
          wasError := True;
        end;
        on e: Exception do
        begin
          nextStatus.Cancelled := True;
          nextStatus.Finished := False;
          SetStatus(nextStatus);
          wasError := True;
        end;
      end;
    finally
      response.Free;
    end;
  finally
    FSrc.Free;
  end;
  if not wasError then
  begin
    nextStatus.Cancelled := False;
    nextStatus.Finished := True;
    SetStatus(nextStatus);
    if Assigned(FOnCompleted) then
      FOnCompleted(self);
  end;
end;

procedure TStreamUploader.Cancel;
begin
  FCancelRequested := True;
end;



end.


