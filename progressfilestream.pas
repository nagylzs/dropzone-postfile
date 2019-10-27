unit ProgressFileStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, IdGlobal;

type
  TProgressFileStream = class;
  TProgressFileStreamEvent = procedure (Sender:TProgressFileStream; Size: longint) of object;

  { TProgressFileStream }

  TProgressFileStream = class(TIdReadFileExclusiveStream)
  private
    FOnReadProgress : TProgressFileStreamEvent;
    FOnWriteProgress : TProgressFileStreamEvent;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property OnReadProgress : TProgressFileStreamEvent
             read FOnReadProgress write FOnReadProgress;
    property OnWriteProgress : TProgressFileStreamEvent
             read FOnWriteProgress write FOnWriteProgress;
  end;

implementation

{ TProgressFileStream }


function TProgressFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=inherited Read(Buffer, Count);
  if Assigned(FOnReadProgress) then FOnReadProgress(self, Result);
end;

function TProgressFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=inherited Write(Buffer, Count);
  if Assigned(FOnWriteProgress) then FOnWriteProgress(self, Result);
end;

end.

