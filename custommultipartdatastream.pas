unit CustomMultiPartDataStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdMultipartFormData, IdGlobal, IdGlobalProtocols;

type

  { TCustomMultipartDataStream }

  TCustomMultipartDataStream = class(TIdMultiPartFormDataStream)
  public
    function AddFileStream(const AFieldName, AFileName: string;
      const ACustomFileStream: TIdReadFileExclusiveStream;
      const AContentType: string = ''): TIdFormDataField;
  end;

implementation

{ TCustomMultipartDataStream }

function TCustomMultipartDataStream.AddFileStream(const AFieldName, AFileName: string;
  const ACustomFileStream: TIdReadFileExclusiveStream;
  const AContentType: string = ''): TIdFormDataField;
var
  LItem: TIdFormDataField;
begin
  LItem := FFields.Add;
  LItem.FieldName := AFieldName;
  LItem.FileName := ExtractFileName(AFileName);
  LItem.FieldStream := ACustomFileStream;
  if AContentType <> '' then
  begin
    LItem.ContentType := AContentType;
  end
  else
  begin
    LItem.ContentType := GetMIMETypeFromFile(AFileName);
  end;
  LItem.ContentTransfer := sContentTransferBinary;

  Result := LItem;
end;

end.
