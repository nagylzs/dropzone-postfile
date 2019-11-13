unit uHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    btnOK: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure ListBox1Click(Sender: TObject);
  private

  public

  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.lfm}

{ TfrmHelp }


procedure TfrmHelp.ListBox1Click(Sender: TObject);
begin

end;

end.

