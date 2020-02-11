unit mainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IdTCPClient, AboutUnit, Forms;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    testButton: TButton;
    HostEdit: TLabeledEdit;
    PortEdit: TLabeledEdit;
    ResultEdit: TLabeledEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure testButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses versiontypes
   , versionresource
   , LCLIntf
   , LCLType
   ;
{$R *.lfm}

{ TForm1 }

function GetVersion: String;
var
  Stream: TResourceStream;
  vr: TVersionResource;
  fi: TVersionFixedInfo;
begin
  Result := '';
  try
  (* This raises an exception if version info has not been incorporated into the  *)
  (* binary (Lazarus Project -> Project Options -> Version Info -> Version        *)
  (* numbering).                                                                  *)
    Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      vr:= TVersionResource.Create;
      try
        vr.SetCustomRawDataStream(Stream);
        fi:= vr.FixedInfo;
        result := Format('%d.%d.%d.%d', [fi.FileVersion[0], fi.FileVersion[1], fi.FileVersion[2], fi.FileVersion[3]]);
        vr.SetCustomRawDataStream(nil);
      finally
        vr.Free
      end;
    finally
     Stream.Free
    end
  except
    result := '(not available)';
  end
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.CreateForm(TAboutBox, AboutBox);
  try
    AboutBox.ProgName := 'portChecker';
    AboutBox.LicensedTo := '';
    AboutBox.VersionInfo:= GetVersion;
    AboutBox.ShowModal;
  finally
    FreeAndNil(AboutBox);
  end;
end;

procedure TForm1.EditChange(Sender: TObject);
var
  xEnable: Boolean;
begin
  xEnable := (HostEdit.Text <> '');
  if xEnable then
  try
    StrToInt(PortEdit.Text);
  except
    xEnable := False;
  end;
  testButton.Enabled := xEnable;
end;

procedure TForm1.testButtonClick(Sender: TObject);
  procedure IsPortActive(AHost : string; APort : Integer);
  begin
    ResultEdit.Text :='...';
//  Application.ProcessMessages;
    ResultEdit.Repaint;
    with TIdTCPClient.Create(nil) do
    try
      try
        ConnectTimeout := 3000;
        Connect(AHost, APort);
        ResultEdit.Text := 'Ok';
      except
        on e: exception do
          ResultEdit.Text := e.Message;
      end;
      Disconnect;
    finally
      Free;
    end;
  end;
begin
  IsPortActive(HostEdit.Text, StrToInt (PortEdit.Text));
  PortEdit.SetFocus;
end;

end.

