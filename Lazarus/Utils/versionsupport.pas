{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
{
    Based on the work of Paul Ishenin, Mike Thompson and others
}
unit VersionSupport;
{$mode objfpc}

interface

uses
  Classes, SysUtils, resource, versiontypes, versionresource, LCLVersion, InterfaceBase;

const
  WIDGETSET_GTK        = 'GTK widget set';
  WIDGETSET_GTK2       = 'GTK 2 widget set';
  WIDGETSET_WIN        = 'Win32/Win64 widget set';
  WIDGETSET_WINCE      = 'WinCE widget set';
  WIDGETSET_CARBON     = 'Carbon widget set';
  WIDGETSET_QT         = 'QT widget set';
  WIDGETSET_fpGUI      = 'fpGUI widget set';
  WIDGETSET_OTHER      = 'Other gui';

type
  TVersionInfo = class
  private
    FResource: TStringList;
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    procedure GetResourceStrings();
    function VersionToString(PV: TFileProductVersion): string;
    function GetFileVersion: string;
    function GetProductVersion: string;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
    function GetCompilatonTime: string;
    function GetCompilerInfo: string;
    function GetTargetInfo: string;
    function GetOS: string;
    function GetCpu: string;
    function GetLCLVersion: string;
    function GetLCLFullVersion: string;
    function GetWidgetSet: string;
  public
    constructor Create;
    destructor Destroy; override;

    property BuildInfoAvailable: Boolean read FBuildInfoAvailable;
    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
    property FileVersion: string read GetFileVersion ;
    property ProductVersion: string read GetProductVersion;
    property CompilatonTime: string read GetCompilatonTime;
    property CompilerInfo: string read GetCompilerInfo;
    property TargetInfo: string read GetTargetInfo;
    property OS: string read GetOS;
    property Resource: TStringList read FResource;
    property LCLVersion: string read GetLCLVersion;
    property LCLFullVersion: string read GetLCLFullVersion;
    property WidgetSet: string read GetWidgetSet;
  end;

  function GetFileVersion: String;
implementation

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  result := FVersResource.FixedInfo;
end;

procedure TVersionInfo.GetResourceStrings();
var
  i, j : Integer;
  oTable : TVersionStringTable;
begin
  if BuildInfoAvailable then
  begin
    for i := 0 to StringFileInfo.Count-1 do
    begin
      oTable := StringFileInfo.Items[i];
      for j := 0 To oTable.Count-1 do
        if Trim(oTable.ValuesByIndex[j])<>'' then
          FResource.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

function TVersionInfo.VersionToString(PV: TFileProductVersion): string;
begin
  result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
end;

function TVersionInfo.GetProductVersion: string;
begin
  if BuildInfoAvailable then
    result := VersionToString(FixedInfo.ProductVersion)
  else
    result := 'No build information available';
end;

function TVersionInfo.GetFileVersion: string;
begin
  if BuildInfoAvailable then
    result := VersionToString(FixedInfo.FileVersion)
  else
    result := 'No build information available';
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  result := FVersResource.VarFileInfo;
end;

constructor TVersionInfo.Create;
var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
begin
  inherited Create;
  FVersResource := TVersionResource.Create;
  FResource := TStringList.Create;
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(HINSTANCE, PChar(PtrInt(ResID)), PChar(RT_VERSION));
  if Res = 0 then Exit;

  Stream := TResourceStream.CreateFromID(HINSTANCE, ResID, PChar(RT_VERSION));
  try
    FVersResource.SetCustomRawDataStream(Stream);
    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  finally
    Stream.Free;
  end;
  GetResourceStrings();
end;

destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;
  FResource.Free;
  inherited Destroy;
end;

function TVersionInfo.GetWidgetSet: string;
begin
  case InterfaceBase.WidgetSet.LCLPlatform of
   lpGtk:    result := WIDGETSET_GTK;
   lpGtk2:   result := WIDGETSET_GTK2;
   lpWin32:  result := WIDGETSET_WIN;
   lpWinCE:  result := WIDGETSET_WINCE;
   lpCarbon: result := WIDGETSET_CARBON;
   lpQT:     result := WIDGETSET_QT;
   lpfpGUI:  result := WIDGETSET_fpGUI;
  else
    result := WIDGETSET_OTHER;
  end;
end;

function TVersionInfo.GetCompilerInfo: string;
begin
  result := {$ifdef fpc}'Free Pascal FPC ' + {$I %FPCVERSION%}{$else}'Delphi'{$endif}
end;

function TVersionInfo.GetTargetInfo: string;
begin
  result := {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
end;

function TVersionInfo.GetOS: string;
begin
  result := {$I %FPCTARGETOS%};
end;

function TVersionInfo.GetCpu: string;
begin
  result := {$I %FPCTARGETCPU%};
end;

function TVersionInfo.GetLCLVersion: string;
begin
  result := {$ifdef fpc}'Lazarus LCL ' + lcl_version{$else}'unknown'{$endif};
end;

function TVersionInfo.GetLCLFullVersion: string;
begin
  result := {$ifdef fpc}'Lazarus LCL ' + IntToStr(lcl_major) + '.' + IntToStr(lcl_minor) + '.' + IntToStr(lcl_release) + '.' + IntToStr(lcl_patch){$else}'unknown'{$endif};
end;

function TVersionInfo.GetCompilatonTime: string;
var
  fs: TFormatSettings;
  dt: TDateTime;
begin
  {$ifdef windows}
  GetLocaleFormatSettings(2057, fs);
  fs.TimeSeparator := ':';
  fs.DecimalSeparator := '.';
  fs.ShortDateFormat := 'yyyy"/"mm"/"dd';
  fs.LongTimeFormat := 'hh":"nn":"ss';
  fs.DateSeparator := '/';
  dt := StrToDateTime({$I %DATE%} + ' ' + {$I %TIME%}, fs);
  result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', dt);
  {$else}
  result := 'not implemented';
  {$endif}
end;

function GetFileVersion: String;
begin
  Result := '';
  with TVersionInfo.Create do
  try
    if BuildInfoAvailable then
      result := FileVersion
    else
      result := '(not available)';
  finally
    Free;
  end;
end;

end.
