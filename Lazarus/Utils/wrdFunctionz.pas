{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit wrdFunctionz;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

function wrdFileDiffencesCount (aNewFile, aRefFile: String): Integer;
procedure wrdFileDiffencesShow (aNewFile, aRefFile: String);
procedure wrdStringToFile (aText, aFileName: String);
procedure wrdStringToPdfFile (aText, aFileName: String);
procedure wrdInitialize;
procedure wrdUninitialize;

var
  wrdInstalled: Boolean;
  wrdDetectFormatChanges, wrdNewDocumentAsReference: Boolean;
  wrdExpectedDifferenceCount: Integer;

implementation

uses SysUtils
   , Variants
   , Classes
   {$ifndef UNIX}
   , OleServer
   , Word_TLB
   , ComObj
   {$endif}
   ;

var
  wrdApplication: Variant;

function wrdFileDiffencesCount (aNewFile, aRefFile: String): Integer;
{$ifdef WINDOWS}
var
  FileName, ConfirmConversions, ReadOnly, AddToRecentFiles,
  PasswordDocument, PasswordTemplate, Revert,
  WritePasswordDocument,
  WritePasswordTemplate,
  Format, Encoding,
  Visible, DoNotSaveChanges,
  OriginalFormat, RouteDocument: Olevariant;
  ndoc, cdoc: Variant;
  xDoUninitialise: Boolean;
begin
  result := -1;
  xDoUninitialise := False;
  if VarIsNull (wrdApplication) then
  begin
    wrdInitialize;
    xDoUninitialise := True;
  end;
  try
    FileName := aRefFile;
    ConfirmConversions := true ;
    ReadOnly :=  false;
    AddToRecentFiles := true;
    PasswordDocument := '';
    PasswordTemplate := '';
    WritePasswordTemplate := '';
    Revert := false;
    WritePasswordDocument := '';
    WritePasswordTemplate := '';
    Format := wdOpenFormatAuto;
    Encoding := '';
    visible := true;
    DoNotSaveChanges := wdDoNotSaveChanges;
    ndoc := wrdApplication.Documents.Open ( FileName
                                          , ConfirmConversions
                                          , ReadOnly
                                          , AddToRecentFiles
                                          , PasswordDocument
                                          , PasswordTemplate
                                          , Revert
                                          , WritePasswordDocument
                                          , WritePasswordTemplate
                                          , Format
                                          , Encoding
                                          , Visible
                                          );
    try
      ndoc.Compare ( aNewFile
                   , // authorname
                   , wdCompareTargetNew
                   , wrdDetectFormatChanges
                   , True // ignorecomparisonwarnings
                   , False // addtorecentfiles
                   , True // removepersonalinformation
                   , True // removedatetime
                   );
      cdoc := wrdApplication.ActiveDocument;
      result := cdoc.Revisions.Count - wrdExpectedDifferenceCount;
      cdoc.Close (DoNotSaveChanges, OriginalFormat, RouteDocument);
    finally
      ndoc.Close (DoNotSaveChanges, OriginalFormat, RouteDocument);
    end;
  finally
    if xDoUninitialise then
      wrdUninitialize;
  end;
{$else}
begin
  result := 0;
{$endif}
end;

procedure wrdFileDiffencesShow (aNewFile, aRefFile: String);
{$ifdef WINDOWS}
var
  FileName, CompareFileName, ConfirmConversions, ReadOnly, AddToRecentFiles,
  PasswordDocument, PasswordTemplate, Revert,
  WritePasswordDocument,
  WritePasswordTemplate,
  Format, Encoding,
  Visible, DoNotSaveChanges,
  OriginalFormat, RouteDocument: Olevariant;
  ndoc, cdoc: Variant;
  word: Variant;
begin
  if wrdNewDocumentAsReference then
  begin
    FileName := aNewFile;
    CompareFileName := aRefFile;
  end
  else
  begin
    FileName := aRefFile;
    CompareFileName := aNewFile;
  end;
  ConfirmConversions := true ;
  ReadOnly :=  false;
  AddToRecentFiles := true;
  PasswordDocument := '';
  PasswordTemplate := '';
  WritePasswordTemplate := '';
  Revert := false;
  WritePasswordDocument := '';
  WritePasswordTemplate := '';
  Format := wdOpenFormatAuto;
  Encoding := '';
  visible := true;
  DoNotSaveChanges := wdDoNotSaveChanges;
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    Word.DisplayAlerts := False;
    word.Visible := False;
    ndoc := word.Documents.Open ( FileName
                                , ConfirmConversions
                                , ReadOnly
                                , AddToRecentFiles
                                , PasswordDocument
                                , PasswordTemplate
                                , Revert
                                , WritePasswordDocument
                                , WritePasswordTemplate
                                , Format
                                , Encoding
                                , Visible
                                );
    try
      ndoc.Compare ( CompareFileName
                   , // authorname
                   , wdCompareTargetNew
                   , wrdDetectFormatChanges
                   , True // ignorecomparisonwarnings
                   , False // addtorecentfiles
                   , True // removepersonalinformation
                   , True // removedatetime
                   );
      cdoc := word.ActiveDocument;
      cdoc.Saved := 1;
      if cdoc.Revisions.Count > 0 then
      begin
        cdoc.Revisions.Item(1).Range.Select;
      end;
      word.Visible := True;
      word.ActiveWindow.Activate;
    finally
      ndoc.Close (DoNotSaveChanges, OriginalFormat, RouteDocument);
    end;
  finally
    Word := Null;
  end;
{$else}
begin
{$endif}
end;

procedure wrdStringToFile (aText, aFileName: String);
var
  ndoc, cdoc: Variant;
  word: Variant;
begin
{$ifdef WINDOWS}
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    Word.DisplayAlerts := False;
    word.Visible := False;
    ndoc := word.Documents.Add (Null ,Null,Null,Null );
    try
      ndoc.Range.Text := aText;
      nDoc.SaveAs2(aFileName);
    finally
      ndoc.Close (wdDoNotSaveChanges);
    end;
  finally
    Word := Null;
  end;
{$endif}
end;

procedure wrdStringToPdfFile (aText, aFileName: String);
{$ifdef WINDOWS}
var
  ndoc: Variant;
  word: Variant;
  xFileName: OleVariant;
  _Doc: _Document;
begin
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    xFileName := UTF8Decode(aFileName);
    Word.DisplayAlerts := False;
    word.Visible := False;
    ndoc := word.Documents.Add ;
    try
      ndoc.Range.Text := UTF8Decode(aText);
      nDoc.SaveAs2(xFileName, wdFormatPDF, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
    finally
      ndoc.Close (wdDoNotSaveChanges);
    end;
  finally
    Word.Quit;
    Word := Null;
  end;
{$else}
begin
{$endif}
end;

procedure wrdInitialize;
begin
{$ifdef WINDOWS}
  try
    wrdApplication := CreateOleObject('Word.Application');
    wrdApplication.DisplayAlerts := False;
    wrdApplication.Visible := False;
    wrdInstalled := True;
  except
    wrdInstalled := False;
  end;
{$endif}
end;

procedure wrdUninitialize;
begin
{$ifdef WINDOWS}
  if wrdInstalled
  and not VarIsNull (wrdApplication) then
  begin
    wrdApplication.Quit;
    wrdApplication := null;
  end;
{$endif}
end;

end.
