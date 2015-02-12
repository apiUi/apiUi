unit wrdFunctionz;

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

uses SysUtils, Variants, Classes, OleServer, Word_TLB, ComObj;

var
  wrdApplication: Variant;

function wrdFileDiffencesCount (aNewFile, aRefFile: String): Integer;
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
end;

procedure wrdFileDiffencesShow (aNewFile, aRefFile: String);
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
end;

procedure wrdStringToFile (aText, aFileName: String);
var
  ndoc, cdoc: Variant;
  word: Variant;
begin
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    Word.DisplayAlerts := False;
    word.Visible := False;
//  ndoc := word.Documents.Add ( ,,, );
    ndoc := word.Documents.Add ();
    try
      ndoc.Range.Text := aText;
      nDoc.SaveAs2(aFileName);
    finally
      ndoc.Close (wdDoNotSaveChanges);
    end;
  finally
    Word := Null;
  end;
end;

procedure wrdStringToPdfFile (aText, aFileName: String);
var
  ndoc: Variant;
  word: Variant;
begin
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    Word.DisplayAlerts := False;
    word.Visible := False;
//  ndoc := word.Documents.Add (,,,);
    ndoc := word.Documents.Add ();
    try
      ndoc.Range.Text := aText;
      nDoc.SaveAs2(aFileName, wdFormatPDF);
    finally
      ndoc.Close (wdDoNotSaveChanges);
    end;
  finally
    Word := Null;
  end;
end;

procedure wrdInitialize;
begin
  try
    wrdApplication := CreateOleObject('Word.Application');
    wrdApplication.DisplayAlerts := False;
    wrdApplication.Visible := False;
    wrdInstalled := True;
  except
    wrdInstalled := False;
  end;
end;

procedure wrdUninitialize;
begin
  if wrdInstalled
  and not VarIsNull (wrdApplication) then
  begin
    wrdApplication.Quit;
    wrdApplication := null;
  end;
end;

end.
