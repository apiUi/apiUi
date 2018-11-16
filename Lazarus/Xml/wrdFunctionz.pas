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

uses SysUtils, Variants, Classes{$ifdef windows}, Word_TLB, ComObj{$endif};

var
  wrdApplication: Variant;

function wrdFileDiffencesCount (aNewFile, aRefFile: String): Integer;
var
  xFileName, xNewFile, xAuthorName, xConfirmConversions, xReadOnly, xAddToRecentFiles,
  xPasswordDocument, xPasswordTemplate, xRevert,
  xWritePasswordDocument,
  xWritePasswordTemplate,
  xFormat, xEncoding,
  xVisible, xDoNotSaveChanges,
  xOriginalFormat, xRouteDocument, xCompareTargetNew, xDetectFormatChanges, xTrue, xFalse: Olevariant;
  ndoc, cdoc: Variant;
  xDoUninitialise: Boolean;
begin
  {$ifdef windows}
  result := -1;
  xDoUninitialise := False;
  if VarIsNull (wrdApplication) then
  begin
    wrdInitialize;
    xDoUninitialise := True;
  end;
  try
    xTrue := True;
    xFalse := False;
    xFileName := aRefFile;
    xNewFile := aNewFile;
    xConfirmConversions := true ;
    xReadOnly :=  false;
    xAddToRecentFiles := false;
    xAuthorName := '';
    xPasswordDocument := '';
    xPasswordTemplate := '';
    xWritePasswordTemplate := '';
    xRevert := false;
    xWritePasswordDocument := '';
    xWritePasswordTemplate := '';
    xFormat := wdOpenFormatAuto;
    xEncoding := '';
    xvisible := true;
    xDoNotSaveChanges := wdDoNotSaveChanges;
    xCompareTargetNew := wdCompareTargetNew;
    xDetectFormatChanges := wrdDetectFormatChanges;
    ndoc := wrdApplication.Documents.Open ( xFileName
                                          , xConfirmConversions
                                          , xReadOnly
                                          , xAddToRecentFiles
                                          , xPasswordDocument
                                          , xPasswordTemplate
                                          , xRevert
                                          , xWritePasswordDocument
                                          , xWritePasswordTemplate
                                          , xFormat
                                          , xEncoding
                                          , xVisible
                                          );
    try
      ndoc.Compare ( xNewFile
                   , xAuthorName
                   , xCompareTargetNew
                   , xDetectFormatChanges
                   , xTrue // ignorecomparisonwarnings
                   , xFalse // addtorecentfiles
                   , xTrue // removepersonalinformation
                   , xTrue // removedatetime
                   );
      cdoc := wrdApplication.ActiveDocument;
      result := cdoc.Revisions.Count - wrdExpectedDifferenceCount;
      cdoc.Close (xDoNotSaveChanges, xOriginalFormat, xRouteDocument);
    finally
      ndoc.Close (xDoNotSaveChanges, xOriginalFormat, xRouteDocument);
    end;
  finally
    if xDoUninitialise then
      wrdUninitialize;
  end;
  {$else}
  raise Exception.Create ('only with ms windos');
  {$endif}
end;

procedure wrdFileDiffencesShow (aNewFile, aRefFile: String);
var
  xFileName, xCompareFileName, xAuthorName, xConfirmConversions, xReadOnly, xAddToRecentFiles,
  xPasswordDocument, xPasswordTemplate, xRevert,
  xWritePasswordDocument,
  xWritePasswordTemplate,
  xFormat, xEncoding,
  xVisible, xDoNotSaveChanges,
  xOriginalFormat, xRouteDocument, xCompareTargetNew, xDetectFormatChanges, xTrue, xFalse: Olevariant;
  ndoc, cdoc: Variant;
  word: Variant;
begin
  {$ifdef windows}
  if wrdNewDocumentAsReference then
  begin
    xFileName := aNewFile;
    xCompareFileName := aRefFile;
  end
  else
  begin
    xFileName := aRefFile;
    xCompareFileName := aNewFile;
  end;
  xAuthorName := '';
  xConfirmConversions := true ;
  xReadOnly :=  false;
  xAddToRecentFiles := false;
  xPasswordDocument := '';
  xPasswordTemplate := '';
  xWritePasswordTemplate := '';
  xRevert := false;
  xWritePasswordDocument := '';
  xWritePasswordTemplate := '';
  xFormat := wdOpenFormatAuto;
  xCompareTargetNew := wdCompareTargetNew;
  xDetectFormatChanges := wrdDetectFormatChanges;
  xEncoding := '';
  xvisible := true;
  xDoNotSaveChanges := wdDoNotSaveChanges;
  xTrue := True;
  xFalse := False;
  try
    word := CreateOleObject('Word.Application');
  except
    on e: Exception do
      raise Exception.Create('Error initialising Word: ' + e.Message);
  end;
  try
    Word.DisplayAlerts := False;
    word.Visible := False;
    ndoc := word.Documents.Open ( xFileName
                                , xConfirmConversions
                                , xReadOnly
                                , xAddToRecentFiles
                                , xPasswordDocument
                                , xPasswordTemplate
                                , xRevert
                                , xWritePasswordDocument
                                , xWritePasswordTemplate
                                , xFormat
                                , xEncoding
                                , xVisible
                                );
    try
      ndoc.Compare ( xCompareFileName
                   , xAuthorName
                   , xCompareTargetNew
                   , xDetectFormatChanges
                   , xTrue // ignorecomparisonwarnings
                   , xFalse // addtorecentfiles
                   , xTrue // removepersonalinformation
                   , xTrue // removedatetime
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
      ndoc.Close (xDoNotSaveChanges, xOriginalFormat, xRouteDocument);
    end;
  finally
    Word := Null;
  end;
  {$else}
  raise Exception.Create('only with ms windows');
  {$endif}
end;

procedure wrdStringToFile (aText, aFileName: String);
var
  ndoc: Variant;
  word: Variant;
  xText, xFileName, xDoNotSaveChanges: OleVariant;
begin
  {$ifdef windows}
  xText := aText;
  xFileName := aFileName;
  xDoNotSaveChanges := wdDoNotSaveChanges;
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
      ndoc.Range.Text := xText;
      nDoc.SaveAs2(xFileName);
    finally
      ndoc.Close (xDoNotSaveChanges);
    end;
  finally
    try
      Word.Quit;
    finally
      Word := Null;
    end;
  end;
  {$else}
  raise Exception.Create('only with ms windows');
  {$endif}
end;

procedure wrdStringToPdfFile (aText, aFileName: String);
var
  ndoc: Variant;
  word: Variant;
  xText: OleVariant;
  xFileName: OleVariant;
  xFormat: OleVariant;
  xDoNotSaveChanges: OleVariant;
begin
  {$ifdef windows}
  xText := aText;
  xFileName := aFileName;
  xFormat := wdFormatPDF;
  xDoNotSaveChanges := wdDoNotSaveChanges;
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
      ndoc.Range.Text := xText;
      nDoc.SaveAs2(xFileName, xFormat);
    finally
      ndoc.Close (xDoNotSaveChanges);
    end;
  finally
    try
      Word.Quit;
    finally
      Word := Null;
    end;
  end;
  {$else}
  raise Exception.Create('only with ms windows');
  {$endif}
end;

procedure wrdInitialize;
begin
  {$ifdef windows}
  if not wrdInstalled then
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
  {$ifdef windows}
  {$endif}
end;

initialization;
  wrdApplication := null;
finalization;
  {$ifdef windows}
  try
    if wrdInstalled
    and not VarIsNull (wrdApplication) then
    begin
      wrdApplication.Quit;
      wrdApplication := null;
    end;
  except
  end;
  {$endif}
end.
