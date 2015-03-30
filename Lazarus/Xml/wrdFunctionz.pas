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

uses SysUtils, Variants, Classes{$ifdef windows}, OleServer, Word_TLB, ComObj{$endif};

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
  OriginalFormat, RouteDocument, CompareTargetNew, DetectFormatChanges, xTrue, xFalse: Olevariant;
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
    CompareTargetNew := wdCompareTargetNew;
    DetectFormatChanges := wrdDetectFormatChanges;
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
                   , Null
                   , CompareTargetNew
                   , DetectFormatChanges
                   , xTrue // ignorecomparisonwarnings
                   , xFalse // addtorecentfiles
                   , xTrue // removepersonalinformation
                   , xTrue // removedatetime
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
  raise Exception.Create ('only with ms windos');
  {$endif}
end;

procedure wrdFileDiffencesShow (aNewFile, aRefFile: String);
var
  FileName, CompareFileName, ConfirmConversions, ReadOnly, AddToRecentFiles,
  PasswordDocument, PasswordTemplate, Revert,
  WritePasswordDocument,
  WritePasswordTemplate,
  Format, Encoding,
  Visible, DoNotSaveChanges,
  OriginalFormat, RouteDocument, CompareTargetNew, DetectFormatChanges, xTrue, xFalse: Olevariant;
  ndoc, cdoc: Variant;
  word: Variant;
begin
  {$ifdef windows}
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
  CompareTargetNew := wdCompareTargetNew;
  DetectFormatChanges := wrdDetectFormatChanges;
  Encoding := '';
  visible := true;
  DoNotSaveChanges := wdDoNotSaveChanges;
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
                   , CompareTargetNew
                   , DetectFormatChanges
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
      ndoc.Close (DoNotSaveChanges, OriginalFormat, RouteDocument);
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
  xText: OleVariant;
  xFileName: OleVariant;
begin
  {$ifdef windows}
  xText := aText;
  xFileName := aFileName;
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
      ndoc.Close (wdDoNotSaveChanges);
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
begin
  {$ifdef windows}
  xText := aText;
  xFileName := aFileName;
  xFormat := wdFormatPDF;
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
      ndoc.Close (wdDoNotSaveChanges);
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
  if wrdInstalled
  and not VarIsNull (wrdApplication) then
  begin
    wrdApplication.Quit;
    wrdApplication := null;
  end;
  {$endif}
end;

initialization;
  wrdApplication := null;
end.
