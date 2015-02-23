unit testerUnit ;

{$mode objfpc}{$H+}

interface

uses
  Classes , SysUtils , FileUtil , Forms , Controls ,
  Graphics , Dialogs , StdCtrls , Word_8_5_TLB ;

type

  { TForm1 }

  TForm1 = class(TForm )
    Button1 : TButton ;
    Button2 : TButton ;
    procedure Button1Click (Sender : TObject );
    procedure Button2Click (Sender : TObject );
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1 : TForm1 ;

implementation

{$R *.lfm}
uses Variants, OleServer, ComObj;

{ TForm1 }

procedure TForm1 .Button1Click (Sender : TObject );
var
  ndoc, cdoc: Variant;
  word: Variant;
  wdoc: TAxcDocument;
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
      ndoc.Range.Text := 'JanBo was here';
      nDoc.SaveAs2('c:\temp\t.docx');
    finally

      ndoc.Close (wdDoNotSaveChanges);
    end;
  finally
    Word := Null;
  end;
end;

procedure TForm1 .Button2Click (Sender : TObject );
var
  FileName, CompareFileName, ConfirmConversions, ReadOnly, AddToRecentFiles,
  PasswordDocument, PasswordTemplate, Revert,
  WritePasswordDocument,
  WritePasswordTemplate,
  Format, Encoding,
  Visible_, DoNotSaveChanges,
  OriginalFormat, RouteDocument: Olevariant;
  ndoc, cdoc: Variant;
  word: Variant;
begin
    FileName := 'c:\temp\t.docx';
    CompareFileName := 'c:\temp\u.docx';
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
  visible_ := true;
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
                                , Visible_
                                );
    try
      ndoc.Compare ( CompareFileName
                   , // authorname
                   , wdCompareTargetNew
                   , False //DetectFormatChanges
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

end.

