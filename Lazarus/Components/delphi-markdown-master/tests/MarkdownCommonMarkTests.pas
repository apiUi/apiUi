﻿Unit MarkdownCommonMarkTests;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes, Character, {$IFDEF WINDOWS} ShellApi, {$ENDIF} Generics.Collections,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} TestFramework, {$ENDIF}
  {$IFDEF FPC} FPJson, JsonParser, {$ELSE} Json, {$ENDIF}
  CommonTestBase, MarkdownCommonMark;

type
  {$IFDEF FPC}
  TJSONValue = TJSONEnum;
  {$ENDIF}

  { TMarkdownCommonMarkTestBase }

  TMarkdownCommonMarkTestBase = class abstract (TCommonTestSuiteCase)
  private
    FTest : TJSONObject;
  protected
    procedure DoTest(tests : TJSONArray; Name : String);
  public
    Constructor Create(test : TJSONObject; name : String);
  end;

  TMarkdownCommonMarkTest = class (TMarkdownCommonMarkTestBase)
  public
    procedure TestCase(Name : String); override;
  end;

  TMarkdownCommonMarkTests = class (TCommonTestSuite)
  public
    constructor Create; override;
  end;

  TMarkdownGFMTest = class (TMarkdownCommonMarkTestBase)
  public
    procedure TestCase(Name : String); override;
  end;

  TMarkdownGFMTests = class (TCommonTestSuite)
  public
    constructor Create; override;
  end;

procedure RegisterTests;

implementation

var
  gTestsBase : TJSONArray = nil;
  gTestsGFM : TJSONArray = nil;

function TestFileCM : String;
begin
  result := IncludeTrailingPathDelimiter(MDTestRoot) + 'resources/commonmark/spec.json';
end;

function TestFileGFM : String;
begin
  result := IncludeTrailingPathDelimiter(MDTestRoot) + 'resources/commonmark/gfm_tests.json';
end;

// ** Utilities ****************************************************************

procedure FreeTests;
begin
  gTestsBase.Free;
  gTestsGFM.Free;
end;

function leftPad(s : String; c : char; l :integer) : String;
begin
  result := s;
  while result.Length < l do
    insert(c, result, 1);
end;

function loadJson(filename : String) : TJsonArray;
var
  f : TFileStream;
  b : TBytes;
  s : String;
  {$IFDEF FPC}
  json : TJSONParser;
  {$ENDIF}
begin
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    SetLength(b, f.Size);
    f.Read(b[0], f.Size);
  finally
    f.Free;
  end;
  s := TEncoding.UTF8.GetString(b);
  {$IFDEF FPC}
  json := TJSONParser.create(s);
  try
    result := json.Parse as TJsonArray;
  finally
    json.free;
  end;
  {$ELSE}
  result :=  TJSONObject.ParseJSONValue(s) as TJSONArray;
  {$ENDIF}
end;

function jsonStr(obj : TJsonObject; name : String; isNumber : boolean = false) : String;
begin
 {$IFDEF FPC}
 result := obj.Strings[name];
 {$ELSE}
 if isNumber then
   result := (obj.values[name] as TJsonString).Value
 else
   result := (obj.values[name] as TJsonString).Value;
 {$ENDIF}
end;

function getTestsBase : TJSONArray;
begin
  if gTestsBase = nil then
    gTestsBase := loadJson(TestFileCM);
  result := gTestsBase;
end;

function getTestsGFM : TJSONArray;
begin
  if gTestsGFM = nil then
    gTestsGFM := loadJson(TestFileGFM);
  result := gTestsGFM;
end;

// ** Test Set up **************************************************************

{ TMarkdownCommonMarkTests }

constructor TMarkdownCommonMarkTests.create;
var
  i : integer;
  t : TJSONObject;
  tests : TJSONArray;
begin
  inherited Create;
  tests := getTestsBase;
  for i := 0 to tests.Count - 1 do
  begin
    t := tests.Items[i] as TJSONObject;
    {$IFDEF FPC}
    if (t.Find('mode') = nil) then
      AddTest(TMarkdownCommonMarkTest.Create(t, leftPad(t.Strings['example'], '0', 4)));
    {$ELSE}
    if (t.Values['mode'] = nil) then
      AddTest(TMarkdownCommonMarkTest.Create(t, leftPad(t.Values['example'].ToString, '0', 4)));
    {$ENDIF}
  end;
end;

{ TMarkdownGFMTests }

constructor TMarkdownGFMTests.create;
var
  i : integer;
  t : TJSONObject;
  tests : TJSONArray;
begin
  inherited Create;
  tests := getTestsGFM;
  for i := 0 to tests.Count - 1 do
  begin
    t := tests.Items[i] as TJSONObject;
    {$IFDEF FPC}
    if (t.Find('mode') = nil) then
      AddTest(TMarkdownGFMTest.Create(t, leftPad(t.Strings['example'], '0', 4)));
    {$ELSE}
    if (t.Values['mode'] = nil) then
      AddTest(TMarkdownGFMTest.Create(t, leftPad(t.Values['example'].ToString, '0', 4)));
    {$ENDIF}
  end;
end;

{ TMarkdownCommonMarkTest }

procedure TMarkdownCommonMarkTest.TestCase(Name: String);
begin
  DoTest(getTestsBase, name);
end;

{ TMarkdownGFMTest }

procedure TMarkdownGFMTest.TestCase(Name: String);
begin
  DoTest(getTestsGFM, name);
end;

{ TMarkdownCommonMarkTestBase }

constructor TMarkdownCommonMarkTestBase.Create(test: TJSONObject; name: String);
begin
  inherited Create(name);
  FTest := test;
end;

procedure TMarkdownCommonMarkTestBase.DoTest(tests : TJSONArray; Name : String);
var
  doc : TCommonMarkDocument;
  src, html, exp : String;
begin
  src := jsonStr(FTest, 'markdown').replace('\n', #10);
  doc := TCommonMarkEngine.parse(src, self is TMarkdownGFMTest);
  try
    html := TCommonMarkEngine.render(doc);
  finally
    doc.Free;
  end;
  exp := jsonStr(FTest, 'html').replace('\n', #10);
  assertEqual(exp, html, 'output does not match expected for input "'+src+'"');
end;


procedure RegisterTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Markdown.CommonMark', TMarkdownCommonMarkTests.create);
  RegisterTest('Markdown.GFM', TMarkdownGFMTests.create);
end;

initialization
finalization
  freeTests;
end.
