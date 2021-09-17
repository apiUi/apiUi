{
This file is part of the apiUi project
Copyright (c) 2009-2021 by Jan Bouwman

See the file COPYING, included in this distribution,
for details about the copyright.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit SelectXmlElement;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls
  , xmlio
  , Wsdlz
  , Xmlz
  , Bind
  , Ipmz
  , igGlobals
  , FormIniFilez, ActnList
  ;

  type TRequestedStringType = (rsThread, rsElementName);
  type
  TSelectXmlElementForm = class(TForm)
    TreeViewPanel: TPanel;
    TreeView: TTreeView;
    Panel1: TPanel;
    ThreadButton: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    OKAction: TAction;
    ElementButton: TButton;
    procedure OKActionUpdate(Sender: TObject);
    procedure OKActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ElementButtonClick(Sender: TObject);
  private
    Captions: TJBStringList;
    fSkipRootNode: Boolean;
    fWsdlOperation: TWsdlOperation;
    procedure ViewXmlItem ( aTreeView: TTreeView
                          ; aNode: TTreeNode
                          ; aBind: TCustomBindable
                          ; aIncludeRecurring: Boolean
                          );
    function getCurrentCaption: String;
    procedure SelectNodeWithCaption (aCaption: String);
    procedure setWsdlOperation(const Value: TWsdlOperation);
  public
    LastCaption: String;
    SelectedCaption: String;
    SelectedBind: TCustomBindable;
    SrceBind: TCustomBindable;
    IncludeRecurring, IncludeInvoked: Boolean;
    maxOccurrences: Integer;
    ElementEnabled: Boolean;
    doShowReq, doShowRpy, doShowWsa, doShowRti, doShowReqRpyInfo: Boolean;
    property CurrentCaption: String read getCurrentCaption;
    property SkipRootNode: Boolean read fSkipRootNode write fSkipRootNode;
    property WsdlOperation: TWsdlOperation read fWsdlOperation write setWsdlOperation;
  end;

var
  SelectXmlElementForm: TSelectXmlElementForm;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSelectXmlElementForm.FormShow(Sender: TObject);
var
  xChild, rChild: TTreeNode;
  x, f: Integer;
  xOperation: TWsdlOperation;
  xBindName: String;
  procedure ShowAllControlBinds(aList: TBindableList;
    aCaption: String);
  var
    x: Integer;
  begin
    if Assigned (aList) then
    begin
      xChild := TreeView.Items.AddChildObject (nil, aCaption, nil);
      for x := 0 to aList.Count - 1 do
      begin
        TreeView.Items.AddChildObject
          ( xChild
          , aList.Strings [x]
          , aList.Bindables [x]
          );
      end;
    end;
  end;

  procedure ShowXmls (aBind: TCustomBindable; aCaption: String);
  begin
    if Assigned (aBind)
//  and (aXml.Items.Count > 0)
    then
    begin
      if not SkipRootNode then
      begin
        xChild := TreeView.Items.AddChildObject (nil, aCaption, nil);
      end
      else
        xChild := nil;
      ViewXmlItem ( TreeView
                  , xChild
                  , aBind
                  , IncludeRecurring
                  );
{
      for x := 0 to aXml.Items.Count - 1 do
      begin
        ViewXmlItem ( TreeView
                    , xChild
                    , aXml.Items.XmlItems [x]
                    , (cCaption = aCaption)
                    , nCaption
                    , IncludeRecurring
                    );
      end;
}
    end;
  end;
begin
  Captions := TJBStringList.Create;
  Captions.Sorted := True;
  if not ElementEnabled then
  begin
    ThreadButton.Caption := '&Ok';
    ElementButton.Visible := False;
  end;
  try
    TreeView.Items.Clear;
    if doShowReq then
    begin
      Captions.Clear;
      rChild := TreeView.Items.AddChildObject (nil, 'Req', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (    IncludeInvoked
            and Assigned (WsdlOperation.invokeList)
            and WsdlOperation.invokeList.Find(allOperations.Operations[x].Alias, f)
           ) then
        begin
          if (allOperations.Operations[x] = WsdlOperation.Cloned) then
            xOperation := WsdlOperation
          else
            xOperation := allOperations.Operations [x];
          if Assigned (xOperation.reqBind)
          and (xOperation.reqBind.Children.Count > 0) then
          begin
            xChild := rChild;
            if (not Assigned (WsdlOperation))
            and (xOperation.reqBind is TIpmItem) then
              xChild := TreeView.Items.AddChildObject (rChild, allOperations.Operations[x].Name, nil);
            xBindName := xOperation.reqBind.Name;
            if xOperation.Alias <> xOperation.reqTagName then
              xOperation.reqBind.Name := xOperation.Alias;
            ViewXmlItem ( TreeView
                        , xChild
                        , xOperation.reqBind
                        , IncludeRecurring
                        );
            xOperation.reqBind.Name := xBindName;
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if doShowRpy then
    begin
      Captions.Clear;
      rChild := TreeView.Items.AddChildObject (nil, 'Rpy', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (    IncludeInvoked
            and Assigned (WsdlOperation.invokeList)
            and WsdlOperation.invokeList.Find(allOperations.Operations[x].Alias, f)
           ) then
        begin
          if (allOperations.Operations[x] = WsdlOperation.Cloned) then
            xOperation := WsdlOperation
          else
            xOperation := allOperations.Operations [x];
          if Assigned (xOperation.rpyBind)
          and (xOperation.rpyBind.Children.Count > 0) then
          begin
            xChild := rChild;
            if (not Assigned (WsdlOperation))
            and (allOperations.Operations[x].rpyBind is TIpmItem) then
              xChild := TreeView.Items.AddChildObject (rChild, allOperations.Operations[x].Name, nil);
            xBindName := xOperation.RpyBind.Name;
            if xOperation.Alias <> xOperation.reqTagName then  // yes, compare with tagname
              xOperation.rpyBind.Name := xOperation.Alias;
            ViewXmlItem ( TreeView
                        , xChild
                        , xOperation.rpyBind
                        , IncludeRecurring
                        );
            xOperation.RpyBind.Name := xBindName;
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if Assigned (SrceBind) then
      ShowXmls (SrceBind, SrceBind.Name);
    if doShowReqRpyInfo then
    begin
      Captions.Clear;
      rChild := TreeView.Items.AddChildObject (nil, 'requestInfo', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (    IncludeInvoked
            and Assigned (WsdlOperation.invokeList)
            and WsdlOperation.invokeList.Find(allOperations.Operations[x].Alias, f)
           ) then
        begin
          if (allOperations.Operations[x] = WsdlOperation.Cloned) then
            xOperation := WsdlOperation
          else
            xOperation := allOperations.Operations [x];
          if Assigned (xOperation.requestInfoBind) then
          begin
            xChild := rChild;
            xBindName := xOperation.requestInfoBind.Name;
            if xOperation.Alias <> xOperation.reqTagName then  // yes, compare with tagname
              xOperation.requestInfoBind.Name := xOperation.Alias;
            ViewXmlItem ( TreeView
                        , xChild
                        , xOperation.requestInfoBind
                        , IncludeRecurring
                        );
            xOperation.requestInfoBind.Name := xBindName;
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if doShowReqRpyInfo then
    begin
      Captions.Clear;
      rChild := TreeView.Items.AddChildObject (nil, 'replyInfo', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (    IncludeInvoked
            and Assigned (WsdlOperation.invokeList)
            and WsdlOperation.invokeList.Find(allOperations.Operations[x].Alias, f)
           ) then
        begin
          if (allOperations.Operations[x] = WsdlOperation.Cloned) then
            xOperation := WsdlOperation
          else
            xOperation := allOperations.Operations [x];
          if Assigned (xOperation.replyInfoBind) then
          begin
            xChild := rChild;
            xBindName := xOperation.replyInfoBind.Name;
            if xOperation.Alias <> xOperation.rpyTagName then  // yes, compare with tagname
              xOperation.replyInfoBind.Name := xOperation.Alias;
            ViewXmlItem ( TreeView
                        , xChild
                        , xOperation.replyInfoBind
                        , IncludeRecurring
                        );
            xOperation.replyInfoBind.Name := xBindName;
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if Assigned (WsdlOperation) then
    begin
      if doShowRpy
      and Assigned (WsdlOperation.fltBind) then
      begin
        Captions.Clear;
        xChild := TreeView.Items.AddChildObject (nil, 'Faults', nil);
        for x := 0 to WsdlOperation.fltBind.Children.Count - 1 do
          ViewXmlItem ( TreeView
                      , xChild
                      , WsdlOperation.fltBind.Children.Bindables [x]
                      , IncludeRecurring
                      );
      end;
      if doShowWsa then
      begin
        ShowXmls (WsdlOperation.reqWsaXml, 'reqWsa');
        ShowXmls (WsdlOperation.rpyWsaXml, 'rpyWsa');
      end;
      if doShowRti then
        if Assigned (_WsdlRtiXml) then
          for x := 0 to _WsdlRtiXml.Children.Count - 1 do
            ShowXmls (_WsdlRtiXml.Children.Bindables[x], _WsdlRtiXml.Name);
    end;
    TreeView.SetFocus;
    Captions.Clear;
  finally
    Captions.Free;
    SelectNodeWithCaption (LastCaption);
  end;
end;

procedure TSelectXmlElementForm.ElementButtonClick(Sender: TObject);
begin
  if TreeView.Selected <> nil then
  begin
    SelectedBind := TCustomBindable(TreeView.Selected.Data);
    SelectedCaption := TreeView.Selected.Text;
  end
  else
  begin
    SelectedCaption := '';
    SelectedBind := nil;
  end;
end;

procedure TSelectXmlElementForm.FormCreate(Sender: TObject);
begin
  with TFormIniFile.Create (Self, True) do
  try
    Restore;
  finally
    Free;
  end;
  maxOccurrences := MaxInt - 1;
  ElementEnabled := False;
end;

procedure TSelectXmlElementForm.FormDestroy(Sender: TObject);
begin
  with TFormIniFile.Create(self, False) do
  try
    Save;
  finally
    Free;
  end;
end;

function TSelectXmlElementForm.getCurrentCaption: String;
  function _CurrentCaption (aNode: TTreeNode): String;
  begin
    if aNode = nil then
      result := ''
    else
      if aNode.Parent = nil then
        result := aNode.Text
      else
        result := _CurrentCaption (aNode.Parent)
                + '.'
                + aNode.Text
                ;
  end;
begin
  result := _CurrentCaption (TreeView.Selected);
end;

procedure TSelectXmlElementForm.ViewXmlItem(aTreeView: TTreeView; aNode: TTreeNode;
  aBind: TCustomBindable; aIncludeRecurring: Boolean);
var
  x: Integer;
  xChild: TTreeNode;
  xBind: TCustomBindable;
  xSubItems: TBindableList;
begin
  if aBind = nil then
    exit;
  if (not aIncludeRecurring)
  and Captions.Find(aBind.FullCaption, x) then
    exit;
  if (aBind is TIpmItem)
  and ((aBind as TIpmItem).Occurrence > maxOccurrences) then
    Exit;
  if (aBind is TXml)
  and ((aBind as TXml).Occurrence > maxOccurrences) then
    Exit;
{
  if (aIpmItem.Occurs > 1) and (not aIncludeRecurring) then
    exit;
}
  xChild := aTreeView.Items.AddChildObject (aNode, aBind.GetCaption, aBind);
  Captions.Add(aBind.GetFullCaption);
  xSubItems := aBind.Children;
  for x := 0 to xSubItems.Count - 1 do
  begin
    xBind := xSubItems.Bindables [x];
    ViewXmlItem ( aTreeView
                , xChild
                , xBind
                , aIncludeRecurring
                );
  end;
  if aBind is TXml then
  begin
    for x := 0 to (aBind as TXml).Attributes.Count - 1 do
    begin
      aTreeView.Items.AddChildObject ( xChild
                                     , (aBind as TXml).Attributes.XmlAttributes[x].Name
                                     , (aBind as TXml).Attributes.XmlAttributes[x]
                                     );
      Captions.Add((aBind as TXml).FullCaption + '.' + (aBind as TXml).Attributes.XmlAttributes[x].Name);
    end;
  end;
  xChild.Collapse(True);
end;

procedure TSelectXmlElementForm.OKActionExecute(Sender: TObject);
  function _FullCaption (aNode: TTreeNode): String;
  begin
    if not Assigned (aNode.Parent) then
      result := aNode.Text
    else
      result := _FullCaption (aNode.Parent) + '.' + aNode.Text;
  end;
begin
  if TreeView.Selected <> nil then
  begin
    SelectedBind := TCustomBindable(TreeView.Selected.Data);
    SelectedCaption := _FullCaption (TreeView.Selected)
  end
  else
  begin
    SelectedCaption := '';
    SelectedBind := nil;
  end;
end;

procedure TSelectXmlElementForm.OKActionUpdate(Sender: TObject);
begin
  OKAction.Enabled := Assigned (Treeview.Selected)
                  and Assigned (TreeView.Selected.Parent)
                  ;
end;

procedure TSelectXmlElementForm.SelectNodeWithCaption(aCaption: String);
  function _NextFullCaption (aCaption: String): String;
  var
    dotPos: Integer;
  begin
    dotPos := Pos ('.', aCaption);
    if dotPos = 0 then
      result := ''
    else
      result := Copy (aCaption, dotPos + 1, Length (aCaption));
  end;

  function _ThisCaption (aCaption: String): String;
  var
    dotPos: Integer;
  begin
    dotPos := Pos ('.', aCaption);
    if dotPos = 0 then
      result := aCaption
    else
      result := LeftStr (aCaption, dotPos - 1);
  end;

  procedure _select (aNode: TTreeNode; cCaption, nCaption: String);
  var
    xNode: TTreeNode;
  begin
    if aNode.Text <> cCaption then Exit;
    TreeView.Selected := aNode;
    cCaption := _ThisCaption(nCaption);
    nCaption := _NextFullCaption(nCaption);
    xNode := aNode.getFirstChild;
    while Assigned (xNode) do
    begin
      _select (xNode, cCaption, nCaption);
      xNode := aNode.GetNextChild(xNode);
    end;
  end;
var
  cCaption, nCaption: String;
  x: Integer;
begin
  TreeView.Selected := nil;
  cCaption := _ThisCaption(aCaption);
  nCaption := _NextFullCaption(aCaption);
  for x := 0 to TreeView.Items.Count - 1 do
    if not Assigned (TreeView.Items.Item[x].Parent) then
      _select (TreeView.Items.Item[x], cCaption, nCaption);
end;

procedure TSelectXmlElementForm.setWsdlOperation(const Value: TWsdlOperation);
begin
  fWsdlOperation := Value;
end;

end.
