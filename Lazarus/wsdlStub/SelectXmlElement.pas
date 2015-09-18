unit SelectXmlElement;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls
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
    Captions: TStringList;
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
    IncludeRecurring: Boolean;
    maxOccurrences: Integer;
    ElementEnabled: Boolean;
    doShowReq, doShowRpy, doShowMq, doShowWsa, doShowRti: Boolean;
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
  xNode: TTreeNode;
  x, f: Integer;
  xXml: TXml;
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
        xNode := TreeView.Items.AddChildObject
          ( xChild
          , aList.Strings [x]
          , aList.Bindables [x]
          );
      end;
    end;
  end;

  procedure ShowXmls (aBind: TCustomBindable; aCaption: String);
  var
    x: Integer;
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
  Captions := TStringList.Create;
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
      rChild := TreeView.Items.AddChildObject (nil, 'Req', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (WsdlOperation.invokeList.Find(allOperations.Operations[x].reqTagName + ';' + allOperations.Operations[x].reqTagNameSpace, f)) then
        begin
          if allOperations.Operations[x].reqBind.Children.Count > 0 then
          begin
            xChild := rChild;
            if (not Assigned (WsdlOperation))
            and (allOperations.Operations[x].reqBind is TIpmItem) then
              xChild := TreeView.Items.AddChildObject (rChild, allOperations.Operations[x].Name, nil);
            ViewXmlItem ( TreeView
                        , xChild
                        , allOperations.Operations[x].reqBind
                        , IncludeRecurring
                        );
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if doShowRpy then
    begin
      rChild := TreeView.Items.AddChildObject (nil, 'Rpy', nil);
      for x := 0 to allOperations.Count - 1 do
      begin
        if (allOperations.Operations[x] = WsdlOperation)
        or (allOperations.Operations[x] = WsdlOperation.Cloned)
        or (WsdlOperation.invokeList.Find(allOperations.Operations[x].reqTagName + ';' + allOperations.Operations[x].reqTagNameSpace, f)) then
        begin
          if allOperations.Operations[x].rpyBind.Children.Count > 0 then
          begin
            xChild := rChild;
            if (not Assigned (WsdlOperation))
            and (allOperations.Operations[x].rpyBind is TIpmItem) then
              xChild := TreeView.Items.AddChildObject (rChild, allOperations.Operations[x].Name, nil);
            ViewXmlItem ( TreeView
                        , xChild
                        , allOperations.Operations[x].rpyBind
                        , IncludeRecurring
                        );
          end;
        end;
      end;
      rChild.Collapse(True);
    end;
    if Assigned (SrceBind) then
      ShowXmls (SrceBind, SrceBind.Name);
    if Assigned (WsdlOperation) then
    begin
      if doShowRpy then
      begin
        xChild := TreeView.Items.AddChildObject (nil, 'Faults', nil);
        for x := 0 to WsdlOperation.fltBind.Children.Count - 1 do
          ViewXmlItem ( TreeView
                      , xChild
                      , WsdlOperation.fltBind.Children.Bindables [x]
                      , IncludeRecurring
                      );
      end;
      if doShowMq then
        ShowXmls (WsdlOperation.StubMqHeaderXml, 'Mq');
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
  maxOccurrences := MaxInt;
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
  xCaption: String;
begin
  if aBind = nil then
    exit;
  if (not aIncludeRecurring)
  and Captions.Find(aBind.FullIndexCaption, x) then
    exit;
  if (aBind is TIpmItem)
  and ((aBind as TIpmItem).Occurrence > maxOccurrences) then
    Exit;
{
  if (aIpmItem.Occurs > 1) and (not aIncludeRecurring) then
    exit;
}
  xChild := aTreeView.Items.AddChildObject (aNode, aBind.GetIndexCaption, aBind);
  Captions.Add(aBind.GetFullIndexCaption);
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
  procedure _select (aNode: TTreeNode; cCaption, nCaption: String);
  var
    xNode: TTreeNode;
  begin
    if aNode.Text <> cCaption then Exit;
    TreeView.Selected := aNode;
    cCaption := ThisCaption(nCaption);
    nCaption := NextFullCaption(nCaption);
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
  cCaption := ThisCaption(aCaption);
  nCaption := NextFullCaption(aCaption);
  for x := 0 to TreeView.Items.Count - 1 do
    if not Assigned (TreeView.Items.Item[x].Parent) then
      _select (TreeView.Items.Item[x], cCaption, nCaption);
end;

procedure TSelectXmlElementForm.setWsdlOperation(const Value: TWsdlOperation);
begin
  fWsdlOperation := Value;
end;

end.
