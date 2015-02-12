unit CHILDWIN;

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
  Classes, Graphics, Forms, Controls, StdCtrls, Bind, Xmlz, Wsdlz, Xsdz,
  ImgList, VirtualTrees, ComCtrls, ExtCtrls, Menus, forminifilez;

type
  TDescrType = (dtXsd, dtWsdl, dtCobol, dtJson);

type
  TAxisDeerCF = class(TForm)
    InWsdlTreeView: TVirtualStringTree;
    TreeviewImageList: TImageList;
    XsdPanel: TPanel;
    Splitter4: TSplitter;
    StatusBar2: TStatusBar;
    InWsdlPropertiesListView: TListView;
    DataTypeDocumentationMemo: TMemo;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    WsdlPopupMenu: TPopupMenu;
    WsdlItemAddMenuItem: TMenuItem;
    WsdlItemDelMenuItem: TMenuItem;
    WsdlItemChangeDataTypeMenuItem: TMenuItem;
    DataTypeDependingMenu: TMenuItem;
    N2: TMenuItem;
    Copytoclipboard1: TMenuItem;
    WsdlPasteFromClipboardMenuItem: TMenuItem;
    WsdlPopulateMenuItem: TMenuItem;
    All1: TMenuItem;
    Required1: TMenuItem;
    N14: TMenuItem;
    Validate1: TMenuItem;
    N3: TMenuItem;
    Expand2: TMenuItem;
    FullExpand1: TMenuItem;
    FullCollapse1: TMenuItem;
    N4: TMenuItem;
    XmlZoomValueAsTextMenuItem: TMenuItem;
    XmlZoomValueAsXMLMenuItem: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InWsdlTreeViewChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure InWsdlTreeViewChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure InWsdlTreeViewClick(Sender: TObject);
    procedure InWsdlTreeViewColumnClick(Sender: TBaseVirtualTree;
      Column: TColumnIndex; Shift: TShiftState);
    procedure InWsdlTreeViewEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure InWsdlTreeViewEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure InWsdlTreeViewExit(Sender: TObject);
    procedure InWsdlTreeViewFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure InWsdlTreeViewFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure InWsdlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure InWsdlTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure InWsdlTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InWsdlTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InWsdlTreeViewNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure InWsdlTreeViewPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WsdlItemAddMenuItemClick(Sender: TObject);
    procedure WsdlPopupMenuPopup(Sender: TObject);
    procedure WsdlItemDelMenuItemClick(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Required1Click(Sender: TObject);
    procedure Validate1Click(Sender: TObject);
    procedure Expand2Click(Sender: TObject);
    procedure FullExpand1Click(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure XmlZoomValueAsTextMenuItemClick(Sender: TObject);
    procedure XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure InWsdlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure InWsdlTreeViewCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
  private
    isLocked: Boolean;
    fdescrFileName: String;
    fXmlFileName: String;
    fXsdDescr: TXsdDescr;
    fXml: TXml;
    IniFile: TFormIniFile;
    fStubChanged: Boolean;
    fExistingFile: Boolean;
    fDescrType: TDescrType;
    fWsdl: TWsdl;
    fOperation: TWsdlOperation;
    xsdElementsWhenRepeatable: Integer;
    function xmlVisibility (aXml: TXml): Boolean;
    procedure RevalidateXmlTreeView (aTreeView: TVirtualStringTree);
    function NodeToBind (aTreeView: TBaseVirtualTree; aNode: PVirtualNode): TCustomBindable;
    function getXmlViewType: TxvViewType;
    procedure UpdateXmlTreeViewNode (aTreeView: TVirtualStringTree; aNode: PVirtualNode);
    procedure SetXmlNodeCheckBox ( aTreeView: TVirtualStringTree
                                 ; aXml: TXml
                                 ; aNode: PVirtualNode
                                 ; ForceChoice: Boolean
                                 );
    function FillNodeWithBind ( aTreeView: TVirtualStringTree
                              ; aRootBind, aBind: TCustomBindable
                              ; aNode: PVirtualNode
                              ): PVirtualNode;
    function AllChecked (Sender: TBaseVirtualTree; aNode:PVirtualNode): Boolean;
    function getXmlFileName: String;
    procedure setXmlFileName(const Value: String);
    procedure setXml(const Value: TXml);
    function FillTreeView ( aTreeView: TVirtualStringTree
                          ; aXml: TXml
                          ; aParentNode: PVirtualNode
                          ): PVirtualNode;
    function InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
    procedure FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
    procedure ChangeDataTypeMenuItemClick(Sender: TObject);
    procedure PopulateXml(aViewType: TxvViewType);
    procedure setStubChanged(const Value: Boolean);
    procedure SaveStringToFile (aFileName: String; aString: AnsiString);
  public
    property descrType: TDescrType read fDescrType write fDescrType;
    property stubChanged: Boolean read fStubChanged write setStubChanged;
    property xmlViewType: TxvViewType read getXmlViewType;
    property descrFileName: String read fdescrFileName write fdescrFileName;
    property XmlFileName: String read getXmlFileName write setXmlFileName;
    property XsdDescr: TXsdDescr read fXsdDescr write fXsdDescr;
    property Wsdl: TWsdl read fWsdl write fWsdl;
    property Operation: TWsdlOperation read fOperation write fOperation;
    property Xml: TXml read fXml write setXml;
    property ExistingFile: Boolean read fExistingFile write fExistingFile;
    procedure SaveToFile (aFileName: String);
  end;
type
  PXmlTreeRec = ^TXmlTreeRec;
  TXmlTreeRec = record
    Bind: TCustomBindable;
  end;
  TPasswordEditLink = class(TStringEditLink)
  public
    constructor Create; override;
  end;


implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses xmlUtilz
   , Dialogs
   , SysUtils
   ;

procedure TAxisDeerCF.SaveStringToFile (aFileName: String; aString: AnsiString);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.Size := Length (aString);
    S.Position := 0;
    S.WriteBuffer(aString[1], S.Size);
    S.SaveToFile(aFileName);
  finally
    S.Free;
  end;
end;

procedure TAxisDeerCF.SaveToFile(aFileName: String);
begin
  if Assigned (fWsdl) then
  begin
    case fOperation.StubAction of
      saRequest: SaveStringToFile(aFileName, fOperation.StreamRequest('axisDeer', True, True, True));
      saStub: SaveStringToFile(aFileName, fOperation.StreamReply('axisDeer', True));
    end;
  end
  else
  begin
    if fXml.jsonType <> jsonNone then
      SaveStringToFile (aFileName, fXml.StreamJSON(0, True))
    else
      SaveStringToFile (aFileName, fXml.AsText(True, 0, True, False));
  end;
  stubChanged := False;
  ExistingFile := True;
  XmlFileName := aFileName;
end;

procedure TAxisDeerCF.All1Click(Sender: TObject);
begin
  PopulateXml (xvAll);
end;

function TAxisDeerCF.AllChecked(Sender: TBaseVirtualTree;
  aNode: PVirtualNode): Boolean;
begin
  if aNode = aNode.NextSibling then
    result := True
  else
  begin
    result := (aNode.CheckState = csCheckedNormal);
    if result then
      result := AllChecked (Sender, aNode.Parent);
  end;
end;

procedure TAxisDeerCF.ChangeDataTypeMenuItemClick(Sender: TObject);
var
  xBind: TCustomBindable;
  xDataType: TXsdDataType;
  xXsd: TXsd;
  xXmlAsText: String;
  xXml, nXml: TXml;
  x: Integer;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml)
  and (Assigned ((xBind as TXml).Xsd)) then
  begin
    xXml := xBind as TXml;
    xXmlAsText := xXml.AsText(False, 0, True, False);
    xDataType := TXsdDataType ((Sender as TMenuItem).Tag);
    if xXml.TypeDef <> xDataType then
    begin
      xXml.TypeDef := xDataType;
      xXml.XsdCreate(0, xXml.Xsd);
      nXml := TXml.Create;
      try
        nXml.LoadFromString(xXmlAsText, nil);
        for x := nXml.Attributes.Count - 1 downto 0 do
          if NameWithoutPrefix(nXml.Attributes.XmlAttributes[x].Name) = 'type' then
            nXml.Attributes.XmlAttributes[x].Value := xDataType.Name;
        xXml.LoadValues(nXml,False,False);
      finally
        nXml.Free;
      end;
    end;
    UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    InWsdlTreeView.FocusedColumn := 0;
    InWsdlTreeView.Expanded [InWsdlTreeView.FocusedNode] := True;
    InWsdlTreeView.Invalidate;
    InWsdlTreeViewFocusChanged(InWsdlTreeView, InWsdlTreeView.FocusedNode, InWsdlTreeView.FocusedColumn);
    stubChanged := True;
  end;
end;

procedure TAxisDeerCF.Copytoclipboard1Click(Sender: TObject);
begin
  XmlUtil.CopyToClipboard (NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode));
end;

procedure TAxisDeerCF.Expand2Click(Sender: TObject);
begin
  if Assigned (inWsdlTreeView.FocusedNode) then
  begin
    inWsdlTreeView.FullCollapse(nil);
    inWsdlTreeView.Expanded [inWsdlTreeView.FocusedNode] := True;
  end;
end;

function TAxisDeerCF.FillNodeWithBind(aTreeView: TVirtualStringTree; aRootBind,
  aBind: TCustomBindable; aNode: PVirtualNode): PVirtualNode;
var
  xChildNode: PVirtualNode;
  xAttributeNode: PVirtualNode;
  xData: PXmlTreeRec;
  x: Integer;
begin
  if not Assigned (aBind) then Exit;
  xChildNode := aTreeView.AddChild(aNode);
  xData := aTreeView.GetNodeData(xChildNode);
  xData.Bind := aBind;
  if aBind is TXml then
  begin
    SetXmlNodeCheckBox (aTreeView, aBind as TXml, xChildNode, aRootBind = aBind);
    aTreeView.IsVisible [xChildNode] := xmlVisibility (aBind as TXml);
    for x := 0 to (aBind as TXml).Attributes.Count - 1 do
    begin
      xAttributeNode := aTreeView.AddChild(xChildNode);
      xData := aTreeView.GetNodeData(xAttributeNode);
      xData.Bind := (aBind as TXml).Attributes.XmlAttributes [x];
      xAttributeNode.CheckType := ctCheckBox;
      if (aBind as TXml).Attributes.XmlAttributes [x].Checked then
        xAttributeNode.CheckState := csCheckedNormal
      else
        xAttributeNode.CheckState := csUnCheckedNormal;
    end;
  end;
  for x := 0 to aBind.Children.Count - 1 do
  begin
    FillNodeWithBind ( aTreeView
                    , aRootBind
                    , aBind.Children.Bindables [x]
                    , xChildNode
                    );
  end;
  result := xChildNode;
end;

function TAxisDeerCF.FillTreeView(aTreeView: TVirtualStringTree; aXml: TXml;
  aParentNode: PVirtualNode): PVirtualNode;
var
  x: Integer;
  xXml: TXml;
  xNode: PVirtualNode;
begin
  result := nil;
  if aXml = nil then
    exit;
  result := FillNodeWithBind (aTreeView, aXml, aXml, aParentNode);
  aTreeView.FullExpand (aTreeView.GetFirst);
end;

procedure TAxisDeerCF.FinishXmlNode(aNode: PVirtualNode; aXml: TXml);
var
  attrNode: PVirtualNode;
  Data: PXmlTreeRec;
  I: Integer;
begin
  Data := InWsdlTreeView.GetNodeData(aNode);
  Data.Bind := aXml;
  if Assigned ((aXml.Parent as TXml).Xsd) then
  begin
    if (aXml.Parent as TXml).TypeDef.ContentModel = 'Choice' then
      aNode.CheckType := ctRadioButton
    else
      aNode.CheckType := ctCheckBox;
    if aXml.Checked then
      aNode.CheckState := csCheckedNormal
    else
      aNode.CheckState := csUnCheckedNormal;
  end
  else
  begin
    aNode.CheckType := ctRadioButton;
    aNode.CheckState := csCheckedNormal;
  end;
  for I := 0 to aXml.Attributes.Count - 1 do
  begin
    attrNode := InWsdlTreeView.AddChild(aNode);
    attrNode.CheckType := ctCheckBox;
    if aXml.Attributes.XmlAttributes [I].Checked then
      attrNode.CheckState := csCheckedNormal
    else
      attrNode.CheckState := csUnCheckedNormal;
    Data := InWsdlTreeView.GetNodeData(AttrNode);
    Data.Bind := aXml.Attributes.XmlAttributes [I];
  end;
  for I := 0 to aXml.Items.Count - 1 do
    FinishXmlNode (InWsdlTreeView.AddChild(aNode), aXml.Items.XmlItems [I]);
end;

procedure TAxisDeerCF.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAxisDeerCF.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  ret: Word;
begin
  CanClose := False;
  if stubChanged then
  begin
    ret := MessageDlg ('Save changes to ' + XmlFileName + '?', mtConfirmation,
      [mbYes, mbNo, mbCancel], 0);
    if (ret = mrYes) then
    begin
      if ExistingFile then
      begin
        SaveStringToFile (XmlFileName, Xml.AsText(True, 0, True, False));
        stubChanged := False;
        ExistingFile := True;
      end
      else
      begin
        with TSaveDialog.Create(nil) do
        try
          FileName := XmlFileName;
          if Execute then
            SaveStringToFile(FileName, Xml.AsText(True, 0, True, False))
          else
            Exit;
        finally
          Free;
        end;
      end;
    end;
    if (ret = mrCancel) then
      Exit;
  end;
  CanClose := True;
end;

procedure TAxisDeerCF.FormCreate(Sender: TObject);
begin
  InWsdlTreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  IniFile := TFormIniFile.Create (Self, 'axisDeerCF', Self.Name = 'axisDeerCF');
  IniFile.Restore;
end;

procedure TAxisDeerCF.FormDestroy(Sender: TObject);
begin
  Xml.Free;
  XsdDescr.Free;
  IniFile.Save;
  IniFile.Free;
end;

procedure TAxisDeerCF.FullCollapse1Click(Sender: TObject);
begin
  if Assigned (inWsdlTreeView.FocusedNode) then
    inWsdlTreeView.FullCollapse (inWsdlTreeView.FocusedNode);
end;

procedure TAxisDeerCF.FullExpand1Click(Sender: TObject);
begin
  if Assigned (inWsdlTreeView.FocusedNode) then
    inWsdlTreeView.FullExpand (inWsdlTreeView.FocusedNode);
end;

function TAxisDeerCF.getXmlFileName: String;
begin
  result := fXmlFileName;
end;

function TAxisDeerCF.getXmlViewType: TxvViewType;
begin
  result := xvAll;
{}{
  result := TxvViewType (ViewStyleComboBox.ItemIndex);
  {}
end;

function TAxisDeerCF.InsertXmlNode(aNode: PVirtualNode; aXml: TXml): PVirtualNode;
begin
  result := InWsdlTreeView.InsertNode(aNode, amInsertAfter);
  FinishXmlNode (result, aXml);
end;

procedure TAxisDeerCF.InWsdlTreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  xBind: TCustomBindable;
  Xml, expXml: TXml;
  xMessage: TWsdlMessage;
begin
  if Column = treeValueColumn then
  begin
    xBind := NodeToBind(Sender, Node);
    if not AllChecked(Sender, Node) then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := bgNilValueColor;
        FillRect( CellRect );
      end;
      Exit;
    end;
    if xBind is TXmlAttribute then
    begin
      with TargetCanvas do
      begin
        Brush.Style := bsSolid;
//      Brush.Color := bgAttributeValueColor;
        FillRect( CellRect );
      end;
      Exit;
    end;
    if xBind is TXml then
    begin
      if ((xBind as TXml).Group)
      or (    (Assigned ((xBind as TXml).Xsd))
          and ((xBind as TXml).TypeDef.ContentModel = 'Empty')
         )
      then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          FillRect( CellRect );
        end;
      end;
    end;
  end;
{}{
  if Column = treeTagColumn then
  begin
    xBind := NodeToBind(Sender, Node);
    if (xBind is TXml)
    or (xBind is TXmlAttribute) then
    begin
      expXml := nil;
      if Assigned (WsdlOperation)
      and Assigned (xMessage) then
        if (WsdlOperation.StubAction = saRequest) then
          expXml := xMessage.rpyBind as TXml
        else
          expXml := xMessage.reqBind as TXml;
      if Assigned (xMessage)
      and (   expXml.IsAncestorOf (xBind)
           or (expXml = xBind)
          ) then
      begin
        with TargetCanvas do
        begin
          Brush.Style := bsSolid;
  //        Brush.Color := bgCorrelationItemColor;
          Brush.Color := bgExpectedValueColor;
          FillRect( CellRect );
        end;
      end;
    end;
  end;
{}
end;

procedure TAxisDeerCF.InWsdlTreeViewChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Sender, Node);
  stubChanged := True;
  xBind.Checked := (Node.CheckState = csCheckedNormal);
  if (not xBind.Checked) then
    InWsdlTreeView.FullCollapse(Node)
  else
  begin
    InWsdlTreeView.FullExpand(Node);
  end;
  RevalidateXmlTreeView(Sender as TVirtualStringTree);
end;

procedure TAxisDeerCF.InWsdlTreeViewChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TAxisDeerCF.InWsdlTreeViewClick(Sender: TObject);
var
  xChanged: Boolean;
begin
  if (Sender = InWsdlTreeView)
  and (InWsdlTreeView.FocusedColumn = treeButtonColumn)
  then
  begin
    xChanged := xmlUtil.editXml ( NodeToBind (inWsdlTreeView, inWsdlTreeView.FocusedNode)
                                , False
                                );
    if xChanged then
    begin
      UpdateXmlTreeViewNode(InWsdlTreeView, inWsdlTreeView.FocusedNode);
      RevalidateXmlTreeView (InWsdlTreeView);
    end;
    stubChanged := stubChanged or xChanged;
    InWsdlTreeView.FocusedColumn := treeValueColumn;
  end
end;

procedure TAxisDeerCF.InWsdlTreeViewColumnClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  Sender.FocusedColumn := Column;
end;

procedure TAxisDeerCF.InWsdlTreeViewCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  Bind: TCustomBindable;
begin
  Bind := NodeToBind(Sender, Node);
  if (Bind is TXml)
  and (Bind.Name <> 'passwordType')
  and Assigned ((Bind as TXml).Xsd)
  and ((Bind as TXml).TypeDef.ElementDefs.Count = 0)
  and ((Bind as TXml).TypeDef.Name = 'passwordType')
  then
    EditLink := TPasswordEditLink.Create
  else
    EditLink := TStringEditLink.Create;
end;

procedure TAxisDeerCF.InWsdlTreeViewEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = treeValueColumn then
  begin
    xmlUtil.CheckValidity(NodeToBind (Sender, Node));
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Sender, Node);
  case Column of
    treeTagColumn, treeButtonColumn: begin
         Allowed := False;
       end;
    treeValueColumn: begin
         Allowed := (xBind is TXmlAttribute)
                 or (    (xBind is TXml)
                     and (not (   ((xBind as TXml).Group)
                               or (    (Assigned ((xBind as TXml).Xsd))
                                   and ((xBind as TXml).TypeDef.ContentModel = 'Empty')
                                  )
                              )
                         )
                    );
       end;
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewExit(Sender: TObject);
begin
  InWsdlTreeView.EndEditNode;
end;

procedure TAxisDeerCF.InWsdlTreeViewFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  x, f: Integer;
  DataType: TXsdDataType;
  Xml: TXml;
  XmlAttr: TXmlAttribute;
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
begin
  Sender.Selected [Sender.FocusedNode] := True;
  xBind := NodeToBind (Sender, Sender.FocusedNode);
  if not Assigned (xBind) then
    exit;
  if xBind is TXmlAttribute then
  begin
    Xml := xBind.Parent as TXml;
    XmlAttr := xBind as TXmlAttribute;
  end;
  if xBind is TXml then
  begin
    Xml := xBind as TXml;
    XmlAttr := nil;
  end;
  xmlUtil.ListXsdProperties(InWsdlPropertiesListView, xBind);
//InWsdlEnumerationsListView.Clear;
  DataTypeDocumentationMemo.Clear;
  DataType := nil;
  StatusBar.Panels [0].Text := xBind.FullCaption;
  if (xBind is TXml) then
    DataTypeDocumentationMemo.Lines.Text := (xBind as TXml).DocumentationText;
  if (xBind is TXmlAttribute) then
    DataTypeDocumentationMemo.Lines.Text := (xBind as TXmlAttribute).XsdAttr.Documentation.Text;
end;

procedure TAxisDeerCF.InWsdlTreeViewFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  xBind: TCustomBindable;
begin
  if Assigned (OldNode) then
  begin
    xBind := NodeToBind(Sender, OldNode);
    if xBind is TXml then
      Sender.IsVisible [OldNode] := xmlVisibility (xBind as TXml);
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  xBind: TCustomBindable;
  Xml: TXml;
  XmlAttr: TXmlAttribute;
  editAllowed: Boolean;
begin
  xBind := NodeToBind(Sender, Node);
  if xBind is TXmlAttribute then
  begin
    XmlAttr := xBind as TXmlAttribute;
    Xml := XmlAttr.Parent as TXml;
  end;
  if xBind is TXml then
  begin
    XmlAttr := nil;
    Xml := xBind as TXml;
  end;
  case Kind of
    ikNormal, ikSelected:
    begin
      case Column of
        treeTagColumn: ;
        treeButtonColumn: ImageIndex := xmlUtil.getImageImdex(xBind, 18);
        treeValueColumn: ;
      end; {case column}
    end; {Kind in ikNormal, ikSelected}
  end; {case Kind}
end;

procedure TAxisDeerCF.InWsdlTreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Sender, Node);
  CellText := '';
  if Assigned (xBind) then
  begin
    case Column of
      treeTagColumn:
        begin
          if xBind is TXmlAttribute then
            CellText := '@' + xBind.Name
          else
            CellText := xBind.Name;
        end;
      treeValueColumn:
        begin
          if (xBind is TXml)
          and (xBind.Name <> 'passwordType')
          and Assigned((xBind as TXml).TypeDef)
          and ((xBind as TXml).TypeDef.Name = 'passwordType')
          and (xBind.Value <> '') then
            CellText := '**********'
          else
            CellText := xBind.Value;
        end;
    end;
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aCol: Integer;
  xBind: TCustomBindable;
  xMessage: String;
begin
  if (Key = VK_F8) then
  begin
    xBind := NodeToBind(Sender as TVirtualStringTree, (Sender as TVirtualStringTree).FocusedNode);
    if (xBind is TXml) then
      ShowMessage ((xBind as TXml).TypeDef.SchemaAsText((xBind as TXml).TypeDef.Name));
  end;
  if (Key = VK_INSERT) then
  begin
    xBind := NodeToBind(Sender as TVirtualStringTree, (Sender as TVirtualStringTree).FocusedNode);
    if not xBind.IsValueValid (xMessage) then
        ShowMessage (xMessage);
  end;
  if (Key = VK_RETURN) then
  begin
    (Sender as TVirtualStringTree).EditNode
      ( (Sender as TVirtualStringTree).FocusedNode
      , (Sender as TVirtualStringTree).FocusedColumn
      );
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TVirtualStringTree).FocusedNode
    := (Sender as TVirtualStringTree).GetNodeAt( X {+ (Sender as TVirtualStringTree).OffsetX }
                                               , Y {+ (Sender as TVirtualStringTree).OffsetY }
                                               ) ;
end;

procedure TAxisDeerCF.InWsdlTreeViewNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  xBind: TCustomBindable;
begin
  xBind := NodeToBind(Sender, Node);
  if Assigned (xBind) then
  begin
    case Column of
      treeTagColumn:
        begin
          if xBind.Name <> NewText then
          begin
            xBind.Name := NewText;
            stubChanged := True;
          end;
        end;
      treeValueColumn:
        begin
          if (NewText = '&nil') then
          begin
            if xBind.Checked then
            begin
              xBind.Checked := False;
              stubChanged := True;
            end;
          end
          else
          begin
            if (NewText <> xBind.Value)
            or (not AllChecked (Sender, Node)) then
            begin
              xBind.Value := NewText;
              xBind.Checked := True;
              stubChanged := True;
            end;
          end;
        end;
    end;
    RevalidateXmlTreeView(Sender as TVirtualStringTree);
  end;
end;

procedure TAxisDeerCF.InWsdlTreeViewPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  xBind: TCustomBindable;
  xChecked: Boolean;
begin
  if (Node = (Sender as TVirtualStringTree).GetFirst) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    exit;
  end;

  xBind := NodeToBind(Sender as TVirtualStringTree, Node);
  if xBind is TXmlAttribute then
  begin
    if (    (Assigned ((xBind as TXmlAttribute).XsdAttr))
        and ((xBind as TXmlAttribute).XsdAttr.Use = 'required')
       ) then
    begin
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      if AllChecked (Sender, Node.Parent) then
        if not (xBind as TXmlAttribute).Checked then
          TargetCanvas.Font.Color := clRed {clLtGray}	;
    end
    else
    begin
      if (not Assigned ((xBind as TXmlAttribute).XsdAttr)) then
        TargetCanvas.Font.Color := clRed {clLtGray}	;
    end;
    exit;
  end;
  if xBind is TXml then
  begin
    if (not Assigned ((xBind as TXml).Xsd)) then
    begin
      TargetCanvas.Font.Color := clRed {clLtGray}	;
      exit;
    end;
    try
      if Assigned ((xBind as TXml).Xsd)
      and (StrToIntDef ((xBind as TXml).Xsd.minOccurs, 0) > 0)
      and Assigned (xBind.Parent)
      and Assigned (TXml(xBind.Parent).Xsd)
      and (TXml(xBind.Parent).TypeDef.ContentModel <> 'Choice')
      then
      begin
        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
        if AllChecked (Sender, Node.Parent) then
          if not xBind.Checked then
            TargetCanvas.Font.Color := clRed {clLtGray}	;
      end;
    except
      ShowMessage ((xBind as TXml).Xsd.minOccurs);
    end;
  end;
end;

function TAxisDeerCF.NodeToBind(aTreeView: TBaseVirtualTree;
  aNode: PVirtualNode): TCustomBindable;
var
  Data: PXmlTreeRec;
begin
  result := nil;
  if Assigned (aNode) then
  begin
    Data := aTreeView.GetNodeData(aNode);
    if Assigned (Data) then
      result := Data.Bind;
  end;
end;

procedure TAxisDeerCF.PopulateXml(aViewType: TxvViewType);
begin
  XmlUtil.Populate(NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode), aViewType);
  stubChanged := True;
  RevalidateXmlTreeView (InWsdlTreeView);
end;

procedure TAxisDeerCF.Required1Click(Sender: TObject);
begin
  PopulateXml (xvRequired);
end;

procedure TAxisDeerCF.RevalidateXmlTreeView(aTreeView: TVirtualStringTree);
var
  xNode: PVirtualNode;
  xBind: TCustomBindable;
begin
  xNode := aTreeView.GetFirst; // search from begin
  while not (xNode = nil) do
  begin
    xBind := NodeToBind (aTreeView, xNode);
    if xBind is TXmlAttribute then
    begin
      if xBind.Checked then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    if (xBind is TXml) then
    begin
      aTreeView.IsVisible [xNode] := xmlVisibility (xBind as TXml);
      if (xBind.Checked) then
        xNode.CheckState := csCheckedNormal
      else
        xNode.CheckState := csUnCheckedNormal;
    end;
    xNode := aTreeView.GetNext (xNode);
  end;
  with aTreeView do
  begin
//  IsVisible [GetNextSibling (GetFirst)] := doCheckExpectedValues;
    Invalidate;
  end;
end;

procedure TAxisDeerCF.setStubChanged(const Value: Boolean);
begin
  fStubChanged := Value;
  if stubChanged then
    Caption := fXmlFileName + ' *'
  else
    Caption := fXmlFileName;
end;

procedure TAxisDeerCF.setXml(const Value: TXml);
begin
  fXml := Value;
  InWsdlTreeView.Clear;
  InWsdlPropertiesListView.Clear;
  DataTypeDocumentationMemo.Clear;
  InWsdlTreeView.RootNodeCount := 0;
  InWsdlTreeView.NodeDataSize := SizeOf(TXmlTreeRec);
  FillTreeView (InWsdlTreeView, fXml, nil);
end;

procedure TAxisDeerCF.setXmlFileName(const Value: String);
begin
  fXmlFileName := Value;
  if stubChanged then
    Caption := fXmlFileName + ' *'
  else
    Caption := fXmlFileName;
end;

procedure TAxisDeerCF.SetXmlNodeCheckBox(aTreeView: TVirtualStringTree;
  aXml: TXml; aNode: PVirtualNode; ForceChoice: Boolean);
var
  xxsd: TXsd;
begin
  xxsd := nil;
{
  if (ForceChoice) then
  begin
    aNode.CheckType := ctRadioButton;
    if aXml.Checked then
      aNode.CheckState := csCheckedNormal
    else
      aNode.CheckState := csUnCheckedNormal;
    Exit;
  end;
}
  if (Assigned (aXml.Parent)) then
    xxsd := (aXml.Parent as TXml).Xsd;
  if (Assigned (aXml.Parent))
  and (Assigned ((aXml.Parent as TXml).Xsd)) then
  begin
    if (aXml.Parent as TXml).TypeDef.ContentModel = 'Choice' then
      aNode.CheckType := ctRadioButton
    else
      aNode.CheckType := ctCheckBox;
    if aXml.Checked then
      aNode.CheckState := csCheckedNormal
    else
      aNode.CheckState := csUnCheckedNormal;
  end
  else
  begin
    aNode.CheckType := ctCheckBox;
    aNode.CheckState := csCheckedNormal;
  end;
end;

procedure TAxisDeerCF.UpdateXmlTreeViewNode(aTreeView: TVirtualStringTree;
  aNode: PVirtualNode);
var
  x: Integer;
  xBind: TCustomBindable;
  xXml: TXml;
  xAttr: TXmlAttribute;
  xAttributeNode: PVirtualNode;
  xData: PXmlTreeRec;
  xExpanded: Boolean;
begin
  xExpanded := aTreeView.Expanded [aNode];
  xBind := NodeToBind (aTreeView, aNode);
  if xBind is TXmlAttribute then
  begin
    aNode := aNode.Parent;
    xBind := xBind.Parent;
  end;
  aTreeView.DeleteChildren(aNode);
  if xBind is TXml then
  begin
    xXml := xBind as TXml;
    SetXmlNodeCheckBox (aTreeView, xXml, aNode, False);
    for x := 0 to xXml.Attributes.Count - 1 do
    begin
      xAttributeNode := aTreeView.AddChild(aNode);
      xData := aTreeView.GetNodeData(xAttributeNode);
      xData.Bind := xXml.Attributes.XmlAttributes [x];
      xAttributeNode.CheckType := ctCheckBox;
      if xXml.Attributes.XmlAttributes [x].Checked then
        xAttributeNode.CheckState := csCheckedNormal
      else
        xAttributeNode.CheckState := csUnCheckedNormal;
    end;
    for x := 0 to xXml.Items.Count - 1 do
    begin
      FillNodeWithBind ( aTreeView
                      , nil
                      , xXml.Items.XmlItems [x]
                      , aNode
                      );
    end;
  end;
  if xExpanded then
    aTreeView.FullExpand(aNode);
end;

procedure TAxisDeerCF.Validate1Click(Sender: TObject);
begin
  XmlUtil.Validate (NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode));
end;

procedure TAxisDeerCF.WsdlItemAddMenuItemClick(Sender: TObject);
var
  Xml: TXml;  {current}
  xXml: TXml; {new created}
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
  x: Integer;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml)
  and (Assigned ((xBind as TXml).Xsd))
  then begin
    Xml := xBind as TXml;
    if (LowerCase (Xml.Xsd.maxOccurs) = 'unbounded')
    or (StrToInt (Xml.Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isAddAllowed (NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode), True) then
        exit;
      xXml := xmlUtil.Add (Xml);
      InWsdlTreeView.FocusedNode := InsertXmlNode(InWsdlTreeView.FocusedNode, xXml);
      InWsdlTreeView.FocusedColumn := 0;
      InWsdlTreeView.Expanded [InWsdlTreeView.FocusedNode] := True;
      InWsdlTreeView.Invalidate;
      stubChanged := True;
    end; {if maxOccurs greater than 1}
  end; {if xml clicked; just to be sure}
end;

procedure TAxisDeerCF.WsdlItemDelMenuItemClick(Sender: TObject);
var
  xXml: TXml; {new created}
  xBind: TCustomBindable;
  xMessage: TWsdlMessage;
  x: Integer;
begin
  xBind := NodeToBind(InWsdlTreeView, InWsdlTreeView.FocusedNode);
  if (xBind is TXml)
  and (Assigned ((xBind as TXml).Xsd))
  then begin
    if (LowerCase ((xBind as TXml).Xsd.maxOccurs) = 'unbounded')
    or (StrToInt ((xBind as TXml).Xsd.maxOccurs) > 1) then
    begin
      if not xmlUtil.isDeleteAllowed (xBind, True) then Exit;
      InWsdlTreeView.BeginUpdate;
      try
        xmlUtil.Delete ((xBind as TXml));
        InWsdlTreeView.DeleteNode(InWsdlTreeView.FocusedNode,True);
        InWsdlTreeView.Invalidate;
        stubChanged := True;
      finally
        InWsdlTreeView.EndUpdate;
      end;
    end; {if maxOccurs greater than 1}
  end; {if xml clicked; just to be sure}
end;

procedure TAxisDeerCF.WsdlPasteFromClipboardMenuItemClick(Sender: TObject);
begin
  try
    XmlUtil.PasteFromClipboard(NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode));
    UpdateXmlTreeViewNode(InWsdlTreeView, InWsdlTreeView.FocusedNode);
    stubChanged := True;
  finally
    RevalidateXmlTreeView (InWsdlTreeView);
  end;
end;

procedure TAxisDeerCF.WsdlPopupMenuPopup(Sender: TObject);
  procedure _createTypeSubMenuItems (aRootMenuItem: TMenuItem; aCurrent, aBase: TXsdDataType);
  var
    xMenuItem: TMenuItem;
    x: Integer;
  begin
    if (aBase <> aCurrent)
    and (not aBase.isAbstract) then
    begin
      xMenuItem := TMenuItem.Create(Self);
      xMenuItem.Caption := aBase.Name
                         + ' ('
                         + aBase.NameSpace
                         + ')'
                         ;
      xMenuItem.OnClick := ChangeDataTypeMenuItemClick;
      xMenuItem.Tag := Integer (aBase);
      aRootMenuItem.Add(xMenuItem);
    end;
    for x := 0 to aBase.ExtendedByList.Count - 1 do
      _createTypeSubMenuItems(aRootMenuItem, aCurrent, aBase.ExtendedByList.XsdDataTypes[x]);
  end;
var
  xBind: TCustomBindable;
  xTypeDef: TXsdDataType;
  xEnableAddMenuItems: Boolean;
  xEnableDelMenuItems: Boolean;
  xAddChildVisible: Boolean;
  xMenuItem: TMenuItem;
  x: Integer;
  xRootBase: TXsdDataType;
begin
  InWsdlTreeView.EndEditNode;
  xBind := NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode);
  xEnableAddMenuItems := False;
  xEnableDelMenuItems := False;
  xAddChildVisible := False;
  if xBind is TXml then
  begin
    if (xBind as TXml).Xsd.maxOccurs <> '1' then
      xEnableAddMenuItems := (not IsLocked);
    if (xBind as TXml).Xsd.maxOccurs <> '1' then
      xEnableDelMenuItems := (not IsLocked)
                         and ((xBind as TXml).IndexOfRepeatableItem >= xsdElementsWhenRepeatable)
                           ;
    xTypeDef := (xBind as TXml).TypeDef;
    if (    xTypeDef.IsComplex
        and (xTypeDef.ElementDefs.Count = 0)
       )
    or (    (xTypeDef.IsBuiltIn)
        and (xTypeDef.BaseDataTypeName = 'anyType')
       )
    or (xTypeDef.Manually) then
      xAddChildVisible := True;
  end;
  WsdlItemAddMenuItem.Enabled := xEnableAddMenuItems;
  WsdlItemDelMenuItem.Enabled := xEnableDelMenuItems;
  WsdlItemChangeDataTypeMenuItem.Clear;
  WsdlItemChangeDataTypeMenuItem.Enabled := (xBind is TXml)
                                        and (Assigned ((xBind as TXml).Xsd))
                                        and ((xBind as TXml).Xsd.IsTypeDefEnabled)
                                          ;
  if WsdlItemChangeDataTypeMenuItem.Enabled then
  begin
    xRootBase := (xBind as TXml).Typedef;
    while xRootBAse.IsExtention and Assigned (xRootBase.BaseDataType) do
      xRootBase := xRootBase.BaseDataType;
    _createTypeSubMenuItems (WsdlItemChangeDataTypeMenuItem, (xBind as TXml).Typedef, xRootBase);
  end;
end;

function TAxisDeerCF.xmlVisibility(aXml: TXml): Boolean;
begin
  if not Assigned (aXml.Parent) then
  begin
    result := True;
    exit;
  end;
  case xmlViewType of
    xvAll:
      begin
        result := True;
        exit;
      end;
    xvRequired:
      begin
        result :=  Assigned (aXml.Xsd) and (aXml.Xsd.minOccurs <> '0');
        exit;
      end;
    xvUsed:
      begin
        result := aXml.Checked;
        exit;
      end;
    xvReqUsed:
      begin
        result := (aXml.Checked)
               or (Assigned (aXml.Xsd) and (aXml.Xsd.minOccurs <> '0'));
        exit;
      end;
  end;
end;

procedure TAxisDeerCF.XmlZoomValueAsTextMenuItemClick(Sender: TObject);
var
  editAllowed: Boolean;
begin
  InWsdlTreeViewEditing(inWsdlTreeView, inWsdlTreeView.FocusedNode, treeValueColumn, editAllowed);
  XmlUtil.ZoomAsText (NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode), not editAllowed);
  if editAllowed then
  begin
    InWsdlTreeViewNewText ( inWsdlTreeView
                          , inWsdlTreeView.FocusedNode
                          , treeValueColumn
                          , XmlUtil.NewValue
                          );
  end;
end;

procedure TAxisDeerCF.XmlZoomValueAsXMLMenuItemClick(Sender: TObject);
begin
  XmlUtil.ZoomAsXml (NodeToBind (InWsdlTreeView, InWsdlTreeView.FocusedNode), True);
end;

{ TPasswordEditLink }

constructor TPasswordEditLink.Create;
begin
  inherited;
  Self.Edit.PasswordChar := '*';
end;

end.
