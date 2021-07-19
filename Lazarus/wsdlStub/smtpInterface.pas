{
    This file is part of the apiUi project
    Copyright (c) 2009-2021 by Jan Bouwman

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit smtpInterface;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses Classes
   , SysUtils
   , IdMessage
   , LazFileUtils
   , Xmlz
   ;

function smtpParseMessageStreamAsXml (aMsg: TStream; var aHeaders, aSubject: String): TXml;
function smtpCreateMessageFromXml (aXml: TXml): TIdMessage;

implementation

uses IdEMailAddress
   , IdMessageParts
   , IdAttachmentFile
   , IdText
   ;

function smtpCreateMessageFromXml (aXml: TXml): TIdMessage;
  procedure _addAddresses (aList: TIdEMailAddressList; aXml: TXml);
  var
    x: Integer;
  begin
    if not Assigned (aXml) then Exit;
    for x := 0 to aXml.Items.Count - 1 do
      if aXml.Items.XmlItems[x].Checked then
        with aList.Add do
          Address := aXml.Items.XmlItems[x].Value;
  end;
var
  x: Integer;
  xXml: TXml;
begin
  result := nil;
  if not Assigned (aXml) then raise Exception.Create('smtpCreateMessageFromXml: XML null argument');
  if aXml.Name <> 'Email' then raise Exception.Create('smtpCreateMessageFromXml: Illegal name ' + aXml.Name);
  result := TIdMessage.Create(nil);
  try
    result.From.Address := aXml.Items.XmlCheckedValueByTag ['From'];
    _addAddresses (result.Recipients, aXml.Items.XmlCheckedItemByTag['To']);
    _addAddresses (result.CCList, aXml.Items.XmlCheckedItemByTag['CC']);
    _addAddresses (result.BccList, aXml.Items.XmlCheckedItemByTag['BCC']);
    result.Subject := aXml.Items.XmlCheckedValueByTag ['Subject'];
    result.Body.Text := aXml.Items.XmlCheckedValueByTag ['Body'];
    xXml := aXml.Items.XmlCheckedItemByTag['Attachments'];
    if Assigned (xXml) then
      for x := 0 to xXml.Items.Count - 1 do
         if xXml.Items.XmlItems[x].Checked then
           if FileExistsUTF8(xXml.Items.XmlItems[x].Value) { *Converted from FileExists* } then
             TIdAttachmentFile.Create(result.MessageParts, xXml.Items.XmlItems[x].Value)
           else
             raise Exception.Create('Can not load attachment, not existing file: ' + xXml.Items.XmlItems[x].Value);
  except
    result.Free;
    result := nil;
    raise;
  end;
end;

function smtpParseMessageStreamAsXml (aMsg: TStream; var aHeaders, aSubject: String): TXml;
  function _createXmlFromString (aName, aValue: String): TXml;
  begin
    if aValue <> '' then
      result := TXml.CreateAsString(aName, aValue)
    else
      result := nil;
  end;
  function _addressListAsXml (aName: String; aList: TIdEmailAddressList): TXml;
  var
    x: Integer;
  begin
    result := nil;
    if Assigned (aList)
    and (aList.Count > 0) then
    begin
      result := TXml.CreateAsString(aName, '');
      for x := 0 to aList.Count -1 do
        result.AddXml(TXml.CreateAsString('EmailAddress', aList.Items[x].Text));
    end;
  end;
  procedure _AddMessagePartsAsXml (aParentXml: TXml; aMsgParts: TIdMessageParts);
  var
    x: Integer;
    xXml: TXml;
    xMsgPart: TIdMessagePart;
  begin
    if not Assigned (aMsgParts)
    or (aMsgParts.Count = 0) then
      exit;
    xXml := aParentXml.AddXml (TXml.CreateAsString ('MessageParts', ''));
    with xXml do
    begin
      for x := 0 to aMsgParts.Count - 1 do
      begin
        xMsgPart := aMsgParts.Items[x];
        with AddXml (TXml.CreateAsString('MessagePart', '')) do
        begin
          AddXml (_createXmlFromString('CharSet', xMsgPart.CharSet));
          AddXml (_createXmlFromString('ContentDescription',  xMsgPart.ContentDescription));
          AddXml (_createXmlFromString('ContentDisposition',  xMsgPart.ContentDisposition));
          AddXml (_createXmlFromString('ContentID',  xMsgPart.ContentID));
          AddXml (_createXmlFromString('ContentLocation',  xMsgPart.ContentLocation));
          AddXml (_createXmlFromString('ContentTransfer',  xMsgPart.ContentTransfer));
          AddXml (_createXmlFromString('ContentType',  xMsgPart.ContentType));
          AddXml (_createXmlFromString('ExtraHeaders',  xMsgPart.ExtraHeaders.Text));
          AddXml (_createXmlFromString('FileName',  xMsgPart.FileName));
          AddXml (_createXmlFromString('Name',  xMsgPart.Name));
{}{
          if xMsgPart is TIdAttachment then
          with xMsgPart as TIdAttachment do
          begin
            xStream := TStringStream.Create;
            try
              SaveToStream (xStream);
              AddXml (TXml.CreateAsString('Content', xStream.DataString));
            finally
              xStream.Free;
            end;
          end;
{}
          if xMsgPart is TIdText then
          with xMsgPart as TIdText do
            AddXml (TXml.CreateAsString('Content', Body.Text));
        end;
      end;
    end;
  end;
var
  xMsg : TIdMessage;
begin
  result := nil;
  if not Assigned (aMsg) then
    exit;
  xMsg := TIdMessage.Create(nil);
  try
    xMsg.LoadFromStream(aMsg, False);
    aHeaders := (xMsg.Headers as TStringList).Text;
    aSubject := xMsg.Subject;
    result := TXml.CreateAsString ('smtp', '');
    with result do
    begin
      AddXml (TXml.CreateAsString('From', xMsg.From.Text));
      AddXml (_addressListAsXml('To', xMsg.Recipients));
      AddXml (_addressListAsXml('CC', xMsg.CCList));
      AddXml (_addressListAsXml('BCC', xMsg.BCCList));
      AddXml (_addressListAsXml('ReplyTo', xMsg.ReplyTo));
      AddXml (TXml.CreateAsString('Subject', xMsg.Subject));
      AddXml (TXml.CreateAsString('Body', xMsg.Body.Text));
      AddXml (_createXmlFromString('MsgId', xMsg.MsgId));
      AddXml (_createXmlFromString('UID', xMsg.UID));
      AddXml (_createXmlFromString('AttachmentEncoding', xMsg.AttachmentEncoding));
      AddXml (_createXmlFromString('CharSet', xMsg.CharSet));
      AddXml (_createXmlFromString('ContentType', xMsg.ContentType));
      AddXml (_createXmlFromString('ContentTransferEncoding', xMsg.ContentTransferEncoding));
      AddXml (_createXmlFromString('ContentDisposition', xMsg.ContentDisposition));
      AddXml (_createXmlFromString('ExtraHeaders', xMsg.ExtraHeaders.Text));
      AddXml (_createXmlFromString('NewsGroups', xMsg.NewsGroups.Text));
      AddXml (_createXmlFromString('Organization', xMsg.Organization));
      AddXml (_createXmlFromString('References', xMsg.References));
      AddXml (_createXmlFromString('InReplyTo', xMsg.InReplyTo));
      //    property MIMEBoundary: TIdMIMEBoundary read FMIMEBoundary write FMIMEBoundary;
      //    property IsMsgSinglePartMime: Boolean read FIsMsgSinglePartMime write FIsMsgSinglePartMime;
      //    property Date: TDateTime read FDate write FDate;
      //    property Encoding: TIdMessageEncoding read FEncoding write SetEncoding;
      //    property NoEncode: Boolean read FNoEncode write FNoEncode default ID_MSG_NODECODE;
      //    property NoDecode: Boolean read FNoDecode write FNoDecode default ID_MSG_NODECODE;
      //    property Priority: TIdMessagePriority read FPriority write FPriority default ID_MSG_PRIORITY;
      //    property ReceiptRecipient: TIdEmailAddressItem read FReceiptRecipient write SetReceiptRecipient;
      _AddMessagePartsAsXml(result, xMsg.MessageParts);
    end;
  finally
    xMsg.Free;
  end;
end;

end.
