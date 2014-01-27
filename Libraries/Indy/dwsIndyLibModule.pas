{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsIndyLibModule;

{$I dws.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, dwsComp, dwsExprs, dwsUtils,

  IdBaseComponent, IdComponent, IdGlobal, IdContext, IdTCPConnection,
  IdTCPClient, IdFTPCommon, IdFTP, IdExplicitTLSClientServerBase, IdSMTPBase,
  IdSMTP, IdMessageClient, IdCustomTCPServer, IdTCPServer, IdCmdTCPServer,
  IdSMTPServer, IdMessage, IdAttachment, IdAttachmentFile, IdEMailAddress;

type
  TdwsIndyLib = class(TDataModule)
    dwsUnitIndy: TdwsUnit;
    procedure dwsTIdEMailAddressItemMethodsGetAddressEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsGetDomainEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsGetNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsGetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsGetUserEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsSetAddressEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsSetDomainEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsSetNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsSetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressItemMethodsSetUserEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsAddEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsGetEMailAddressesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsGetItemEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsSetEMailAddressesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsSetItemEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdEMailAddressListMethodsSortByDomainEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPCleanUp(ExternalObject: TObject);
    procedure dwsTIdFTPConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTIdFTPMethodsAbortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsChangeDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsChangeDirUpEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsConnectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsCRCEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsDeleteEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsDisconnectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetAccountEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetAUTHCmdEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetAutoIssueFEATEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetBoundIPEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetBoundPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetBoundPortMaxEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetBoundPortMinEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetCapabilitiesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetConnectionTimeOutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetDataPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetDataPortMaxEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetDataPortMinEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetDataPortProtectionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetExternalIPEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetIPVersionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetListenTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetPassiveEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetPassiveUseControlHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetProxySettingsEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetReadTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetResumeSupportedEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetReuseSocketEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetSupportsTLSEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetTransferTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetTransferTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetTryNATFastTrackEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseCCCEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseExtensionDataPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseHOSTEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseMLISEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseNagleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUserNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsGetUseTLSEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsLoginEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsMakeDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsNoopEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsPutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsRemoveDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsRenameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsRetrieveCurrentDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetAccountEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetAUTHCmdEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetAutoIssueFEATEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetBoundIPEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetBoundPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetBoundPortMaxEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetConnectionTimeOutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetDataPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetDataPortMaxEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetDataPortMinEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetDataPortProtectionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetExternalIPEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetIPVersionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetListenTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetPassiveEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetPassiveUseControlHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetProxySettingsEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetReadTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetReuseSocketEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetTransferTimeoutEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetTransferTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetTryNATFastTrackEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseCCCEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseExtensionDataPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseHOSTEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseMLISEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseNagleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUserNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSetUseTLSEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPMethodsSizeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsGetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsGetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsGetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsGetProxyTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsGetUsernameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsSetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsSetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsSetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsSetProxyTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdFTPProxySettingsMethodsSetUsernameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageCleanUp(ExternalObject: TObject);
    procedure dwsTIdMailMessageConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsAddHeaderEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsClearBodyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsClearEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsClearHeaderEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsetContentTransferEncodingEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGenerateHeaderEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetAttachmentEncodingEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetAttachmentTempDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetBodyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetCharsetEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetContentDispositionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetContentTransferEncodingEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetContentTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetConvertPreambleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetEncodingEval(Info: TProgramInfo;ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetFlagsEval(Info: TProgramInfo;ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetInReplyToEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetIsEncodedEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetIsMsgSinglePartMimeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetMsgIDEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetNoDecodeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetNoEncodeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetOrganizationEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetReferencesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetSubjectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetUIDEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsGetUseNowForDateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsLoadFromFileEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsProcessHeadersEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSaveToFileEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetAttachmentEncodingEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetAttachmentTempDirEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetBodyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetCharsetEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetContentDispositionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetContentTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetConvertPreambleEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetEncodingEval(Info: TProgramInfo;ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetFlagsEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetInReplyToEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetIsEncodedEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetIsMsgSinglePartMimeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetMsgIDEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetNoDecodeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetNoEncodeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetOrganizationEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetReferencesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetSubjectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetUIDEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdMailMessageMethodsSetUseNowForDateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPCleanUp(ExternalObject: TObject);
    procedure dwsTIdSMTPConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTIdSMTPMethodsAuthenticateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsConnectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsDisconnectEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetAuthTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetDidAuthenticateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetHeloNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetMailAgentEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetPipeLineEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetUseEhloEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetUsernameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetUseTLSEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetUseVerpEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetValidateAuthLoginCapabilityEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsGetVerpDelimsEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSendEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetAuthTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetHeloNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetHostEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetMailAgentEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetPasswordEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetPipeLineEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetPortEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetUseEhloEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetUsernameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetUseTLSEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetUseVerpEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetValidateAuthLoginCapabilityEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsSetVerpDelimsEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTIdSMTPMethodsVerifyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUploadEval(info: TProgramInfo);
    procedure dwsUploadPortEval(info: TProgramInfo);
    procedure dwsSendEmailEval(info: TProgramInfo);

  private
    procedure SetScript(aScript : TDelphiWebScript);

  public
    property Script : TDelphiWebScript write SetScript;
  end;

implementation

{$R *.DFM}

uses
  dwsSymbols;

{ TdwsIndyLib }

procedure TdwsIndyLib.SetScript(aScript : TDelphiWebScript);
begin
   dwsUnitIndy.Script:=aScript;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsSetItemEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  ExtObj: TObject;
begin
  ExtObj := Info.Vars['value'].ExternalObject;
  if ExtObj is TIdEMailAddressItem then
    TIdEMailAddressList(ExtObject).Items[Info.ParamAsInteger[0]] := TIdEMailAddressItem(ExtObj);
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsGetItemEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  EMailAddressItem: TIdEMailAddressItem;
  ClassSym: TClassSymbol;
  NewScriptObj: IScriptObj;
begin
  ClassSym := TClassSymbol(Info.FindSymbolInUnits('TEMailAddressList'));
  if Assigned(ClassSym) then
  begin
    EMailAddressItem := TIdEMailAddressList(ExtObject).Items[Info.ParamAsInteger[0]];
    NewScriptObj := TScriptObjInstance.Create(ClassSym, Info.Execution);
    NewScriptObj.ExternalObject := EMailAddressItem;
    Info.ResultAsVariant := NewScriptObj;
  end;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsSetEMailAddressesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEMailAddressList(ExtObject).EMailAddresses := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsGetEMailAddressesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEMailAddressList(ExtObject).EMailAddresses;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsSortByDomainEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEMailAddressList(ExtObject).SortByDomain;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressListMethodsAddEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  EMailAddressItem: TIdEMailAddressItem;
  ClassSym: TClassSymbol;
  NewScriptObj: IScriptObj;
begin
  ClassSym := TClassSymbol(Info.FindSymbolInUnits('TEMailAddressList'));
  if Assigned(ClassSym) then
  begin
    EMailAddressItem := TIdEMailAddressList(ExtObject).Add;
    NewScriptObj := TScriptObjInstance.Create(ClassSym, Info.Execution);
    NewScriptObj.ExternalObject := EMailAddressItem;
    Info.ResultAsVariant:= Info.RegisterExternalObject(EMailAddressItem, True);
  end;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetAccountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).Account;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetAUTHCmdEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).AUTHCmd);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetAutoIssueFEATEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).AutoIssueFEAT;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetBoundIPEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).BoundIP;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetBoundPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).BoundPort;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetBoundPortMaxEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).BoundPortMax;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetBoundPortMinEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).BoundPortMin;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetCapabilitiesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).Capabilities.Text;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetConnectionTimeOutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).ConnectTimeout;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetDataPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).DataPort;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetDataPortMaxEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).DataPortMax;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetDataPortMinEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).DataPortMin;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetDataPortProtectionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).DataPortProtection);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetExternalIPEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).ExternalIP
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).Host;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetIPVersionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).IPVersion);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetListenTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).ListenTimeout;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetPassiveUseControlHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).PassiveUseControlHost;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetProxySettingsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).ProxySettings);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetReadTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).ReadTimeout;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetReuseSocketEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).ReuseSocket);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetSupportsTLSEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).SupportsTLS;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetTransferTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).TransferTimeout;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetTransferTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).TransferType);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetTryNATFastTrackEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).TryNATFastTrack;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseCCCEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).UseCCC;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseExtensionDataPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).UseExtensionDataPort;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseHOSTEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).UseHOST;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseMLISEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).UseMLIS;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseNagleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).UseNagle;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUseTLSEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFTP(ExtObject).UseTLS);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetAccountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Account := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetAUTHCmdEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).AUTHCmd := TAuthCmd(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetAutoIssueFEATEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).AutoIssueFEAT := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetBoundIPEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).BoundIP := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetBoundPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).BoundPort := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetBoundPortMaxEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).BoundPortMax := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetConnectionTimeOutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ConnectTimeout := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetDataPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).DataPort := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetDataPortMaxEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).DataPortMax := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetDataPortMinEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).DataPortMin := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetDataPortProtectionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).DataPortProtection := TIdFTPDataPortSecurity(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetExternalIPEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ExternalIP := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Host := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetIPVersionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).IPVersion := TIdIPVersion(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetListenTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ListenTimeout := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetPassiveUseControlHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).PassiveUseControlHost := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetProxySettingsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ProxySettings := TIdFtpProxySettings(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetReadTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ReadTimeout := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetReuseSocketEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ReuseSocket := TIdReuseSocket(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetTransferTimeoutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).TransferTimeout := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetTransferTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).TransferType := TIdFTPTransferType(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetTryNATFastTrackEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).TryNATFastTrack := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseCCCEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseCCC := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseExtensionDataPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseExtensionDataPort := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseHOSTEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseHOST := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseMLISEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseMLIS := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseNagleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseNagle := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUseTLSEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).UseTLS := TIdUseTLS(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsGetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFtpProxySettings(ExtObject).Host;
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsGetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFtpProxySettings(ExtObject).Password;
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsGetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFtpProxySettings(ExtObject).Port;
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsGetProxyTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdFtpProxySettings(ExtObject).ProxyType);
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsGetUsernameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFtpProxySettings(ExtObject).Username;
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsSetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFtpProxySettings(ExtObject).Host := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsSetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFtpProxySettings(ExtObject).Password := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsSetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFtpProxySettings(ExtObject).Port := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsSetProxyTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFtpProxySettings(ExtObject).ProxyType := TIdFtpProxyType(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPProxySettingsMethodsSetUsernameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFtpProxySettings(ExtObject).Username := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsGetAddressEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEmailAddressItem(ExtObject).Address;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsGetDomainEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEmailAddressItem(ExtObject).Domain;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsGetNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEmailAddressItem(ExtObject).Name;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEmailAddressItem(ExtObject).Text;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsGetUserEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdEmailAddressItem(ExtObject).User;
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsSetAddressEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEmailAddressItem(ExtObject).Address := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsSetDomainEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEmailAddressItem(ExtObject).Domain := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsSetNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEmailAddressItem(ExtObject).Name := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsSetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEmailAddressItem(ExtObject).Text := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdEMailAddressItemMethodsSetUserEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdEmailAddressItem(ExtObject).User := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetContentDispositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ContentDisposition := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetContentDispositionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).ContentDisposition;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetUIDEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).UID := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetUIDEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).UID;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetAttachmentTempDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).AttachmentTempDirectory := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetAttachmentTempDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).AttachmentTempDirectory;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetConvertPreambleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ConvertPreamble := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetConvertPreambleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).ConvertPreamble;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Encoding := TIdMessageEncoding(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdMessage(ExtObject).Encoding);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetFlagsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
//  TIdMessage(ExtObject).Flags := TIdMessageFlagsSet(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetFlagsEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
//  Info.ResultAsInteger := Integer(TIdMessage(ExtObject).Flags);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetUseNowForDateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).UseNowForDate := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetUseNowForDateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).UseNowForDate;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetSubjectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Subject := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetSubjectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).Subject;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetIsMsgSinglePartMimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).IsMsgSinglePartMime := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetIsMsgSinglePartMimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).IsMsgSinglePartMime;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetInReplyToEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).InReplyTo := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetInReplyToEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).InReplyTo;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetReferencesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).References := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetReferencesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).References;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetOrganizationEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Organization := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetOrganizationEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).Organization;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetNoDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).NoDecode := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetNoDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).NoDecode;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetNoEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).NoEncode := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetNoEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).NoEncode;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsetContentTransferEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ContentTransferEncoding := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetContentTransferEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).ContentTransferEncoding;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ContentType := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).ContentType;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetCharsetEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Charset := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetCharsetEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).Charset;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetBodyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Body.Text := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetBodyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).Body.Text;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetAttachmentEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).AttachmentEncoding := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetAttachmentEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).AttachmentEncoding;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetMsgIDEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).MsgID := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetMsgIDEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdMessage(ExtObject).MsgId;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSetIsEncodedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).IsEncoded := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGetIsEncodedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdMessage(ExtObject).IsEncoded;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsSaveToFileEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).SaveToFile(Info.ParamAsString[0], Info.ParamAsBoolean[1]);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsProcessHeadersEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ProcessHeaders;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsLoadFromFileEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).LoadFromFile(Info.ParamAsString[0], Info.ParamAsBoolean[1]);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsGenerateHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).GenerateHeader;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsClearHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ClearHeader;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsAddHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).AddHeader(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsClearBodyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).ClearBody;
end;

procedure TdwsIndyLib.dwsTIdMailMessageMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdMessage(ExtObject).Clear;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetHeloNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).HeloName;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetMailAgentEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).MailAgent;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetPipeLineEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).PipeLine;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetUseEhloEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).UseEhlo;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetUseVerpEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).UseVerp;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetVerpDelimsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).VerpDelims;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetHeloNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).HeloName := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetMailAgentEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).MailAgent := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetPipeLineEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).PipeLine := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetUseEhloEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).UseEhlo := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetUseVerpEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).UseVerp := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetVerpDelimsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).VerpDelims := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdMailMessageConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TIdMessage.Create(nil);
end;

procedure TdwsIndyLib.dwsTIdSMTPConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TIdSMTP.Create(nil);
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsAuthenticateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).Authenticate;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsConnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Connect;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsDisconnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Disconnect;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetAuthTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdSMTP(ExtObject).AuthType);
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetDidAuthenticateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).DidAuthenticate;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).Host;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).Password;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdSMTP(ExtObject).Port;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetUsernameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).Username;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetUseTLSEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TIdSMTP(ExtObject).UseTLS);
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsGetValidateAuthLoginCapabilityEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdSMTP(ExtObject).ValidateAuthLoginCapability;
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSendEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Send(TIdMessage(Info.Vars['MailMessage'].ExternalObject));
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetAuthTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).AuthType := TIdSMTPAuthenticationType(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetHostEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Host := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Password := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Port := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetUsernameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).Username := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetUseTLSEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).UseTLS := TIdUseTls(Info.ParamAsInteger[0]);
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsSetValidateAuthLoginCapabilityEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdSMTP(ExtObject).ValidateAuthLoginCapability := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdSMTPMethodsVerifyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdSMTP(ExtObject).Verify(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdSMTPCleanUp(
  ExternalObject: TObject);
begin
  ExternalObject.Free;
end;

procedure TdwsIndyLib.dwsTIdMailMessageCleanUp(
  ExternalObject: TObject);
begin
  ExternalObject.Free;
end;

procedure TdwsIndyLib.dwsTIdFTPCleanUp(ExternalObject: TObject);
begin
  if ExternalObject is TIdFTP then
    TIdFTP(ExternalObject).Free;
end;

procedure TdwsIndyLib.dwsTIdFTPConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TIdFTP.Create(Self);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsAbortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Abort;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsChangeDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ChangeDir(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsChangeDirUpEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).ChangeDirUp;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsConnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Connect;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsCRCEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  TIdFTP(ExtObject).CRC(Info.ParamAsString[0], Info.ParamAsInteger[1],
    Info.ParamAsInteger[2]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsDeleteEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Delete(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsDisconnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Disconnect;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  TIdFTP(ExtObject).Get(Info.ParamAsString[0], Info.ParamAsString[1],
    Info.ParamAsBoolean[2], Info.ParamAsBoolean[3]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetPassiveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).Passive;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).Password;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).Port
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetResumeSupportedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIdFTP(ExtObject).ResumeSupported;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsGetUserNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).Username
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsLoginEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Login;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsMakeDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).MakeDir(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsNoopEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  TIdFTP(ExtObject).Noop;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsPutEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  TIdFTP(ExtObject).Put(Info.ParamAsString[0], Info.ParamAsString[1],
                        Info.ParamAsBoolean[2]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsRemoveDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).RemoveDir(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsRenameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Rename(Info.ParamAsString[0], Info.ParamAsString[1]);
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsRetrieveCurrentDirEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TIdFTP(ExtObject).RetrieveCurrentDir;
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetPassiveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Passive := Info.ParamAsBoolean[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetPasswordEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Password := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Port := Info.ParamAsInteger[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSetUserNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIdFTP(ExtObject).Username := Info.ParamAsString[0];
end;

procedure TdwsIndyLib.dwsTIdFTPMethodsSizeEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  Info.ResultAsInteger := TIdFTP(ExtObject).Size(Info.ParamAsString[0]);
end;

procedure TdwsIndyLib.dwsSendEmailEval(info: TProgramInfo);
var
  MailMessage: TIdMessage;
begin
  with TIdSMTP.Create(nil) do
  try
    // setup SMTP
    Host := Info.ParamAsString[0];
    Port := 25;

    // setup mail message
    MailMessage := TIdMessage.Create(nil);
    try
      MailMessage.From.Address := 'SmartMS';
      MailMessage.Recipients.EMailAddresses := Info.ParamAsString[1];

      MailMessage.Subject := Info.ParamAsString[3];
      MailMessage.Body.Text := Info.ParamAsString[2];

      if FileExists(Info.ParamAsString[4]) then
        TIdAttachmentFile.Create(MailMessage.MessageParts, Info.ParamAsString[4]);

      // send mail
      try
        Connect;
        Send(MailMessage);
      finally
        if Connected then
          Disconnect;
      end;
    finally
      FreeAndNil(MailMessage);
    end;
  finally
    Free;
  end;
end;

procedure TdwsIndyLib.dwsUploadEval(info: TProgramInfo);
begin
  with TIdFtp.Create(nil) do
  try
    Host := info.ValueAsString['Host'];
    Username := info.ValueAsString['Username'];
    Password := info.ValueAsString['Password'];
    AutoLogin := True;
    Connect;
    ChangeDir(info.ValueAsString['Path']);
    Put(info.ValueAsString['Filemame']);
    Disconnect;
  finally
    Free;
  end;
end;

procedure TdwsIndyLib.dwsUploadPortEval(info: TProgramInfo);
begin
  with TIdFtp.Create(nil) do
  try
    Host := info.ValueAsString['Host'];
    Port := info.ValueAsInteger['Port'];
    Username := info.ValueAsString['Username'];
    Password := info.ValueAsString['Password'];
    AutoLogin := True;
    Connect;
    ChangeDir(info.ValueAsString['Path']);
    Put(info.ValueAsString['Filemame']);
    Disconnect;
  finally
    Free;
  end;
end;

end.
