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
{    The Initial Developer of the Original DWS Code is Matthias        }
{    Ackermann.                                                        }
{    For other initial contributors, see DWS contributors.txt          }
{    Ackermann.                                                        }
{    DWS code is currently maintained by Eric Grange.                  }
{                                                                      }
{    Current maintainer of the IDE utility: Brian Frost                }
{                                                                      }
{**********************************************************************}

unit UDwsIdeConfig;

interface

uses
  XmlDom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLProjectConfigType = interface;
  IXMLEditorPagesType = interface;
  IXMLEditorPageType = interface;
  IXMLEditorPageTypeList = interface;
  IXMLBreakpointsType = interface;
  IXMLBreakpointType = interface;
  IXMLWatchesType = interface;
  IXMLWatchType = interface;

{ IXMLProjectConfigType }

  IXMLProjectConfigType = interface(IXMLNode)
    ['{7ACD731B-21B0-40BA-A21A-75347D15003A}']
    { Property Accessors }
    function Get_EditorPages: IXMLEditorPagesType;
    function Get_Breakpoints: IXMLBreakpointsType;
    function Get_Watches: IXMLWatchesType;
    { Methods & Properties }
    property EditorPages: IXMLEditorPagesType read Get_EditorPages;
    property Breakpoints: IXMLBreakpointsType read Get_Breakpoints;
    property Watches: IXMLWatchesType read Get_Watches;
  end;

{ IXMLEditorPagesType }

  IXMLEditorPagesType = interface(IXMLNode)
    ['{3B76CC46-FAC8-4E13-BF45-EF98B173B873}']
    { Property Accessors }
    function Get_EditorPage: IXMLEditorPageTypeList;
    function Get_ActivePageIndex: Integer;
    procedure Set_ActivePageIndex(Value: Integer);
    { Methods & Properties }
    property EditorPage: IXMLEditorPageTypeList read Get_EditorPage;
    property ActivePageIndex: Integer read Get_ActivePageIndex write Set_ActivePageIndex;
  end;

{ IXMLEditorPageType }

  IXMLEditorPageType = interface(IXMLNode)
    ['{ABA0B20A-9A99-4428-B2D1-60C40A74DC56}']
    { Property Accessors }
    function Get_Name: UnicodeString;
    function Get_FileName: UnicodeString;
    procedure Set_Name(Value: UnicodeString);
    procedure Set_FileName(Value: UnicodeString);
    { Methods & Properties }
    property Name: UnicodeString read Get_Name write Set_Name;
    property FileName: UnicodeString read Get_FileName write Set_FileName;
  end;

{ IXMLEditorPageTypeList }

  IXMLEditorPageTypeList = interface(IXMLNodeCollection)
    ['{A65E76C1-5BA4-40DD-8099-CC034F71690D}']
    { Methods & Properties }
    function Add: IXMLEditorPageType;
    function Insert(const Index: Integer): IXMLEditorPageType;

    function Get_Item(Index: Integer): IXMLEditorPageType;
    property Items[Index: Integer]: IXMLEditorPageType read Get_Item; default;
  end;

{ IXMLBreakpointsType }

  IXMLBreakpointsType = interface(IXMLNodeCollection)
    ['{DDCEF234-FC76-4CF4-886B-79349BC0FC55}']
    { Property Accessors }
    function Get_Breakpoint(Index: Integer): IXMLBreakpointType;
    { Methods & Properties }
    function Add: IXMLBreakpointType;
    function Insert(const Index: Integer): IXMLBreakpointType;
    property Breakpoint[Index: Integer]: IXMLBreakpointType read Get_Breakpoint; default;
  end;

{ IXMLBreakpointType }

  IXMLBreakpointType = interface(IXMLNode)
    ['{5EF45653-B9C1-4257-A6BE-559C276FAEDC}']
    { Property Accessors }
    function Get_SourceName: UnicodeString;
    function Get_LineNum: Integer;
    function Get_Enabled: Boolean;
    procedure Set_SourceName(Value: UnicodeString);
    procedure Set_LineNum(Value: Integer);
    procedure Set_Enabled(Value: Boolean);
    { Methods & Properties }
    property SourceName: UnicodeString read Get_SourceName write Set_SourceName;
    property LineNum: Integer read Get_LineNum write Set_LineNum;
    property Enabled: Boolean read Get_Enabled write Set_Enabled;
  end;

{ IXMLWatchesType }

  IXMLWatchesType = interface(IXMLNodeCollection)
    ['{6356F1D3-2211-4F93-B5A9-3E697A9C9905}']
    { Property Accessors }
    function Get_Watch(Index: Integer): IXMLWatchType;
    { Methods & Properties }
    function Add: IXMLWatchType;
    function Insert(const Index: Integer): IXMLWatchType;
    property Watch[Index: Integer]: IXMLWatchType read Get_Watch; default;
  end;

{ IXMLWatchType }

  IXMLWatchType = interface(IXMLNode)
    ['{4E786A81-3270-42BD-8CF9-4AC059B876D7}']
    { Property Accessors }
    function Get_Expression: UnicodeString;
    procedure Set_Expression(Value: UnicodeString);
    { Methods & Properties }
    property Expression: UnicodeString read Get_Expression write Set_Expression;
  end;

{ Forward Decls }

  TXMLProjectConfigType = class;
  TXMLEditorPagesType = class;
  TXMLEditorPageType = class;
  TXMLEditorPageTypeList = class;
  TXMLBreakpointsType = class;
  TXMLBreakpointType = class;
  TXMLWatchesType = class;
  TXMLWatchType = class;

{ TXMLProjectConfigType }

  TXMLProjectConfigType = class(TXMLNode, IXMLProjectConfigType)
  protected
    { IXMLProjectConfigType }
    function Get_EditorPages: IXMLEditorPagesType;
    function Get_Breakpoints: IXMLBreakpointsType;
    function Get_Watches: IXMLWatchesType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEditorPagesType }

  TXMLEditorPagesType = class(TXMLNode, IXMLEditorPagesType)
  private
    FEditorPage: IXMLEditorPageTypeList;
  protected
    { IXMLEditorPagesType }
    function Get_EditorPage: IXMLEditorPageTypeList;
    function Get_ActivePageIndex: Integer;
    procedure Set_ActivePageIndex(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEditorPageType }

  TXMLEditorPageType = class(TXMLNode, IXMLEditorPageType)
  protected
    { IXMLEditorPageType }
    function Get_Name: UnicodeString;
    function Get_FileName: UnicodeString;
    procedure Set_Name(Value: UnicodeString);
    procedure Set_FileName(Value: UnicodeString);
  end;

{ TXMLEditorPageTypeList }

  TXMLEditorPageTypeList = class(TXMLNodeCollection, IXMLEditorPageTypeList)
  protected
    { IXMLEditorPageTypeList }
    function Add: IXMLEditorPageType;
    function Insert(const Index: Integer): IXMLEditorPageType;

    function Get_Item(Index: Integer): IXMLEditorPageType;
  end;

{ TXMLBreakpointsType }

  TXMLBreakpointsType = class(TXMLNodeCollection, IXMLBreakpointsType)
  protected
    { IXMLBreakpointsType }
    function Get_Breakpoint(Index: Integer): IXMLBreakpointType;
    function Add: IXMLBreakpointType;
    function Insert(const Index: Integer): IXMLBreakpointType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBreakpointType }

  TXMLBreakpointType = class(TXMLNode, IXMLBreakpointType)
  protected
    { IXMLBreakpointType }
    function Get_SourceName: UnicodeString;
    function Get_LineNum: Integer;
    function Get_Enabled: Boolean;
    procedure Set_SourceName(Value: UnicodeString);
    procedure Set_LineNum(Value: Integer);
    procedure Set_Enabled(Value: Boolean);
  end;

{ TXMLWatchesType }

  TXMLWatchesType = class(TXMLNodeCollection, IXMLWatchesType)
  protected
    { IXMLWatchesType }
    function Get_Watch(Index: Integer): IXMLWatchType;
    function Add: IXMLWatchType;
    function Insert(const Index: Integer): IXMLWatchType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLWatchType }

  TXMLWatchType = class(TXMLNode, IXMLWatchType)
  protected
    { IXMLWatchType }
    function Get_Expression: UnicodeString;
    procedure Set_Expression(Value: UnicodeString);
  end;

{ Global Functions }

function GetProjectConfig(Doc: IXMLDocument): IXMLProjectConfigType;
function LoadProjectConfig(const FileName: string): IXMLProjectConfigType;
function NewProjectConfig: IXMLProjectConfigType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetProjectConfig(Doc: IXMLDocument): IXMLProjectConfigType;
begin
  Result := Doc.GetDocBinding('ProjectConfig', TXMLProjectConfigType, TargetNamespace) as IXMLProjectConfigType;
end;

function LoadProjectConfig(const FileName: string): IXMLProjectConfigType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ProjectConfig', TXMLProjectConfigType, TargetNamespace) as IXMLProjectConfigType;
end;

function NewProjectConfig: IXMLProjectConfigType;
begin
  Result := NewXMLDocument.GetDocBinding('ProjectConfig', TXMLProjectConfigType, TargetNamespace) as IXMLProjectConfigType;
end;

{ TXMLProjectConfigType }

procedure TXMLProjectConfigType.AfterConstruction;
begin
  RegisterChildNode('EditorPages', TXMLEditorPagesType);
  RegisterChildNode('Breakpoints', TXMLBreakpointsType);
  RegisterChildNode('Watches', TXMLWatchesType);
  inherited;
end;

function TXMLProjectConfigType.Get_EditorPages: IXMLEditorPagesType;
begin
  Result := ChildNodes['EditorPages'] as IXMLEditorPagesType;
end;

function TXMLProjectConfigType.Get_Breakpoints: IXMLBreakpointsType;
begin
  Result := ChildNodes['Breakpoints'] as IXMLBreakpointsType;
end;

function TXMLProjectConfigType.Get_Watches: IXMLWatchesType;
begin
  Result := ChildNodes['Watches'] as IXMLWatchesType;
end;

{ TXMLEditorPagesType }

procedure TXMLEditorPagesType.AfterConstruction;
begin
  RegisterChildNode('EditorPage', TXMLEditorPageType);
  FEditorPage := CreateCollection(TXMLEditorPageTypeList, IXMLEditorPageType, 'EditorPage') as IXMLEditorPageTypeList;
  inherited;
end;

function TXMLEditorPagesType.Get_EditorPage: IXMLEditorPageTypeList;
begin
  Result := FEditorPage;
end;

function TXMLEditorPagesType.Get_ActivePageIndex: Integer;
begin
  Result := ChildNodes['ActivePageIndex'].NodeValue;
end;

procedure TXMLEditorPagesType.Set_ActivePageIndex(Value: Integer);
begin
  ChildNodes['ActivePageIndex'].NodeValue := Value;
end;

{ TXMLEditorPageType }

function TXMLEditorPageType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLEditorPageType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('name', Value);
end;

function TXMLEditorPageType.Get_FileName: UnicodeString;
begin
  Result := ChildNodes['FileName'].Text;
end;

procedure TXMLEditorPageType.Set_FileName(Value: UnicodeString);
begin
  ChildNodes['FileName'].NodeValue := Value;
end;

{ TXMLEditorPageTypeList }

function TXMLEditorPageTypeList.Add: IXMLEditorPageType;
begin
  Result := AddItem(-1) as IXMLEditorPageType;
end;

function TXMLEditorPageTypeList.Insert(const Index: Integer): IXMLEditorPageType;
begin
  Result := AddItem(Index) as IXMLEditorPageType;
end;

function TXMLEditorPageTypeList.Get_Item(Index: Integer): IXMLEditorPageType;
begin
  Result := List[Index] as IXMLEditorPageType;
end;

{ TXMLBreakpointsType }

procedure TXMLBreakpointsType.AfterConstruction;
begin
  RegisterChildNode('Breakpoint', TXMLBreakpointType);
  ItemTag := 'Breakpoint';
  ItemInterface := IXMLBreakpointType;
  inherited;
end;

function TXMLBreakpointsType.Get_Breakpoint(Index: Integer): IXMLBreakpointType;
begin
  Result := List[Index] as IXMLBreakpointType;
end;

function TXMLBreakpointsType.Add: IXMLBreakpointType;
begin
  Result := AddItem(-1) as IXMLBreakpointType;
end;

function TXMLBreakpointsType.Insert(const Index: Integer): IXMLBreakpointType;
begin
  Result := AddItem(Index) as IXMLBreakpointType;
end;

{ TXMLBreakpointType }

function TXMLBreakpointType.Get_SourceName: UnicodeString;
begin
  Result := ChildNodes['SourceName'].Text;
end;

procedure TXMLBreakpointType.Set_SourceName(Value: UnicodeString);
begin
  ChildNodes['SourceName'].NodeValue := Value;
end;

function TXMLBreakpointType.Get_LineNum: Integer;
begin
  Result := ChildNodes['LineNum'].NodeValue;
end;

procedure TXMLBreakpointType.Set_LineNum(Value: Integer);
begin
  ChildNodes['LineNum'].NodeValue := Value;
end;

function TXMLBreakpointType.Get_Enabled: Boolean;
begin
  Result := ChildNodes['Enabled'].NodeValue;
end;

procedure TXMLBreakpointType.Set_Enabled(Value: Boolean);
begin
  ChildNodes['Enabled'].NodeValue := Value;
end;

{ TXMLWatchesType }

procedure TXMLWatchesType.AfterConstruction;
begin
  RegisterChildNode('Watch', TXMLWatchType);
  ItemTag := 'Watch';
  ItemInterface := IXMLWatchType;
  inherited;
end;

function TXMLWatchesType.Get_Watch(Index: Integer): IXMLWatchType;
begin
  Result := List[Index] as IXMLWatchType;
end;

function TXMLWatchesType.Add: IXMLWatchType;
begin
  Result := AddItem(-1) as IXMLWatchType;
end;

function TXMLWatchesType.Insert(const Index: Integer): IXMLWatchType;
begin
  Result := AddItem(Index) as IXMLWatchType;
end;

{ TXMLWatchType }

function TXMLWatchType.Get_Expression: UnicodeString;
begin
  Result := ChildNodes['Expression'].Text;
end;

procedure TXMLWatchType.Set_Expression(Value: UnicodeString);
begin
  ChildNodes['Expression'].NodeValue := Value;
end;

end.
