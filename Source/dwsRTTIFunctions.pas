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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsRTTIFunctions;

{$I dws.inc}

interface

uses dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators, dwsStack,
   dwsTokenizer, SysUtils, dwsUtils, dwsMagicExprs, dwsUnitSymbols, dwsCoreExprs;

type
   TRTTIRawAttributesFunc = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TTypeOfClassFunc = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TRTTITypeInfoNameMethod = class(TInternalRecordMethod)
      procedure Execute(info : TProgramInfo); override;
   end;

   TSameRTTITypeInfoFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
   end;

   TRTTISymbolNameMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;
   TRTTISymbolTypMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TRTTIPropertyCapabilitiesMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
      class function Capabilities(propSym : TPropertySymbol) : Integer; static;
   end;
   TRTTIPropertyGetterMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;
   TRTTIPropertySetterMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

const
   SYS_TRTTITYPEINFO = 'TRTTITypeInfo';
   SYS_TRTTIRAWATTRIBUTE = 'TRTTIRawAttribute';
   SYS_TRTTIRAWATTRIBUTES = 'TRTTIRawAttributes';
   SYS_RTTIRAWATTRIBUTES = 'RTTIRawAttributes';

   SYS_RTTIPROPERTYATTRIBUTE = 'RTTIPropertyAttribute';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsInfo;

// RegisterRTTITypes
//
procedure RegisterRTTITypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                            unitTable : TSymbolTable);
var
   typTypInfo : TRecordSymbol;
   typRawAttribute : TRecordSymbol;
   clsPropAttribute : TClassSymbol;
begin
   typTypInfo:=TRecordSymbol.Create(SYS_TRTTITYPEINFO, nil);
   systemTable.AddSymbol(typTypInfo);
   typTypInfo.AddField(TFieldSymbol.Create('ID', systemTable.TypInteger, cvPrivate));
   TRTTITypeInfoNameMethod.Create(mkFunction, [], 'Name',
                                  [], SYS_STRING,
                                  typTypInfo, cvPublic, systemTable);

   typRawAttribute:=TRecordSymbol.Create(SYS_TRTTIRAWATTRIBUTE, nil);
   systemTable.AddSymbol(typRawAttribute);
   typRawAttribute.AddField(TFieldSymbol.Create('T', typTypInfo, cvPublic));
   typRawAttribute.AddField(TFieldSymbol.Create('A', systemTable.TypCustomAttribute, cvPublic));

   clsPropAttribute:=TClassSymbol.Create(SYS_RTTIPROPERTYATTRIBUTE, nil);
   systemTable.AddSymbol(clsPropAttribute);
   clsPropAttribute.InheritFrom(systemTable.TypCustomAttribute);
   TRTTISymbolNameMethod.Create(mkFunction, [], 'Name',
                                  [], SYS_STRING, clsPropAttribute, cvPublic, systemTable);
   TRTTISymbolTypMethod.Create(mkFunction, [], 'Typ',
                                  [], SYS_TRTTITYPEINFO, clsPropAttribute, cvPublic, systemTable);
   clsPropAttribute.AddConst(TClassConstSymbol.Create('capReadable', systemTable.TypInteger, 1));
   clsPropAttribute.AddConst(TClassConstSymbol.Create('capWriteable', systemTable.TypInteger, 2));
   TRTTIPropertyCapabilitiesMethod.Create(mkFunction, [], 'Capabilities',
                                    [], SYS_INTEGER,
                                    clsPropAttribute, cvPublic, systemTable);
   TRTTIPropertyGetterMethod.Create(mkFunction, [], 'Getter',
                                    ['handle', SYS_VARIANT], SYS_VARIANT,
                                    clsPropAttribute, cvPublic, systemTable);
   TRTTIPropertySetterMethod.Create(mkFunction, [], 'Setter',
                                    ['handle', SYS_VARIANT, 'value', SYS_VARIANT], SYS_STRING,
                                    clsPropAttribute, cvPublic, systemTable);

   systemTable.AddSymbol(
      TDynamicArraySymbol.Create(SYS_TRTTIRAWATTRIBUTES,
                                 typRawAttribute,
                                 systemTable.TypInteger)
      );
end;

// RegisterRTTIOperators
//
procedure RegisterRTTIOperators(systemTable : TSystemSymbolTable;
                                unitTable : TSymbolTable; operators : TOperators);
var
   typTypInfo : TRecordSymbol;
begin
   typTypInfo:=systemTable.FindTypeSymbol(SYS_TRTTITYPEINFO, cvMagic) as TRecordSymbol;

   operators.RegisterOperator(ttEQ, unitTable.FindSymbol('SameRTTITypeInfo', cvMagic) as TFuncSymbol, typTypInfo, typTypInfo);
end;

// PrepareRTTIRawAttributes
//
procedure PrepareRTTIRawAttributes(info : TProgramInfo; var scriptObj : IScriptObj);
var
   typRawAttribute : TRecordSymbol;
   dynArray : TScriptDynamicArray;
   attributes : TdwsSymbolAttributes;
   properties : TSimpleSymbolList;
   i, n : Integer;
   attrib : TdwsSymbolAttribute;
   symbolClassType : TClass;
   rttiPropertyAttributeCreate : IInfo;
   attribute : IInfo;
   symbol : TSymbol;
   propertySymbol : TPropertySymbol;
   fieldSymbol : TFieldSymbol;
begin
   typRawAttribute:=info.Execution.Prog.Table.FindTypeSymbol(SYS_TRTTIRAWATTRIBUTE, cvPublic) as TRecordSymbol;
   dynArray:=TScriptDynamicArray.Create(typRawAttribute);
   scriptObj:=dynArray;
   info.Execution.RTTIRawAttributes:=scriptObj;

   rttiPropertyAttributeCreate:=Info.Vars[SYS_RTTIPROPERTYATTRIBUTE].Method[SYS_TOBJECT_CREATE];

   properties:=info.Execution.Prog.CollectAllPublishedSymbols;
   try

      attributes:=info.Execution.Prog.Attributes;

      dynArray.Length:=attributes.Count+properties.Count*2;

      for i:=0 to attributes.Count-1 do begin
         attrib:=attributes[i];
         symbolClassType:=attrib.Symbol.ClassType;
         if symbolClassType=TClassSymbol then begin
            dynArray.Data[i*2]:=Int64(attrib.Symbol);
            dynArray.Data[i*2+1]:=attrib.AttributeConstructor.Eval(info.Execution);
         end else Assert(False);
      end;

      n:=attributes.Count*2;
      for i:=0 to properties.Count-1 do begin
         symbol:=properties[i];
         symbolClassType:=symbol.ClassType;
         if symbolClassType=TPropertySymbol then begin
            propertySymbol:=TPropertySymbol(symbol);
            dynArray.Data[n]:=Int64(propertySymbol.OwnerSymbol);
            attribute:=rttiPropertyAttributeCreate.Call;
            dynArray.Data[n+1]:=attribute.Value;
            attribute.ExternalObject:=propertySymbol;
            Inc(n, 2);
         end else if symbolClassType=TFieldSymbol then begin
            fieldSymbol:=TFieldSymbol(symbol);
            dynArray.Data[n]:=Int64(fieldSymbol.StructSymbol);
            attribute:=rttiPropertyAttributeCreate.Call;
            dynArray.Data[n+1]:=attribute.Value;
            attribute.ExternalObject:=fieldSymbol;
            Inc(n, 2);
         end;
      end;

   finally
      properties.Free;
   end;
end;

// ------------------
// ------------------ TRTTIRawAttributesFunc ------------------
// ------------------

// Execute
//
procedure TRTTIRawAttributesFunc.Execute(info : TProgramInfo);
var
   scriptObj : IScriptObj;
begin
   scriptObj:=info.Execution.RTTIRawAttributes;
   if not Assigned(scriptObj) then
      PrepareRTTIRawAttributes(info, scriptObj);
   info.Vars[SYS_RESULT].Value:=scriptObj;
end;

// ------------------
// ------------------ TTypeOfClassFunc ------------------
// ------------------

// Execute
//
procedure TTypeOfClassFunc.Execute(info : TProgramInfo);
begin
   info.Vars[SYS_RESULT].Member['ID'].Value:=info.ValueAsVariant['class'];
end;

// ------------------
// ------------------ TRTTITypeInfoNameMethod ------------------
// ------------------

// Execute
//
procedure TRTTITypeInfoNameMethod.Execute(info : TProgramInfo);
begin
   Info.ResultAsString:=TSymbol(Info.Vars['Self'].Member['ID'].ValueAsInteger).Name;
end;

// ------------------
// ------------------ TRTTISymbolNameMethod ------------------
// ------------------

// Execute
//
procedure TRTTISymbolNameMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   Info.ResultAsString:=TSymbol(ExternalObject).Name;
end;

// ------------------
// ------------------ TSameRTTITypeInfoFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TSameRTTITypeInfoFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
begin
   Result:=(args.ExprBase[0].Eval(args.Exec)=args.ExprBase[1].Eval(args.Exec));
end;

// ------------------
// ------------------ TRTTISymbolTypMethod ------------------
// ------------------

// Execute
//
procedure TRTTISymbolTypMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   Info.ResultAsInteger:=Int64((ExternalObject as TSymbol).Typ);
end;

// ------------------
// ------------------ TRTTIPropertyCapabilitiesMethod ------------------
// ------------------

// Execute
//
procedure TRTTIPropertyCapabilitiesMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   propSym : TSymbol;
begin
   propSym:=ExternalObject as TSymbol;
   if propSym.ClassType=TPropertySymbol then
      Info.ResultAsInteger:=Capabilities(TPropertySymbol(propSym))
   else if propSym.ClassType=TFieldSymbol then
      Info.ResultAsInteger:=3
   else Info.ResultAsInteger:=0;
end;

// Capabilities
//
class function TRTTIPropertyCapabilitiesMethod.Capabilities(propSym : TPropertySymbol) : Integer;
begin
   Result:=0;
   if propSym.ReadSym<>nil then
      Inc(Result, 1);
   if propSym.WriteSym<>nil then
      Inc(Result, 2);
end;

// ------------------
// ------------------ TRTTIPropertyGetterMethod ------------------
// ------------------

// Execute
//
procedure TRTTIPropertyGetterMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   handle, propInfo : IInfo;
   propSym : TSymbol;
begin
   propSym:=ExternalObject as TSymbol;
   handle:=info.Vars['handle'];

   if propSym.ClassType=TPropertySymbol then
      propInfo:=TInfoProperty.Create(info, propSym.Typ, nil, 0, TPropertySymbol(propSym), handle.ScriptObj)
   else if propSym.ClassType=TFieldSymbol then
      propInfo:=TInfoData.Create(info, propSym.Typ, handle.ScriptObj.Data, TFieldSymbol(propSym).Offset);

   Info.ResultAsVariant:=propInfo.Value;
end;

// ------------------
// ------------------ TRTTIPropertySetterMethod ------------------
// ------------------

// Execute
//
procedure TRTTIPropertySetterMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   handle, propInfo : IInfo;
   propSym : TSymbol;
begin
   propSym:=ExternalObject as TSymbol;
   handle:=info.Vars['handle'];

   if propSym.ClassType=TPropertySymbol then
      propInfo:=TInfoProperty.Create(info, propSym.Typ, nil, 0, TPropertySymbol(propSym), handle.ScriptObj)
   else if propSym.ClassType=TFieldSymbol then
      propInfo:=TInfoData.Create(info, propSym.Typ, handle.ScriptObj.Data, TFieldSymbol(propSym).Offset);

   propInfo.Value:=Info.ValueAsVariant['value'];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterRTTITypes);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterRTTIOperators);

   RegisterInternalFunction(TRTTIRawAttributesFunc, SYS_RTTIRAWATTRIBUTES, [], SYS_TRTTIRAWATTRIBUTES, []);

   RegisterInternalFunction(TTypeOfClassFunc, 'TypeOf', ['class', SYS_TCLASS], SYS_TRTTITYPEINFO, [iffOverloaded]);
   RegisterInternalBoolFunction(TSameRTTITypeInfoFunc, 'SameRTTITypeInfo',
                                ['&t1', SYS_TRTTITYPEINFO, '&t2', SYS_TRTTITYPEINFO], [iffOverloaded]);

end.
