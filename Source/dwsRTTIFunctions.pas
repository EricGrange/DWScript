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

uses
   dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators,
   dwsStack, dwsDataContext, dwsExprList,
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
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
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

   TRTTIMethodInfoMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
      class function Info(methSym : TMethodSymbol) : Integer; static;
   end;
   TRTTIMethodVMTMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;
   TRTTIMethodCallMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

type
   TRTTIMethodInfoFlags = (
      infoOverlap = 1,
      infoOverride = 2,
      infoStatic = 4,
      infoClass = 8,
      infoOverload = 16,
      infoAbstract = 32,
      infoFinal = 64,
      infoConstructor = 128,
      infoDestructor = 256
   );

const
   SYS_TRTTITYPEINFO = 'TRTTITypeInfo';
   SYS_TRTTIRAWATTRIBUTE = 'TRTTIRawAttribute';
   SYS_TRTTIRAWATTRIBUTES = 'TRTTIRawAttributes';
   SYS_RTTIRAWATTRIBUTES = 'RTTIRawAttributes';

   SYS_RTTIPROPERTYATTRIBUTE = 'RTTIPropertyAttribute';
   SYS_RTTIMETHODATTRIBUTE = 'RTTIMethodAttribute';

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

   procedure AddIntClassConst(clsSym : TClassSymbol; const name : String; value : Integer);
   var
      ccs : TClassConstSymbol;
   begin
      ccs:=TClassConstSymbol.CreateValue(name, systemTable.TypInteger, value);
      ccs.Visibility:=cvPublic;
      clsSym.AddConst(ccs);
   end;

var
   typTypInfo : TRecordSymbol;
   typRawAttribute : TRecordSymbol;
   clsPropAttribute : TClassSymbol;
   clsMethAttribute : TClassSymbol;
   field : TFieldSymbol;
begin
   if systemTable.FindLocal(SYS_TRTTITYPEINFO)<>nil then exit;

   typTypInfo:=TRecordSymbol.Create(SYS_TRTTITYPEINFO, nil);
   typTypInfo.IsImmutable:=True;
   systemTable.AddSymbol(typTypInfo);
   typTypInfo.AddField(TFieldSymbol.Create('ID', systemTable.TypInteger, cvPrivate));
   TRTTITypeInfoNameMethod.Create(mkFunction, [], 'Name',
                                  [], SYS_STRING,
                                  typTypInfo, cvPublic, systemTable);

   typRawAttribute:=TRecordSymbol.Create(SYS_TRTTIRAWATTRIBUTE, nil);
   typRawAttribute.IsImmutable:=True;
   systemTable.AddSymbol(typRawAttribute);
   field:=TFieldSymbol.Create('T', typTypInfo, cvPublic);
   field.ExternalName:=field.Name;
   typRawAttribute.AddField(field);
   field:=TFieldSymbol.Create('A', systemTable.TypCustomAttribute, cvPublic);
   field.ExternalName:=field.Name;
   typRawAttribute.AddField(field);

   clsPropAttribute:=TClassSymbol.Create(SYS_RTTIPROPERTYATTRIBUTE, nil);
   systemTable.AddSymbol(clsPropAttribute);
   clsPropAttribute.InheritFrom(systemTable.TypCustomAttribute);
   TRTTISymbolNameMethod.Create(mkFunction, [], 'Name',
                                [], SYS_STRING, clsPropAttribute, cvPublic, systemTable);
   TRTTISymbolTypMethod.Create(mkFunction, [], 'Typ',
                               [], SYS_TRTTITYPEINFO, clsPropAttribute, cvPublic, systemTable);
   AddIntClassConst(clsPropAttribute, 'capReadable', 1);
   AddIntClassConst(clsPropAttribute, 'capWriteable', 2);
   TRTTIPropertyCapabilitiesMethod.Create(mkFunction, [], 'Capabilities',
                                          [], SYS_INTEGER,
                                          clsPropAttribute, cvPublic, systemTable);
   TRTTIPropertyGetterMethod.Create(mkFunction, [], 'Getter',
                                    ['handle', SYS_VARIANT], SYS_VARIANT,
                                    clsPropAttribute, cvPublic, systemTable);
   TRTTIPropertySetterMethod.Create(mkFunction, [], 'Setter',
                                    ['handle', SYS_VARIANT, 'value', SYS_VARIANT], SYS_STRING,
                                    clsPropAttribute, cvPublic, systemTable);

   clsMethAttribute:=TClassSymbol.Create(SYS_RTTIMETHODATTRIBUTE, nil);
   systemTable.AddSymbol(clsMethAttribute);
   clsMethAttribute.InheritFrom(systemTable.TypCustomAttribute);
   TRTTISymbolNameMethod.Create(mkFunction, [], 'Name',
                                [], SYS_STRING, clsMethAttribute, cvPublic, systemTable);
   TRTTISymbolTypMethod.Create(mkFunction, [], 'Typ',
                               [], SYS_TRTTITYPEINFO, clsMethAttribute, cvPublic, systemTable);
   AddIntClassConst(clsMethAttribute, 'infoAbstract', Ord(infoAbstract));
   AddIntClassConst(clsMethAttribute, 'infoOverride', Ord(infoOverride));
   AddIntClassConst(clsMethAttribute, 'infoFinal', Ord(infoFinal));
   AddIntClassConst(clsMethAttribute, 'infoOverload', Ord(infoOverload));
   AddIntClassConst(clsMethAttribute, 'infoOverlap', Ord(infoOverlap));
   AddIntClassConst(clsMethAttribute, 'infoClass', Ord(infoClass));
   AddIntClassConst(clsMethAttribute, 'infoStatic', Ord(infoStatic));
   TRTTIMethodInfoMethod.Create(mkFunction, [], 'Info',
                                [], SYS_INTEGER,
                                clsMethAttribute, cvPublic, systemTable);
   TRTTIMethodVMTMethod.Create(mkFunction, [], 'VMTIndex',
                               [], SYS_INTEGER,
                               clsMethAttribute, cvPublic, systemTable);
   TRTTIMethodCallMethod.Create(mkFunction, [], 'Call',
                                ['instance', SYS_VARIANT, 'args', 'array of const'], SYS_VARIANT,
                                clsMethAttribute, cvPublic, systemTable);

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
procedure PrepareRTTIRawAttributes(info : TProgramInfo; var scriptDynArray : IScriptDynArray);
var
   typRawAttribute : TRecordSymbol;
   dynArray : TScriptDynamicArray;
   attributes : TdwsSymbolAttributes;
   publishedSymbols : TSimpleSymbolList;
   i, n : Integer;
   attrib : TdwsSymbolAttribute;
   symbolClassType : TClass;
   rttiPropertyAttributeCreate : IInfo;
   rttiMethodAttributeCreate : IInfo;
   attribute : IInfo;
   symbol : TSymbol;
   propertySymbol : TPropertySymbol;
   fieldSymbol : TFieldSymbol;
   methSymbol : TMethodSymbol;
begin
   typRawAttribute:=info.Execution.Prog.Table.FindTypeSymbol(SYS_TRTTIRAWATTRIBUTE, cvPublic) as TRecordSymbol;
   dynArray:=TScriptDynamicArray.CreateNew(typRawAttribute);
   scriptDynArray:=dynArray;
   info.Execution.RTTIRawAttributes:=scriptDynArray;

   rttiPropertyAttributeCreate:=Info.Vars[SYS_RTTIPROPERTYATTRIBUTE].Method[SYS_TOBJECT_CREATE];
   rttiMethodAttributeCreate:=Info.Vars[SYS_RTTIMETHODATTRIBUTE].Method[SYS_TOBJECT_CREATE];

   publishedSymbols:=info.Execution.Prog.CollectAllPublishedSymbols(False);
   try

      attributes:=info.Execution.Prog.Attributes;

      dynArray.ArrayLength:=attributes.Count+publishedSymbols.Count*2;

      for i:=0 to attributes.Count-1 do begin
         attrib:=attributes[i];
         symbolClassType:=attrib.Symbol.ClassType;
         if symbolClassType=TClassSymbol then begin
            dynArray.AsInteger[i*2]:=Int64(attrib.Symbol);
            attrib.AttributeConstructor.EvalAsVariant(info.Execution, dynArray.AsPVariant(i*2+1)^);
         end else Assert(False);
      end;

      n:=attributes.Count*2;
      for i:=0 to publishedSymbols.Count-1 do begin
         symbol:=publishedSymbols[i];
         symbolClassType:=symbol.ClassType;
         if symbolClassType=TPropertySymbol then begin
            propertySymbol:=TPropertySymbol(symbol);
            dynArray.AsInteger[n]:=Int64(propertySymbol.OwnerSymbol);
            attribute:=rttiPropertyAttributeCreate.Call;
            dynArray.AsVariant[n+1]:=attribute.Value;
            attribute.ExternalObject:=propertySymbol;
            Inc(n, 2);
         end else if symbolClassType=TFieldSymbol then begin
            fieldSymbol:=TFieldSymbol(symbol);
            dynArray.AsInteger[n]:=Int64(fieldSymbol.StructSymbol);
            attribute:=rttiPropertyAttributeCreate.Call;
            dynArray.AsVariant[n+1]:=attribute.Value;
            attribute.ExternalObject:=fieldSymbol;
            Inc(n, 2);
         end else if symbolClassType.InheritsFrom(TMethodSymbol) then begin
            methSymbol:=TMethodSymbol(symbol);
            dynArray.AsInteger[n]:=Int64(methSymbol.StructSymbol);
            attribute:=rttiMethodAttributeCreate.Call;
            dynArray.AsVariant[n+1]:=attribute.Value;
            attribute.ExternalObject:=methSymbol;
            Inc(n, 2);
         end;
      end;

   finally
      publishedSymbols.Free;
   end;
end;

// ------------------
// ------------------ TRTTIRawAttributesFunc ------------------
// ------------------

// Execute
//
procedure TRTTIRawAttributesFunc.Execute(info : TProgramInfo);
var
   scriptDynArray : IScriptDynArray;
begin
   scriptDynArray:=info.Execution.RTTIRawAttributes;
   if not Assigned(scriptDynArray) then
      PrepareRTTIRawAttributes(info, scriptDynArray);
   info.Vars[SYS_RESULT].Value:=scriptDynArray;
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
var
   id : Int64;
begin
   id:=Info.Vars['Self'].Member['ID'].ValueAsInteger;
   if id<>0 then
      Info.ResultAsString:=TSymbol(id).Name
   else Info.ResultAsString:='';
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
function TSameRTTITypeInfoFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   v1, v2 : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v1);
   args.ExprBase[1].EvalAsVariant(args.Exec, v2);
   Result:=(v1=v2);
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
   locData : IDataContext;
begin
   propSym:=ExternalObject as TSymbol;
   handle:=info.Vars['handle'];

   if propSym.ClassType=TPropertySymbol then
      propInfo:=TInfoProperty.Create(info, propSym.Typ, info.Execution.DataContext_Nil,
                                     TPropertySymbol(propSym), handle.ScriptObj)
   else if propSym.ClassType=TFieldSymbol then begin
      info.Execution.DataContext_Create(handle.ScriptObj.AsData, TFieldSymbol(propSym).Offset, locData);
      propInfo:=TInfoData.Create(info, propSym.Typ, locData);
   end;

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
   locData : IDataContext;
begin
   propSym:=ExternalObject as TSymbol;
   handle:=info.Vars['handle'];

   if propSym.ClassType=TPropertySymbol then
      propInfo:=TInfoProperty.Create(info, propSym.Typ, info.Execution.DataContext_Nil,
                                     TPropertySymbol(propSym), handle.ScriptObj)
   else if propSym.ClassType=TFieldSymbol then begin
      info.Execution.DataContext_Create(handle.ScriptObj.AsData, TFieldSymbol(propSym).Offset, locData);
      propInfo:=TInfoData.Create(info, propSym.Typ, locData);
   end;

   propInfo.Value:=Info.ValueAsVariant['value'];
end;

// ------------------
// ------------------ TRTTIMethodInfoMethod ------------------
// ------------------

// Execute
//
procedure TRTTIMethodInfoMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   methSym : TMethodSymbol;
begin
   methSym:=ExternalObject as TMethodSymbol;
   Info.ResultAsInteger := Self.Info(methSym);
end;

// Info
//
class function TRTTIMethodInfoMethod.Info(methSym : TMethodSymbol) : Integer;
begin
   Result:=0;
   if methSym.IsAbstract then
      Inc(Result, Ord(infoAbstract));
   if methSym.IsOverride then
      Inc(Result, Ord(infoOverride));
   if methSym.IsFinal then
      Inc(Result, Ord(infoFinal));
   if methSym.IsOverloaded then
      Inc(Result, Ord(infoOverload));
   if methSym.IsOverlap then
      Inc(Result, Ord(infoOverlap));
   if methSym.IsClassMethod then
      Inc(Result, Ord(infoClass));
   if methSym.IsStatic then
      Inc(Result, Ord(infoStatic));
end;

// ------------------
// ------------------ TRTTIMethodVMTMethod ------------------
// ------------------

// Execute
//
procedure TRTTIMethodVMTMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   methSym : TMethodSymbol;
begin
   methSym:=ExternalObject as TMethodSymbol;
   Info.ResultAsInteger := methSym.VMTIndex;
end;

// ------------------
// ------------------ TRTTIMethodCallMethod ------------------
// ------------------

// Execute
//
procedure TRTTIMethodCallMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   methSym : TMethodSymbol;
   instanceInfo : IInfo;
   methInfo : IInfo;
   resultInfo : IInfo;
   data : TData;
   locData : IDataContext;
begin
   methSym:=ExternalObject as TMethodSymbol;

   if methSym.IsClassMethod then begin
      if methSym.IsStatic then begin
         SetLength(data, 1);
         data[0]:=Int64(methSym.StructSymbol);
         info.Execution.DataContext_Create(data, 0, locData);
         instanceInfo:=TInfoClass.Create(info, methSym.StructSymbol, locData);
         methInfo:=TInfoFunc.Create(info, methSym, info.Execution.DataContext_Nil,
                                    nil, nil, TClassSymbol(methSym.StructSymbol));
      end else begin
         SetLength(data, 1);
         data[0]:=info.Vars['instance'].ValueAsInteger;
         info.Execution.DataContext_Create(data, 0, locData);
         instanceInfo:=TInfoClass.Create(info, methSym.StructSymbol, locData);
         methInfo:=TInfoFunc.Create(info, methSym, info.Execution.DataContext_Nil,
                                    nil, IScriptObj(IUnknown(data[0])), TClassSymbol(methSym.StructSymbol));
      end;
   end else begin
      data:=info.Vars['instance'].Data;
      info.Execution.DataContext_Create(data, 0, locData);
      instanceInfo:=TInfoClassObj.Create(info, methSym.StructSymbol,locData);
      methInfo:=TInfoFunc.Create(info, methSym, info.Execution.DataContext_Nil,
                                 nil, IScriptObj(IUnknown(data[0])), TClassSymbol(methSym.StructSymbol));
   end;
   resultInfo:=methInfo.Call(info.Vars['args'].Data);
   if methSym.Typ<>nil then
      Info.ResultAsVariant:=resultInfo.Value;
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
