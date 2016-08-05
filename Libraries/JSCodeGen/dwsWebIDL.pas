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
unit dwsWebIDL;

{$I dws.inc}

interface

uses
  Classes, SysUtils, Variants, dwsUtils, dwsErrors, dwsTokenizer,
  dwsWebIDLTokenizer;

type
   TSpecialQualifier = (sqNone, sqGetter, sqSetter, sqCreator, sqDeleter,
      sqLegacyCaller);
   TSpecialQualifiers = set of TSpecialQualifier;

   TArgumentNameKeyword = (ankNone, ankAttribute, ankCallback, ankConst,
      ankCreator, ankDeleter, ankDictionary, ankEnum, ankException, ankGetter,
      ankImplements, ankInherit, ankInterface, ankLegacycaller, ankPartial,
      ankSetter, ankStatic, ankStringifier, ankTypedef, ankUnrestricted);

   TdwsWebIDL = class;

   TWiType = class(TRefCountedObject)
      protected
         function GetTypeName: string; virtual; abstract;
         function GetPascalTypeName: string; virtual;
      public
         property TypeName: string read GetTypeName;
         property PascalTypeName: string read GetPascalTypeName;
   end;

   TWiAnyType = class(TWiType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiBasePrimitiveType = class(TWiType)
   end;

   TWiBaseFloatType = class(TWiBasePrimitiveType)
      private
         FUnrestricted: Boolean;
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
      public
         property Unrestricted: Boolean read FUnrestricted write FUnrestricted;
   end;

   TWiFloatType = class(TWiBaseFloatType)
      public
         function GetTypeName: string; override;
   end;

   TWiDoubleType = class(TWiBaseFloatType)
      public
         function GetTypeName: string; override;
   end;

   TWiBaseIntegerType = class(TWiBasePrimitiveType)
      private
         FUnsigned: Boolean;
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
      public
         property Unsigned: Boolean read FUnsigned write FUnsigned;
   end;

   TWiShortType = class(TWiBaseIntegerType)
      protected
         function GetTypeName: string; override;
   end;

   TWiLongType = class(TWiBaseIntegerType)
      protected
         function GetTypeName: string; override;
   end;

   TWiLongLongType = class(TWiBaseIntegerType)
      protected
         function GetTypeName: string; override;
   end;

   TWiBooleanType = class(TWiBasePrimitiveType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiByteType = class(TWiBasePrimitiveType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiOctetType = class(TWiBasePrimitiveType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiDOMStringType = class(TWiType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiIdentifierType = class(TWiType)
      strict private
         FTypeName: string;
         FWebIDL: TdwsWebIDL;
      protected
         function GetPascalTypeName: string; override;
         function GetTypeName: string; override;
      public
         constructor Create(TypeName: string; WebIDL: TdwsWebIDL);
   end;

   TWiSequence = class(TWiType)
      private
         FElementType: TWiType;
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
      public
         constructor Create(ElementType: TWiType);

         property ElementType: TWiType read FElementType;
   end;

   TWiArray = class(TWiSequence)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiPromise = class(TWiType)
      strict private
         FTypeName: string;
      private
         FElementType: TWiType;
      protected
         function GetTypeName: string; override;
      public
         constructor Create(TypeName: string; ElementType: TWiType);

         property ElementType: TWiType read FElementType;
   end;

   TWiObjectType = class(TWiType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;

   TWiUnionType = class(TWiType)
      protected
         function GetTypeName: string; override;
         function GetPascalTypeName: string; override;
   end;


   TWiExtendedAttribute = class(TRefCountedObject)
      private
         FText: string;
      public
         constructor Create(Text: string);

         property Text: string read FText;
   end;

   TWiExtendedAttributeList = TObjectList<TWiExtendedAttribute>;

   TWiObject = class abstract (TRefCountedObject)
      protected
         function GetAsPascalCode: string; virtual; abstract;
   end;

   TWiWithExtendedAttribute = class abstract (TWiObject)
      private
         FExtendedAttributeList: TWiExtendedAttributeList;
         function GetExtendedAttribute(Index: Integer): TWiExtendedAttribute;
         function GetExtendedAttributeCount: Integer;
      protected
         constructor Create(ExtendedAttributeList: TWiExtendedAttributeList);

         function GetAsPascalCode: string; override;
      public
         procedure AddExtendedAttribute(const Value: TWiExtendedAttribute);

         property ExtendedAttribute[Index: Integer]: TWiExtendedAttribute read GetExtendedAttribute;
         property ExtendedAttributeCount: Integer read GetExtendedAttributeCount;
   end;

   TWiArgument = class(TWiWithExtendedAttribute)
      private
         FArgumentName: string;
         FArgumentType: TWiType;
         FIsOptional: Boolean;
         FHasEllipsis: Boolean;
         FDefaultValue: Variant;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(ArgumentName: string; ArgumentType: TWiType;
            ExtendedAttributeList: TWiExtendedAttributeList;
            IsOptional: Boolean = False);

         property ArgumentName: string read FArgumentName;
         property ArgumentType: TWiType read FArgumentType;
         property HasEllipsis: Boolean read FHasEllipsis write FHasEllipsis;
         property IsOptional: Boolean read FIsOptional;
         property DefaultValue: Variant read FDefaultValue write FDefaultValue;
         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiArgumentList = TObjectList<TWiArgument>;

   TWiMember = class(TWiWithExtendedAttribute);

   TWiInterfaceMember = class(TWiMember);

   TWiOperation = class(TWiInterfaceMember)
      private
         FOperationName: string;
         FArguments: TWiArgumentList;
         FReturnType: TWiType;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(ReturnType: TWiType; Arguments: TWiArgumentList;
            ExtendedAttributeList: TWiExtendedAttributeList); overload;
         constructor Create(OperationName: string; ReturnType: TWiType;
            Arguments: TWiArgumentList;
            ExtendedAttributeList: TWiExtendedAttributeList); overload;

         property ReturnType: TWiType read FReturnType write FReturnType;
         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiAttribute = class(TWiInterfaceMember)
      private
         FAttributeName: string;
         FAttributeType: TWiType;
         FReadOnly: Boolean;
         FInherit: Boolean;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(AttributeName: string; AttributeType: TWiType;
            ExtendedAttributeList: TWiExtendedAttributeList;
            Inherit: Boolean = False; ReadOnly: Boolean = False);

         property AttributeName: string read FAttributeName;
         property AttributeType: TWiType read FAttributeType;
         property &ReadOnly: Boolean read FReadOnly;
         property Inherit: Boolean read FInherit;

         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiDictionaryField = class(TWiMember)
      private
         FFieldName: string;
         FFieldType: TWiType;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(FieldName: string; FieldType: TWiType;
            ExtendedAttributeList: TWiExtendedAttributeList);

         property FieldName: string read FFieldName;
         property FieldType: TWiType read FFieldType;
   end;

   TWiExceptionField = class(TWiMember)
      private
         FFieldName: string;
         FFieldType: TWiType;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(FieldName: string; FieldType: TWiType;
            ExtendedAttributeList: TWiExtendedAttributeList);

         property FieldName: string read FFieldName;
         property FieldType: TWiType read FFieldType;
   end;

   TWiDefinition = class(TRefCountedObject)
      private
         FExtendedAttributeList: TWiExtendedAttributeList;
         function GetExtendedAttribute(Index: Integer): TWiExtendedAttribute;
      protected
         function GetAsPascalCode: string; virtual; abstract;
      public
         constructor Create(ExtendedAttributeList: TWiExtendedAttributeList); virtual;
         destructor Destroy; override;

         procedure AddExtendedAttribute(const Value: TWiExtendedAttribute);

         property ExtendedAttribute[Index: Integer]: TWiExtendedAttribute read GetExtendedAttribute;
         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiTypeDef = class(TWiDefinition)
      private
         FTypeName: string;
         FTypeType: TWiType;
    function GetPascalTypeName: string;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(TypeName: string; TypeType: TWiType;
            ExtendedAttributeList: TWiExtendedAttributeList); reintroduce;

         property TypeName: string read FTypeName;
         property TypeType: TWiType read FTypeType;
         property PascalTypeName: string read GetPascalTypeName;
         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiEnumerationItem = class(TRefCountedObject)
      private
         FElementName: string;
      public
         constructor Create(ElementName: string);
         property ElementName: string read FElementName;
   end;

   TWiEnumeration = class(TWiDefinition)
      strict private
         FEnumerationName: string;
         FSymbols: TTightList;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(EnumerationName: string); reintroduce;

         procedure AddItem(Item: TWiEnumerationItem);
   end;

   TWiCallback = class(TWiDefinition)
      private
         FCallbackName: string;
         FArgumentList: TWiArgumentList;
         FReturnType: TWiType;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(CallbackName: string; ReturnType: TWiType;
            ArgumentList: TWiArgumentList;
            ExtendedAttributeList: TWiExtendedAttributeList); reintroduce;

         property CallbackName: string read FCallbackName;
         property ReturnType: TWiType read FReturnType;
   end;

   TWiInheritableDefinition = class(TWiDefinition)
      private
         FAncestor: string;
      public
         property Ancestor: string read FAncestor write FAncestor;
   end;

   TWiConst = class(TWiObject)
      private
         FName: string;
         FType: TWiType;
         FValue: Variant;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(&Type: TWiType; Name: string; Value: Variant);

         property Name: string read FName;
         property &Type: TWiType read FType;
         property Value: Variant read FValue;
         property AsPascalCode: string read GetAsPascalCode;
   end;

   TWiInheritableDefinitionWithConst = class(TWiInheritableDefinition)
      private
         FConsts: TObjectList<TWiConst>;
      public
         constructor Create(ExtendedAttributeList: TWiExtendedAttributeList); override;
         destructor Destroy; override;
   end;

   TWiInterface = class(TWiInheritableDefinitionWithConst)
      private
         FInterfaceName: string;
         FIsCallbackInterface: Boolean;
         FIsPartial: Boolean;
         FAttributes: TObjectList<TWiAttribute>;
         FOperations: TObjectList<TWiOperation>;
         function GetAttributeCount: Integer;
         function GetOperationCount: Integer;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(InterfaceName: string;
            ExtendedAttributeList: TWiExtendedAttributeList;
            IsCallbackInterface: Boolean = False); reintroduce;
         destructor Destroy; override;

         property InterfaceName: string read FInterfaceName;
         property AttributeCount: Integer read GetAttributeCount;
         property OperationCount: Integer read GetOperationCount;
         property IsPartial: Boolean read FIsPartial write FIsPartial;
         property IsCallbackInterface: Boolean read FIsCallbackInterface;
   end;

   TWiDictionary = class(TWiInheritableDefinition)
      private
         FDictionaryName: string;
         FFields: TObjectList<TWiDictionaryField>;
         function GetFieldCount: Integer;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(DictionaryName: string;
            ExtendedAttributeList: TWiExtendedAttributeList); reintroduce;
         destructor Destroy; override;

         property DictionaryName: string read FDictionaryName;
         property FieldCount: Integer read GetFieldCount;
   end;

   TWiException = class(TWiInheritableDefinitionWithConst)
      private
         FExceptionName: string;
         FFields: TObjectList<TWiExceptionField>;
      protected
         function GetAsPascalCode: string; override;
      public
         constructor Create(ExceptionName: string;
            ExtendedAttributeList: TWiExtendedAttributeList); reintroduce;
         destructor Destroy; override;

         property ExceptionName: string read FExceptionName;
   end;

   TdwsWebIDL = class(TRefCountedObject)
      private
         FTypeDefs: TObjectList<TWiTypeDef>;
         FInterfaceList: TObjectList<TWiInterface>;
         FDictionaryList: TObjectList<TWiDictionary>;
         FExceptionList: TObjectList<TWiException>;
         FCallbackList: TObjectList<TWiCallback>;
         FDefinitions: TObjectList<TWiDefinition>;
         function GetAsPascalCode: string;
      public
         constructor Create;
         destructor Destroy; override;

         property AsPascalCode: string read GetAsPascalCode;
   end;

   TdwsWebIDLInterpreter = class
      private
         FRules: TWebIdlTokenizerStateRules;
         FMsgs: TdwsCompileMessageList;
         FTok: TTokenizer;
         FWebIDL: TdwsWebIDL;

         function ReadIdentifier(out Identifier: string; DeleteToken: Boolean = False): Boolean;

         procedure ReadDefinitions;
         procedure ReadDefinition(
            ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadCallbackOrInterface(Token: TToken;
            ExtendedAttributeList: TWiExtendedAttributeList): Boolean;
         procedure ReadCallbackRestOrInterface(
            ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadInterface(
            ExtendedAttributeList: TWiExtendedAttributeList;
            IsCallbackInterface: Boolean = False): TWiInterface;
         procedure ReadPartial(ExtendedAttributeList: TWiExtendedAttributeList);
         procedure ReadPartialDefinition(ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadPartialInterface(ExtendedAttributeList: TWiExtendedAttributeList): TWiInterface;
         procedure ReadInterfaceMembers(WebInterface: TWiInterface);
         procedure ReadInterfaceMember(WebInterface: TWiInterface;
            ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadDictionary(ExtendedAttributeList: TWiExtendedAttributeList): TWiDictionary;
         procedure ReadDictionaryMembers(Dictionary: TWiDictionary);
         procedure ReadDictionaryMember(Dictionary: TWiDictionary;
            ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadPartialDictionary(ExtendedAttributeList: TWiExtendedAttributeList): TWiDictionary;
         function ReadDefault(ValueType: TWiType; out Value: Variant): Boolean;
         function ReadDefaultValue(ValueType: TWiType): Variant;
         function ReadException(ExtendedAttributeList: TWiExtendedAttributeList): TWiException;
         procedure ReadExceptionMembers(WebException: TWiException);
         procedure ReadInheritance(Definition: TWiInheritableDefinition);
         function ReadEnum(ExtendedAttributeList: TWiExtendedAttributeList): TWiEnumeration;
         procedure ReadEnumValueList(Enum: TWiEnumeration);
         procedure ReadEnumValues(Enum: TWiEnumeration);

         function ReadCallbackRest(
            ExtendedAttributeList: TWiExtendedAttributeList): TWiCallback;

         procedure ReadTypeDef(ExtendedAttributeList: TWiExtendedAttributeList);
         procedure ReadImplementsStatement(ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadConst(Definition: TWiInheritableDefinitionWithConst): Boolean;
         function ReadConstValue(ValueType: TWiType): Variant;

         procedure ReadAttributeOrOperation(WebInterface: TWiInterface;
            ExtendedAttributeList: TWiExtendedAttributeList);
         function ReadStringifierAttributeOrOperation(
            ExtendedAttributeList: TWiExtendedAttributeList): Boolean;
         function ReadAttribute(ExtendedAttributeList: TWiExtendedAttributeList)
            : TWiAttribute;

         function ReadOperation(ExtendedAttributeList: TWiExtendedAttributeList): TWiOperation; (* includes Qualifiers with Specials *)
         function ReadOperationRest(ExtendedAttributeList: TWiExtendedAttributeList): TWiOperation;

         function ReadArgumentList: TWiArgumentList; (* includes Arguments and Argument *)

         function ReadOptionalOrRequiredArgument(ExtendedAttributeList: TWiExtendedAttributeList): TWiArgument;
         function ReadArgumentName: string;
         function ReadEllipsis: Boolean;

         procedure ReadExceptionMember(WebException: TWiException; ExtendedAttributeList: TWiExtendedAttributeList);
         procedure ReadExceptionField(WebException: TWiException; ExtendedAttributeList: TWiExtendedAttributeList);

         function ReadExtendedAttributeList: TWiExtendedAttributeList;
         procedure ReadExtendedAttribute(var Text: string);
         procedure ReadExtendedAttributeRest(var Text: string);
         procedure ReadExtendedAttributeInner(var Text: string);

         function ReadOther: string;
         function ReadArgumentNameKeyword: TArgumentNameKeyword;
         procedure ReadOtherOrComma;

         function ReadType: TWiType;
         function ReadSingleType: TWiType;
         function ReadNonAnyType(Token: TToken = nil): TWiType;
         function ReadUnionType: TWiType;
         procedure ReadUnionMemberType(UnionType: TWiUnionType);
         function ReadPrimitiveType(Token: TToken): TWiType;

         procedure ReadTypeSuffix(var WiType: TWiType;
           var IsNullable: Boolean);
         procedure ReadTypeSuffixStartingWithArray(var WiType: TWiType;
           var IsNullable: Boolean);

         function ReadConstType: TWiType;
         function ReadReturnType: TWiType;
      public
         constructor Create;
         destructor Destroy; override;

         function Parse(const aCodeText: String; UnitName: String = ''): TdwsWebIDL;

         property Msgs : TdwsCompileMessageList read FMsgs;
   end;

implementation

uses
   dwsPascalTokenizer;

resourcestring
  RStrSemicolonExpected = ''';'' expected';
  RStrTokenExpected = '''%s'' expected';
  RStrIdentifierExpected = 'Identifier expected';
  RStrAnyTokenExpected = 'Token expected';

function IsKeyword(Value: string): Boolean;
var
  Index: Integer;
begin
   Result := False;
   for Index := 0 to Length(cTokenStrings) - 1 do
      if UnicodeSameText(Value, cTokenStrings[TTokenType(Index)]) then
         Exit(True);
end;

{ TWiType }

function TWiType.GetPascalTypeName: string;
begin
   Result := TypeName;
end;


{ TWiBaseFloatType }

function TWiBaseFloatType.GetPascalTypeName: string;
begin
   Result := 'Float';
end;

function TWiBaseFloatType.GetTypeName: string;
begin
   Result := '';
   if Unrestricted then
      Result := 'unregistered ';
end;

{ TWiFloatType }

function TWiFloatType.GetTypeName: string;
begin
   Result := inherited GetTypeName + 'float';
end;


{ TWiDoubleType }

function TWiDoubleType.GetTypeName: string;
begin
   Result := inherited GetTypeName + 'float';
end;


{ TWiBaseIntegerType }

function TWiBaseIntegerType.GetPascalTypeName: string;
begin
   Result := 'Integer';
end;

function TWiBaseIntegerType.GetTypeName: string;
begin
   Result := '';
   if Unsigned then
      Result := 'unsigned ';
end;


{ TWiShortType }

function TWiShortType.GetTypeName: string;
begin
   Result := inherited GetTypeName + 'short';
end;


{ TWiLongType }

function TWiLongType.GetTypeName: string;
begin
   Result := inherited GetTypeName + 'long';
end;


{ TWiLongLongType }

function TWiLongLongType.GetTypeName: string;
begin
   Result := inherited GetTypeName + 'long long';
end;


{ TWiBooleanType }

function TWiBooleanType.GetPascalTypeName: string;
begin
   Result := 'Boolean';
end;

function TWiBooleanType.GetTypeName: string;
begin
   Result := 'boolean';
end;


{ TWiByteType }

function TWiByteType.GetPascalTypeName: string;
begin
   Result := 'Integer';
end;

function TWiByteType.GetTypeName: string;
begin
   Result := 'byte';
end;


{ TWiOctetType }

function TWiOctetType.GetPascalTypeName: string;
begin
   Result := 'Integer';
end;

function TWiOctetType.GetTypeName: string;
begin
   Result := 'octet';
end;


{ TWiDOMStringType }

function TWiDOMStringType.GetPascalTypeName: string;
begin
   Result := 'String';
end;

function TWiDOMStringType.GetTypeName: string;
begin
   Result := 'DOMString';
end;


{ TWiUnionType }

function TWiUnionType.GetPascalTypeName: string;
begin
   Result := 'Union';
end;

function TWiUnionType.GetTypeName: string;
begin
   Result := 'union';
end;


{ TWiAnyType }

function TWiAnyType.GetPascalTypeName: string;
begin
   Result := 'Variant';
end;

function TWiAnyType.GetTypeName: string;
begin
   Result := 'any';
end;


{ TWiSequence }

constructor TWiSequence.Create(ElementType: TWiType);
begin
   FElementType := ElementType;
end;

function TWiSequence.GetPascalTypeName: string;
begin
   Result := 'array of ' + FElementType.PascalTypeName;
end;

function TWiSequence.GetTypeName: string;
begin
   Result := 'array of ' + FElementType.TypeName;
end;


{ TWiArray }

function TWiArray.GetPascalTypeName: string;
begin
   Result := 'array of ' + FElementType.PascalTypeName;
end;

function TWiArray.GetTypeName: string;
begin
   Result := 'array of ' + FElementType.TypeName;
end;


{ TWiPromise }

constructor TWiPromise.Create(TypeName: string; ElementType: TWiType);
begin
   FTypeName := TypeName;
   FElementType := ElementType;
end;

function TWiPromise.GetTypeName: string;
begin
   Result := FTypeName;
end;


{ TWiObjectType }

function TWiObjectType.GetPascalTypeName: string;
begin
  Result := 'Variant';
end;

function TWiObjectType.GetTypeName: string;
begin
  Result := 'object';
end;


{ TWiIdentifierType }

constructor TWiIdentifierType.Create(TypeName: string; WebIDL: TdwsWebIDL);
begin
   FWebIDL := WebIDL;
   FTypeName := TypeName;
end;

function TWiIdentifierType.GetPascalTypeName: string;
var
  Index: Integer;
begin
//   if True then
   for Index := 0 to FWebIDL.FTypeDefs.Count - 1 do
   begin
      if FWebIDL.FTypeDefs[Index].TypeName = FTypeName then
        Exit(FWebIDL.FTypeDefs[Index].PascalTypeName);
   end;

   Result := 'J' + FTypeName;
end;

function TWiIdentifierType.GetTypeName: string;
begin
   Result := FTypeName;
end;


{ TWiEnumerationItem }

constructor TWiEnumerationItem.Create(ElementName: string);
begin
   FElementName := ElementName;
end;


{ TWiExtendedAttribute }

constructor TWiExtendedAttribute.Create(Text: string);
begin
   FText := Text;
end;


{ TWiWithExtendedAttribute }

constructor TWiWithExtendedAttribute.Create(ExtendedAttributeList: TWiExtendedAttributeList);
begin
   FExtendedAttributeList := ExtendedAttributeList;
end;

procedure TWiWithExtendedAttribute.AddExtendedAttribute(
  const Value: TWiExtendedAttribute);
begin
   FExtendedAttributeList.Add(Value);
end;

function TWiWithExtendedAttribute.GetExtendedAttribute(
  Index: Integer): TWiExtendedAttribute;
begin
   Result := FExtendedAttributeList[Index];
end;

function TWiWithExtendedAttribute.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := '';

   if FExtendedAttributeList.Count > 0 then
   begin
      Result := Result + ' { ' + FExtendedAttributeList.Items[0].Text;
      for Index := 1 to FExtendedAttributeList.Count - 1 do
         Result := Result + ', ' + FExtendedAttributeList.Items[Index].Text;
      Result := Result + ' } '
   end;
end;

function TWiWithExtendedAttribute.GetExtendedAttributeCount: Integer;
begin
   Result := FExtendedAttributeList.Count;
end;


{ TWiTypeDef }

constructor TWiTypeDef.Create(TypeName: string; TypeType: TWiType;
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   FTypeName := TypeName;
   FTypeType := TypeType;
   inherited Create (ExtendedAttributeList);
end;

function TWiTypeDef.GetAsPascalCode: string;
begin
   Result := #9 + PascalTypeName + ' = ' + TypeType.PascalTypeName + ';';
end;

function TWiTypeDef.GetPascalTypeName: string;
begin
   Result := 'T' + TypeName;
   if IsKeyword(Result) then
      Result := '&' + Result;
end;


{ TWiArgument }

constructor TWiArgument.Create(ArgumentName: string; ArgumentType: TWiType;
   ExtendedAttributeList: TWiExtendedAttributeList; IsOptional: Boolean = False);
begin
   FArgumentName := ArgumentName;
   FArgumentType := ArgumentType;
   FIsOptional := IsOptional;
   FDefaultValue := Unassigned;
   inherited Create (ExtendedAttributeList);
end;

function TWiArgument.GetAsPascalCode: string;
var
   ArgName: string;
begin
   ArgName := ArgumentName;
   if IsKeyword(ArgName) then
      ArgName := '&' + ArgName;
   Result := ArgName + ': ' + ArgumentType.PascalTypeName;

   if not VarIsEmpty(DefaultValue) then
      Result := Result + ' = ' + VarToStr(DefaultValue);

   Result := Result + inherited GetAsPascalCode;
end;


{ TWiDictionaryField }

constructor TWiDictionaryField.Create(FieldName: string; FieldType: TWiType;
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);
   FFieldName := FieldName;
   FFieldType := FieldType;
end;

function TWiDictionaryField.GetAsPascalCode: string;
var
  FieldName: string;
begin
   // unescape field name
   FieldName := FFieldName;
   if IsKeyword(FieldName) then
      FieldName := '&' + FieldName;

   Result := #9#9 + FieldName + ': ' +
      FFieldType.PascalTypeName + ';' + inherited GetAsPascalCode + #13#10;
end;


{ TWiExceptionField }

constructor TWiExceptionField.Create(FieldName: string; FieldType: TWiType;
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);
   FFieldName := FieldName;
   FFieldType := FieldType;
end;

function TWiExceptionField.GetAsPascalCode: string;
var
  FieldName: string;
begin
   // unescape field name
   FieldName := FFieldName;
   if IsKeyword(FieldName) then
      FieldName := '&' + FieldName;

   Result := #9#9 + FieldName + ': ' +
      FFieldType.PascalTypeName + ';' + inherited GetAsPascalCode + #13#10;
end;


{ TWiAttribute }

constructor TWiAttribute.Create(AttributeName: string; AttributeType: TWiType;
   ExtendedAttributeList: TWiExtendedAttributeList; Inherit: Boolean = False;
   ReadOnly: Boolean = False);
begin
   inherited Create(ExtendedAttributeList);
   FAttributeName := AttributeName;
   FAttributeType := AttributeType;
   FReadOnly := ReadOnly;
   FInherit := Inherit;
end;

function TWiAttribute.GetAsPascalCode: string;
begin
   Result := #9#9 + FAttributeName + ': ' +
      FAttributeType.PascalTypeName + ';' + inherited GetAsPascalCode + #13#10;
end;


{ TWiOperation }

constructor TWiOperation.Create(ReturnType: TWiType;
   Arguments: TWiArgumentList; ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);

   FOperationName := '';
   FArguments := Arguments;
   FReturnType := ReturnType;
end;

constructor TWiOperation.Create(OperationName: string; ReturnType: TWiType;
  Arguments: TWiArgumentList; ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);

   FOperationName := OperationName;
   FArguments := Arguments;
   FReturnType := ReturnType;
end;

function TWiOperation.GetAsPascalCode: string;

var
   Index: Integer;
   OverloadIndex: Integer;
   TotalOverloadCount: Integer;
   OverloadCount: Integer;
   IsOverload: Boolean;
   HasDefaultValue: Boolean;
begin
   TotalOverloadCount := 1;
   HasDefaultValue := True;
   if Assigned(FArguments) and (FArguments.Count > 0) then
      for Index := FArguments.Count - 1 downto 0 do
      begin
         if VarIsEmpty(FArguments.Items[Index].FDefaultValue) then
            HasDefaultValue := False;

         if FArguments.Items[Index].IsOptional and not HasDefaultValue then
            Inc(TotalOverloadCount);
      end;
   IsOverload := TotalOverloadCount > 1;
   OverloadCount := TotalOverloadCount;

   Result := '';
   repeat
      OverloadIndex := 1;
      if Assigned(ReturnType) then
         Result := Result + #9#9 + 'function '
      else
         Result := Result + #9#9 + 'procedure ';

      Result := Result + FOperationName;

      if Assigned(FArguments) and (FArguments.Count > 0) then
      begin
         Result := Result + '(';

         if (not FArguments.Items[0].IsOptional) or
            (TotalOverloadCount - OverloadIndex >= OverloadCount) or
            (not VarIsEmpty(FArguments.Items[0].DefaultValue)) then
         begin
            Result := Result + FArguments.Items[0].AsPascalCode;

            if FArguments.Items[0].IsOptional then
               Inc(OverloadIndex);
         end;

         for Index := 1 to FArguments.Count - 1 do
         begin
            if (not FArguments.Items[Index].IsOptional) or
               (TotalOverloadCount - OverloadIndex >= OverloadCount) or
               (not VarIsEmpty(FArguments.Items[Index].DefaultValue)) then
            begin
               Result := Result + '; ' + FArguments.Items[Index].AsPascalCode;

               if FArguments.Items[Index].IsOptional then
                  Inc(OverloadIndex);
            end;
         end;

         Result := Result + ')';
      end;

      if Assigned(ReturnType) then
         Result := Result + ': ' + ReturnType.GetPascalTypeName;

      Result := Result + ';';

      if IsOverload then
         Result := Result + ' overload;';

      Dec(OverloadCount);

      Result := Result + inherited GetAsPascalCode + #13#10;
   until OverloadCount = 0;

   // eventually replace empty parens
   Result := StringReplace(Result, '()', '', [rfReplaceAll]);
end;


{ TWiDefinition }

constructor TWiDefinition.Create(ExtendedAttributeList: TWiExtendedAttributeList);
begin
   FExtendedAttributeList := ExtendedAttributeList;
end;

destructor TWiDefinition.Destroy;
begin
   FExtendedAttributeList.Free;
   inherited;
end;

procedure TWiDefinition.AddExtendedAttribute(const Value: TWiExtendedAttribute);
begin
   FExtendedAttributeList.Add(Value);
end;

function TWiDefinition.GetExtendedAttribute(
  Index: Integer): TWiExtendedAttribute;
begin
   Result := FExtendedAttributeList[Index];
end;


{ TWiEnumeration }

constructor TWiEnumeration.Create(EnumerationName: string);
begin
   FEnumerationName := EnumerationName;
end;

function TWiEnumeration.GetAsPascalCode: string;
var
   Index: Integer;
   Prefix: string;
   ElementName: string;
begin
   Result := #9 + 'J' + FEnumerationName;

   Prefix := '';
   for Index := 1 to Length(FEnumerationName) do
   begin
      if CharInSet(FEnumerationName[Index], ['A'..'Z']) then
         Prefix := Prefix + FEnumerationName[Index];
   end;

   if Length(Prefix) > Length(FEnumerationName) shr 2 then
      Prefix := ''
   else
      Prefix := LowerCase(Prefix);

   if FSymbols.Count > 0 then
   begin
      ElementName := TWiEnumerationItem(FSymbols.List^[0]).ElementName;
      if (ElementName <> '') and (Prefix <> '') then
      begin
         ElementName[1] := Upcase(ElementName[1]);
         ElementName := Prefix + ElementName;
      end;
      Result := Result + ' = (' + ElementName;

      for Index := 1 to FSymbols.Count - 1 do
      begin
         ElementName := TWiEnumerationItem(FSymbols.List^[Index]).ElementName;
         if ElementName = '' then
           Continue;

         if Prefix <> '' then
         begin
            ElementName[1] := Upcase(ElementName[1]);
            ElementName := Prefix + ElementName;
         end;

         Result := Result + ', ' + ElementName;
      end;

      Result := Result + ')';
   end;

   Result := Result + ';' + #13#10;
end;

procedure TWiEnumeration.AddItem(Item: TWiEnumerationItem);
begin
   FSymbols.Add(Item);
end;


{ TWiCallback }

constructor TWiCallback.Create(CallbackName: string; ReturnType: TWiType;
   ArgumentList: TWiArgumentList; ExtendedAttributeList: TWiExtendedAttributeList);
begin
   FCallbackName := CallbackName;
   FArgumentList := ArgumentList;
   FReturnType := ReturnType;
   inherited Create(ExtendedAttributeList);
end;

function TWiCallback.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := #9 + 'J' + FCallbackName;
   if Assigned(FReturnType) then
      Result := Result + ' = function'
   else
      Result := Result + ' = procedure';

   if Assigned(FArgumentList) and (FArgumentList.Count > 0) then
   begin
      Result := Result + '(';
      Result := Result + FArgumentList.Items[0].ArgumentName + ': ' +
         FArgumentList.Items[0].ArgumentType.PascalTypeName;

      for Index := 1 to FArgumentList.Count - 1 do
      begin
         Result := Result + '; ' + FArgumentList.Items[Index].ArgumentName +
            ': ' + FArgumentList.Items[Index].ArgumentType.PascalTypeName;
      end;

      Result := Result + ')';
   end;

   if Assigned(FReturnType) then
      Result := Result + ': ' + FReturnType.PascalTypeName;
   Result := Result + ';' + #13#10;
end;


{ TWiDictionary }

constructor TWiDictionary.Create(DictionaryName: string;
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);

   FDictionaryName := DictionaryName;
   FFields := TObjectList<TWiDictionaryField>.Create;
end;

destructor TWiDictionary.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TWiDictionary.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := '';
   for Index := 0 to FExtendedAttributeList.Count - 1 do
      Result := Result + #9 + '// ' + FExtendedAttributeList.Items[Index].Text + #13#10;

   Result := Result + #9 + 'J' + DictionaryName + ' = class external ''' +
     DictionaryName + '''';

   if Ancestor <> ''  then
      Result := Result + ' (' + 'J' + Ancestor + ')';
   Result := Result + #13#10;

   Result := Result + #9 + 'public' + #13#10;

   for Index := 0 to FFields.Count - 1 do
      Result := Result + FFields[Index].GetAsPascalCode;

   Result := Result + #9 + 'end;' + #13#10;
end;

function TWiDictionary.GetFieldCount: Integer;
begin
   Result := FFields.Count;
end;


{ TWiException }

constructor TWiException.Create(ExceptionName: string;
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);

   FExceptionName := ExceptionName;
   FFields := TObjectList<TWiExceptionField>.Create;
end;

destructor TWiException.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TWiException.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := '';
   for Index := 0 to FExtendedAttributeList.Count - 1 do
      Result := Result + #9 + '// ' + FExtendedAttributeList.Items[Index].Text + #13#10;

   Result := Result + #9 + 'J' + ExceptionName + ' = class external ''' +
     ExceptionName + '''';

   if Ancestor <> ''  then
      Result := Result + ' (' + 'J' + Ancestor + ')';
   Result := Result + #13#10;

   // write const section
   if FConsts.Count > 0 then
      Result := Result + #9 + 'const ' + #13#10;
   for Index := 0 to FConsts.Count - 1 do
      Result := Result + FConsts[Index].GetAsPascalCode;

   // write public section
   Result := Result + #9 + 'public ' + #13#10;
   for Index := 0 to FFields.Count - 1 do
      Result := Result + FFields[Index].GetAsPascalCode;

   Result := Result + #9 + 'end;' + #13#10;
end;


{ TWiConst }

constructor TWiConst.Create(&Type: TWiType; Name: string; Value: Variant);
begin
   inherited Create;

   FType := &Type;
   FName := Name;
   FValue := Value;
end;

function TWiConst.GetAsPascalCode: string;
var
  EscapedName: string;
begin
   EscapedName := FName;
   if IsKeyword(EscapedName) then
      EscapedName := '&' + EscapedName;

  Result := #9#9 + EscapedName + ': ' + FType.GetPascalTypeName +
     ' = ' + VarToStr(FValue) + ';';

  Result := Result + #13#10;;
end;


{ TWiInheritableDefinitionWithConst }

constructor TWiInheritableDefinitionWithConst.Create(
  ExtendedAttributeList: TWiExtendedAttributeList);
begin
   inherited Create(ExtendedAttributeList);
   FConsts := TObjectList<TWiConst>.Create;
end;

destructor TWiInheritableDefinitionWithConst.Destroy;
begin
  FConsts.Free;
  inherited;
end;


{ TWiInterface }

constructor TWiInterface.Create(InterfaceName: string;
   ExtendedAttributeList: TWiExtendedAttributeList;
   IsCallbackInterface: Boolean = False);
begin
   inherited Create(ExtendedAttributeList);

   FInterfaceName := InterfaceName;
   FIsCallbackInterface := IsCallbackInterface;
   FIsPartial := False;

   FAttributes := TObjectList<TWiAttribute>.Create;
   FOperations := TObjectList<TWiOperation>.Create;
end;

destructor TWiInterface.Destroy;
begin
   FreeAndNil(FAttributes);
   FreeAndNil(FOperations);
end;

function TWiInterface.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := '';
   for Index := 0 to FExtendedAttributeList.Count - 1 do
      Result := Result + #9 + '// ' + FExtendedAttributeList.Items[Index].Text + #13#10;

   Result := Result + #9 + 'J' + InterfaceName + ' = ';

   if FIsPartial then
     Result := Result + 'partial ';

   Result := Result + 'class external ''' +
     InterfaceName + '''';

   if Ancestor <> ''  then
      Result := Result + ' (' + 'J' + Ancestor + ')';
   Result := Result + #13#10;

   // write const section
   if FConsts.Count > 0 then
      Result := Result + #9 + 'const ' + #13#10;
   for Index := 0 to FConsts.Count - 1 do
      Result := Result + FConsts[Index].GetAsPascalCode;

   // write public section
   if FAttributes.Count + FOperations.Count > 0 then
      Result := Result + #9 + 'public' + #13#10;
   for Index := 0 to FAttributes.Count - 1 do
      Result := Result + FAttributes[Index].GetAsPascalCode;

   for Index := 0 to FOperations.Count - 1 do
      Result := Result + FOperations[Index].GetAsPascalCode;

   Result := Result + #9 + 'end;' + #13#10;
end;

function TWiInterface.GetAttributeCount: Integer;
begin
   Result := FAttributes.Count;
end;

function TWiInterface.GetOperationCount: Integer;
begin
   Result := FOperations.Count;
end;


{ TdwsWebIDL }

constructor TdwsWebIDL.Create;
begin
   FTypeDefs := TObjectList<TWiTypeDef>.Create;
   FInterfaceList := TObjectList<TWiInterface>.Create;
   FDictionaryList := TObjectList<TWiDictionary>.Create;
   FExceptionList := TObjectList<TWiException>.Create;
   FCallbackList := TObjectList<TWiCallback>.Create;
   FDefinitions := TObjectList<TWiDefinition>.Create;
   inherited;
end;

destructor TdwsWebIDL.Destroy;
begin
   FDefinitions.Free;
   FCallbackList.Free;
   FDictionaryList.Free;
   FExceptionList.Free;
   FInterfaceList.Free;
   FTypeDefs.Free;
   inherited;
end;

function TdwsWebIDL.GetAsPascalCode: string;
var
   Index: Integer;
begin
   Result := 'unit ' + #13#10 + #13#10;
   Result := 'interface' + #13#10 + #13#10;
   Result := 'type' + #13#10;

   for Index := 0 to FDefinitions.Count - 1 do
      Result := Result + FDefinitions[Index].AsPascalCode + #13#10;
end;


{ TdwsWebIDLInterpreter }

constructor TdwsWebIDLInterpreter.Create;
begin
   FRules := TWebIdlTokenizerStateRules.Create;
   FMsgs := TdwsCompileMessageList.Create;
   {$IFDEF RELEASE}
   FMsgs.HintsLevel := hlDisabled;
   {$ENDIF}
   FTok := TTokenizer.Create(FRules, FMsgs);
end;

destructor TdwsWebIDLInterpreter.Destroy;
begin
   FRules.Free;
   FMsgs.Free;
   FTok.Free;
end;

function TdwsWebIDLInterpreter.Parse(const aCodeText: String; UnitName: String = ''): TdwsWebIDL;
var
   SourceFile: TSourceFile;
begin
   FWebIDL := TdwsWebIDL.Create;

   SourceFile := TSourceFile.Create;
   SourceFile.Code := aCodeText;
   SourceFile.Name := UnitName;
   FTok.BeginSourceFile(SourceFile);
   try
      ReadDefinitions;
   finally
      FTok.EndSourceFile;
   end;

   Result := FWebIDL;
   FWebIDL := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TdwsWebIDLInterpreter.ReadDefinitions;
var
   ExtendedAttributeList: TWiExtendedAttributeList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Definitions

   while FTok.HasTokens do
   begin
      ExtendedAttributeList := ReadExtendedAttributeList;
      ReadDefinition(ExtendedAttributeList);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadDefinition(ExtendedAttributeList: TWiExtendedAttributeList);
var
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Definition
   if not FTok.HasTokens then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);

   Token := FTok.GetToken;
   if not ReadCallbackOrInterface(Token, ExtendedAttributeList) then
   if Token.AsString = 'partial' then
      ReadPartial(ExtendedAttributeList)
   else
   if Token.AsString = 'dictionary' then
      ReadDictionary(ExtendedAttributeList)
   else
   if Token.AsString = 'exception' then
      ReadException(ExtendedAttributeList)
   else
   if Token.AsString = 'enum' then
      ReadEnum(ExtendedAttributeList)
   else
   if Token.AsString = 'typedef' then
      ReadTypeDef(ExtendedAttributeList)
   else
      ReadImplementsStatement(ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadCallbackOrInterface(Token: TToken;
  ExtendedAttributeList: TWiExtendedAttributeList): Boolean;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-CallbackRestOrInterface

   Result := False;
   if (Token.AsString = 'callback') then
   begin
      FTok.KillToken;
      ReadCallbackRestOrInterface(ExtendedAttributeList);
      Result := True;
   end
   else
   if (Token.AsString = 'interface') then
   begin
      ReadInterface(ExtendedAttributeList);
      Result := True;
   end;
end;

procedure TdwsWebIDLInterpreter.ReadCallbackRestOrInterface(
   ExtendedAttributeList: TWiExtendedAttributeList);
var
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-CallbackRestOrInterface

   if not FTok.HasTokens then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);

   Token := FTok.GetToken;
   if (Token.AsString = 'interface') then
      ReadInterface(ExtendedAttributeList, True)
   else
     ReadCallbackRest(ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadInterface(
   ExtendedAttributeList: TWiExtendedAttributeList;
   IsCallbackInterface: Boolean = False): TWiInterface;
var
  InterfaceName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Interface

   // kill "interface"
   FTok.KillToken;

   // read interface name
   if not ReadIdentifier(InterfaceName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   Result := TWiInterface.Create(InterfaceName, ExtendedAttributeList,
      IsCallbackInterface);

   // read inheritance
   ReadInheritance(Result);

   // check for forward declaration
   if FTok.TestDelete(ttSEMI) then
     Exit;

   if not FTok.TestDelete(ttCLEFT) then
   begin
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, RStrTokenExpected, ['{']);

      repeat
        FTok.KillToken;

        // check if tokens are available at all
        if not FTok.HasTokens then
          Exit;

      until FTok.TestDelete(ttCLEFT);
   end;

   ReadInterfaceMembers(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   // add interface to interface and definition list
   FWebIDL.FInterfaceList.Add(Result);
   FWebIDL.FDefinitions.Add(Result);
end;

procedure TdwsWebIDLInterpreter.ReadPartial(ExtendedAttributeList: TWiExtendedAttributeList);
var
  TokenName: string;
  ScriptPos: TScriptPos;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Partial

   // read/delete token name
   if not FTok.TestDeleteNamePos(TokenName, ScriptPos) then
      FMsgs.AddCompilerStop(ScriptPos, 'Name token expected');

   if TokenName <> 'partial' then
      FMsgs.AddCompilerStopFmt(ScriptPos, RStrTokenExpected, ['partial']);

   ReadPartialDefinition(ExtendedAttributeList);
end;

procedure TdwsWebIDLInterpreter.ReadPartialDefinition(ExtendedAttributeList: TWiExtendedAttributeList);
var
  WebInterface: TWiInterface;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-PartialDefinition

   WebInterface := ReadPartialInterface(ExtendedAttributeList);
   if not Assigned(WebInterface) then
      ReadPartialDictionary(ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadPartialInterface(ExtendedAttributeList: TWiExtendedAttributeList): TWiInterface;
var
  InterfaceName: string;
  ItemIndex: Integer;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-PartialInterface

   Result := nil;
   ReadIdentifier(InterfaceName, False);
   if not (InterfaceName = 'interface') then
      Exit;

   // kill 'interface' token
   FTok.KillToken;

   // read interface name
   if not ReadIdentifier(InterfaceName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   // locate interface in interface list
   Result := nil;
   for ItemIndex := 0 to FWebIDL.FInterfaceList.Count - 1 do
      if FWebIDL.FInterfaceList.Items[ItemIndex].InterfaceName = InterfaceName then
      begin
         Result := FWebIDL.FInterfaceList.Items[ItemIndex];
         Break;
      end;

   if not Assigned(Result) then
   begin
      Result := TWiInterface.Create(InterfaceName, ExtendedAttributeList);
      Result.IsPartial := True;

      // add interface to interface and definition list
      FWebIDL.FInterfaceList.Add(Result);
      FWebIDL.FDefinitions.Add(Result);
   end;

   // check if interface could be found
   if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Interface could not be located');

   if not FTok.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['{']);

   ReadInterfaceMembers(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);
end;

procedure TdwsWebIDLInterpreter.ReadInterfaceMembers(WebInterface: TWiInterface);
var
   ExtendedAttributeList: TWiExtendedAttributeList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-InterfaceMembers

   while not FTok.Test(ttCRIGHT) and FTok.HasTokens do
   begin
      ExtendedAttributeList := ReadExtendedAttributeList;
      ReadInterfaceMember(WebInterface, ExtendedAttributeList);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadInterfaceMember(WebInterface: TWiInterface;
   ExtendedAttributeList: TWiExtendedAttributeList);
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-InterfaceMember
   if not ReadConst(WebInterface) then
      ReadAttributeOrOperation(WebInterface, ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadDictionary(ExtendedAttributeList: TWiExtendedAttributeList): TWiDictionary;
var
   DictionaryName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Dictionary

   // kill "dictionary"
   FTok.KillToken;

   // read dictionary name
   if not ReadIdentifier(DictionaryName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   Result := TWiDictionary.Create(DictionaryName, ExtendedAttributeList);

   // read inheritance
   ReadInheritance(Result);

   if not FTok.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['{']);

   ReadDictionaryMembers(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   // add dictionary to dictionary and definition list
   FWebIDL.FDictionaryList.Add(Result);
   FWebIDL.FDefinitions.Add(Result);
end;

procedure TdwsWebIDLInterpreter.ReadDictionaryMembers(Dictionary: TWiDictionary);
var
   ExtendedAttributeList: TWiExtendedAttributeList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-DictionaryMembers

   while not FTok.Test(ttCRIGHT) do
   begin
      ExtendedAttributeList := ReadExtendedAttributeList;
      ReadDictionaryMember(Dictionary, ExtendedAttributeList);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadDictionaryMember(Dictionary: TWiDictionary;
   ExtendedAttributeList: TWiExtendedAttributeList);
var
   MemberType: TWiType;
   MemberName: string;
   DefaultValue: Variant;
   Field: TWiDictionaryField;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-DictionaryMember

   // read type symbol
   MemberType := ReadType;

   // read dictionary member name
   if not ReadIdentifier(MemberName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   // read default
   if not ReadDefault(MemberType, DefaultValue) then
      DefaultValue := Unassigned;

   // read and check if semicolon is present
   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   Field := TWiDictionaryField.Create(MemberName, MemberType, ExtendedAttributeList);

   Assert(Assigned(Dictionary));
   Dictionary.FFields.Add(Field);
end;

function TdwsWebIDLInterpreter.ReadPartialDictionary(ExtendedAttributeList: TWiExtendedAttributeList): TWiDictionary;
var
   DictionaryName: string;
   ItemIndex: Integer;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-PartialDictionary

   Result := nil;
   ReadIdentifier(DictionaryName, False);
   if not (DictionaryName = 'dictionary') then
      Exit;

   // kill "dictionary"
   FTok.KillToken;

   // read dictionary name
   if not ReadIdentifier(DictionaryName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   // locate dictionary in dictionary list
   Result := nil;
   for ItemIndex := 0 to FWebIDL.FDictionaryList.Count - 1 do
      if FWebIDL.FDictionaryList.Items[ItemIndex].DictionaryName = DictionaryName then
      begin
         Result := FWebIDL.FDictionaryList.Items[ItemIndex];
         Break;
      end;

   if not Assigned(Result) then
   begin
      Result := TWiDictionary.Create(DictionaryName, ExtendedAttributeList);

      // add dictionary to interface and definition list
      FWebIDL.FDictionaryList.Add(Result);
      FWebIDL.FDefinitions.Add(Result);
   end;

   // check if dictionary could be found
   if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Interface could not be located');

   if not FTok.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['{']);

   ReadDictionaryMembers(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);
end;

function TdwsWebIDLInterpreter.ReadDefault(ValueType: TWiType; out Value: Variant): Boolean;
begin
  // see http://www.w3.org/TR/WebIDL/#proddef-Default

  Result := FTok.TestDelete(ttEQ);
  if Result then
     Value := ReadDefaultValue(ValueType);
end;

function TdwsWebIDLInterpreter.ReadDefaultValue(ValueType: TWiType): Variant;
begin
  // see http://www.w3.org/TR/WebIDL/#proddef-DefaultValue

  if FTok.Test(ttStrVal) then
  begin
     Result := FTok.GetToken.AsString;
     FTok.KillToken;
  end
  else if FTok.TestDelete(ttALEFT) then
  begin
     if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, [']']);

     Result := Null;
  end
  else
     Result := ReadConstValue(ValueType);
end;

function TdwsWebIDLInterpreter.ReadException(ExtendedAttributeList: TWiExtendedAttributeList): TWiException;
var
  ExceptionName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Exception

   // kill "exception"
   FTok.KillToken;

   // read exception name
   if not ReadIdentifier(ExceptionName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   Result := TWiException.Create(ExceptionName, ExtendedAttributeList);

   // read inheritance
   ReadInheritance(Result);

   if not FTok.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['{']);

   ReadExceptionMembers(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   // add exception to exception and definition list
   FWebIDL.FExceptionList.Add(Result);
   FWebIDL.FDefinitions.Add(Result);
end;

procedure TdwsWebIDLInterpreter.ReadExceptionMembers(WebException: TWiException);
var
   ExtendedAttributeList: TWiExtendedAttributeList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExceptionMembers

   while not FTok.Test(ttCRIGHT) do
   begin
      ExtendedAttributeList := ReadExtendedAttributeList;
      ReadExceptionMember(WebException, ExtendedAttributeList);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadInheritance(Definition: TWiInheritableDefinition);
var
  AncestorName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Inheritance

   if FTok.TestDelete(ttCOLON) then
   begin
      // read ancestor name
      if not ReadIdentifier(AncestorName, True) then
         FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

      Definition.Ancestor := AncestorName;

      // TODO: need to ensure whether ancestor (interface) has and identical
      // IsCallbackInterface setting

      FMsgs.AddCompilerHint(FTok.HotPos, 'Inheritance not yet supported');
   end;
end;

function TdwsWebIDLInterpreter.ReadEnum(ExtendedAttributeList: TWiExtendedAttributeList): TWiEnumeration;
var
   TokenName: string;
   ScriptPos: TScriptPos;
   EnumName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Enum

   // read/delete token name
   if not ReadIdentifier(TokenName, True) then
      FMsgs.AddCompilerStop(ScriptPos, 'Name token expected');

   if TokenName <> 'enum' then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['enum']);

   // read enum name
   if not ReadIdentifier(EnumName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   if not FTok.TestDelete(ttCLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['{']);

   Result := TWiEnumeration.Create(EnumName);
   ReadEnumValueList(Result);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['}']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   // add interface to definition list
   FWebIDL.FDefinitions.Add(Result);
end;

procedure TdwsWebIDLInterpreter.ReadEnumValueList(Enum: TWiEnumeration);
var
   Token: TToken;
   Element: TWiEnumerationItem;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-EnumValueList

   if not FTok.Test(ttStrVal) then
      FMsgs.AddCompilerStop(FTok.HotPos, 'string expected');

   Token := FTok.GetToken;
   Element := TWiEnumerationItem.Create(Token.AsString);
   Enum.AddItem(Element);
   FTok.KillToken;

   ReadEnumValues(Enum);
end;

procedure TdwsWebIDLInterpreter.ReadEnumValues(Enum: TWiEnumeration);
var
   Token: TToken;
   Element: TWiEnumerationItem;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-EnumValues

   while FTok.TestDelete(ttCOMMA) do
   begin
      if not FTok.Test(ttStrVal) then
         FMsgs.AddCompilerStop(FTok.HotPos, 'string expected');

      Token := FTok.GetToken;
      Element := TWiEnumerationItem.Create(Token.AsString);
      Enum.AddItem(Element);
      FTok.KillToken;
   end;
end;

function TdwsWebIDLInterpreter.ReadCallbackRest(
   ExtendedAttributeList: TWiExtendedAttributeList): TWiCallback;
var
   Token: TToken;
   CallbackName: string;
   ReturnType: TWiType;
   ArgumentList: TWiArgumentList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-CallbackRest

   // read callback name and kill token
   Token := FTok.GetToken;
   CallbackName := Token.AsString;
   FTok.KillToken;

   if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['=']);

   // read return type
   ReturnType := ReadReturnType;

   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['(']);

   // read argument list
   ArgumentList := nil;
   if not FTok.Test(ttBRIGHT) then
      ArgumentList := ReadArgumentList;

   if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, [')']);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   Result := TWiCallback.Create(CallbackName, ReturnType, ArgumentList,
      ExtendedAttributeList);

   // add callback to callback & definition list
   FWebIDL.FCallbackList.Add(Result);
   FWebIDL.FDefinitions.Add(Result);
end;

procedure TdwsWebIDLInterpreter.ReadTypedef(ExtendedAttributeList: TWiExtendedAttributeList);
var
   TokenName: string;
   ScriptPos: TScriptPos;
   TypeType: TWiType;
   TypeName: string;
   TypeDef: TWiTypeDef;
   InnerExtendedAttributeList: TWiExtendedAttributeList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Typedef

   // read/delete token name
   if not FTok.TestDeleteNamePos(TokenName, ScriptPos) then
      FMsgs.AddCompilerStop(ScriptPos, 'Name token expected');

   if TokenName <> 'typedef' then
      FMsgs.AddCompilerStop(ScriptPos, '''typedef'' expected');

   InnerExtendedAttributeList := ReadExtendedAttributeList;

   TypeType := ReadType;

   if not FTok.TestDeleteNamePos(TypeName, ScriptPos) then
      FMsgs.AddCompilerStop(ScriptPos, 'Name token expected');

   TypeDef := TWiTypeDef.Create(TypeName, TypeType, InnerExtendedAttributeList);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   FWebIDL.FTypeDefs.Add(TypeDef);
   FWebIDL.FDefinitions.Add(TypeDef);
end;

function TdwsWebIDLInterpreter.ReadIdentifier(out Identifier: string;
  DeleteToken: Boolean = False): Boolean;
var
   Token: TToken;
begin
   Result := FTok.TestName;
   if Result then
   begin
      Token := FTok.GetToken;
      Identifier := Token.AsString;
      if DeleteToken then
         FTok.KillToken;
   end
   else Identifier := '';
end;

procedure TdwsWebIDLInterpreter.ReadImplementsStatement(ExtendedAttributeList: TWiExtendedAttributeList);
var
   ClassName, InterfaceName: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-CallbackOrInterface

   ClassName := FTok.GetToken.AsString;
   FTok.KillToken;

   if not (FTok.TestName and (FTok.GetToken.AsString = 'implements')) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['implements']);
   FTok.KillToken;

   if not ReadIdentifier(InterfaceName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Semicolon expected');
end;

function TdwsWebIDLInterpreter.ReadConst(Definition: TWiInheritableDefinitionWithConst): Boolean;
var
   WiConst: TWiConst;
   ConstName: string;
   ConstValue: Variant;
   ValueType: TWiType;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Const

   Result := FTok.TestDelete(ttCONST);

   if Result then
   begin
      ValueType := ReadConstType;

      if not ReadIdentifier(ConstName, True) then
         FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

      if not FTok.TestDelete(ttEQ) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['=']);

      ConstValue := ReadConstValue(ValueType);

      if not FTok.TestDelete(ttSEMI) then
         FMsgs.AddCompilerStop(FTok.HotPos, 'Semicolon expected');

      WiConst := TWiConst.Create(ValueType, ConstName, ConstValue);

      Definition.FConsts.Add(WiConst)
   end;
end;

function TdwsWebIDLInterpreter.ReadConstValue(ValueType: TWiType): Variant;

   function ReadBooleanLiteral: Boolean;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-BooleanLiteral

      Result := True;
      case FTok.TestDeleteAny([ttTRUE, ttFALSE]) of
         ttTRUE:
            ReadConstValue := True;
         ttFALSE:
            ReadConstValue := False;
      else
         Result := False;
      end;
   end;

   function ReadFloatLiteral: Boolean;
   var
      Token: TToken;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-FloatLiteral

      Result := False;
      if FTok.Test(ttFloatVal) then
      begin
         Token := FTok.GetToken;
         ReadConstValue := Token.FFloat;
         Result := True;
      end
      else
   end;

var
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#prod-ConstValue

   if ReadBooleanLiteral then
   begin
      FTok.KillToken;
      Exit;
   end;

   if ReadFloatLiteral then
   begin
      FTok.KillToken;
      Exit;
   end;

   if FTok.Test(ttIntVal) then
   begin
      Token := FTok.GetToken;
      ReadConstValue := Token.FInteger;

      FTok.KillToken;
      Exit;
   end;

   if not FTok.TestName then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['null']);

   Token := FTok.GetToken;

   if not (Token.AsString = 'null') then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['null']);
   FTok.KillToken;

   ReadConstValue := Null;
end;

procedure TdwsWebIDLInterpreter.ReadAttributeOrOperation(
   WebInterface: TWiInterface; ExtendedAttributeList: TWiExtendedAttributeList);
var
  Attribute: TWiAttribute;
  Operation: TWiOperation;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-AttributeOrOperation

   // first check whether a name is provided
   if not FTok.TestName then
     FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);

   if not ReadStringifierAttributeOrOperation(ExtendedAttributeList) then
   begin
      // try reading attribute
      Attribute := ReadAttribute(ExtendedAttributeList);

      // check if attribute is present, otherwise read operation
      if Assigned(Attribute) then
      begin
         Assert(Assigned(WebInterface));
         WebInterface.FAttributes.Add(Attribute)
      end
      else
      begin
         Operation := ReadOperation(ExtendedAttributeList);
         if Assigned(WebInterface) and Assigned(Operation) then
            WebInterface.FOperations.Add(Operation);
      end;
   end;
end;

function TdwsWebIDLInterpreter.ReadStringifierAttributeOrOperation(
   ExtendedAttributeList: TWiExtendedAttributeList): Boolean;
var
  Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-StringifierAttributeOrOperation

   Token := FTok.GetToken;

   // check if 'stringifier' is the next token
   Result := (Token.AsString = 'stringifier');
   if Result then
   begin
      // kill 'stringifier' token
      FTok.KillToken;

      if not FTok.TestDelete(ttSEMI) then
      begin
         Token := FTok.GetToken;
         if (Token.AsString = 'inherit') or
            (Token.AsString = 'readonly') or
            (Token.AsString = 'attribute') then
         begin
            ReadAttribute(ExtendedAttributeList);
         end else
            ReadOperationRest(ExtendedAttributeList);
     end;
   end;
end;

function TdwsWebIDLInterpreter.ReadAttribute(
   ExtendedAttributeList: TWiExtendedAttributeList): TWiAttribute;
var
   Token: TToken;

   function ReadInherit: Boolean;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-Inherit

      Result := Token.AsString = 'inherit';
      if Result then
      begin
         FTok.KillToken;
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);
         Token := FTok.GetToken;
      end;
   end;

   function ReadReadOnly: Boolean;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-ReadOnly

      Result := Token.AsString = 'readonly';
      if Result then
      begin
         FTok.KillToken;
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);
         Token := FTok.GetToken;
      end;
   end;

var
   TypeSymbol: TWiType;
   IsInherit, IsReadOnly, IsAttribute: Boolean;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Attribute

   Result := nil;
   Token := FTok.GetToken;

   IsInherit := ReadInherit;
   IsReadOnly := ReadReadOnly;
   IsAttribute := (Token.AsString = 'attribute');

   // check if token is attribute at all
   if not IsAttribute then
      if not (IsInherit or IsReadOnly) then
         Exit
      else
         FMsgs.AddCompilerStop(FTok.HotPos, 'attribute expected');

   FTok.KillToken;

   // read type
   TypeSymbol := ReadType;
   if not Assigned(TypeSymbol) then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Type expected');

   // read Identifier
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);
   Token := FTok.GetToken;

   Result := TWiAttribute.Create(Token.AsString, TypeSymbol,
      ExtendedAttributeList, IsInherit, IsReadOnly);

   FTok.KillToken;

   if not FTok.TestDelete(ttSEMI) then
   begin
      FMsgs.AddCompilerError(FTok.HotPos, 'Semicolon expected');

      repeat
        FTok.KillToken;

        // check if tokens are available at all
        if not FTok.HasTokens then
          Exit(nil);

      until FTok.TestDelete(ttSEMI);
   end;
end;

function TdwsWebIDLInterpreter.ReadOperation(ExtendedAttributeList: TWiExtendedAttributeList): TWiOperation;
var
  Specials: TSpecialQualifiers;
  IsStatic: Boolean;

   procedure ReadQualifiers;
   var
      Token: TToken;

      function ReadSpecials: TSpecialQualifiers;

         function ReadSpecial: TSpecialQualifier;
         var
           QualifierIndex: TSpecialQualifier;
         const
            CSpecialQualifierName: array [TSpecialQualifier] of string = ('',
              'getter', 'setter', 'creator', 'deleter', 'legacycaller');
         begin
            // see http://www.w3.org/TR/WebIDL/#proddef-Special

            Token := FTok.GetToken;

            Result := sqNone;
            for QualifierIndex := TSpecialQualifier(1) to High(CSpecialQualifierName) do
               if (Token.AsString = CSpecialQualifierName[QualifierIndex]) then
                  Exit(QualifierIndex);
         end;

      var
        SpecialQualifier: TSpecialQualifier;
      begin
         // see http://www.w3.org/TR/WebIDL/#proddef-Specials

         while FTok.HasTokens do
         begin
            SpecialQualifier := ReadSpecial;

            if SpecialQualifier = sqNone then
               Break;

            FTok.KillToken;
            Result := Result + [SpecialQualifier];
         end;
      end;

   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-Qualifiers

      Token := FTok.GetToken;

      Specials := [];
      IsStatic := (Token.AsString = 'static');
      if IsStatic then
         FTok.KillToken
      else
         Specials := ReadSpecials;
   end;

begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Operation

   ReadQualifiers;
   Result := ReadOperationRest(ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadOperationRest(ExtendedAttributeList: TWiExtendedAttributeList): TWiOperation;
var
   ReturnType: TWiType;
   OptionalIdentifier: string;

   function ReadOptionalIdentifier: Boolean;
   var
      Token: TToken;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-OptionalIdentifier

      Result := FTok.TestName;
      if Result then
      begin
         Token := FTok.GetToken;
         OptionalIdentifier := Token.AsString;
         FTok.KillToken;
      end
      else
         OptionalIdentifier := '';
   end;

var
   Arguments: TWiArgumentList;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-OperationRest

   ReturnType := ReadReturnType;

   ReadOptionalIdentifier;

   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['(']);

   Arguments := nil;
   if not FTok.Test(ttBRIGHT) then
      Arguments := ReadArgumentList;

   if not FTok.TestDelete(ttBRIGHT) then
   begin
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, RStrTokenExpected, [')']);

      repeat
        FTok.KillToken;

        // check if tokens are available at all
        if not FTok.HasTokens then
          Exit(nil);

      until FTok.TestDelete(ttBRIGHT);
   end;

   if not FTok.TestDelete(ttSEMI) then
   begin
      FMsgs.AddCompilerError(FTok.HotPos, RStrSemicolonExpected);

      repeat
        FTok.KillToken;

        // check if tokens are available at all
        if not FTok.HasTokens then
          Exit(nil);

      until FTok.TestDelete(ttSEMI);
   end;

   Result := TWiOperation.Create(OptionalIdentifier, ReturnType, Arguments,
      ExtendedAttributeList);
end;

function TdwsWebIDLInterpreter.ReadArgumentList: TWiArgumentList;
var
   ArgumentList: TWiArgumentList;

   procedure ReadArgument;
   var
      ExtendedAttributeList: TWiExtendedAttributeList;
      Argument: TWiArgument;
   begin
      // see http://www.w3.org/TR/WebIDL/#proddef-Argument
      ExtendedAttributeList := ReadExtendedAttributeList;
      Argument := ReadOptionalOrRequiredArgument(ExtendedAttributeList);
      ArgumentList.Add(Argument);
   end;

begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ArgumentList
   // and http://www.w3.org/TR/WebIDL/#proddef-Arguments

   ArgumentList := TWiArgumentList.Create;
   try
      ReadArgument;
      while FTok.TestDelete(ttCOMMA) do
         ReadArgument;

      Result := ArgumentList;
   except
      Result := nil;
      ArgumentList.Free;
   end;
end;

function TdwsWebIDLInterpreter.ReadOptionalOrRequiredArgument(
   ExtendedAttributeList: TWiExtendedAttributeList): TWiArgument;
var
   IsOptional: Boolean;
   HasEllipsis: Boolean;
   Token: TToken;
   ArgumentType: TWiType;
   ArgumentName: string;
   DefaultValue: Variant;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-OptionalOrRequiredArgument

   IsOptional := False;
   HasEllipsis := False;

   if FTok.TestName then
   begin
      Token := FTok.GetToken;
      IsOptional := Token.AsString = 'optional';
      if IsOptional then
         FTok.KillToken;
   end;

   ArgumentType := ReadType;
   if not IsOptional then
      HasEllipsis := ReadEllipsis;

   ArgumentName := ReadArgumentName;
   if ArgumentName = '' then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Argument name may not be empty');
   FTok.KillToken;

   Result := TWiArgument.Create(ArgumentName, ArgumentType, ExtendedAttributeList, IsOptional);
   Result.HasEllipsis := HasEllipsis;

   if IsOptional then
   begin
      if not ReadDefault(ArgumentType, DefaultValue) then
         DefaultValue := Unassigned;
      Result.DefaultValue := DefaultValue;
   end;
end;

function TdwsWebIDLInterpreter.ReadArgumentName: string;
var
   ArgumentNameKeyword: TArgumentNameKeyword;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ArgumentName

   ArgumentNameKeyword := ReadArgumentNameKeyword;

   case ArgumentNameKeyword of
      ankAttribute:
         Result := 'Attribute';
      ankCallback:
         Result := 'Callback';
      ankConst:
         Result := 'Const';
      ankCreator:
         Result := 'Creator';
      ankDeleter:
         Result := 'Deleter';
      ankDictionary:
         Result := 'Dictionary';
      ankEnum:
         Result := 'Enum';
      ankException:
         Result := 'Exception';
      ankGetter:
         Result := 'Getter';
      ankImplements:
         Result := 'Implements';
      ankInherit:
         Result := 'Inherit';
      ankInterface:
         Result := 'Interface';
      ankLegacycaller:
         Result := 'Legacycaller';
      ankPartial:
         Result := 'Partial';
      ankSetter:
         Result := 'Setter';
      ankStatic:
         Result := 'Static';
      ankStringifier:
         Result := 'Stringifier';
      ankTypedef:
         Result := 'Typedef';
      ankUnrestricted:
         Result := 'Unrestricted';
      else
         if not ReadIdentifier(Result, True) then
            FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);
   end;
end;

function TdwsWebIDLInterpreter.ReadEllipsis: Boolean;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Ellipsis

   // ensure ellipsis is not identified as '..'
   Assert(not FTok.Test(ttDOTDOT));

   // check if first dot is found
   Result := FTok.TestDelete(ttDOT);
   if Result then
   begin
      if not FTok.TestDelete(ttDOT) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['.']);

      if not FTok.TestDelete(ttDOT) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['.']);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadExceptionMember(WebException: TWiException;
   ExtendedAttributeList: TWiExtendedAttributeList);
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExceptionMember

   if not ReadConst(WebException) then
     ReadExceptionField(WebException, ExtendedAttributeList);
end;

procedure TdwsWebIDLInterpreter.ReadExceptionField(WebException: TWiException;
   ExtendedAttributeList: TWiExtendedAttributeList);
var
   FieldType: TWiType;
   FieldName: string;
   ExceptionField: TWiExceptionField;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExceptionField

   FieldType := ReadType;

   // read exception member name
   if not ReadIdentifier(FieldName, True) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrIdentifierExpected);

   // read and check if semicolon is present
   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrSemicolonExpected);

   ExceptionField := TWiExceptionField.Create(FieldName, FieldType,
      ExtendedAttributeList);

   Assert(Assigned(WebException));
   WebException.FFields.Add(ExceptionField);
end;

function TdwsWebIDLInterpreter.ReadExtendedAttributeList: TWiExtendedAttributeList;
var
   Text: string;
   ExtendedAttribute: TWiExtendedAttribute;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExtendedAttributeList
   // and http://www.w3.org/TR/WebIDL/#proddef-ExtendedAttributes

   // create ExtendedAttributeList
   Result := TWiExtendedAttributeList.Create;

   if FTok.TestDelete(ttALEFT) then
   begin
      Text := '';
      ReadExtendedAttribute(Text);
      ExtendedAttribute := TWiExtendedAttribute.Create(Text);
      Result.Add(ExtendedAttribute);

      while FTok.TestDelete(ttCOMMA) do
      begin
         Text := '';
         ReadExtendedAttribute(Text);
         ExtendedAttribute := TWiExtendedAttribute.Create(Text);
         Result.Add(ExtendedAttribute);
      end;

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, 'right bracket expected');
   end;
end;

procedure TdwsWebIDLInterpreter.ReadExtendedAttribute(var Text: string);
var
  OtherAsString: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExtendedAttribute

   case FTok.TestDeleteAny([ttALEFT, ttBLEFT, ttCLEFT]) of
      ttALEFT:
         begin
            Text := Text + '[';
            ReadExtendedAttributeInner(Text);

(*
            if not FTok.TestDelete(ttARIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, 'right bracket expected');
*)

            ReadExtendedAttributeRest(Text);
         end;
      ttBLEFT:
         begin
            Text := Text + '(';
            ReadExtendedAttributeInner(Text);

(*
            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, 'right bracket expected');
*)

            ReadExtendedAttributeRest(Text);
         end;
      ttCLEFT:
         begin
            Text := Text + '{';
            ReadExtendedAttributeInner(Text);

(*
            if not FTok.TestDelete(ttCRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, 'right bracket expected');
*)

            ReadExtendedAttributeRest(Text);
         end;
      else
         begin
            OtherAsString := ReadOther;
            if OtherAsString <> '' then
            begin
               Text := Text + OtherAsString;
               FTok.KillToken;
            end
            else
               FMsgs.AddCompilerStop(FTok.HotPos, 'Other expected');

            ReadExtendedAttributeRest(Text);
         end;
   end;
end;

procedure TdwsWebIDLInterpreter.ReadExtendedAttributeRest(var Text: string);
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExtendedAttributeRest

   if not FTok.Test(ttARIGHT) then
     ReadExtendedAttribute(Text);
end;

procedure TdwsWebIDLInterpreter.ReadExtendedAttributeInner(var Text: string);
var
  OtherAsString: string;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ExtendedAttributeInner

   case FTok.TestDeleteAny([ttALEFT, ttARIGHT, ttBLEFT, ttBRIGHT, ttCLEFT, ttCRIGHT]) of
      ttALEFT:
         begin
            Text := Text + '[';
            ReadExtendedAttributeInner(Text);
         end;
      ttBLEFT:
         begin
            Text := Text + '(';
            ReadExtendedAttributeInner(Text);
         end;
      ttCLEFT:
         begin
            Text := Text + '{';
            ReadExtendedAttributeInner(Text);
         end;
      ttARIGHT:
         begin
            Text := Text + ']';
            Exit;
         end;
      ttBRIGHT:
         begin
            Text := Text + ')';
            Exit;
         end;
      ttCRIGHT:
         begin
            Text := Text + '}';
            Exit;
         end;
      else
         begin
            OtherAsString := ReadOther;
            if OtherAsString <> '' then
            begin
               Text := Text + ' ' + OtherAsString;
               FTok.KillToken;

               ReadExtendedAttributeInner(Text);
            end;
         end;
   end;
end;

function TdwsWebIDLInterpreter.ReadOther: string;
var
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Other

   if not FTok.HasTokens then
      FMsgs.AddCompilerStop(FTok.HotPos, RStrAnyTokenExpected);

   Token := FTok.GetToken;

   case FTok.TestAny([ttIntVal, ttFloatVal, ttStrVal, ttMINUS, ttDOT,
      ttCOLON, ttSEMI, ttLESS, ttEQ, ttGTR, ttQUESTION, ttFALSE, ttTRUE]) of

      ttIntVal:
         begin
           Result := IntToStr(Token.FInteger);
         end;

      ttFloatVal:
         begin
           Result := FloatToStr(Token.FFloat);
         end;

      ttStrVal, ttMINUS, ttDOT, ttCOLON, ttSEMI, ttLESS, ttEQ, ttGTR,
      ttQUESTION, ttFALSE, ttTRUE:
          begin
             Result := Token.AsString;
          end;

      else
         begin
            // get string
            Result := Token.AsString;
         end;
   end;
end;

function TdwsWebIDLInterpreter.ReadArgumentNameKeyword: TArgumentNameKeyword;
var
   TokenName: string;
   ArgNameKeyIndex: TArgumentNameKeyword;
const
   CArgumentNameKeyword: array [TArgumentNameKeyword] of string =
     ('', 'attribute', 'callback', 'const', 'creator', 'deleter', 'dictionary',
     'enum', 'exception', 'getter', 'implements', 'inherit', 'interface',
     'legacycaller', 'partial', 'setter', 'static', 'stringifier', 'typedef',
     'unrestricted');

begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ArgumentNameKeyword

   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Token name expected');
   TokenName := FTok.GetToken.AsString;

   Result := ankNone;
   for ArgNameKeyIndex := TArgumentNameKeyword(1) to High(CArgumentNameKeyword) do
      if (TokenName = CArgumentNameKeyword[ArgNameKeyIndex]) then
      begin
         FTok.KillToken;
         Exit(ArgNameKeyIndex);
      end;
end;

procedure TdwsWebIDLInterpreter.ReadOtherOrComma;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-OtherOrComma

   if not FTok.TestDelete(ttCOMMA) then
     ReadOther;
end;

function TdwsWebIDLInterpreter.ReadType: TWiType;
var
   IsNullable: Boolean;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-Type

   if FTok.Test(ttBLEFT) then
   begin
      Result := ReadUnionType;
      IsNullable := False;
      ReadTypeSuffix(Result, IsNullable);
   end
   else
      Result := ReadSingleType;
end;

function TdwsWebIDLInterpreter.ReadPrimitiveType(Token: TToken): TWiType;

   function TestDeleteUnrestrictedFloatType: TWiType;

      function TestDeleteFloatType: TWiType;

         function TestDeleteFloat: TWiType;
         begin
            Result := nil;
            if (Token.AsString = 'float') then
            begin
               FTok.KillToken;
               Result := TWiFloatType.Create;
            end;
         end;

         function TestDeleteDouble: TWiType;
         begin
            Result := nil;
            if (Token.AsString = 'double') then
            begin
               FTok.KillToken;
               Result := TWiDoubleType.Create;
            end;
         end;

      begin
         Result := TestDeleteFloat;
         if not Assigned(Result) then
           Result := TestDeleteDouble;
      end;

   var
      IsUnrestricted: Boolean;
   begin
      IsUnrestricted := (Token.AsString = 'unrestricted');
      if IsUnrestricted then
      begin
         FTok.KillToken;

         // check for next token (must be a name!)
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');
         Token := FTok.GetToken;
      end;

      Result := TestDeleteFloatType;

      // check if unregistered was found without float or double
      if IsUnrestricted and (Result = nil) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['float'' or ''double']);
   end;

   function TestDeleteUnsignedIntegerType: TWiType;

      function TestDeleteIntegerType: TWiType;

         function TestDeleteShort: TWiType;
         begin
            Result := nil;
            if (Token.AsString = 'short') then
            begin
               FTok.KillToken;
               Result := TWiShortType.Create;
            end;
         end;

         function TestDeleteLong: TWiType;
         begin
            Result := nil;
            if (Token.AsString = 'long') then
            begin
               FTok.KillToken;
               Result := TWiLongType.Create;
            end;
         end;

      begin
         Result := TestDeleteShort;
         if not Assigned(Result) then
         begin
            Result := TestDeleteLong;
            if FTok.HasTokens and Assigned(Result) then
            begin
               Token := FTok.GetToken;
               if (Token.AsString = 'long') then
               begin
                  FTok.KillToken;
                  // Type: long long
               end;
            end;
         end;
      end;

   var
      IsUnsigned: Boolean;
   begin
      IsUnsigned := (Token.AsString = 'unsigned');
      if IsUnsigned then
      begin
         FTok.KillToken;

         // check for next token (must be a name!)
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');
         Token := FTok.GetToken;
      end;

      Result := TestDeleteIntegerType;

      // check if unregistered was found without float or double
      if IsUnsigned and (Result = nil) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['short'' or ''long']);
   end;

   function TestDeleteBoolean: TWiType;
   begin
      Result := nil;
      if (Token.AsString = 'boolean') then
      begin
         FTok.KillToken;
         Result := TWiBooleanType.Create;
      end;
   end;

   function TestDeleteByte: TWiType;
   begin
      Result := nil;
      if (Token.AsString = 'byte') then
      begin
         FTok.KillToken;
         Result := TWiByteType.Create;
      end;
   end;

   function TestDeleteOctet: TWiType;
   begin
      Result := nil;
      if (Token.AsString = 'octet') then
      begin
         FTok.KillToken;
         Result := TWiOctetType.Create;
      end;
   end;

begin
   // see http://www.w3.org/TR/WebIDL/#proddef-PrimitiveType

   Result := TestDeleteUnsignedIntegerType;
   if not Assigned(Result) then
      Result := TestDeleteUnrestrictedFloatType;
   if not Assigned(Result) then
      Result := TestDeleteBoolean;
   if not Assigned(Result) then
      Result := TestDeleteByte;
   if not Assigned(Result) then
      Result := TestDeleteOctet;
end;

function TdwsWebIDLInterpreter.ReadSingleType: TWiType;
var
   Token: TToken;
   IsNullable: Boolean;
begin
   // see http://www.w3.org/TR/WebIDL/#prod-SingleType

   Result := nil;
   IsNullable := False;

   // IsConst := FTok.TestDelete(ttCONST);  // Workaround for WebCL

   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');

   Token := FTok.GetToken;

   if Token.AsString = 'any' then
   begin
      Result := TWiAnyType.Create;
      FTok.KillToken;

      ReadTypeSuffixStartingWithArray(Result, IsNullable);
   end else
      Result := ReadNonAnyType(Token);
end;

function TdwsWebIDLInterpreter.ReadNonAnyType(Token: TToken = nil): TWiType;
var
   IsNullable: Boolean;
   ElementType: TWiType;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-NonAnyType

   Result := nil;
   IsNullable := False;

   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');

   if Token = nil then
     Token := FTok.GetToken;

   Result := ReadPrimitiveType(Token);
   if Assigned(Result) then
   begin
      ReadTypeSuffix(Result, IsNullable);
      Exit;
   end;

   if Token.AsString = 'DOMString' then
   begin
      Result := TWiDOMStringType.Create;
      FTok.KillToken;

      ReadTypeSuffix(Result, IsNullable);
   end else
   if Token.AsString = 'sequence' then
   begin
      FTok.KillToken;
      if not FTok.TestDelete(ttLESS) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['<']);

      ElementType := ReadType;
      Result := TWiSequence.Create(ElementType);

      if not FTok.TestDelete(ttGTR) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['>']);

      // check if type is nullable
      IsNullable := FTok.TestDelete(ttQUESTION);
   end else
   if Token.AsString = 'Promise' then
   begin
      FTok.KillToken;
      if not FTok.TestDelete(ttLESS) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['<']);

      ElementType := ReadType;
      Result := TWiPromise.Create(ElementType.TypeName, ElementType);

      if not FTok.TestDelete(ttGTR) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['>']);

      // check if type is nullable
      IsNullable := FTok.TestDelete(ttQUESTION);
   end else
   if Token.AsString = 'object' then
   begin
      FMsgs.AddCompilerWarning(FTok.HotPos, 'object type not fully supported yet');

      Result := TWiObjectType.Create;
      FTok.KillToken;

      ReadTypeSuffix(Result, IsNullable);
   end else
   if Token.AsString = 'Date' then
   begin
      FMsgs.AddCompilerWarning(FTok.HotPos, 'date type not supported yet');
      Result := TWiIdentifierType.Create('Date', FWebIDL);

      FTok.KillToken;

      ReadTypeSuffix(Result, IsNullable);
   end else
   begin
      Result := TWiIdentifierType.Create(Token.AsString, FWebIDL);
      FTok.KillToken;

      ReadTypeSuffix(Result, IsNullable);
   end;
end;


procedure TdwsWebIDLInterpreter.ReadUnionMemberType(UnionType: TWiUnionType);
var
   NestedType: TWiType;
   IsNullable: Boolean;
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#prod-UnionMemberType

   if FTok.TestDelete(ttBLEFT) then
   begin
      IsNullable := False;
      NestedType := ReadUnionType;
      ReadTypeSuffix(NestedType, IsNullable);
   end
   else
   begin
      Token := FTok.GetToken;

      // check if 'any' type
      if Token.AsString = 'any' then
      begin
         if not FTok.TestDelete(ttALEFT) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['[']);
         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, [']']);
         ReadTypeSuffix(NestedType, IsNullable);
      end
      else
         NestedType := ReadNonAnyType;
   end;
end;

function TdwsWebIDLInterpreter.ReadUnionType: TWiType;
var
   UnionType: TWiUnionType;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-UnionType
   // and http://www.w3.org/TR/WebIDL/#proddef-UnionMemberTypes

   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['(']);

   UnionType := TWiUnionType.Create;
   try
      ReadUnionMemberType(UnionType);

      if not FTok.TestDelete(ttOR) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['or']);

      ReadUnionMemberType(UnionType);

      while not FTok.TestDelete(ttBRIGHT) do
      begin
         if not FTok.TestDelete(ttOR) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, ['or']);

         ReadUnionMemberType(UnionType);
      end;

      Result := UnionType;
   except
      Result := nil;
      UnionType.Free;
   end;
end;

procedure TdwsWebIDLInterpreter.ReadTypeSuffix(var WiType: TWiType;
  var IsNullable: Boolean);
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-TypeSuffix

   if FTok.TestDelete(ttALEFT) then
   begin
      WiType := TWiArray.Create(WiType);
      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, [']']);
      ReadTypeSuffix(WiType, IsNullable);
   end
   else
   if FTok.TestDelete(ttQUESTION) then
   begin
     IsNullable := True;
     ReadTypeSuffixStartingWithArray(WiType, IsNullable);
   end;
end;

procedure TdwsWebIDLInterpreter.ReadTypeSuffixStartingWithArray(
  var WiType: TWiType; var IsNullable: Boolean);
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-TypeSuffix

   if FTok.TestDelete(ttARIGHT) then
   begin
      WiType := TWiSequence.Create(WiType);
      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, RStrTokenExpected, [']']);
      ReadTypeSuffix(WiType, IsNullable);
   end;
end;

function TdwsWebIDLInterpreter.ReadConstType: TWiType;
var
   Token: TToken;
begin
   // see http://www.w3.org/TR/WebIDL/#proddef-ConstType

   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');

   Token := FTok.GetToken;

   Result := ReadPrimitiveType(Token);

   if not Assigned(Result) then
   begin
      Result := TWiIdentifierType.Create(Token.AsString, FWebIDL);
      FTok.KillToken;

      if not Assigned(Result) then
      begin
         {$IFDEF DEBUG}
         FMsgs.AddCompilerWarning(FTok.HotPos, 'Const type not recognized properly!');
         {$ENDIF}
      end;
   end;
end;


function TdwsWebIDLInterpreter.ReadReturnType: TWiType;
var
   Token: TToken;
begin
   if not FTok.HasTokens then
      FMsgs.AddCompilerStop(FTok.HotPos, 'Name token expected');

   Token := FTok.GetToken;

   // see http://www.w3.org/TR/WebIDL/#proddef-ReturnType
   if FTok.TestName and (Token.AsString = 'void') then
   begin
     Result := nil; // void!
     FTok.KillToken;
   end
   else
     Result := ReadType;
end;

end.
