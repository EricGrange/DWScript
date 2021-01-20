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
unit dwsInfo;

{$I dws.inc}

interface

uses
   Classes,
   dwsDataContext, dwsSymbols, dwsJSON;

type

   // Helper object for access to symbols
   IInfo = interface
      ['{8D534D16-4C6B-11D5-8DCB-0000216D9E86}']
      function Call: IInfo; overload;
      function Call(const Params: array of Variant): IInfo; overload;
      function Element(const Indices: array of Integer): IInfo;
      function GetConstructor(const MethName: String; ExtObject: TObject): IInfo;
      function GetData : TData;
      function GetExternalObject: TObject;
      function GetMember(const s: String): IInfo;
      function GetFieldMemberNames : TStrings;
      function GetMethod(const s: String): IInfo;
      function GetScriptObj: IScriptObj;
      function GetScriptDynArray: IScriptDynArray;
      function GetParameter(const s: String): IInfo;
      function GetTypeSym: TSymbol;
      function GetValue : Variant;
      function GetValueIsEmpty : Boolean;
      function GetValueAsString : String;
      function GetValueAsDataString : RawByteString;
      function GetValueAsInteger : Int64;
      function GetValueAsBoolean : Boolean;
      function GetValueAsFloat : Double;
      function GetInherited: IInfo;
      function GetExec : IdwsExecution;
      procedure SetData(const Data: TData);
      procedure SetExternalObject(ExtObject: TObject);
      procedure SetValue(const Value: Variant);
      procedure SetValueAsInteger(const value : Int64);
      procedure SetValueAsString(const value : String);
      procedure SetValueAsDataString(const value : RawByteString);

      procedure WriteToJSON(writer : TdwsJSONWriter);

      property Data: TData read GetData write SetData;
//      property Data: TData write SetData;
      property ExternalObject: TObject read GetExternalObject write SetExternalObject;
      property Member[const s : String]: IInfo read GetMember;
      property FieldMemberNames : TStrings read GetFieldMemberNames;
      property Method[const s : String]: IInfo read GetMethod;

      property Exec: IdwsExecution read GetExec;
      property ScriptObj: IScriptObj read GetScriptObj;
      property ScriptDynArray: IScriptDynArray read GetScriptDynArray;
      property Parameter[const s: String]: IInfo read GetParameter;
      property TypeSym: TSymbol read GetTypeSym;
      property Value: Variant read GetValue write SetValue;
      property ValueIsEmpty : Boolean read GetValueIsEmpty;
      property ValueAsString : String read GetValueAsString write SetValueAsString;
      property ValueAsDataString : RawByteString read GetValueAsDataString write SetValueAsDataString;
      property ValueAsInteger : Int64 read GetValueAsInteger write SetValueAsInteger;
      property ValueAsBoolean : Boolean read GetValueAsBoolean;
      property ValueAsFloat : Double read GetValueAsFloat;
   end;

implementation

end.
