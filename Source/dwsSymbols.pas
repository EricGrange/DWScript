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
{$I dws.inc}
unit dwsSymbols;

interface

uses Windows, SysUtils, Variants, Classes, dwsStrings, dwsStack, dwsErrors;

type
   TBaseTypeID = (
      typIntegerID,
      typFloatID,
      typStringID,
      typBooleanID,
      typVariantID,
      typConnectorID,
      typClassID,
      typNoneID
   );

type

   IScriptObj = interface;

   // Base class for all Exprs
   TExprBase = class
      function Eval : Variant; virtual; abstract;
      function EvalAsInteger : Int64; virtual; abstract;
      function EvalAsBoolean : Boolean; virtual; abstract;
      procedure EvalAsFloat(var Result : Double); virtual; abstract;
      procedure EvalAsString(var Result : String); overload; virtual; abstract;
      procedure EvalAsVariant(var Result : Variant); overload; virtual; abstract;
      procedure EvalAsScriptObj(var Result : IScriptObj); virtual; abstract;

      procedure AssignValue(const value : Variant); virtual; abstract;
      procedure AssignValueAsInteger(const value : Int64); virtual; abstract;
      procedure AssignValueAsBoolean(const value : Boolean); virtual; abstract;
      procedure AssignValueAsFloat(var value : Double); virtual; abstract;
      procedure AssignValueAsString(const value: String); virtual; abstract;
   end;

   // TTightList
   //
   {: Compact list embedded in a record.<p>
      If the list holds only 1 item, no dynamic memory is allocated
      (the list pointer is used).
      Make sure to Clear or Clear in the destructor of the Owner. }
   TTightList = record
      private
         FList: PPointerList;
         FCount: Integer;

         procedure RaiseIndexOutOfBounds;
         function GetList : PPointerList;

      public
         property List : PPointerList read GetList;
         property Count : Integer read FCount;

         procedure Clean;
         procedure Clear;
         function Add(item : Pointer) : Integer;
         procedure Assign(const aList : TTightList);
         function IndexOf(item : Pointer) : Integer;
         function Remove(item : Pointer) : Integer;
         procedure Delete(index : Integer);
         procedure Insert(index : Integer; item : Pointer);
         procedure Move(curIndex, newIndex : Integer);
         procedure Exchange(index1, index2 : Integer);
   end;

   // TExprBaseList
   //
   TExprBaseList = ^TExprBaseListRec;
   TExprBaseListRec = record
      private
         FList : TTightList;

         function GetExprBase(const x : Integer): TExprBase;
         procedure SetExprBase(const x : Integer; expr : TExprBase);
         function GetAsInteger(const x : Integer) : Int64;
         procedure SetAsInteger(const x : Integer; const value : Int64);
         function GetAsBoolean(const x : Integer) : Boolean;
         function GetAsFloat(const x : Integer) : Double;
         function GetAsString(const x : Integer) : String;
         function GetAsDataString(const x : Integer) : RawByteString;

      public
         procedure Clean;

         function Add(expr : TExprBase) : Integer; inline;
         procedure Insert(index : Integer; expr : TExprBase);
         procedure Delete(index : Integer);

         property ExprBase[const x : Integer] : TExprBase read GetExprBase write SetExprBase; default;
         property Count : Integer read FList.FCount;

         property AsInteger[const x : Integer] : Int64 read GetAsInteger write SetAsInteger;
         property AsBoolean[const x : Integer] : Boolean read GetAsBoolean;
         property AsFloat[const x : Integer] : Double read GetAsFloat;
         property AsString[const x : Integer] : String read GetAsString;
         property AsDataString[const x : Integer] : RawByteString read GetAsDataString;
   end;

   TSymbol = class;
   TBaseSymbol = class;
   TDataSymbol = class;
   TFuncSymbol = class;
   TMethodSymbol = class;
   TFieldSymbol = class;
   TClassSymbol = class;
   TRecordSymbol = class;
   TParamSymbol = class;
   TVarParamSymbol = class;
   TSymbolTable = class;
   TTypeSymbol = class;
   TParamsSymbolTable = class;

   // All functions callable from the script implement this interface
   IExecutable = interface
      ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
      procedure InitSymbol(Symbol: TSymbol);
      procedure InitExpression(Expr: TExprBase);
   end;

   TAddrGeneratorSign = (agsPositive, agsNegative);

   // TAddrGenerator
   //
   TAddrGeneratorRec = record
      private
         FDataSize: Integer;
         FLevel: Integer;
         FSign: TAddrGeneratorSign;

         function GetDataSize: Integer;

      public
         constructor CreatePositive(aLevel: Integer; anInitialSize: Integer = 0);
         constructor CreateNegative(aLevel: Integer);

         function GetStackAddr(Size: Integer): Integer;

         property DataSize: Integer read GetDataSize;
         property Level: Integer read FLevel;
   end;
   TAddrGenerator = ^TAddrGeneratorRec;

   // TSymbol
   //
   // Named item in the script
   TSymbol = class
      strict private
         FName : String;

      protected
         FSize : Integer;
         FTyp : TSymbol;
         function GetCaption : String; virtual;
         function GetDescription : String; virtual;

      public
         constructor Create(const name : string; typ : TSymbol);

         procedure InitData(const data : TData; offset : Integer); virtual;
         procedure Initialize; virtual;
         function BaseType : TTypeSymbol; virtual;
         procedure SetName(const newName : String);

         function IsCompatible(typSym : TSymbol) : Boolean; virtual;

         function BaseTypeID : TBaseTypeID; virtual;
         function IsBaseTypeIDValue(aBaseTypeID : TBaseTypeID) : Boolean;
         function IsBaseTypeIDArray(aBaseTypeID : TBaseTypeID) : Boolean;

         property Caption : String read GetCaption;
         property Description : String read GetDescription;
         property Name : String read FName;
         property Typ : TSymbol read FTyp write FTyp;
         property Size : Integer read FSize;
   end;

   TSymbolClass = class of TSymbol;

   // All Symbols containing a value
   TValueSymbol = class(TSymbol)
   protected
     function GetCaption: string; override;
     function GetDescription: string; override;
   end;

   // named constant: const x = 123;
   TConstSymbol = class(TValueSymbol)
   protected
     FData: TData;
     function GetCaption: string; override;
     function GetDescription: string; override;
   public
     constructor Create(const Name: string; Typ: TSymbol; const Value: Variant); overload;
     constructor Create(const Name: string; Typ: TSymbol; const Data: TData; Addr: Integer); overload;
     procedure Initialize; override;
     property Data: TData read FData;
   end;

   // variable: var x: Integer;
   TDataSymbol = class(TValueSymbol)
   protected
     FLevel: Integer;
     FStackAddr: Integer;
     function GetDescription: string; override;
   public
     procedure InitData(const Data: TData; Offset: Integer); override;
     property Level: Integer read FLevel write FLevel;
     property StackAddr: Integer read FStackAddr write FStackAddr;
   end;

   // parameter: procedure P(x: Integer);
   TParamSymbol = class(TDataSymbol)
   private
   protected
     function GetDescription: string; override;
   public
   end;

   TParamSymbolWithDefaultValue = class(TParamSymbol)
   private
     FDefaultValue : TData;
   protected
     function GetDescription: string; override;
   public
     procedure SetDefaultValue(const Data: TData; Addr: Integer); overload;
     procedure SetDefaultValue(const Value: Variant); overload;
     property DefaultValue : TData read FDefaultValue;
   end;

   // const/var parameter: procedure P(const/var x: Integer)
   TByRefParamSymbol = class(TParamSymbol)
   protected
   public
     constructor Create(const Name: string; Typ: TSymbol);
   end;

   // const parameter: procedure P(const x: Integer)
   TConstParamSymbol = class(TByRefParamSymbol)
   protected
     function GetDescription: string; override;
   public
   end;

   // var parameter: procedure P(var x: Integer)
   TVarParamSymbol = class(TByRefParamSymbol)
   protected
     function GetDescription: string; override;
   public
   end;

   // variable with functions for read/write: var x: integer; extern 'type' in 'selector';
   TExternalVarSymbol = class(TValueSymbol)
   private
     FReadFunc: TFuncSymbol;
     FWriteFunc: TFuncSymbol;
   protected
     function GetReadFunc: TFuncSymbol; virtual;
     function GetWriteFunc: TFuncSymbol; virtual;
   public
     destructor Destroy; override;
     property ReadFunc: TFuncSymbol read GetReadFunc write FReadFunc;
     property WriteFunc: TFuncSymbol read GetWriteFunc write FWriteFunc;
   end;

   // Base class for all types
   TTypeSymbol = class(TSymbol)
     function BaseType: TTypeSymbol; override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
   end;

   TFuncKind = (fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod);

   // Record used for TFuncSymbol.Generate
   TParamRec = record
     IsVarParam: Boolean;
     IsConstParam: Boolean;
     ParamName: string;
     ParamType: string;
     HasDefaultValue: Boolean;
     DefaultValue: TData;
   end;
   TParamArray = array of TParamRec;

   // A script function / procedure: procedure X(param: Integer);
   TFuncSymbol = class(TTypeSymbol)
   protected
     FAddrGenerator: TAddrGeneratorRec;
     FExecutable: IExecutable;
     FInternalParams: TSymbolTable;
     FIsForwarded: Boolean;
     FDeprecatedMessage : String;
     FIsDeprecated: Boolean;
     FIsStateless : Boolean;
     FKind: TFuncKind;
     FParams: TParamsSymbolTable;
     FResult: TDataSymbol;
     procedure SetType(const Value: TSymbol); virtual;
     function GetCaption: string; override;
     function GetDescription: string; override;
     function GetLevel: Integer;
     function GetParamSize: Integer;
     function GetIsDeprecated : Boolean;
     procedure SetIsDeprecated(const val : Boolean);
   public
     constructor Create(const Name: string; FuncKind: TFuncKind; FuncLevel: Integer);
     destructor Destroy; override;

     constructor Generate(Table: TSymbolTable; const FuncName: string;
                          const FuncParams: TParamArray; const FuncType: string);
     function IsCompatible(typSym: TSymbol): Boolean; override;
     procedure AddParam(param: TParamSymbol); virtual;
     procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
     procedure Initialize; override;
     procedure InitData(const Data: TData; Offset: Integer); override;

     function ParamsDescription : String;

     property Executable: IExecutable read FExecutable write FExecutable;
     property DeprecatedMessage : String read FDeprecatedMessage write FDeprecatedMessage;
     property IsDeprecated : Boolean read GetIsDeprecated write SetIsDeprecated;
     property IsStateless : Boolean read FIsStateless write FIsStateless;
     property IsForwarded: Boolean read FIsForwarded write FIsForwarded;
     property Kind: TFuncKind read FKind write FKind;
     property Level: Integer read GetLevel;
     property Params: TParamsSymbolTable read FParams;
     property ParamSize: Integer read GetParamSize;
     property Result: TDataSymbol read FResult;
     property Typ: TSymbol read FTyp write SetType;
     property InternalParams: TSymbolTable read FInternalParams;
   end;

   TSourceFuncSymbol = class(TFuncSymbol)
   end;

   TMagicFuncDoEvalEvent = function(args : TExprBaseList) : Variant of object;
   TMagicProcedureDoEvalEvent = procedure(args : TExprBaseList) of object;
   TMagicFuncDoEvalAsIntegerEvent = function(args : TExprBaseList) : Int64 of object;
   TMagicFuncDoEvalAsBooleanEvent = function(args : TExprBaseList) : Boolean of object;
   TMagicFuncDoEvalAsFloatEvent = procedure(args : TExprBaseList; var Result : Double) of object;
   TMagicFuncDoEvalAsStringEvent = procedure(args : TExprBaseList; var Result : String) of object;

   // TMagicFuncSymbol
   //
   TMagicFuncSymbol = class(TFuncSymbol)
      private
         FInternalFunction : TObject;
      public
         destructor Destroy; override;
         procedure Initialize; override;
         property InternalFunction : TObject read FInternalFunction write FInternalFunction;
   end;

   TMethodKind = ( mkProcedure, mkFunction, mkConstructor, mkDestructor, mkMethod,
                   mkClassProcedure, mkClassFunction, mkClassMethod );
   TMethodAttribute = (maVirtual, maOverride, maReintroduce, maAbstract);
   TMethodAttributes = set of TMethodAttribute;

   // A method of a script class: TMyClass = class procedure X(param: String); end;
   TMethodSymbol = class(TFuncSymbol)
   private
     FClassSymbol: TClassSymbol;
     FIsAbstract: Boolean;
     FIsVirtual: Boolean;
     FIsOverride: Boolean;
     FIsOverlap: Boolean;
     FParentMeth: TMethodSymbol;
     FSelfSym: TDataSymbol;
   protected
     function GetIsClassMethod: Boolean;
   public
     constructor Create(const Name: string; FuncKind: TFuncKind; ClassSym: TSymbol;
       FuncLevel: Integer = 1); virtual;
     constructor Generate(Table: TSymbolTable; MethKind: TMethodKind;
                          Attributes: TMethodAttributes; const MethName: string;
                          const MethParams: TParamArray;
                          const MethType: string; Cls: TClassSymbol);
     procedure SetOverride(meth: TMethodSymbol);
     procedure SetOverlap(meth: TMethodSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
     property ClassSymbol: TClassSymbol read FClassSymbol;
     property IsAbstract: Boolean read FIsAbstract write FIsAbstract;
     property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
     property IsOverride: Boolean read FIsOverride;
     property IsOverlap: Boolean read FIsOverlap;
     property IsClassMethod: Boolean read GetIsClassMethod;
     property ParentMeth: TMethodSymbol read FParentMeth;
     property SelfSym: TDataSymbol read FSelfSym write FSelfSym;
   end;

   TSourceMethodSymbol = class(TMethodSymbol)
   end;

   TNameSymbol = class(TTypeSymbol)
   end;

   // type x = TMyType;
   TAliasSymbol = class(TNameSymbol)
   public
     constructor Create(const Name: string; Typ: TTypeSymbol);
     function BaseType: TTypeSymbol; override;
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
   end;

   // integer/string/float/boolean/variant
   TBaseSymbol = class(TNameSymbol)
   protected
     FDefault: Variant;
     FID : TBaseTypeID;
   public
     constructor Create(const Name: string; Id: TBaseTypeID; const Default: Variant);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
     function BaseTypeID : TBaseTypeID; override;
     property ID : TBaseTypeID read FID;
   end;

   IConnectorType = interface;

   IConnector = interface
     ['{8D534D1A-4C6B-11D5-8DCB-0000216D9E86}']
     function ConnectorCaption: string;
     function ConnectorName: string;
     function GetUnit(const UnitName: string): IConnectorType;
   end;

   TConnectorArgs = array of TData;

   IConnectorCall = interface
     ['{8D534D1B-4C6B-11D5-8DCB-0000216D9E86}']
     function Call(const Base: Variant; Args: TConnectorArgs): TData;
   end;

   IConnectorMember = interface
     ['{8D534D1C-4C6B-11D5-8DCB-0000216D9E86}']
     function Read(const Base: Variant): TData;
     procedure Write(const Base: Variant; const Data: TData);
   end;

   TConnectorParam = record
     IsVarParam: Boolean;
     TypSym: TSymbol;
   end;

   TConnectorParamArray = array of TConnectorParam;

   IConnectorType = interface
     ['{8D534D1D-4C6B-11D5-8DCB-0000216D9E86}']
     function ConnectorCaption: string;
     function HasMethod(const MethodName: string; const Params: TConnectorParamArray; var TypSym:
       TSymbol): IConnectorCall;
     function HasMember(const MemberName: string; var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
     function HasIndex(const PropName: string; const Params: TConnectorParamArray; var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
   end;

   TConnectorSymbol = class(TBaseSymbol)
   private
     FConnectorType: IConnectorType;
   public
     constructor Create(const Name: string; ConnectorType: IConnectorType);
     procedure InitData(const Data: TData; Offset: Integer); override;
     property ConnectorType: IConnectorType read FConnectorType write
       FConnectorType;
   end;

   TArraySymbol = class(TTypeSymbol)
   end;

   // array of FTyp
   TDynamicArraySymbol = class(TArraySymbol)
   protected
     function GetCaption: string; override;
   public
     constructor Create(const Name: string; Typ: TSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(TypSym: TSymbol): Boolean; override;
   end;

   // array [FLowBound..FHighBound] of FTyp
   TStaticArraySymbol = class(TArraySymbol)
   private
     FHighBound: Integer;
     FLowBound: Integer;
     FElementCount: Integer;
   protected
     function GetCaption: string; override;
   public
     constructor Create(const Name: string; Typ: TSymbol; LowBound, HighBound: Integer);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(TypSym: TSymbol): Boolean; override;
     procedure AddElement;
     property HighBound: Integer read FHighBound;
     property LowBound: Integer read FLowBound;
     property ElementCount: Integer read FElementCount;
   end;

   // static array whose bounds are contextual
   TOpenArraySymbol = class (TStaticArraySymbol)
     constructor Create(const Name: string; Typ: TSymbol);
     function IsCompatible(TypSym: TSymbol): Boolean; override;
   end;

   // Member of a record
   TMemberSymbol = class(TValueSymbol)
   protected
     FRecordSymbol: TRecordSymbol;
     FOffset: Integer;
   public
     procedure InitData(const Data: TData; Offset: Integer); override;
     property Offset: Integer read FOffset write FOffset;
     property RecordSymbol: TRecordSymbol read FRecordSymbol write FRecordSymbol;
   end;

   // record member1: Integer; member2: Integer end;
   TRecordSymbol = class(TTypeSymbol)
   private
   protected
     FMembers: TSymbolTable;
     function GetCaption: string; override;
     function GetDescription: string; override;
   public
     constructor Create(const Name: string);
     destructor Destroy; override;
     procedure AddMember(Member: TMemberSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
     property Members: TSymbolTable read FMembers;
   end;

   // Field of a script object
   TFieldSymbol = class(TValueSymbol)
   protected
     FClassSymbol: TClassSymbol;
     FOffset: Integer;
   public
     property Offset: Integer read FOffset;
     property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
   end;

   // property X: Integer read FReadSym write FWriteSym;
   TPropertySymbol = class(TValueSymbol)
   private
     FClassSymbol: TClassSymbol;
     FReadSym: TSymbol;
     FWriteSym: TSymbol;
     FArrayIndices: TSymbolTable;
     FIndexSym: TSymbol;
     FIndexValue: TData;
   protected
     function GetCaption: string; override;
     function GetDescription: string; override;
     function GetReadSym: TSymbol; virtual;
     function GetWriteSym: TSymbol; virtual;
     function GetIsDefault: Boolean; virtual;
     procedure AddParam(Param: TParamSymbol);
   public
     constructor Create(const Name: string; Typ: TSymbol);
     destructor Destroy; override;
     procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
     procedure SetIndex(const Data: TData; Addr: Integer; Sym: TSymbol);
     function GetArrayIndicesDescription: string;
     property ArrayIndices: TSymbolTable read FArrayIndices;
     property ReadSym: TSymbol read GetReadSym write FReadSym;
     property WriteSym: TSymbol read GetWriteSym write FWriteSym;
     property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
     property IsDefault: Boolean read GetIsDefault;
     property IndexValue: TData read FIndexValue;
     property IndexSym: TSymbol read FIndexSym;
   end;

   // type X = class of TMyClass;
   TClassOfSymbol = class(TTypeSymbol)
   protected
     function GetCaption: string; override;
   public
     constructor Create(const Name: string; Typ: TClassSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
   end;

   TObjectDestroyEvent = procedure(ExternalObject: TObject) of object;

   // type X = class ... end;
   TClassSymbol = class(TTypeSymbol)
   private
     FClassOfSymbol: TClassOfSymbol;
     FIsAbstract: Boolean;
     FIsForward: Boolean;
     FMembers: TSymbolTable;
     FInstanceSize: Integer;
     FOnObjectDestroy: TObjectDestroyEvent;
     FParent: TClassSymbol;
     FDefaultProperty: TPropertySymbol;
   protected
     function CreateMembersTable: TSymbolTable; virtual;
     function GetDescription: string; override;
   public
     constructor Create(const Name: string);
     destructor Destroy; override;
     procedure AddField(Sym: TFieldSymbol);
     procedure AddMethod(Sym: TMethodSymbol);
     procedure AddProperty(Sym: TPropertySymbol);
     procedure InheritFrom(Typ: TClassSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     procedure Initialize; override;
     function IsCompatible(typSym: TSymbol): Boolean; override;
     function InstanceSize : Integer; // avoids warning
     property ClassOf: TClassOfSymbol read FClassOfSymbol;
     property IsAbstract: Boolean read FIsAbstract write FIsAbstract;
     property IsForward: Boolean read FIsForward write FIsForward;
     property Members: TSymbolTable read FMembers;
     property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
     property Parent: TClassSymbol read FParent;
     property DefaultProperty: TPropertySymbol read FDefaultProperty write FDefaultProperty;
   end;

   // nil "class"
   TNilSymbol = class(TTypeSymbol)
   protected
     function GetCaption: string; override;
   public
     constructor Create;
     function IsCompatible(typSym: TSymbol): Boolean; override;
   end;

   // Invisible symbol for units (e. g. for TdwsUnit)
   TUnitSymbol = class sealed (TNameSymbol)
   private
     FIsTableOwner: Boolean;
     FTable: TSymbolTable;
     FInitialized: Boolean;
   public
     constructor Create(const Name: string; Table: TSymbolTable; IsTableOwner: Boolean = False);
     destructor Destroy; override;
     procedure Initialize; override;
     property Table: TSymbolTable read FTable write FTable;
   end;

   // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
   TElementSymbol = class(TConstSymbol)
   private
     FIsUserDef: Boolean;
     FUserDefValue: Integer;
   protected
     function GetDescription: string; override;
   public
     constructor Create(const Name: string; Typ: TSymbol; Value: Integer; IsUserDef: Boolean);
     property IsUserDef: Boolean read FIsUserDef;
     property UserDefValue: Integer read FUserDefValue;
   end;

   // Enumeration type. E. g. "type myEnum = (One, Two, Three);"
   TEnumerationSymbol = class(TNameSymbol)
   private
     FElements: TSymbolTable;
   protected
     function GetCaption: string; override;
     function GetDescription: string; override;
   public
     constructor Create(const Name: string; BaseType: TTypeSymbol);
     destructor Destroy; override;
     procedure AddElement(Element: TElementSymbol);
     property Elements: TSymbolTable read FElements;
     function ShortDescription : String;
   end;

   IObjectOwner = interface
     procedure ReleaseObject;
   end;

   // A table of symbols connected to other symboltables (property Parents)
   TSymbolTable = class
   private
     FAddrGenerator: TAddrGenerator;
     FSymbols: TTightList;
     FParents: TTightList;
     FSymbolsSorted : Boolean;

     function GetParentCount: Integer;
     function GetParents(Index: Integer): TSymbolTable;

   protected
     function GetSymbol(Index: Integer): TSymbol;
     function GetCount: Integer;

     procedure SortSymbols(minIndex, maxIndex : Integer);
     function FindLocalSorted(const name: string) : TSymbol;
     function FindLocalUnSorted(const name: string) : TSymbol;

   public
     constructor Create(Parent: TSymbolTable = nil; AddrGenerator: TAddrGenerator = nil);
     destructor Destroy; override;

     procedure InsertParent(Index: Integer; Parent: TSymbolTable); virtual;
     function RemoveParent(Parent: TSymbolTable): Integer; virtual;
     function IndexOfParent(Parent: TSymbolTable): Integer;
     procedure MoveParent(CurIndex, NewIndex: Integer);
     procedure ClearParents;
     procedure AddParent(Parent: TSymbolTable);

     function AddSymbol(Sym: TSymbol): Integer;
     function FindLocal(const Name: string): TSymbol; virtual;
     function Remove(Sym: TSymbol): Integer;
     procedure Clear;

     function FindSymbol(const Name: string): TSymbol; virtual;

     procedure Initialize; virtual;

     property AddrGenerator: TAddrGenerator read FAddrGenerator;
     property Count: Integer read GetCount;
     property Symbols[x: Integer]: TSymbol read GetSymbol; default;
     property ParentCount: Integer read GetParentCount;
     property Parents[Index: Integer]: TSymbolTable read GetParents;
   end;

   TSymbolTableClass = class of TSymbolTable;

   // TParamsSymbolTable
   //
   TParamsSymbolTable = class (TSymbolTable)
      private

      public
         function FindLocal(const Name: string): TSymbol; override;
   end;

   // TProgramSymbolTable
   //
   TProgramSymbolTable = class (TSymbolTable)
      private
         FDestructionList: TTightList;

      public
         destructor Destroy; override;

         procedure AddToDestructionList(Sym: TSymbol);
   end;

   // TUnitSymbolTable
   //
   TUnitSymbolTable = class (TSymbolTable)
      private
         FObjects: TInterfaceList;

      public
         destructor Destroy; override;
         procedure BeforeDestruction; override;

         procedure AddObjectOwner(AOwner : IObjectOwner);
   end;

   TStaticSymbolTable = class (TUnitSymbolTable)
   private
     FRefCount: Integer;
     FInitialized: Boolean;
   public
     constructor Create(Parent: TStaticSymbolTable = nil; Reference: Boolean = True);
     destructor Destroy; override;
     procedure Initialize; override;
     procedure InsertParent(Index: Integer; Parent: TSymbolTable); override;
     function RemoveParent(Parent: TSymbolTable): Integer; override;
     procedure _AddRef;
     procedure _Release;
   end;

   TLinkedSymbolTable = class (TUnitSymbolTable)
   private
     FParent: TStaticSymbolTable;
   public
     constructor Create(Parent: TStaticSymbolTable; AddrGenerator: TAddrGenerator = nil);
     destructor Destroy; override;
     function FindLocal(const Name: string): TSymbol; override;
     function FindSymbol(const Name: string): TSymbol; override;
     procedure Initialize; override;
     property Parent: TStaticSymbolTable read FParent;
   end;

   IScriptObj = interface
     ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
     function GetClassSym: TClassSymbol;
     function GetData: TData;
     procedure SetData(const Dat: TData);
     function GetExternalObject: TObject;
     procedure SetExternalObject(Value: TObject);
     property ClassSym: TClassSymbol read GetClassSym;
     property Data: TData read GetData write SetData;
     property ExternalObject: TObject read GetExternalObject write SetExternalObject;

     function DataOfAddr(addr : Integer) : Variant;
     function DataOfAddrAsString(addr : Integer) : String;
     function DataOfAddrAsInteger(addr : Integer) : Int64;
     procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
   end;


   // Is thrown by "raise" statements in script code
   EScriptException = class(Exception)
      private
         FTyp: TSymbol;
         FValue: Variant;
         FPos: TScriptPos;
      public
         constructor Create(const Message: string; const ExceptionObj: IScriptObj; const Pos: TScriptPos); overload;
         constructor Create(const Message: string; const Value: Variant; Typ: TSymbol; const Pos: TScriptPos); overload;

         property ExceptionObj: Variant read FValue;
         property Value: Variant read FValue;
         property Typ: TSymbol read FTyp;
         property Pos: TScriptPos read FPos;
   end;

function IsBaseTypeCompatible(AType, BType: TBaseTypeID): Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TExprBaseListRec ------------------
// ------------------

// Destroy
//
procedure TExprBaseListRec.Clean;
begin
   FList.Clean;
end;

// Add
//
function TExprBaseListRec.Add(expr : TExprBase) : Integer;
begin
   Result:=FList.Add(expr);
end;

// Insert
//
procedure TExprBaseListRec.Insert(index : Integer; expr : TExprBase);
begin
   FList.Insert(index, expr);
end;

// Delete
//
procedure TExprBaseListRec.Delete(index : Integer);
begin
   FList.Delete(index);
end;

// GetExprBase
//
function TExprBaseListRec.GetExprBase(const x: Integer): TExprBase;
begin
   Result:=TExprBase(FList.List[x]);
end;

// SetExprBase
//
procedure TExprBaseListRec.SetExprBase(const x : Integer; expr : TExprBase);
begin
   FList.List[x]:=expr;
end;

// GetAsInteger
//
function TExprBaseListRec.GetAsInteger(const x : Integer) : Int64;
begin
   Result:=TExprBase(FList.List[x]).EvalAsInteger;
end;

// SetAsInteger
//
procedure TExprBaseListRec.SetAsInteger(const x : Integer; const value : Int64);
begin
   TExprBase(FList.List[x]).AssignValueAsInteger(value);
end;

// GetAsBoolean
//
function TExprBaseListRec.GetAsBoolean(const x : Integer) : Boolean;
begin
   Result:=TExprBase(FList.List[x]).EvalAsBoolean;
end;

// GetAsFloat
//
function TExprBaseListRec.GetAsFloat(const x : Integer) : Double;
begin
   TExprBase(FList.List[x]).EvalAsFloat(Result);
end;

// GetAsString
//
function TExprBaseListRec.GetAsString(const x : Integer) : String;
begin
   TExprBase(FList.List[x]).EvalAsString(Result);
end;

// GetAsDataString
//
function TExprBaseListRec.GetAsDataString(const x : Integer) : RawByteString;
begin
   Result:=RawByteString(GetAsString(x));
end;

{ TTightList }

// Clean
//
procedure TTightList.Clean;
var
   i : Integer;
begin
   case Count of
      0 : Exit;
      1 : TObject(FList).Free;
   else
      for i:=Count-1 downto 0 do
         TObject(FList[i]).Free;
   end;
   Clear;
end;

// Clear
//
procedure TTightList.Clear;
begin
   case Count of
      0 : Exit;
      1 : ;
   else
      FreeMem(FList);
   end;
   FList:=nil;
   FCount:=0;
end;

// GetList
//
function TTightList.GetList : PPointerList;
begin
   if Count=1 then
      Result:=@FList
   else Result:=FList;
end;

// Add
//
function TTightList.Add(item : Pointer) : Integer;
var
   buf : Pointer;
begin
   case Count of
      0 : begin
         FList:=item;
         FCount:=1;
      end;
      1 : begin
         buf:=FList;
         FList:=AllocMem(2*SizeOf(Pointer));
         FList[0]:=buf;
         FList[1]:=item;
         FCount:=2;
      end;
   else
      Inc(FCount);
      ReallocMem(FList, Count*SizeOf(Pointer));
      FList[Count-1]:=item;
   end;
   Result:=FCount-1;
end;

// Assign
//
procedure TTightList.Assign(const aList : TTightList);
begin
   Clear;
   FCount:=aList.FCount;
   case Count of
      1 : FList:=aList.FList;
   else
      ReallocMem(FList, Count*SizeOf(Pointer));
      System.Move(aList.FList^, FList^, Count*SizeOf(Pointer));
   end;
end;

// IndexOf
//
function TTightList.IndexOf(item : Pointer) : Integer;
begin
   case Count of
      0 : Result:=-1;
      1 : if FList=item then Result:=0 else Result:=-1;
   else
      Result:=0;
      while Result<FCount do begin
         if FList[Result]=item then Exit;
         Inc(Result);
      end;
      Result:=-1;
   end;
end;

// Remove
//
function TTightList.Remove(item : Pointer) : Integer;
begin
   Result:=IndexOf(item);
   if Result>=0 then
      Delete(Result);
end;

// Delete
//
procedure TTightList.Delete(index : Integer);
var
   i : Integer;
   buf : Pointer;
begin
   if Cardinal(index)>=Cardinal(Count) then
      RaiseIndexOutOfBounds
   else begin
      case Count of
         1 : begin
            FList:=nil;
            FCount:=0;
         end;
         2 : begin
            buf:=FList;
            if index=0 then
               FList:=FList[1]
            else FList:=FList[0];
            FreeMem(buf);
            FCount:=1;
         end;
      else
         for i:=index+1 to Count-1 do
            FList[i-1]:=FList[i];
         Dec(FCount);
      end;
   end;
end;

// Insert
//
procedure TTightList.Insert(index : Integer; item : Pointer);
var
   i : Integer;
begin
   if Cardinal(index)>Cardinal(FCount) then
      RaiseIndexOutOfBounds
   else case Count of
      0 : begin
         FList:=item;
         FCount:=1;
      end;
      1 : begin
         if index=1 then
            Add(item)
         else begin
            Add(FList);
            FList[0]:=item;
         end;
      end;
   else
      ReallocMem(FList, (FCount+1)*SizeOf(Pointer));
      for i:=Count-1 downto index do
         FList[i+1]:=FList[i];
      Inc(FCount);
      FList[index]:=item;
   end;
end;

// Move
//
procedure TTightList.Move(curIndex, newIndex : Integer);
var
   item : Pointer;
begin
   if (Cardinal(curIndex)>=Cardinal(FCount)) or (Cardinal(newIndex)>=Cardinal(FCount)) then
      RaiseIndexOutOfBounds
   else if curIndex<>newIndex then begin
      item:=FList[curIndex];
      Delete(curIndex);
      Insert(newIndex, item);
   end;
end;

// Exchange
//
procedure TTightList.Exchange(index1, index2 : Integer);
var
   item : Pointer;
begin
   if index1<>index2 then begin
      item:=FList[index1];
      FList[index1]:=FList[index2];
      FList[index2]:=item;
   end;
end;

// RaiseIndexOutOfBounds
//
procedure TTightList.RaiseIndexOutOfBounds;
begin
   raise Exception.Create('List index out of bounds');
end;

{ TSymbol }

constructor TSymbol.Create(const Name: string; Typ: TSymbol);
begin
//  FName := Name;
  UnifyCopyString(Name, FName);
  FTyp := Typ;
  if Assigned(FTyp) then
    FSize := FTyp.FSize
  else
    FSize := 0;
end;

{ TVarSymbol }

function TSymbol.GetCaption: string;
begin
  Result := FName;
end;

function TSymbol.GetDescription: string;
begin
  Result := Caption;
end;

procedure TSymbol.InitData(const Data: TData; Offset: Integer);
begin
end;

procedure TSymbol.Initialize;
begin
end;

function TSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := False;
end;

// BaseTypeID
//
function TSymbol.BaseTypeID : TBaseTypeID;
begin
   if Typ<>nil then
      Result:=Typ.BaseTypeID
   else if (BaseType<>nil) and (BaseType<>Self) then
      Result:=BaseType.BaseTypeID
   else Result:=typNoneID;
end;

// IsBaseTypeIDValue
//
function TSymbol.IsBaseTypeIDValue(aBaseTypeID : TBaseTypeID) : Boolean;
begin
   Result:=Assigned(Self) and (FSize<=1) and (BaseTypeID=aBaseTypeID);
end;

// IsBaseTypeIDArray
//
function TSymbol.IsBaseTypeIDArray(aBaseTypeID : TBaseTypeID) : Boolean;
begin
   Result:=(FSize>1) and (BaseTypeID=aBaseTypeID);
end;

function TSymbol.BaseType: TTypeSymbol;
begin
  Result := nil;
end;

// SetName
//
procedure TSymbol.SetName(const newName : String);
begin
   Assert(FName='');
   FName:=newName;
end;

{ TRecordSymbol }

constructor TRecordSymbol.Create;
begin
  inherited Create(Name, nil);
  FMembers := TSymbolTable.Create(nil);
end;

destructor TRecordSymbol.Destroy;
begin
  FMembers.Free;
  inherited;
end;

procedure TRecordSymbol.AddMember(Member: TMemberSymbol);
begin
  Member.RecordSymbol := Self;
  Member.Offset := FSize;
  FSize := FSize + Member.Typ.Size;
  FMembers.AddSymbol(Member);
end;

procedure TRecordSymbol.InitData(const Data: TData; Offset: Integer);
var
  x: Integer;
begin
  for x := 0 to FMembers.Count - 1 do
    FMembers[x].InitData(Data, Offset + TMemberSymbol(FMembers[x]).Offset);
end;

function TRecordSymbol.IsCompatible(typSym: TSymbol): Boolean;
var
  x: Integer;
begin
  typSym := typSym.BaseType;
  Result := (typSym is TRecordSymbol) and (FMembers.Count =
    TRecordSymbol(typSym).FMembers.Count);

  x := 0;
  while Result and (x < FMembers.Count) do
  begin
    Result := FMembers[x].Typ.IsCompatible(TRecordSymbol(TypSym).FMembers[x].Typ);
    Inc(x);
  end;
end;

function TRecordSymbol.GetCaption: string;
var
  x: Integer;
begin
  Result := 'record';
  if FMembers.Count > 0 then
  begin
    Result := Result + ' ' + FMembers[0].Typ.Caption;
    for x := 1 to FMembers.Count - 1 do
      Result := Result + ', ' + FMembers[x].Typ.Caption;
  end;
  Result := Result + ' end';
end;

function TRecordSymbol.GetDescription: string;
var
   x: Integer;
begin
   Result:=Name+' = record'#13#10;
   for x:=0 to FMembers.Count-1 do
      Result:=Result+'   '+FMembers[x].Name+' : '+FMembers[x].Typ.Name+';'#13#10;
   Result:=Result+'end;';
end;

{ TFuncSymbol }

constructor TFuncSymbol.Create(const Name: string; FuncKind: TFuncKind;
  FuncLevel: Integer);
begin
  inherited Create(Name, nil);
  FKind := FuncKind;
  FAddrGenerator := TAddrGeneratorRec.CreateNegative(FuncLevel);
  FInternalParams := TSymbolTable.Create(nil, @FAddrGenerator);
  FParams := TParamsSymbolTable.Create(FInternalParams, @FAddrGenerator);
  FSize := 1;
end;

destructor TFuncSymbol.Destroy;
begin
  FParams.Free;
  FInternalParams.Free;
//  FAddrGenerator.Free;
  inherited;
end;

constructor TFuncSymbol.Generate(Table: TSymbolTable; const FuncName: string;
  const FuncParams: TParamArray; const FuncType: string);
var
  typSym: TSymbol;
begin
  if FuncType <> '' then
  begin
    Self.Create(FuncName, fkFunction, 1);
    // Set function type
    typSym := Table.FindSymbol(FuncType);
    if not (Assigned(typSym) and (typSym.BaseType <> nil)) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [FuncType]);
    Self.SetType(typSym);
  end
  else
    Self.Create(FuncName, fkProcedure, 1);

  GenerateParams(Table, FuncParams);
end;

procedure TFuncSymbol.AddParam(param: TParamSymbol);
begin
  Params.AddSymbol(param);
end;

procedure TFuncSymbol.SetType;
begin
  FTyp := Value;
  FResult := TDataSymbol.Create(SYS_RESULT, Value);
  FInternalParams.AddSymbol(FResult);
end;

type TAddParamProc = procedure (param: TParamSymbol) of object;

procedure GenerateParams(const Name: String; Table: TSymbolTable; FuncParams: TParamArray; AddProc: TAddParamProc);
var
  x: Integer;
  typSym: TSymbol;
  paramSym: TParamSymbol;
begin
  for x := 0 to Length(FuncParams) - 1 do
  begin
    typSym := Table.FindSymbol(FuncParams[x].ParamType);
    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeForParamNotFound, [FuncParams[x].ParamType,
        FuncParams[x].ParamName, Name]);

    if FuncParams[x].HasDefaultValue then begin

       if FuncParams[x].IsVarParam then
          raise Exception.Create(CPE_VarParamCantHaveDefaultValue);
       if FuncParams[x].IsConstParam then
          raise Exception.Create(CPE_ConstParamCantHaveDefaultValue);

       paramSym := TParamSymbolWithDefaultValue.Create(FuncParams[x].ParamName, typSym);
       TParamSymbolWithDefaultValue(paramSym).SetDefaultValue(FuncParams[x].DefaultValue,0);

    end else begin

       if FuncParams[x].IsVarParam then
          paramSym := TVarParamSymbol.Create(FuncParams[x].ParamName, typSym)
       else if FuncParams[x].IsConstParam then
          paramSym := TConstParamSymbol.Create(FuncParams[x].ParamName, typSym)
       else
          paramSym := TParamSymbol.Create(FuncParams[x].ParamName, typSym);
          
    end;

    AddProc(paramSym);
  end;
end;

procedure TFuncSymbol.GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
begin
  dwsSymbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

function TFuncSymbol.GetCaption: string;
var
  i: Integer;
  nam : String;
begin
  if Name <> '' then
    nam := Name
  else
    case Kind of
      fkFunction    : nam := 'function ';
      fkProcedure   : nam := 'procedure ';
      fkConstructor : nam := 'constructor ';
      fkDestructor  : nam := 'destructor ';
      fkMethod      : nam := 'method ';
    end;

  if Params.Count > 0 then
  begin
    Result := Params[0].Typ.Caption;
    for i := 1 to Params.Count - 1 do
      Result := Result + ', ' + Params[i].Typ.Caption;
    Result := '(' + Result + ')';
  end
  else
    Result := '';

  if Typ <> nil then
    Result := nam + Result + ': ' + Typ.Name
  else
    Result := nam + Result;
end;

function TFuncSymbol.GetDescription: string;
begin
   Result:=ParamsDescription;
   case FKind of
      fkFunction : begin
         Result:='function '+Name+Result+': ';
         if Typ <> nil then
            Result:=Result+Typ.Name
        else Result:=Result+'???';
      end;
      fkProcedure :
         Result:='procedure '+Name+Result;
      fkConstructor :
         Result:='constructor '+Name+Result;
      fkDestructor :
         Result:='destructor '+Name+Result;
      fkMethod : begin
         Result:='method '+Name+Result;
         if Typ<>nil then
            Result:=Result+': '+Typ.Name;
      end;
   else
      Assert(False)
   end;
end;

procedure TFuncSymbol.Initialize;
begin
  inherited;
  FInternalParams.Initialize;
  if Assigned(FExecutable) then
    FExecutable.InitSymbol(Self)
  else if Level >= 0 then
      raise Exception.CreateFmt(CPE_ForwardNotImplemented, [Name]);
end;

function TFuncSymbol.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

function TFuncSymbol.GetParamSize: Integer;
begin
  Result := FAddrGenerator.DataSize;
end;

// GetIsDeprecated
//
function TFuncSymbol.GetIsDeprecated : Boolean;
begin
   Result:=(FDeprecatedMessage<>'');
end;

// SetIsDeprecated
//
procedure TFuncSymbol.SetIsDeprecated(const val : Boolean);
begin
   if val then
      FDeprecatedMessage:='!'
   else FDeprecatedMessage:='';
end;

function TFuncSymbol.IsCompatible(typSym: TSymbol): Boolean;
var
  funcSym : TFuncSymbol;
begin
  typSym := typSym.BaseType;
  if typSym is TNilSymbol then
    Result := True
  else if Size <> typSym.Size then
    Result := False
  else begin
    Result := False;
    if not (typSym is TFuncSymbol) then
      Exit;
    funcSym := TFuncSymbol(typSym);
    if (Kind <> funcSym.Kind) or (Params.Count <> funcSym.Params.Count) then
      Exit;
    // TODO : Compare Params
    Result := True;
  end;
end;

procedure TFuncSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := nilIntf;
end;

// ParamsDescription
//
function TFuncSymbol.ParamsDescription : String;
var
   i : Integer;
begin
   if Params.Count>0 then begin
      Result:=Params.Symbols[0].Description;
      for i:=1 to Params.Count-1 do
         Result:=Result+'; '+Params.Symbols[i].Description;
      Result:='('+Result+')';
  end else Result:='()';
end;

// ------------------
// ------------------ TMagicFuncSymbol ------------------
// ------------------

procedure TMagicFuncSymbol.Initialize;
begin
   FInternalParams.Initialize;
end;

// Destroy
//
destructor TMagicFuncSymbol.Destroy;
begin
   FreeAndNil(FInternalFunction);
   inherited;
end;

{ TMethodSymbol }

constructor TMethodSymbol.Create(const Name: string; FuncKind: TFuncKind;
  ClassSym: TSymbol; FuncLevel: Integer);
begin
  inherited Create(Name, FuncKind, FuncLevel);
  if ClassSym is TClassSymbol then
  begin
    // Method
    FClassSymbol := TClassSymbol(ClassSym);
    FSelfSym := TDataSymbol.Create(SYS_SELF, ClassSym);
    FInternalParams.AddSymbol(FSelfSym);
    FSize := 2; // code + data
  end
  else
    // Class function -> self is "class of"
    FClassSymbol := TClassSymbol(ClassSym.Typ);
  FParams.AddParent(FClassSymbol.Members);
end;

constructor TMethodSymbol.Generate(Table: TSymbolTable; MethKind: TMethodKind;
  Attributes: TMethodAttributes; const MethName: string; const MethParams: TParamArray;
  const MethType: string; Cls: TClassSymbol);
var
  typSym: TSymbol;
  meth: TSymbol;
begin
  // Check if name is already used
  meth := Cls.Members.FindSymbol(MethName);
  if meth is TFieldSymbol then
    raise Exception.CreateFmt(CPE_FieldExists, [MethName])
  else if meth is TPropertySymbol then
    raise Exception.CreateFmt(CPE_PropertyExists, [MethName])
  else if meth is TMethodSymbol then
  begin
    if TMethodSymbol(meth).ClassSymbol = Cls then
      raise Exception.CreateFmt(CPE_MethodExists, [MethName]);
  end;

  // Initialize MethodSymbol
  case MethKind of
    mkConstructor:
      Create(MethName, fkConstructor, Cls);
    mkDestructor:
      Create(MethName, fkDestructor, Cls);
    mkProcedure:
      Create(MethName, fkProcedure, Cls);
    mkFunction:
      Create(MethName, fkFunction, Cls);
    mkMethod :
      Create(MethName, fkMethod, Cls);
    mkClassProcedure:
      Create(MethName, fkProcedure, Cls.ClassOf);
    mkClassFunction:
      Create(MethName, fkFunction, Cls.ClassOf);
    mkClassMethod:
      Create(MethName, fkMethod, Cls.ClassOf);
  end;

  // Set Resulttype
  if MethType <> '' then
  begin
    if not (Kind in [fkFunction, fkMethod]) then
      raise Exception.Create(CPE_NoResultTypeRequired);

    typSym := Table.FindSymbol(MethType);
    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [MethType]);
    SetType(typSym);
  end;

  if (Kind = fkFunction) and (MethType = '') then
    raise Exception.Create(CPE_ResultTypeExpected);

  GenerateParams(Table, MethParams);

  if Assigned(meth) then
    SetOverlap(TMethodSymbol(meth));

  if Attributes = [maVirtual] then
    FIsVirtual := True
  else if Attributes = [maVirtual, maAbstract] then
  begin
    FIsVirtual := True;
    FIsAbstract := True;
  end
  else if Attributes = [maOverride] then
  begin
    if FIsOverlap then
      SetOverride(TMethodSymbol(meth))
    else
      raise Exception.CreateFmt(CPE_CanNotOverride, [Name]);
  end
  else if Attributes = [maReintroduce] then
  else if Attributes = [] then
  else
    raise Exception.Create(CPE_InvalidArgCombination);
end;

function TMethodSymbol.GetIsClassMethod: Boolean;
begin
  Result := not Assigned(FSelfSym);
end;

procedure TMethodSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  inherited;
  if Size = 2 then
    Data[Offset + 1] := nilIntf;
end;

function TMethodSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := inherited IsCompatible(typSym);
end;

procedure TMethodSymbol.SetOverlap(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  FIsOverride := False;
  FIsOverlap := True;
end;

procedure TMethodSymbol.SetOverride(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  FIsOverride := True;
  FIsVirtual := True;
  FIsOverlap := False;
end;

{ TPropertySymbol }

procedure TPropertySymbol.AddParam(Param: TParamSymbol);
begin
  ArrayIndices.AddSymbol(Param);
end;

constructor TPropertySymbol.Create(const Name: string; Typ: TSymbol);
begin
  inherited;
  FArrayIndices := TSymbolTable.Create;
  FIndexValue := nil;
end;

destructor TPropertySymbol.Destroy;
begin
  FArrayIndices.Free;
  inherited;
end;

procedure TPropertySymbol.GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
begin
  dwsSymbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

function TPropertySymbol.GetCaption: string;
begin
  Result := GetDescription;
end;

function TPropertySymbol.GetArrayIndicesDescription: string;
var
   i, j : Integer;
   sym, nextSym : TSymbol;
begin
   if ArrayIndices.Count=0 then
      Result:=''
   else begin
      Result:='[';
      i:=0;
      while i<ArrayIndices.Count do begin
         sym:=ArrayIndices[i];
         if i>0 then
            Result:=Result+', ';
         Result:=Result+sym.Name;
         for j:=i+1 to ArrayIndices.Count-1 do begin
            nextSym:=ArrayIndices[j];
            if nextSym.Typ<>sym.Typ then Break;
            Result:=Result+', '+nextSym.Name;
            i:=j;
         end;
         Result:=Result+': '+sym.Typ.Name;
         Inc(i);
      end;
      Result:=Result+']';
    end;
end;

function TPropertySymbol.GetDescription: string;
begin
   Result := Format('property %s%s: %s', [Name, GetArrayIndicesDescription, Typ.Name]);

   if Assigned(FReadSym) then
      Result := Result + ' read ' + FReadSym.Name;

   if Assigned(FWriteSym) then
      Result := Result + ' write ' + FWriteSym.Name;

   if ClassSymbol.DefaultProperty = Self then
      Result := Result + '; default';
end;

function TPropertySymbol.GetIsDefault: Boolean;
begin
  Result := ClassSymbol.DefaultProperty = Self;
end;

function TPropertySymbol.GetReadSym: TSymbol;
begin
  Result := FReadSym;
end;

function TPropertySymbol.GetWriteSym: TSymbol;
begin
  Result := FWriteSym;
end;

procedure TPropertySymbol.SetIndex(const Data: TData; Addr: Integer; Sym: TSymbol);
begin
  FIndexSym := Sym;
  SetLength(FIndexValue,FIndexSym.Size);
  CopyData(Data, Addr, FIndexValue, 0, FIndexSym.Size);
end;

{ TClassSymbol }

constructor TClassSymbol.Create;
begin
  inherited Create(Name, nil);
  FSize := 1;
  FMembers := CreateMembersTable;
  FClassOfSymbol := TClassOfSymbol.Create('class of ' + Name, Self);
  FMembers.AddSymbol(TAliasSymbol.Create('Self',Self)); // private member ?!
end;

destructor TClassSymbol.Destroy;
begin
  FMembers.Free;
  FClassOfSymbol.Free;
  inherited;
end;

function TClassSymbol.CreateMembersTable: TSymbolTable;
begin
  Result := TSymbolTable.Create(nil);
end;

procedure TClassSymbol.AddField(Sym: TFieldSymbol);
begin
  FMembers.AddSymbol(Sym);
  Sym.FClassSymbol := Self;

  Sym.FOffset := FInstanceSize;
  FInstanceSize := FInstanceSize + Sym.Typ.Size;
end;

procedure TClassSymbol.AddMethod(Sym: TMethodSymbol);
var
  x: Integer;
begin
  FMembers.AddSymbol(Sym);
  sym.FClassSymbol := Self;

  // Check if class is abstract or not
  if Sym.IsAbstract then
    FIsAbstract := True
  else if Sym.IsOverride and Sym.FParentMeth.IsAbstract then
  begin
    FIsAbstract := False;
    for x := 0 to FMembers.Count - 1 do
      if (FMembers[x] is TMethodSymbol) and (TMethodSymbol(FMembers[x]).IsAbstract) then
      begin
        FIsAbstract := True;
        break;
      end;
  end;
end;

procedure TClassSymbol.AddProperty(Sym: TPropertySymbol);
begin
  FMembers.AddSymbol(Sym);
  sym.FClassSymbol := Self;
end;

procedure TClassSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := IUnknown(nilIntf);
end;

procedure TClassSymbol.Initialize;
var
  x: Integer;
  Err: EClassMethodImplIncompleteError;
begin
  // Check validity of the class declaration

  if FIsForward then
    raise Exception.CreateFmt(CPE_ClassNotCompletelyDefined, [Caption]);

  for x := 0 to FMembers.Count - 1 do
    if FMembers[x] is TMethodSymbol then
    begin
      if not TMethodSymbol(FMembers[x]).IsAbstract then
      begin
        if Assigned(TMethodSymbol(FMembers[x]).FExecutable) then
          TMethodSymbol(FMembers[x]).FExecutable.InitSymbol(FMembers[x])
        else
        begin
          Err := EClassMethodImplIncompleteError.CreateFmt(CPE_MethodNotImplemented,
            [FMembers[x].Caption, TMethodSymbol(FMembers[x]).ClassSymbol.Caption]);
          Err.ClassSymObj := Self;
          raise Err;
        end;
      end;
    end;
end;

procedure TClassSymbol.InheritFrom(Typ: TClassSymbol);
begin
  FMembers.AddParent(Typ.Members);
  FInstanceSize := Typ.InstanceSize;
  FParent := Typ;
end;

function TClassSymbol.IsCompatible(typSym: TSymbol): Boolean;
var
  csym: TClassSymbol;
begin
  Result := False;
  typSym := typSym.BaseType;
  if typSym is TNilSymbol then
    Result := True
  else if typSym is TClassSymbol then
  begin
    csym := TClassSymbol(typSym);
    while csym <> nil do
    begin
      if csym = Self then
      begin
        Result := True;
        exit;
      end;
      csym := csym.Parent;
    end;
  end;
end;

function TClassSymbol.GetDescription: string;
var
  i: Integer;
begin
  if FParent <> nil then
    Result := Name + ' = class (' + FParent.Name + ')'#13#10
  else
    Result := Name + ' = class'#13#10;

  for i := 0 to Members.Count - 1 do
    Result := Result + '   ' + Members.Symbols[i].Description + ';'#13#10;

  Result := Result + 'end';
end;

function TClassSymbol.InstanceSize: Integer;
begin
  Result := FInstanceSize;
end;

{ TNilSymbol }

constructor TNilSymbol.Create;
begin
  inherited Create('', nil);
  FSize := 1;
end;

function TNilSymbol.GetCaption: string;
begin
  Result := 'nil';
end;

function TNilSymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  Result := (TypSym is TClassSymbol) or (TypSym is TNilSymbol);
end;

{ TClassOfSymbol }

constructor TClassOfSymbol.Create;
begin
  inherited Create(Name, Typ);
end;

function TClassOfSymbol.GetCaption: string;
begin
  if Typ <> nil then
    Result := 'class of ' + Typ.Name
  else
    Result := 'class of ???';
end;

procedure TClassOfSymbol.InitData(const Data: TData; Offset: Integer);
begin
  Data[Offset] := '';
end;

function TClassOfSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  Result := (typSym is TClassOfSymbol) and Typ.IsCompatible(typSym.Typ);
end;

function IsBaseTypeCompatible(AType, BType: TBaseTypeID): Boolean;
const
{(*}
  compatiblityMask: array[TBaseTypeID, TBaseTypeID] of Boolean =
  (
   //int    flt    str    bool   var    conn   class   none
    (true,  false, false, false, true,  true,  false, false), // int
    (false, true,  false, false, true,  true,  false, false), // flt
    (false, false, true,  false, true,  true,  false, false), // str
    (false, false, false, true,  true,  true,  false, false), // bool
    (true,  true,  true,  true,  true,  true,  false, false), // var
    (true,  true,  true,  true,  true,  true,  false, false), // conn
    (false, false, false, false, false, false, true,  false), // class
    (false, false, false, false, false, false, false, false)  // none
  );
{*)}
begin
  Result := compatiblityMask[AType, BType];
end;

{ TBaseSymbol }

constructor TBaseSymbol.Create(const Name: string; Id: TBaseTypeID; const Default: Variant);
begin
  inherited Create(Name, nil);
  FId := Id;
  FDefault := Default;
  FSize := 1;
end;

procedure TBaseSymbol.InitData(const Data: TData; Offset: Integer);
begin
  VarCopy(Data[Offset], FDefault);
end;

function TBaseSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
   if typSym=nil then
      Exit(False)
   else begin
      typSym:=typSym.BaseType;
      if typSym=nil then
         Exit(False);
      if typSym is TEnumerationSymbol then
         typSym:=TEnumerationSymbol(typSym).Typ.BaseType;
      Result:=    (typSym is TBaseSymbol)
              and IsBaseTypeCompatible(Self.FId, TBaseSymbol(typSym).FId);
   end;
end;

// BaseTypeID
//
function TBaseSymbol.BaseTypeID : TBaseTypeID;
begin
   Result:=ID;
end;

{ TConnectorSymbol }

constructor TConnectorSymbol.Create(const Name: string; ConnectorType: IConnectorType);
begin
  inherited Create(Name, typConnectorID, Null);
  FConnectorType := ConnectorType;
end;

procedure TConnectorSymbol.InitData(const Data: TData; Offset: Integer);
begin
  VarClear(Data[Offset]);
end;

{ TValueSymbol }

function TValueSymbol.GetCaption: string;
begin
  Result := Name + ': ' + FTyp.Caption;
end;

function TValueSymbol.GetDescription: string;
begin
  Result := Name + ': ' + FTyp.Description;
end;

{ TConstSymbol }

constructor TConstSymbol.Create(const Name: string; Typ: TSymbol; const Value: Variant);
begin
  inherited Create(Name, Typ);
  SetLength(FData, 1);
  VarCopy(FData[0], Value);
end;

constructor TConstSymbol.Create(const Name: string; Typ: TSymbol; const Data: TData;
  Addr: Integer);
begin
  inherited Create(Name, Typ);
  SetLength(FData, Typ.Size);
  CopyData(Data, Addr, FData, 0, Typ.Size);
end;

function TConstSymbol.GetCaption: string;
begin
  Result := 'const ' + inherited GetCaption;
end;

function TConstSymbol.GetDescription: string;
begin
  if VarType(FData[0]) = varError then
    Result := 'const ' + inherited GetDescription + ' = [varError]'
  else
    Result := 'const ' + inherited GetDescription + ' = ' + VarToStr(FData[0]);
end;

procedure TConstSymbol.Initialize;
begin
end;

{ TMemberSymbol }

procedure TMemberSymbol.InitData(const Data: TData; Offset: Integer);
begin
  Typ.InitData(Data, Offset);
end;

{ TDataSymbol }

function TDataSymbol.GetDescription: string;
begin
  if Assigned(FTyp) then
    Result := Name + ': ' + FTyp.Name
  else
    Result := Name;
end;

procedure TDataSymbol.InitData(const Data: TData; Offset: Integer);
begin
  Typ.InitData(Data, Offset);
end;

{ TParamSymbol }

function TParamSymbol.GetDescription: string;
begin
  if Typ <> nil then
    Result := Name + ': ' + Typ.Name
  else
    Result := Name + ': ???';
end;

{ TParamSymbol }

function TParamSymbolWithDefaultValue.GetDescription: string;
begin
  Result := inherited GetDescription;

  // Has a default parameter. Format display of param to show it.
  if Length(FDefaultValue) > 0 then
    if VarIsStr(FDefaultValue[0]) then
      Result := Result + ' = ''' + VarToStr(FDefaultValue[0]) + ''''  // put quotes around value
    else if VarIsOrdinal(FDefaultValue[0]) or VarIsFloat(FDefaultValue[0]) then
      Result := Result + ' = ' + VarToStr(FDefaultValue[0]);
end;

procedure TParamSymbolWithDefaultValue.SetDefaultValue(const Data: TData; Addr: Integer);
begin
  SetLength(FDefaultValue, Typ.Size);
  CopyData(Data, Addr, FDefaultValue, 0, Typ.Size);
end;

procedure TParamSymbolWithDefaultValue.SetDefaultValue(const Value: Variant);
begin
  Assert(Typ.Size = 1);
  SetLength(FDefaultValue, 1);
  VarCopy(FDefaultValue[0], Value);
end;

{ TByRefParamSymbol }

constructor TByRefParamSymbol.Create(const Name: string; Typ: TSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

{ TConstParamSymbol }

function TConstParamSymbol.GetDescription: string;
begin
  Result := 'const ' + inherited GetDescription;
end;

{ TVarParamSymbol }

function TVarParamSymbol.GetDescription: string;
begin
  Result := 'var ' + inherited GetDescription;
end;

{ TSymbolTable }

constructor TSymbolTable.Create(Parent: TSymbolTable; AddrGenerator: TAddrGenerator);
begin
  FAddrGenerator := AddrGenerator;
  if Assigned(Parent) then
    AddParent(Parent);
end;

destructor TSymbolTable.Destroy;
begin
   FSymbols.Clean;
   ClearParents;
   FParents.Clear;
   inherited;
end;

procedure TSymbolTable.Initialize;
var
   x: Integer;
begin
   for x := 0 to FSymbols.Count - 1 do
      TSymbol(FSymbols.List[x]).Initialize;
end;

function TSymbolTable.FindLocal(const Name: string): TSymbol;
var
   n : Integer;
begin
   n:=FSymbols.Count;
   if n>6 then begin
      if not FSymbolsSorted then begin
         SortSymbols(0, n-1);
         FSymbolsSorted:=True;
      end;
      Result:=FindLocalSorted(Name);
   end else begin
      Result:=FindLocalUnSorted(Name);
   end;
end;

// SortSymbols
//
procedure TSymbolTable.SortSymbols(minIndex, maxIndex : Integer);
var
  i, j, p : Integer;
  pSym : TSymbol;
  ptrList : PPointerList;
begin
   if (maxIndex<=minIndex) then
      Exit;
   ptrList:=FSymbols.List;
   repeat
      i:=minIndex;
      j:=maxIndex;
      p:=((i+j) shr 1);
      repeat
         pSym:=TSymbol(ptrList[p]);
         while CompareText(TSymbol(ptrList[i]).Name, pSym.Name)<0 do Inc(i);
         while CompareText(TSymbol(ptrList[j]).Name, pSym.Name)>0 do Dec(j);
         if i<=j then begin
            FSymbols.Exchange(i, j);
            if p=i then
               p:=j
            else if p=j then
               p:=i;
            Inc(i);
            Dec(j);
         end;
      until i>j;
      if minIndex<j then
         SortSymbols(minIndex, j);
      minIndex:=i;
   until i>=maxIndex;
end;

// FindLocalSorted
//
function TSymbolTable.FindLocalSorted(const name: string): TSymbol;
var
   lo, hi, mid, cmpResult: Integer;
  ptrList : PPointerList;
begin
   lo:=0;
   hi:=FSymbols.Count-1;
   ptrList:=FSymbols.List;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      cmpResult:=CompareText(TSymbol(ptrList[mid]).Name, name);
      if cmpResult<0 then
         lo:=mid+1
      else begin
         if cmpResult=0 then
            Exit(TSymbol(ptrList[mid]))
         else hi:=mid-1;
      end;
   end;
   Result:=nil;
end;

// FindLocalUnSorted
//
function TSymbolTable.FindLocalUnSorted(const name: string) : TSymbol;
var
   x : Integer;
   ptrList : PPointerList;
begin
   ptrList:=FSymbols.List;
   for x:=FSymbols.Count-1 downto 0 do begin
      if CompareText(TSymbol(ptrList[x]).Name, Name)=0 then
         Exit(TSymbol(ptrList[x]));
   end;
   Result:=nil;
end;


function TSymbolTable.FindSymbol(const Name: string): TSymbol;
var
  x: Integer;
begin
  // Find Symbol in the local List
  Result := FindLocal(Name);
  if Assigned(Result) then
    Exit;

  // Find Symbol in all parent lists
  x := 0;
  while not Assigned(Result) and (x < ParentCount) do
  begin
    Result := Parents[x].FindSymbol(Name);
    Inc(x);
  end;
end;

function TSymbolTable.GetCount: Integer;
begin
   Result := FSymbols.Count
end;

function TSymbolTable.GetSymbol(Index: Integer): TSymbol;
begin
  Result := TSymbol(FSymbols.List[Index])
end;

function TSymbolTable.AddSymbol(Sym: TSymbol): Integer;
begin
   Result := FSymbols.Add(sym);
   if (sym is TDataSymbol) and (FAddrGenerator <> nil) then begin
      TDataSymbol(sym).Level := FAddrGenerator.Level;
      TDataSymbol(sym).StackAddr := FAddrGenerator.GetStackAddr(sym.Size);
   end;
   FSymbolsSorted:=False;
end;

function TSymbolTable.Remove(Sym: TSymbol): Integer;
begin
   Result:=FSymbols.Remove(Sym);
end;

procedure TSymbolTable.Clear;
begin
   FSymbols.Clear;
end;

procedure TSymbolTable.AddParent(Parent: TSymbolTable);
begin
  InsertParent(ParentCount,Parent);
end;

procedure TSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
begin
   FParents.Insert(Index,Parent);
end;

function TSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
   Result := FParents.Remove(Parent);
end;

procedure TSymbolTable.ClearParents;
begin
   while FParents.Count > 0 do
      RemoveParent(FParents.List[0]);
end;

function TSymbolTable.GetParentCount: Integer;
begin
   Result := FParents.Count
end;

function TSymbolTable.GetParents(Index: Integer): TSymbolTable;
begin
  Result := TSymbolTable(FParents.List[Index]);
end;

function TSymbolTable.IndexOfParent(Parent: TSymbolTable): Integer;
begin
   Result := FParents.IndexOf(Parent)
end;

procedure TSymbolTable.MoveParent(CurIndex, NewIndex: Integer);
begin
   FParents.Move(CurIndex,NewIndex);
end;

// ------------------
// ------------------ TParamsSymbolTable ------------------
// ------------------

// FindLocal
//
function TParamsSymbolTable.FindLocal(const Name: string): TSymbol;
begin
   Result:=FindLocalUnSorted(name);
end;

// ------------------
// ------------------ TProgramSymbolTable ------------------
// ------------------

// Destroy
//
destructor TProgramSymbolTable.Destroy;
begin
   inherited;
   FDestructionList.Clean;
end;

// AddToDestructionList
//
procedure TProgramSymbolTable.AddToDestructionList(Sym: TSymbol);
begin
   FDestructionList.Add(sym);
end;

// ------------------
// ------------------ TUnitSymbolTable ------------------
// ------------------

// Destroy
//
destructor TUnitSymbolTable.Destroy;
begin
   inherited;
   FObjects.Free;
end;

// BeforeDestruction
//
procedure TUnitSymbolTable.BeforeDestruction;
var
  O : IObjectOwner;
begin
  if Assigned(FObjects) then begin
    while FObjects.Count > 0 do
    begin
      O := IObjectOwner(FObjects[0]);
      FObjects.Delete(0);
      O.ReleaseObject;
    end;
  end;
  inherited;
end;

// AddObjectOwner
//
procedure TUnitSymbolTable.AddObjectOwner(AOwner : IObjectOwner);
begin
   if not Assigned(FObjects) then
      FObjects := TInterfaceList.Create;
   FObjects.Add(AOwner);
end;

{ TExternalVarSymbol }

destructor TExternalVarSymbol.Destroy;
begin
  FReadFunc.Free;
  FWriteFunc.Free;
  inherited;
end;

function TExternalVarSymbol.GetReadFunc: TFuncSymbol;
begin
  Result := FReadFunc;
end;

function TExternalVarSymbol.GetWriteFunc: TFuncSymbol;
begin
  Result := FWriteFunc;
end;

{ TUnitSymbol }

constructor TUnitSymbol.Create(const Name: string; Table: TSymbolTable;
  IsTableOwner: Boolean = False);
begin
  inherited Create(Name, nil);
  FIsTableOwner := IsTableOwner;
  FTable := Table;
  FInitialized := False;
end;

destructor TUnitSymbol.Destroy;
begin
  if FIsTableOwner then
    FTable.Free;
  inherited;
end;

procedure TUnitSymbol.Initialize;
begin
  if not FInitialized then
  begin
    FTable.Initialize;
    FInitialized := True;
  end;
end;

{ TAddrGeneratorRec }

// CreatePositive
//
constructor TAddrGeneratorRec.CreatePositive(aLevel: Integer; anInitialSize: Integer = 0);
begin
   FLevel := aLevel;
   FDataSize := anInitialSize;
   FSign := agsPositive;
end;

// CreateNegative
//
constructor TAddrGeneratorRec.CreateNegative(aLevel: Integer);
begin
   FLevel := aLevel;
   FDataSize := 0;
   FSign := agsNegative;
end;

// GetStackAddr
//
function TAddrGeneratorRec.GetStackAddr(Size: Integer): Integer;
begin
   if FSign=agsPositive then begin
      Result := FDataSize;
      Inc(FDataSize, Size);
   end else begin
      Dec(FDataSize, Size);
      Result := FDataSize;
   end;
end;

// GetDataSize
//
function TAddrGeneratorRec.GetDataSize: Integer;
begin
   if FSign=agsPositive then
      Result:=FDataSize
   else Result:=-FDataSize;
end;

{ TDynamicArraySymbol }

constructor TDynamicArraySymbol.Create(const Name: string; Typ: TSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

function TDynamicArraySymbol.GetCaption: string;
begin
  Result := 'array of ' + FTyp.Caption
end;

procedure TDynamicArraySymbol.InitData(const Data: TData; Offset: Integer);
begin
  Data[Offset] := Null; // ADR
end;

function TDynamicArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  Result :=     (TypSym is TDynamicArraySymbol)
            and (Typ.IsCompatible(TypSym.Typ) or (TypSym.Typ is TNilSymbol));
end;

{ TStaticArraySymbol }

constructor TStaticArraySymbol.Create(const Name: string; Typ: TSymbol; LowBound, HighBound: Integer);
begin
  inherited Create(Name, Typ);
  FLowBound := LowBound;
  FHighBound := HighBound;
  FElementCount := HighBound - LowBound + 1;
  FSize := FElementCount * Typ.Size;
end;

procedure TStaticArraySymbol.InitData(const Data: TData; Offset: Integer);
var
  x: Integer;
begin
  for x := 1 to ElementCount do
  begin
    Typ.InitData(Data, Offset);
    Inc(Offset, Typ.BaseType.Size);
  end;
end;

function TStaticArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  TypSym := TypSym.BaseType;
  Result :=     (TypSym is TStaticArraySymbol)
            and (ElementCount = TStaticArraySymbol(TypSym).ElementCount)
            and Typ.IsCompatible(TypSym.Typ);
end;

// AddElement
//
procedure TStaticArraySymbol.AddElement;
begin
   Inc(FHighBound);
   Inc(FElementCount);
end;

function TStaticArraySymbol.GetCaption;
begin
  Result := 'array [' + IntToStr(FLowBound) + '..' + IntToStr(FHighBound) + '] of ';
  if Assigned(FTyp) then
    Result := Result + FTyp.Caption
  else
    Result := Result + '<unknown>';
end;

// ------------------
// ------------------ TOpenArraySymbol ------------------
// ------------------

// Create
//
constructor TOpenArraySymbol.Create(const Name: string; Typ: TSymbol);
begin
   inherited Create(Name, Typ, 0, -1);
end;

// IsCompatible
//
function TOpenArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  TypSym := TypSym.BaseType;
  Result :=     (TypSym is TStaticArraySymbol)
            and Typ.IsCompatible(TypSym.Typ);
end;

{ TElementSymbol }

constructor TElementSymbol.Create(const Name: string; Typ: TSymbol;
  Value: Integer; IsUserDef: Boolean);
begin
  inherited Create(Name, Typ, Value);
  FIsUserDef := IsUserDef;
  FUserDefValue := Value;
end;

function TElementSymbol.GetDescription: string;
begin
  if FIsUserDef then
    Result := Name + ' = ' + IntToStr(Data[0])
  else
    Result := Name;  //inherited GetDescription; <= can cause stack overflow
end;

{ TEnumerationSymbol }

constructor TEnumerationSymbol.Create(const Name: string; BaseType: TTypeSymbol);
begin
  inherited Create(Name, BaseType);
  FElements := TSymbolTable.Create;
end;

destructor TEnumerationSymbol.Destroy;
begin
  FElements.Clear;
  FElements.Free;
  inherited;
end;

procedure TEnumerationSymbol.AddElement(Element: TElementSymbol);
begin
  FElements.AddSymbol(Element);
end;

function TEnumerationSymbol.GetCaption: string;
begin
  Result := Name;
end;

function TEnumerationSymbol.GetDescription: string;
var
  x: Integer;
begin
  Result := '(';
  for x := 0 to FElements.Count - 1 do
  begin
    if x <> 0 then
      Result := Result + ', ';
    Result := Result + FElements[x].GetDescription;
  end;
  Result := Result + ')';
end;

function TEnumerationSymbol.ShortDescription : String;
begin
   case FElements.Count of
      0 : Result:=' ';
      1 : Result:=FElements[0].GetDescription;
   else
      Result:=FElements[0].Name+',...';
   end;
   Result:='('+Result+')';
end;

{ TStaticSymbolTable }

constructor TStaticSymbolTable.Create(Parent: TStaticSymbolTable; Reference: Boolean);
begin
  inherited Create(Parent);
  FInitialized := False;
  FRefCount := 0;
  if Reference then
    _AddRef;
end;

procedure TStaticSymbolTable._AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TStaticSymbolTable._Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    Free;
end;

procedure TStaticSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
var
  staticSymbols: TStaticSymbolTable;
begin
  // accept only static parents
  if Parent is TLinkedSymbolTable then
    staticSymbols := TLinkedSymbolTable(Parent).Parent
  else if Parent is TStaticSymbolTable then
    staticSymbols := TStaticSymbolTable(Parent)
  else
    staticSymbols := nil;

  if Assigned(StaticSymbols) then
  begin
    staticSymbols._AddRef;
    inherited InsertParent(Index, staticSymbols);
  end
  else
    raise Exception.Create(CPE_NoStaticSymbols);
end;

function TStaticSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
  Result := inherited RemoveParent(Parent);
  (Parent as TStaticSymbolTable)._Release;
end;

destructor TStaticSymbolTable.Destroy;
begin
  Assert(FRefCount = 0);
  inherited;
end;

procedure TStaticSymbolTable.Initialize;
begin
  if not FInitialized then
  begin
    inherited;
    FInitialized := True;
  end;
end;

{ TLinkedSymbolTable }

constructor TLinkedSymbolTable.Create(Parent: TStaticSymbolTable;
  AddrGenerator: TAddrGenerator);
begin
  inherited Create(nil,AddrGenerator);
  FParent := Parent;
  FParent._AddRef;
end;

destructor TLinkedSymbolTable.Destroy;
begin
  FParent._Release;
  inherited;
end;

function TLinkedSymbolTable.FindLocal(const Name: string): TSymbol;
begin
  Result := FParent.FindLocal(Name);
  if not Assigned(Result) then
    Result := inherited FindLocal(Name);
end;

function TLinkedSymbolTable.FindSymbol(const Name: string): TSymbol;
begin
  Result := FParent.FindSymbol(Name);
  if not Assigned(Result) then
    Result := inherited FindSymbol(Name);
end;

procedure TLinkedSymbolTable.Initialize;
begin
  FParent.Initialize;
  inherited;
end;

{ TAliasSymbol }

function TAliasSymbol.BaseType: TTypeSymbol;
begin
  Result := Typ.BaseType;
end;

constructor TAliasSymbol.Create(const Name: string; Typ: TTypeSymbol);
begin
  Assert(Assigned(Typ));
  inherited Create(Name,Typ);
end;

procedure TAliasSymbol.InitData(const Data: TData; Offset: Integer);
begin
  BaseType.InitData(Data, Offset);
end;

function TAliasSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := BaseType.IsCompatible(typSym);
end;

{ TTypeSymbol }

function TTypeSymbol.BaseType: TTypeSymbol;
begin
  Result := Self;
end;

function TTypeSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := BaseType = typSym.BaseType;
end;

{ EScriptException }

constructor EScriptException.Create(const Message: string; const Value: Variant;
  Typ: TSymbol; const Pos: TScriptPos);
begin
  inherited Create(Message);
  FValue := Value;
  FTyp := Typ;
  FPos := Pos;
end;

constructor EScriptException.Create(const Message: string;
  const ExceptionObj: IScriptObj; const Pos: TScriptPos);
begin
  Create(Message,ExceptionObj,ExceptionObj.ClassSym,Pos);
end;

end.



