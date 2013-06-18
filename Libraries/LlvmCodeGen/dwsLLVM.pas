{**************************************************************************}
{                                                                          }
{    This file is release under dual licenses, MPL 2.0 and LGPL.           }
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{    file, You can obtain one at http://mozilla.org/MPL/2.0/.              }
{                                                                          }
{    This program is free software: you can redistribute it and/or modify  }
{    it under the terms of the GNU Lesser General Public License as        }
{    published by the Free Software Foundation, either version 3 of the    }
{    License, or (at your option) any later version.                       }
{    Yu can obtain a copy of the LGPL at http://www.gnu.org/licenses/      }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Original Copyright: Christian Budde                                   }
{                                                                          }
{    Roughly based on LLVM-Pascal API unit llvmAPI.pas                     }
{      -> http://code.google.com/p/llvm-pascal                             }
{                                                                          }
{**************************************************************************}
unit dwsLLVM;

{$I dws.inc}

interface

uses
   Windows, SysUtils, dwsXPlatform;

const
   CLLVMLibraryMajorVersion = 3;
   CLLVMLibraryMinorVersion = 3;
   CLLVMLibraryPrefix = 'LLVM';
   CLLVMLibraryExtension = '.dll';
   {$IFDEF CPUX86}
   CLLVMLibraryPlatform = 'x86';
   {$ENDIF}
   {$IFDEF CPUX64}
   CLLVMLibraryPlatform = 'x64';
   {$ENDIF}
   CLTO_API_VERSION = 4;

type
   { The top-level container for all LLVM global data. See the LLVMContext class. }
   TLLVMOpaqueContext = packed record
   end;
   PLLVMContext = ^TLLVMOpaqueContext;

   { The top-level container for all other LLVM Intermediate Representation
     (IR) objects. }
   TLLVMOpaqueModule = packed record
   end;
   PLLVMModule = ^TLLVMOpaqueModule;

   { Each value in the LLVM IR has a type, an PLLVMType. }
   TLLVMOpaqueType = packed record
   end;
   PLLVMType = ^TLLVMOpaqueType;

   TLLVMTypePtrArray = array [0 .. 0] of PLLVMType;
   PLLVMTypePtrArray = ^TLLVMTypePtrArray;

   { Represents an individual value in LLVM IR. }
   TLLVMOpaqueValue = packed record
   end;
   PLLVMValue = ^TLLVMOpaqueValue;

   TLLVMValuePtrArray = array [0 .. 0] of PLLVMValue;
   PLLVMValuePtrArray = ^TLLVMValuePtrArray;

   { Represents a basic block of instructions in LLVM IR. }
   TLLVMOpaqueBasicBlock = packed record
   end;
   PLLVMBasicBlock = ^TLLVMOpaqueBasicBlock;

   TLLVMBasicBlockPtrArray = array [0 .. 0] of PLLVMBasicBlock;
   PLLVMBasicBlockPtrArray = ^TLLVMBasicBlockPtrArray;

   { Represents an LLVM basic block builder. }
   TLLVMOpaqueBuilder = packed record
   end;
   PLLVMBuilder = ^TLLVMOpaqueBuilder;

   { Interface used to provide a module to JIT or interpreter.
     This is now just a synonym for llvm::Module, but we have to keep using the
     different type to keep binary compatibility. }
   TLLVMOpaqueModuleProvider = packed record
   end;
   PLLVMModuleProvider = ^TLLVMOpaqueModuleProvider;

   { Used to provide a module to JIT or interpreter. }
   TLLVMOpaqueMemoryBuffer = packed record
   end;
   PLLVMMemoryBuffer = ^TLLVMOpaqueMemoryBuffer;

   TLLVMOpaquePassManager = packed record
   end;
   PLLVMPassManager = ^TLLVMOpaquePassManager;

   TLLVMOpaquePassRegistry = packed record
   end;
   PLLVMPassRegistry = ^TLLVMOpaquePassRegistry;

   { Used to get the users and usees of a Value. }
   TLLVMOpaqueUse = packed record
   end;
   PLLVMUse = ^TLLVMOpaqueUse;

   TLLVMOpaqueTargetData = packed record
   end;
   PLLVMTargetData = ^TLLVMOpaqueTargetData;

   TLLVMOpaqueTargetLibraryInfotData = packed record
   end;
   PLLVMTargetLibraryInfo = ^TLLVMOpaqueTargetLibraryInfotData;

   TLLVMOpaqueTargetMachine = packed record
   end;
   PLLVMTargetMachine = ^TLLVMOpaqueTargetMachine;

   TLLVMOpaqueTarget = packed record
   end;
   PLLVMTarget = ^TLLVMOpaqueTarget;

   TLLVMStructLayout = packed record
   end;
   PLLVMStructLayout = ^TLLVMStructLayout;

   TLLVMOpaqueObjectFile = packed record
   end;
   PLLVMObjectFile = ^TLLVMOpaqueObjectFile;

   TLLVMOpaqueSectionIterator = packed record
   end;
   PLLVMSectionIterator = ^TLLVMOpaqueSectionIterator;

   TLLVMOpaqueSymbolIterator = packed record
   end;
   PLLVMSymbolIterator = ^TLLVMOpaqueSymbolIterator;

   TLLVMOpaqueRelocationIterator = packed record
   end;
   PLLVMRelocationIterator = ^TLLVMOpaqueRelocationIterator;

   TLLVMOpaqueGenericValue = packed record
   end;
   PLLVMGenericValue = ^TLLVMOpaqueGenericValue;

   TLLVMOpaqueExecutionEngine = packed record
   end;
   PLLVMExecutionEngine = ^TLLVMOpaqueExecutionEngine;

   TLLVMOpaquePassManagerBuilder = packed record
   end;
   PLLVMPassManagerBuilder = ^TLLVMOpaquePassManagerBuilder;

   TLTOModule = packed record
   end;
   PLTOModule = ^TLTOModule;

   TLTOCodeGenerator = packed record
   end;
   PLTOCodeGenerator = ^TLTOCodeGenerator;

{$Z4}

   TLLVMAttribute = (
      LLVMZExtAttribute            = 1 shl 0,
      LLVMSExtAttribute            = 1 shl 1,
      LLVMNoReturnAttribute        = 1 shl 2,
      LLVMInRegAttribute           = 1 shl 3,
      LLVMStructRetAttribute       = 1 shl 4,
      LLVMNoUnwindAttribute        = 1 shl 5,
      LLVMNoAliasAttribute         = 1 shl 6,
      LLVMByValAttribute           = 1 shl 7,
      LLVMNestAttribute            = 1 shl 8,
      LLVMReadNoneAttribute        = 1 shl 9,
      LLVMReadOnlyAttribute        = 1 shl 10,
      LLVMNoInlineAttribute        = 1 shl 11,
      LLVMAlwaysInlineAttribute    = 1 shl 12,
      LLVMOptimizeForSizeAttribute = 1 shl 13,
      LLVMStackProtectAttribute    = 1 shl 14,
      LLVMStackProtectReqAttribute = 1 shl 15,
      LLVMAlignment                = 31 shl 16,
      LLVMNoCaptureAttribute       = 1 shl 21,
      LLVMNoRedZoneAttribute       = 1 shl 22,
      LLVMNoImplicitFloatAttribute = 1 shl 23,
      LLVMNakedAttribute           = 1 shl 24,
      LLVMInlineHintAttribute      = 1 shl 25,
      LLVMStackAlignment           = 7 shl 26,
      LLVMReturnsTwice             = 1 shl 29,
      LLVMUWTable                  = 1 shl 30,
      LLVMNonLazyBind              = 1 shl 31
   );

   TLLVMOpcode = (
      // Terminator Instructions
      LLVMRet            = 1,
      LLVMBr             = 2,
      LLVMSwitch         = 3,
      LLVMIndirectBr     = 4,
      LLVMInvoke         = 5,
      LLVMUnreachable    = 7,

      // Standard Binary Operators
      LLVMAdd            = 8,
      LLVMFAdd           = 9,
      LLVMSub            = 10,
      LLVMFSub           = 11,
      LLVMMul            = 12,
      LLVMFMul           = 13,
      LLVMUDiv           = 14,
      LLVMSDiv           = 15,
      LLVMFDiv           = 16,
      LLVMURem           = 17,
      LLVMSRem           = 18,
      LLVMFRem           = 19,

      // Logical Operators
      LLVMShl            = 20,
      LLVMLShr           = 21,
      LLVMAShr           = 22,
      LLVMAnd            = 23,
      LLVMOr             = 24,
      LLVMXor            = 25,

      // Memory Operators
      LLVMAlloca         = 26,
      LLVMLoad           = 27,
      LLVMStore          = 28,
      LLVMGetElementPtr  = 29,

      // Cast Operators
      LLVMTrunc          = 30,
      LLVMZExt           = 31,
      LLVMSExt           = 32,
      LLVMFPToUI         = 33,
      LLVMFPToSI         = 34,
      LLVMUIToFP         = 35,
      LLVMSIToFP         = 36,
      LLVMFPTrunc        = 37,
      LLVMFPExt          = 38,
      LLVMPtrToInt       = 39,
      LLVMIntToPtr       = 40,
      LLVMBitCast        = 41,

      // Other Operators
      LLVMICmp           = 42,
      LLVMFCmp           = 43,
      LLVMPHI            = 44,
      LLVMCall           = 45,
      LLVMSelect         = 46,
      LLVMUserOp1        = 47,
      LLVMUserOp2        = 48,
      LLVMVAArg          = 49,
      LLVMExtractElement = 50,
      LLVMInsertElement  = 51,
      LLVMShuffleVector  = 52,
      LLVMExtractValue   = 53,
      LLVMInsertValue    = 54,

      // Atomic operators
      LLVMFence          = 55,
      LLVMAtomicCmpXchg  = 56,
      LLVMAtomicRMW      = 57,

      // Exception Handling Operators
      LLVMResume         = 58,
      LLVMLandingPad     = 59
   );

   TLLVMTypeKind = (
      LLVMVoidTypeKind,        // type with no size
      LLVMHalfTypeKind,        // 16 bit floating point type
      LLVMFloatTypeKind,       // 32 bit floating point type
      LLVMDoubleTypeKind,      // 64 bit floating point type
      LLVMX86_FP80TypeKind,    // 80 bit floating point type (X87)
      LLVMFP128TypeKind,       // 128 bit floating point type (112-bit mantissa)
      LLVMPPC_FP128TypeKind,   // 128 bit floating point type (two 64-bits)
      LLVMLabelTypeKind,       // Labels
      LLVMIntegerTypeKind,     // Arbitrary bit width integers
      LLVMFunctionTypeKind,    // Functions
      LLVMStructTypeKind,      // Structures
      LLVMArrayTypeKind,       // Arrays
      LLVMPointerTypeKind,     // Pointers
      LLVMVectorTypeKind,      // SIMD 'packed' format, or other vector type
      LLVMMetadataTypeKind,    // Metadata
      LLVMX86_MMXTypeKind      // X86 MMX
   );

   TLLVMLinkage = (
      LLVMExternalLinkage,    // Externally visible function
      LLVMAvailableExternallyLinkage,
      LLVMLinkOnceAnyLinkage, // Keep one copy of function when linking (inline)
      LLVMLinkOnceODRLinkage, // Same, but only replaced by something equivalent.
      LLVMLinkOnceODRAutoHideLinkage, // Like LinkOnceODR, but possibly hidden.
      LLVMWeakAnyLinkage,     // Keep one copy of function when linking (weak)
      LLVMWeakODRLinkage,     // Same, but only replaced by something equivalent.
      LLVMAppendingLinkage,   // Special purpose, only applies to global arrays
      LLVMInternalLinkage,    // Rename collisions when linking (static functions)
      LLVMPrivateLinkage,     // Like Internal, but omit from symbol table
      LLVMDLLImportLinkage,   // Function to be imported from DLL
      LLVMDLLExportLinkage,   // Function to be accessible from DLL
      LLVMExternalWeakLinkage,// ExternalWeak linkage description
      LLVMGhostLinkage,       // Obsolete
      LLVMCommonLinkage,      // Tentative definitions
      LLVMLinkerPrivateLinkage, // Like Private, but linker removes.
      LLVMLinkerPrivateWeakLinkage // Like LinkerPrivate, but is weak.
   );

   TLLVMVisibility = (
      LLVMDefaultVisibility,  // The GV is visible
      LLVMHiddenVisibility,   // The GV is hidden
      LLVMProtectedVisibility // The GV is protected
   );

   TLLVMCallConv = (
      LLVMCCallConv           = 0,
      LLVMFastCallConv        = 8,
      LLVMColdCallConv        = 9,
      LLVMX86StdcallCallConv  = 64,
      LLVMX86FastcallCallConv = 65
   );

   TLLVMIntPredicate = (
      LLVMIntEQ = 32, // equal
      LLVMIntNE,      // not equal
      LLVMIntUGT,     // unsigned greater than
      LLVMIntUGE,     // unsigned greater or equal
      LLVMIntULT,     // unsigned less than
      LLVMIntULE,     // unsigned less or equal
      LLVMIntSGT,     // signed greater than
      LLVMIntSGE,     // signed greater or equal
      LLVMIntSLT,     // signed less than
      LLVMIntSLE      // signed less or equal
   );

   TLLVMRealPredicate = (
      LLVMRealPredicateFalse, // Always false (always folded)
      LLVMRealOEQ,            // True if ordered and equal
      LLVMRealOGT,            // True if ordered and greater than
      LLVMRealOGE,            // True if ordered and greater than or equal
      LLVMRealOLT,            // True if ordered and less than
      LLVMRealOLE,            // True if ordered and less than or equal
      LLVMRealONE,            // True if ordered and operands are unequal
      LLVMRealORD,            // True if ordered (no nans)
      LLVMRealUNO,            // True if unordered: isnan(X) | isnan(Y)
      LLVMRealUEQ,            // True if unordered or equal
      LLVMRealUGT,            // True if unordered or greater than
      LLVMRealUGE,            // True if unordered, greater than, or equal
      LLVMRealULT,            // True if unordered or less than
      LLVMRealULE,            // True if unordered, less than, or equal
      LLVMRealUNE,            // True if unordered or not equal
      LLVMRealPredicateTrue   // Always true (always folded)
   );

   TLLVMCodeGenOptLevel = (
      LLVMCodeGenLevelNone,
      LLVMCodeGenLevelLess,
      LLVMCodeGenLevelDefault,
      LLVMCodeGenLevelAggressive
   );

   TLLVMRelocMode = (
      LLVMRelocDefault,
      LLVMRelocStatic,
      LLVMRelocPIC,
      LLVMRelocDynamicNoPic
   );

   TLLVMCodeModel = (
      LLVMCodeModelDefault,
      LLVMCodeModelJITDefault,
      LLVMCodeModelSmall,
      LLVMCodeModelKernel,
      LLVMCodeModelMedium,
      LLVMCodeModelLarge
   );

   TLLVMCodeGenFileType = (
      LLVMAssemblyFile,
      LLVMObjectFile
   );

   TLLVMLandingPadClauseTy = (
      LLVMLandingPadCatch,    // A catch clause
      LLVMLandingPadFilter    // A filter clause
   );

   TLLVMVerifierFailureAction = (
      LLVMAbortProcessAction, // verifier will print to stderr and abort
      LLVMPrintMessageAction, // verifier will print to stderr and return 1
      LLVMReturnStatusAction  // verifier will just return 1
   );

   TLLVMByteOrdering = (
      LLVMBigEndian,
      LLVMLittleEndian
   );

   TLTOSymbolAttributes = (
      LTO_SYMBOL_ALIGNMENT_MASK              = $0000001F, (* log2 of alignment *)
      LTO_SYMBOL_PERMISSIONS_MASK            = $000000E0,
      LTO_SYMBOL_PERMISSIONS_CODE            = $000000A0,
      LTO_SYMBOL_PERMISSIONS_DATA            = $000000C0,
      LTO_SYMBOL_PERMISSIONS_RODATA          = $00000080,
      LTO_SYMBOL_DEFINITION_MASK             = $00000700,
      LTO_SYMBOL_DEFINITION_REGULAR          = $00000100,
      LTO_SYMBOL_DEFINITION_TENTATIVE        = $00000200,
      LTO_SYMBOL_DEFINITION_WEAK             = $00000300,
      LTO_SYMBOL_DEFINITION_UNDEFINED        = $00000400,
      LTO_SYMBOL_DEFINITION_WEAKUNDEF        = $00000500,
      LTO_SYMBOL_SCOPE_MASK                  = $00003800,
      LTO_SYMBOL_SCOPE_INTERNAL              = $00000800,
      LTO_SYMBOL_SCOPE_HIDDEN                = $00001000,
      LTO_SYMBOL_SCOPE_PROTECTED             = $00002000,
      LTO_SYMBOL_SCOPE_DEFAULT               = $00001800,
      LTO_SYMBOL_SCOPE_DEFAULT_CAN_BE_HIDDEN = $00002800
   );

   TLTODebugModel = (
      LTO_DEBUG_MODEL_NONE  = 0,
      LTO_DEBUG_MODEL_DWARF = 1
   );

   TLTOCodegenModel = (
      LTO_CODEGEN_PIC_MODEL_STATIC         = 0,
      LTO_CODEGEN_PIC_MODEL_DYNAMIC        = 1,
      LTO_CODEGEN_PIC_MODEL_DYNAMIC_NO_PIC = 2
   );

   TLLVMDisassemblerVariantKind = (
      dvkNone = 0,
      dvkArmHi16 = 1,
      dvkArmLo16 = 2
   );

   TLLVMDisassemblerReferenceTypeIn = (
      drtiOutNone = 0,
      drtiBranch = 1,
      drtiPCrelLoad = 2
   );

   TLLVMDisassemblerReferenceTypeOut = (
      drtoSymbolStub = 1,
      drtoLitPoolSymAddr = 2,
      drtoLitPoolCstrAddr = 3
   );

   TLLVMDisassemblerOption = (
      doUseMarkup = 1
   );
   TLLVMDisassemblerOptions = set of TLLVMDisassemblerOption;

   PLLVMDisasmContext = Pointer;
   TLLVMOpInfoCallback = function (DisInfo: Pointer; PC: UInt64; Offset: UInt64; Size: UInt64; TagType: Integer; TagBuf: Pointer): Integer; cdecl;

   TLLVMOpInfoSymbol1 = packed record
      Present: UInt64;  // 1 if this symbol is present
      Name: PAnsiChar;  // symbol name if not NULL
      Value: UInt64;    // symbol value if name is NULL
   end;

   TLLVMOpInfo1 = packed record
      AddSymbol: TLLVMOpInfoSymbol1;
      SubtractSymbol: TLLVMOpInfoSymbol1;
      Value: UInt64;
      VariantKind: UInt64;
   end;

   TLLVMSymbolLookupCallback = function(DisInfo: Pointer; ReferenceValue: Cardinal; ReferenceType: PCardinal; ReferencePC: Cardinal; var ReferenceName: PAnsiChar): PAnsiChar; cdecl;

var
   LLVMInitializeCore: procedure(R: PLLVMPassRegistry); cdecl;

   { Error handling }

   LLVMDisposeMessage: procedure(&Message: PAnsiChar); cdecl;

   { Create a new context.
     Every call to this function should be paired with a call to
     LLVMContextDispose or the context will leak memory. }
   LLVMContextCreate: function: PLLVMContext; cdecl;

   { Obtain the global context instance. }
   LLVMGetGlobalContext: function: PLLVMContext; cdecl;

   { Destroy a context instance.
     This should be called for every call to LLVMContextCreate or memory
     will be leaked. }
   LLVMContextDispose: procedure(C: PLLVMContext); cdecl;

   LLVMGetMDKindIDInContext: function(C: PLLVMContext; const Name: PAnsiChar; SLen: Cardinal): Cardinal; cdecl;
   LLVMGetMDKindID: function(const Name: PAnsiChar; SLen: Cardinal): Cardinal; cdecl;

   { Create a new, empty module in the global context.
     This is equivalent to calling LLVMModuleCreateWithNameInContext with
     LLVMGetGlobalContext as the context parameter.
     Every invocation should be paired with LLVMDisposeModule or memory
     will be leaked. }
   LLVMModuleCreateWithName: function(const ModuleID: PAnsiChar): PLLVMModule; cdecl;

   { Create a new, empty module in a specific context.
     Every invocation should be paired with LLVMDisposeModule or memory
     will be leaked. }
   LLVMModuleCreateWithNameInContext: function(const ModuleID: PAnsiChar; C: PLLVMContext): PLLVMModule; cdecl;

   { Destroy a module instance.
     This must be called for every created module or memory will be
     leaked. }
   LLVMDisposeModule: procedure(M: PLLVMModule); cdecl;

   { Obtain the data layout for a module. }
   LLVMGetDataLayout: function(M: PLLVMModule): PAnsiChar; cdecl;

   { Set the data layout for a module. }
   LLVMSetDataLayout: procedure(M: PLLVMModule; const Triple: PAnsiChar); cdecl;

   { Obtain the target triple for a module. }
   LLVMGetTarget: function(M: PLLVMModule): PAnsiChar; cdecl;

   { Set the target triple for a module. }
   LLVMSetTarget: procedure(M: PLLVMModule; const Triple: PAnsiChar); cdecl;

   { Dump a representation of a module to stderr. }
   LLVMDumpModule: procedure(M: PLLVMModule); cdecl;

   { Print a representation of a module to a file.
     The ErrorMessage needs to be disposed with LLVMDisposeMessage.
     Returns 0 on success, 1 otherwise. }
   LLVMPrintModuleToFile: function(M: PLLVMModule; const Filename: PAnsiChar; var ErrorMessage: PAnsiChar): Boolean; cdecl;

   { Set inline assembly for a module. }
   LLVMSetModuleInlineAsm: procedure(M: PLLVMModule; const &Asm: PAnsiChar); cdecl;

   { Obtain the context to which this module is associated. }
   LLVMGetModuleContext: function(M: PLLVMModule): PLLVMContext; cdecl;

   { Obtain a Type from a module by its registered name. }
   LLVMGetTypeByName: function(M: PLLVMModule; const Name: PAnsiChar): PLLVMType; cdecl;

   { Obtain the number of operands for named metadata in a module. }
   LLVMGetNamedMetadataNumOperands: function(M: PLLVMModule; const Name: PAnsiChar): Cardinal; cdecl;

   { Obtain the named metadata operands for a module.
     The passed PLLVMValue pointer should refer to an array of
     PLLVMValue at least LLVMGetNamedMetadataNumOperands long. This
     array will be populated with the PLLVMValue instances. Each
     instance corresponds to a llvm::MDNode. }
   LLVMGetNamedMetadataOperands: procedure(M: PLLVMModule; const Name: PAnsiChar; var Dest: PLLVMValue); cdecl;

   { Add an operand to named metadata. }
   LLVMAddNamedMetadataOperand: procedure(M: PLLVMModule; const Name: PAnsiChar; Val: PLLVMValue); cdecl;

   { Add a function to a module under a specified name. }
   LLVMAddFunction: function(M: PLLVMModule; const Name: PAnsiChar; FunctionTy: PLLVMType): PLLVMValue; cdecl;

   { Obtain a Function value from a Module by its name.
     The returned value corresponds to a llvm::Function value. }
   LLVMGetNamedFunction: function(M: PLLVMModule; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Obtain an iterator to the first Function in a Module. }
   LLVMGetFirstFunction: function(M: PLLVMModule): PLLVMValue; cdecl;

   { Obtain an iterator to the last Function in a Module. }
   LLVMGetLastFunction: function(M: PLLVMModule): PLLVMValue; cdecl;

   { Advance a Function iterator to the next Function.
     Returns NULL if the iterator was already at the end and there are no more
     functions. }
   LLVMGetNextFunction: function(Fn: PLLVMValue): PLLVMValue; cdecl;

   { Decrement a Function iterator to the previous Function.
     Returns NULL if the iterator was already at the beginning and there are
     no previous functions. }
   LLVMGetPreviousFunction: function(Fn: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the enumerated type of a Type instance. }
   LLVMGetTypeKind: function(Ty: PLLVMType): TLLVMTypeKind; cdecl;

   { Whether the type has a known size.
     Things that don't have a size are abstract types, labels, and void.a }
   LLVMTypeIsSized: function(Ty: PLLVMType): Boolean; cdecl;

   { Obtain the context to which this type instance is associated. }
   LLVMGetTypeContext: function(Ty: PLLVMType): PLLVMContext; cdecl;

   { Obtain an integer type from a context with specified bit width. }
   LLVMInt1TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;
   LLVMInt8TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;
   LLVMInt16TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;
   LLVMInt32TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;
   LLVMInt64TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;
   LLVMIntTypeInContext: function(C: PLLVMContext; NumBits: Cardinal): PLLVMType; cdecl;

   { Obtain a 16-bit floating point type from a context. }
   LLVMHalfTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain a 32-bit floating point type from a context. }
   LLVMFloatTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain a 64-bit floating point type from a context. }
   LLVMDoubleTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain a 80-bit floating point type (X87) from a context. }
   LLVMX86FP80TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain a 128-bit floating point type (112-bit mantissa) from a
     context. }
   LLVMFP128TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain a 128-bit floating point type (two 64-bits) from a context. }
   LLVMPPCFP128TypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Obtain an integer type from the global context with a specified bit width. }
   LLVMInt1Type: function: PLLVMType; cdecl;
   LLVMInt8Type: function: PLLVMType; cdecl;
   LLVMInt16Type: function: PLLVMType; cdecl;
   LLVMInt32Type: function: PLLVMType; cdecl;
   LLVMInt64Type: function: PLLVMType; cdecl;
   LLVMIntType: function(NumBits: Cardinal): PLLVMType; cdecl;
   LLVMGetIntTypeWidth: function(IntegerTy: PLLVMType): Cardinal; cdecl;

   { Obtain a floating point type from the global context.
     These map to the functions in this group of the same name. }
   LLVMHalfType: function: PLLVMType; cdecl;
   LLVMFloatType: function: PLLVMType; cdecl;
   LLVMDoubleType: function: PLLVMType; cdecl;
   LLVMX86FP80Type: function: PLLVMType; cdecl;
   LLVMFP128Type: function: PLLVMType; cdecl;
   LLVMPPCFP128Type: function: PLLVMType; cdecl;

   { Obtain a function type consisting of a specified signature.
     The function is defined as a tuple of a return Type, a list of
     parameter types, and whether the function is variadic. }
   LLVMFunctionType: function(ReturnType: PLLVMType; ParamTypes: PLLVMTypePtrArray; ParamCount: Cardinal; IsVarArg: Boolean): PLLVMType; cdecl;

   { Returns whether a function type is variadic. }
   LLVMIsFunctionVarArg: function(FunctionTy: PLLVMType): Boolean; cdecl;

   { Obtain the Type this function Type returns. }
   LLVMGetReturnType: function(FunctionTy: PLLVMType): PLLVMType; cdecl;

   { Obtain the number of parameters this function accepts. }
   LLVMCountParamTypes: function(FunctionTy: PLLVMType): Cardinal; cdecl;

   { Obtain the types of a function's parameters.
     The Dest parameter should point to a pre-allocated array of
     PLLVMType at least LLVMCountParamTypes large. On return, the
     first LLVMCountParamTypes entries in the array will be populated
     with PLLVMType instances.
        param FunctionTy The function type to operate on.
        param Dest Memory address of an array to be filled with result. }
   LLVMGetParamTypes: procedure(FunctionTy: PLLVMType; var Dest: PLLVMType); cdecl;

   { Create a new structure type in a context.
     A structure is specified by a list of inner elements/types and
     whether these can be packed together. }
   LLVMStructTypeInContext: function(C: PLLVMContext; ElementTypes: PLLVMTypePtrArray; ElementCount: Cardinal; &Packed: Boolean): PLLVMType; cdecl;

   { Create a new structure type in the global context. }
   LLVMStructType: function(ElementTypes: PLLVMTypePtrArray; ElementCount: Cardinal; &Packed: Boolean): PLLVMType; cdecl;

   { Create an empty structure in a context having a specified name. }
   LLVMStructCreateNamed: function(C: PLLVMContext; const Name: PAnsiChar): PLLVMType; cdecl;

   { Obtain the name of a structure. }
   LLVMGetStructName: function(Ty: PLLVMType): PAnsiChar; cdecl;

   { Set the contents of a structure type. }
   LLVMStructSetBody: procedure(StructTy: PLLVMType; ElementTypes: PLLVMTypePtrArray; ElementCount: Cardinal; &Packed: Boolean); cdecl;

   { Get the number of elements defined inside the structure. }
   LLVMCountStructElementTypes: function(StructTy: PLLVMType): Cardinal; cdecl;

   { Get the elements within a structure.
     The function is passed the address of a pre-allocated array of
     PLLVMType at least LLVMCountStructElementTypes long. After
     invocation, this array will be populated with the structure's
     elements. The objects in the destination array will have a lifetime
     of the structure type itself, which is the lifetime of the context it
     is contained in. }
   LLVMGetStructElementTypes: procedure(StructTy: PLLVMType; var Dest: PLLVMType); cdecl;

   { Determine whether a structure is packed. }
   LLVMIsPackedStruct: function(StructTy: PLLVMType): Boolean; cdecl;

   { Determine whether a structure is opaque. }
   LLVMIsOpaqueStruct: function(StructTy: PLLVMType): Boolean; cdecl;

   { Obtain the type of elements within a sequential type.
     This works on array, vector, and pointer types. }
   LLVMGetElementType: function(Ty: PLLVMType): PLLVMType; cdecl;

   { Create a fixed size array type that refers to a specific type.
     The created type will exist in the context that its element type
     exists in. }
   LLVMArrayType: function(ElementType: PLLVMType; ElementCount: Cardinal): PLLVMType; cdecl;

   { Obtain the length of an array type.
     This only works on types that represent arrays. }
   LLVMGetArrayLength: function(ArrayTy: PLLVMType): Cardinal; cdecl;

   { Create a pointer type that points to a defined type.
     The created type will exist in the context that its pointee type
     exists in. }
   LLVMPointerType: function(ElementType: PLLVMType; AddressSpace: Cardinal): PLLVMType; cdecl;

   { Obtain the address space of a pointer type.
     This only works on types that represent pointers. }
   LLVMGetPointerAddressSpace: function(PointerTy: PLLVMType): Cardinal; cdecl;

   { Create a vector type that contains a defined type and has a specific
     number of elements.
     The created type will exist in the context thats its element type
     exists in. }
   LLVMVectorType: function(ElementType: PLLVMType; ElementCount: Cardinal): PLLVMType; cdecl;

   { Obtain the number of elements in a vector type.
     This only works on types that represent vectors. }
   LLVMGetVectorSize: function(VectorTy: PLLVMType): Cardinal; cdecl;

   { Create a void type in a context. }
   LLVMVoidTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Create a label type in a context. }
   LLVMLabelTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { Create a X86 MMX type in a context. }
   LLVMX86MMXTypeInContext: function(C: PLLVMContext): PLLVMType; cdecl;

   { These are similar to the above functions except they operate on the
     global context. }
   LLVMVoidType: function: PLLVMType; cdecl;
   LLVMLabelType: function: PLLVMType; cdecl;
   LLVMX86MMXType: function: PLLVMType; cdecl;

   { Obtain the type of a value. }
   LLVMTypeOf: function(Val: PLLVMValue): PLLVMType; cdecl;

   { Obtain the string name of a value. }
   LLVMGetValueName: function(Val: PLLVMValue): PAnsiChar; cdecl;

   { Set the string name of a value. }
   LLVMSetValueName: procedure(Val: PLLVMValue; const Name: PAnsiChar); cdecl;

   { Dump a representation of a value to stderr. }
   LLVMDumpValue: procedure(Val: PLLVMValue); cdecl;

   { Replace all uses of a value with another one. }
   LLVMReplaceAllUsesWith: procedure(OldVal: PLLVMValue; NewVal: PLLVMValue); cdecl;

   { Determine whether the specified constant instance is constant. }
   LLVMIsConstant: function(Val: PLLVMValue): Boolean; cdecl;

   { Determine whether a value instance is undefined. }
   LLVMIsUndef: function(Val: PLLVMValue): Boolean; cdecl;

   { Convert value instances between types.
     Internally, a PLLVMValue is "pinned" to a specific type. This series of
     functions allows you to cast an instance to a specific type.
     If the cast is not valid for the specified type, NULL is returned. }
   LLVMIsAArgument: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsABasicBlock: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAInlineAsm: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMDNode: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMDString: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAUser: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstant: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsABlockAddress: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantAggregateZero: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantArray: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantExpr: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantFP: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantInt: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantPointerNull: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantStruct: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAConstantVector: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAGlobalValue: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFunction: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAGlobalAlias: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAGlobalVariable: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAUndefValue: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAInstruction: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsABinaryOperator: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsACallInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAIntrinsicInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsADbgInfoIntrinsic: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsADbgDeclareInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMemIntrinsic: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMemCpyInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMemMoveInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAMemSetInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsACmpInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFCmpInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAICmpInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAExtractElementInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAGetElementPtrInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAInsertElementInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAInsertValueInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsALandingPadInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAPHINode: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsASelectInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAShuffleVectorInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAStoreInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsATerminatorInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsABranchInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAIndirectBrInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAInvokeInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAReturnInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsASwitchInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAUnreachableInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAResumeInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAUnaryInstruction: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAAllocaInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsACastInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsABitCastInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFPExtInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFPToSIInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFPToUIInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAFPTruncInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAIntToPtrInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAPtrToIntInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsASExtInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsASIToFPInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsATruncInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAUIToFPInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAZExtInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAExtractValueInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsALoadInst: function(Val: PLLVMValue): PLLVMValue; cdecl;
   LLVMIsAVAArgInst: function(Val: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the first use of a value.
     Uses are obtained in an iterator fashion. First, call this function
     to obtain a reference to the first use. Then, call LLVMGetNextUse
     on that instance and all subsequently obtained instances until
     LLVMGetNextUse returns NULL. }
   LLVMGetFirstUse: function(Val: PLLVMValue): PLLVMUse; cdecl;

   { Obtain the next use of a value.
     This effectively advances the iterator. It returns NULL if you are on
     the final use and no more are available. }
   LLVMGetNextUse: function(U: PLLVMUse): PLLVMUse; cdecl;

   { Obtain the user value for a user.
     The returned value corresponds to a llvm::User type. }
   LLVMGetUser: function(U: PLLVMUse): PLLVMValue; cdecl;

   { Obtain the value this use corresponds to. }
   LLVMGetUsedValue: function(U: PLLVMUse): PLLVMValue; cdecl;

   { Obtain an operand at a specific index in a llvm::User value. }
   LLVMGetOperand: function(Val: PLLVMValue; Index: Cardinal): PLLVMValue; cdecl;

   { Set an operand at a specific index in a llvm::User value. }
   LLVMSetOperand: procedure(User: PLLVMValue; Index: Cardinal; Val: PLLVMValue); cdecl;

   { Obtain the number of operands in a llvm::User value. }
   LLVMGetNumOperands: function(Val: PLLVMValue): Integer; cdecl;

   { Obtain a constant value referring to the null instance of a type. }
   LLVMConstNull: function(Ty: PLLVMType): PLLVMValue; cdecl;

   { Obtain a constant value referring to the instance of a type
     consisting of all ones.
     This is only valid for integer types. }
   LLVMConstAllOnes: function(Ty: PLLVMType): PLLVMValue; cdecl;

   { Obtain a constant value referring to an undefined value of a type. }
   LLVMGetUndef: function(Ty: PLLVMType): PLLVMValue; cdecl;

   { Determine whether a value instance is null. }
   LLVMIsNull: function(Val: PLLVMValue): Boolean; cdecl;

   { Obtain a constant that is a constant pointer pointing to NULL for a
     specified type. }
   LLVMConstPointerNull: function(Ty: PLLVMType): PLLVMValue; cdecl;

   { Obtain a constant value for an integer type.
     The returned value corresponds to a llvm::ConstantInt.
        @param IntTy Integer type to obtain value of.
        @param N The value the returned instance should refer to.
        @param SignExtend Whether to sign extend the produced value. }
   LLVMConstInt: function(IntTy: PLLVMType; N: UInt64; SignExtend: Boolean): PLLVMValue; cdecl;

   { Obtain a constant value for an integer of arbitrary precision. }
   LLVMConstIntOfArbitraryPrecision: function(IntTy: PLLVMType; NumWords: Cardinal; Words: PUInt64): PLLVMValue; cdecl;

   { Obtain a constant value for an integer parsed from a string.
     A similar API, LLVMConstIntOfStringAndSize is also available. If the
     string's length is available, it is preferred to call that function
     instead. }
   LLVMConstIntOfString: function(IntTy: PLLVMType; const Text: PAnsiChar; Radix: Byte): PLLVMValue; cdecl;

   { Obtain a constant value for an integer parsed from a string with
     specified length. }
   LLVMConstIntOfStringAndSize: function(IntTy: PLLVMType; const Text: PAnsiChar; SLen: Cardinal; Radix: Byte): PLLVMValue; cdecl;

   { Obtain a constant value referring to a double floating point value. }
   LLVMConstReal: function(RealTy: PLLVMType; N: Double): PLLVMValue; cdecl;

   { Obtain a constant for a floating point value parsed from a string.
     A similar API, LLVMConstRealOfStringAndSize is also available. It
     should be used if the input string's length is known. }
   LLVMConstRealOfString: function(RealTy: PLLVMType; const Text: PAnsiChar): PLLVMValue; cdecl;

   { Obtain a constant for a floating point value parsed from a string. }
   LLVMConstRealOfStringAndSize: function(RealTy: PLLVMType; const Text: PAnsiChar; SLen: Cardinal): PLLVMValue; cdecl;

   { Obtain the zero extended value for an integer constant value. }
   LLVMConstIntGetZExtValue: function(ConstantVal: PLLVMValue): UInt64; cdecl;

   { Obtain the sign extended value for an integer constant value. }
   LLVMConstIntGetSExtValue: function(ConstantVal: PLLVMValue): Int64; cdecl;

   { Create a ConstantDataSequential and initialize it with a string. }
   LLVMConstStringInContext: function(C: PLLVMContext; const Str: PAnsiChar; Length: Cardinal; DontNullTerminate: Boolean): PLLVMValue; cdecl;

   { Create a ConstantDataSequential with string content in the global context.
    This is the same as LLVMConstStringInContext except it operates on the
    global context. }
   LLVMConstString: function(const Str: PAnsiChar; Length: Cardinal; DontNullTerminate: Boolean): PLLVMValue; cdecl;

   { Create an anonymous ConstantStruct with the specified values. }
   LLVMConstStructInContext: function(C: PLLVMContext; ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean): PLLVMValue; cdecl;

   { Create a ConstantStruct in the global Context.
     This is the same as LLVMConstStructInContext except it operates on the
     global Context. }
   LLVMConstStruct: function(ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean): PLLVMValue; cdecl;

   { Create a ConstantArray from values. }
   LLVMConstArray: function(ElementTy: PLLVMType; ConstantVals: PLLVMValuePtrArray; Length: Cardinal): PLLVMValue; cdecl;

   { Create a non-anonymous ConstantStruct from values. }
   LLVMConstNamedStruct: function(StructTy: PLLVMType; ConstantVals: PLLVMValuePtrArray; Count: Cardinal): PLLVMValue; cdecl;

   { Create a ConstantVector from values. }
   LLVMConstVector: function(ScalarConstantVals: PLLVMValuePtrArray; Size: Cardinal): PLLVMValue; cdecl;

   { Functions below correspond to APIs on llvm::ConstantExpr. }
   LLVMGetConstOpcode: function(ConstantVal: PLLVMValue): TLLVMOpcode; cdecl;
   LLVMAlignOf: function(Ty: PLLVMType): PLLVMValue; cdecl;
   LLVMSizeOf: function(Ty: PLLVMType): PLLVMValue; cdecl;
   LLVMConstNeg: function(ConstantVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNSWNeg: function(ConstantVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNUWNeg: function(ConstantVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFNeg: function(ConstantVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNot: function(ConstantVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstAdd: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNSWAdd: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNUWAdd: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFAdd: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstSub: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNSWSub: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNUWSub: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFSub: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstMul: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNSWMul: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstNUWMul: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFMul: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstUDiv: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstSDiv: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstExactSDiv: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFDiv: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstURem: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstSRem: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFRem: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstAnd: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstOr: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstXor: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstICmp: function(Predicate: TLLVMIntPredicate; LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstFCmp: function(Predicate: TLLVMIntPredicate; LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstShl: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstLShr: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstAShr: function(LHSConstant: PLLVMValue; RHSConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstGEP: function(ConstantVal: PLLVMValue; ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal): PLLVMValue; cdecl;
   LLVMConstInBoundsGEP: function(ConstantVal: PLLVMValue; ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal): PLLVMValue; cdecl;
   LLVMConstTrunc: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstSExt: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstZExt: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstFPTrunc: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstFPExt: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstUIToFP: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstSIToFP: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstFPToUI: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstFPToSI: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstPtrToInt: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstIntToPtr: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstBitCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstZExtOrBitCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstSExtOrBitCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstTruncOrBitCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstPointerCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstIntCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType; isSigned: Boolean): PLLVMValue; cdecl;
   LLVMConstFPCast: function(ConstantVal: PLLVMValue; ToType: PLLVMType): PLLVMValue; cdecl;
   LLVMConstSelect: function(ConstantCondition: PLLVMValue; ConstantIfTrue: PLLVMValue; ConstantIfFalse: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstExtractElement: function(VectorConstant: PLLVMValue; IndexConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstInsertElement: function(VectorConstant: PLLVMValue; ElementValueConstant: PLLVMValue; IndexConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstShuffleVector: function(VectorAConstant: PLLVMValue; VectorBConstant: PLLVMValue; MaskConstant: PLLVMValue): PLLVMValue; cdecl;
   LLVMConstExtractValue: function(AggConstant: PLLVMValue; IdxList: PCardinal; NumIdx: Cardinal): PLLVMValue; cdecl;
   LLVMConstInsertValue: function(AggConstant: PLLVMValue; ElementValueConstant: PLLVMValue; IdxList: PCardinal; NumIdx: Cardinal): PLLVMValue; cdecl;
   LLVMConstInlineAsm: function(Ty: PLLVMType; const AsmString: PAnsiChar; const Constraints: PAnsiChar; HasSideEffects: Boolean; IsAlignStack: Boolean): PLLVMValue; cdecl;
   LLVMBlockAddress: function(F: PLLVMValue; BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { This group contains functions that operate on global values. Functions in
    this group relate to functions in the llvm::GlobalValue class tree. }

   LLVMGetGlobalParent: function(Global: PLLVMValue): PLLVMModule; cdecl;
   LLVMIsDeclaration: function(Global: PLLVMValue): Boolean; cdecl;
   LLVMGetLinkage: function(Global: PLLVMValue): TLLVMLinkage; cdecl;
   LLVMSetLinkage: procedure(Global: PLLVMValue; Linkage: TLLVMLinkage); cdecl;
   LLVMGetSection: function(Global: PLLVMValue): PAnsiChar; cdecl;
   LLVMSetSection: procedure(Global: PLLVMValue; const Section: PAnsiChar); cdecl;
   LLVMGetVisibility: function(Global: PLLVMValue): TLLVMVisibility; cdecl;
   LLVMSetVisibility: procedure(Global: PLLVMValue; Viz: TLLVMVisibility); cdecl;
   LLVMGetAlignment: function(Global: PLLVMValue): Cardinal; cdecl;
   LLVMSetAlignment: procedure(Global: PLLVMValue; Bytes: Cardinal); cdecl;

   { This group contains functions that operate on global variable values. }
   LLVMAddGlobal: function(M: PLLVMModule; Ty: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMAddGlobalInAddressSpace: function(M: PLLVMModule; Ty: PLLVMType; const Name: PAnsiChar; AddressSpace: Cardinal): PLLVMValue; cdecl;
   LLVMGetNamedGlobal: function(M: PLLVMModule; Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMGetFirstGlobal: function(M: PLLVMModule): PLLVMValue; cdecl;
   LLVMGetLastGlobal: function(M: PLLVMModule): PLLVMValue; cdecl;
   LLVMGetNextGlobal: function(GlobalVar: PLLVMValue): PLLVMValue; cdecl;
   LLVMGetPreviousGlobal: function(GlobalVar: PLLVMValue): PLLVMValue; cdecl;
   LLVMDeleteGlobal: procedure(GlobalVar: PLLVMValue); cdecl;
   LLVMGetInitializer: function(GlobalVar: PLLVMValue): PLLVMValue; cdecl;
   LLVMSetInitializer: procedure(GlobalVar: PLLVMValue; ConstantVal: PLLVMValue); cdecl;
   LLVMIsThreadLocal: function(GlobalVar: PLLVMValue): Boolean; cdecl;
   LLVMSetThreadLocal: procedure(GlobalVar: PLLVMValue; IsThreadLocal: Boolean); cdecl;
   LLVMIsGlobalConstant: function(GlobalVar: PLLVMValue): Boolean; cdecl;
   LLVMSetGlobalConstant: procedure(GlobalVar: PLLVMValue; IsConstant: Boolean); cdecl;

   { This group contains function that operate on global alias values. }
   LLVMAddAlias: function(M: PLLVMModule; Ty: PLLVMType; Aliasee: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Remove a function from its containing module and deletes it. }
   LLVMDeleteFunction: procedure(Fn: PLLVMValue); cdecl;

   { Obtain the ID number from a function instance. }
   LLVMGetIntrinsicID: function(Fn: PLLVMValue): Cardinal; cdecl;

   { Obtain the calling function of a function.
    The returned value corresponds to the TLLVMCallConv enumeration. }
   LLVMGetFunctionCallConv: function(Fn: PLLVMValue): Cardinal; cdecl;

   { Set the calling convention of a function. }
   LLVMSetFunctionCallConv: procedure(Fn: PLLVMValue; CC: Cardinal); cdecl;

   { Obtain the name of the garbage collector to use during code
    generation. }
   LLVMGetGC: function(Fn: PLLVMValue): PAnsiChar; cdecl;

   { Define the garbage collector to use during code generation. }
   LLVMSetGC: procedure(Fn: PLLVMValue; const Name: PAnsiChar); cdecl;

   { Add an attribute to a function. }
   LLVMAddFunctionAttr: procedure(Fn: PLLVMValue; PA: TLLVMAttribute); cdecl;

   { Obtain an attribute from a function. }
   LLVMGetFunctionAttr: function(Fn: PLLVMValue): TLLVMAttribute; cdecl;

   { Remove an attribute from a function. }
   LLVMRemoveFunctionAttr: procedure(Fn: PLLVMValue; PA: TLLVMAttribute); cdecl;

   { Obtain the number of parameters in a function. }
   LLVMCountParams: function(Fn: PLLVMValue): Cardinal; cdecl;

   { Obtain the parameters in a function.
     The takes a pointer to a pre-allocated array of PLLVMValue that is
     at least LLVMCountParams long. This array will be filled with
     PLLVMValue instances which correspond to the parameters the
     function receives. Each PLLVMValue corresponds to a llvm::Argument
     instance. }
   LLVMGetParams: procedure(Fn: PLLVMValue; Params: PLLVMValuePtrArray); cdecl;

   { Obtain the parameter at the specified index.
     Parameters are indexed from 0. }
   LLVMGetParam: function(Fn: PLLVMValue; Index: Cardinal): PLLVMValue; cdecl;

   { Obtain the function to which this argument belongs.
     Unlike other functions in this group, this one takes a PLLVMValue
     that corresponds to a llvm::Attribute.
     The returned PLLVMValue is the llvm::Function to which this
     argument belongs. }
   LLVMGetParamParent: function(Inst: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the first parameter to a function. }
   LLVMGetFirstParam: function(Fn: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the last parameter to a function. }
   LLVMGetLastParam: function(Fn: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the next parameter to a function.
     This takes a PLLVMValue obtained from LLVMGetFirstParam (which is actually
     a wrapped iterator) and obtains the next parameter from the underlying
     iterator. }
   LLVMGetNextParam: function(Arg: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the previous parameter to a function.
     This is the opposite of LLVMGetNextParam. }
   LLVMGetPreviousParam: function(Arg: PLLVMValue): PLLVMValue; cdecl;

   { Add an attribute to a function argument. }
   LLVMAddAttribute: procedure(Arg: PLLVMValue; PA: TLLVMAttribute); cdecl;

   { Remove an attribute from a function argument. }
   LLVMRemoveAttribute: procedure(Arg: PLLVMValue; PA: TLLVMAttribute); cdecl;

   { Get an attribute from a function argument. }
   LLVMGetAttribute: function(Arg: PLLVMValue): TLLVMAttribute; cdecl;

   { Set the alignment for a function parameter. }
   LLVMSetParamAlignment: procedure(Arg: PLLVMValue; align: Cardinal); cdecl;

   { Obtain a MDString value from a context.
     The returned instance corresponds to the llvm::MDString class.
     The instance is specified by string data of a specified length. The
     string content is copied, so the backing memory can be freed after this
     function returns. }
   LLVMMDStringInContext: function(C: PLLVMContext; const Str: PAnsiChar; SLen: Cardinal): PLLVMValue; cdecl;

   { Obtain a MDString value from the global context. }
   LLVMMDString: function(const Str: PAnsiChar; SLen: Cardinal): PLLVMValue; cdecl;

   { Obtain a MDNode value from a context.
     The returned value corresponds to the llvm::MDNode class. }
   LLVMMDNodeInContext: function(C: PLLVMContext; Vals: PLLVMValuePtrArray; Count: Cardinal): PLLVMValue; cdecl;

   { Obtain a MDNode value from the global context. }
   LLVMMDNode: function(Vals: PLLVMValuePtrArray; Count: Cardinal): PLLVMValue; cdecl;

   { Obtain the underlying string from a MDString value.
        @param V Instance to obtain string from.
        @param Len Memory address which will hold length of returned string.
        @return String data in MDString. }
   LLVMGetMDString: function(V: PLLVMValue; Len: PCardinal): PAnsiChar; cdecl;

   { Obtain the number of operands from an MDNode value.
        @param V MDNode to get number of operands from.
        @return Number of operands of the MDNode. }
   LLVMGetMDNodeNumOperands: function(V: PLLVMValue): Cardinal; cdecl;

   { Obtain the given MDNode's operands.
     The passed PLLVMValue pointer should point to enough memory to hold all of
     the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
     LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
     MDNode's operands.
         @param V MDNode to get the operands from.
         @param Dest Destination array for operands. }
   LLVMGetMDNodeOperands: procedure(V: PLLVMValue; var Dest: PLLVMValue); cdecl;

   { Convert a basic block instance to a value type. }
   LLVMBasicBlockAsValue: function(BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { Determine whether a PLLVMValue is itself a basic block. }
   LLVMValueIsBasicBlock: function(Val: PLLVMValue): Boolean; cdecl;

   { Convert a PLLVMValue to a PLLVMBasicBlock instance. }
   LLVMValueAsBasicBlock: function(Val: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Obtain the function to which a basic block belongs. }
   LLVMGetBasicBlockParent: function(BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { Obtain the terminator instruction for a basic block.
     If the basic block does not have a terminator (it is not well-formed if
     it doesn't), then NULL is returned.
     The returned PLLVMValue corresponds to a llvm::TerminatorInst. }
   LLVMGetBasicBlockTerminator: function(BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { Obtain the number of basic blocks in a function.
        @param Fn Function value to operate on. }
   LLVMCountBasicBlocks: function(Fn: PLLVMValue): Cardinal; cdecl;

   { Obtain all of the basic blocks in a function.
     This operates on a function value. The BasicBlocks parameter is a
     pointer to a pre-allocated array of PLLVMBasicBlock of at least
     LLVMCountBasicBlocks in length. This array is populated with
     PLLVMBasicBlock instances. }
   LLVMGetBasicBlocks: procedure(Fn: PLLVMValue; BasicBlocks: PLLVMBasicBlockPtrArray); cdecl;

   { Obtain the first basic block in a function.
     The returned basic block can be used as an iterator. You will likely
     eventually call into LLVMGetNextBasicBlock with it. }
   LLVMGetFirstBasicBlock: function(Fn: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Obtain the last basic block in a function. }
   LLVMGetLastBasicBlock: function(Fn: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Advance a basic block iterator. }
   LLVMGetNextBasicBlock: function(BB: PLLVMBasicBlock): PLLVMBasicBlock; cdecl;

   { Go backwards in a basic block iterator. }
   LLVMGetPreviousBasicBlock: function(BB: PLLVMBasicBlock): PLLVMBasicBlock; cdecl;

   { Obtain the basic block that corresponds to the entry point of a
    function. }
   LLVMGetEntryBasicBlock: function(Fn: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Append a basic block to the end of a function. }
   LLVMAppendBasicBlockInContext: function(C: PLLVMContext; Fn: PLLVMValue; const Name: PAnsiChar): PLLVMBasicBlock; cdecl;

   { Append a basic block to the end of a function using the global
    context. }
   LLVMAppendBasicBlock: function(Fn: PLLVMValue; const Name: PAnsiChar): PLLVMBasicBlock; cdecl;

   { Insert a basic block in a function before another basic block.
     The function to add to is determined by the function of the
     passed basic block. }
   LLVMInsertBasicBlockInContext: function(C: PLLVMContext; BB: PLLVMBasicBlock; const Name: PAnsiChar): PLLVMBasicBlock; cdecl;

   { Insert a basic block in a function using the global context. }
   LLVMInsertBasicBlock: function(InsertBeforeBB: PLLVMBasicBlock; const Name: PAnsiChar): PLLVMBasicBlock; cdecl;

   { Remove a basic block from a function and delete it.
     This deletes the basic block from its containing function and deletes
     the basic block itself. }
   LLVMDeleteBasicBlock: procedure(BB: PLLVMBasicBlock); cdecl;

   { Remove a basic block from a function.
     This deletes the basic block from its containing function but keep
     the basic block alive. }
   LLVMRemoveBasicBlockFromParent: procedure(BB: PLLVMBasicBlock); cdecl;

   { Move a basic block to before another one. }
   LLVMMoveBasicBlockBefore: procedure(BB: PLLVMBasicBlock; MovePos: PLLVMBasicBlock); cdecl;

   { Move a basic block to after another one. }
   LLVMMoveBasicBlockAfter: procedure(BB: PLLVMBasicBlock; MovePos: PLLVMBasicBlock); cdecl;

   { Obtain the first instruction in a basic block.
    The returned PLLVMValue corresponds to a llvm::Instruction
    instance. }
   LLVMGetFirstInstruction: function(BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { Obtain the last instruction in a basic block.
     The returned PLLVMValue corresponds to a LLVM:Instruction. }
   LLVMGetLastInstruction: function(BB: PLLVMBasicBlock): PLLVMValue; cdecl;

   { Determine whether an instruction has any metadata attached. }
   LLVMHasMetadata: function(Val: PLLVMValue): Integer; cdecl;

   { Return metadata associated with an instruction value. }
   LLVMGetMetadata: function(Val: PLLVMValue; KindID: Cardinal): PLLVMValue; cdecl;

   { Set metadata associated with an instruction value. }
   LLVMSetMetadata: procedure(Val: PLLVMValue; KindID: Cardinal; Node: PLLVMValue); cdecl;

   { Obtain the basic block to which an instruction belongs. }
   LLVMGetInstructionParent: function(Inst: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Obtain the instruction that occurs after the one specified.
     The next instruction will be from the same basic block. If this is the
     last instruction in a basic block, NULL will be returned. }
   LLVMGetNextInstruction: function(Inst: PLLVMValue): PLLVMValue; cdecl;

   { Obtain the instruction that occurred before this one.
     If the instruction is the first instruction in a basic block, NULL will be
     returned. }
   LLVMGetPreviousInstruction: function(Inst: PLLVMValue): PLLVMValue; cdecl;

   { Remove and delete an instruction.
     The instruction specified is removed from its containing building block
     and then deleted. }
   LLVMInstructionEraseFromParent: procedure(Inst: PLLVMValue); cdecl;

   { Obtain the code opcode for an individual instruction. }
   LLVMGetInstructionOpcode: function(Inst: PLLVMValue): TLLVMOpcode; cdecl;

   { Obtain the predicate of an instruction.
     This is only valid for instructions that correspond to llvm::ICmpInst or
     llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp. }
   LLVMGetICmpPredicate: function(Inst: PLLVMValue): TLLVMIntPredicate; cdecl;

   { Set the calling convention for a call instruction.
     This expects an PLLVMValue that corresponds to a llvm::CallInst or
     llvm::InvokeInst. }
   LLVMSetInstructionCallConv: procedure(Instr: PLLVMValue; CC: Cardinal); cdecl;

   { Obtain the calling convention for a call instruction.
     This is the opposite of LLVMSetInstructionCallConv. Reads its
     usage. }
   LLVMGetInstructionCallConv: function(Instr: PLLVMValue): Cardinal; cdecl;

   LLVMAddInstrAttribute: procedure(Instr: PLLVMValue; Index: Cardinal; Unknown: TLLVMAttribute); cdecl;
   LLVMRemoveInstrAttribute: procedure(Instr: PLLVMValue; Index: Cardinal; Unknown: TLLVMAttribute); cdecl;
   LLVMSetInstrParamAlignment: procedure(Instr: PLLVMValue; Index: Cardinal; align: Cardinal); cdecl;

   { Obtain whether a call instruction is a tail call.
     This only works on llvm::CallInst instructions. }
   LLVMIsTailCall: function(CallInst: PLLVMValue): Boolean; cdecl;

   { Set whether a call instruction is a tail call.
     This only works on llvm::CallInst instructions. }
   LLVMSetTailCall: procedure(CallInst: PLLVMValue; IsTailCall: Boolean); cdecl;

   { Obtain the default destination basic block of a switch instruction.
     This only works on llvm::SwitchInst instructions. }
   LLVMGetSwitchDefaultDest: function(SwitchInstr: PLLVMValue): PLLVMBasicBlock; cdecl;

   { Add an incoming value to the end of a PHI list. }
   LLVMAddIncoming: procedure(PhiNode: PLLVMValue; IncomingValues: PLLVMValuePtrArray; IncomingBlocks: PLLVMBasicBlockPtrArray; Count: Cardinal); cdecl;

   { Obtain the number of incoming basic blocks to a PHI node. }
   LLVMCountIncoming: function(PhiNode: PLLVMValue): Cardinal; cdecl;

   { Obtain an incoming value to a PHI node as a PLLVMValue. }
   LLVMGetIncomingValue: function(PhiNode: PLLVMValue; Index: Cardinal): PLLVMValue; cdecl;

   { Obtain an incoming value to a PHI node as a PLLVMBasicBlock. }
   LLVMGetIncomingBlock: function(PhiNode: PLLVMValue; Index: Cardinal): PLLVMBasicBlock; cdecl;

   LLVMCreateBuilderInContext: function(C: PLLVMContext): PLLVMBuilder; cdecl;
   LLVMCreateBuilder: function: PLLVMBuilder; cdecl;
   LLVMPositionBuilder: procedure(Builder: PLLVMBuilder; Block: PLLVMBasicBlock; Instr: PLLVMValue); cdecl;
   LLVMPositionBuilderBefore: procedure(Builder: PLLVMBuilder; Instr: PLLVMValue); cdecl;
   LLVMPositionBuilderAtEnd: procedure(Builder: PLLVMBuilder; Block: PLLVMBasicBlock); cdecl;
   LLVMGetInsertBlock: function(Builder: PLLVMBuilder): PLLVMBasicBlock; cdecl;
   LLVMClearInsertionPosition: procedure(Builder: PLLVMBuilder); cdecl;
   LLVMInsertIntoBuilder: procedure(Builder: PLLVMBuilder; Instr: PLLVMValue); cdecl;
   LLVMInsertIntoBuilderWithName: procedure(Builder: PLLVMBuilder; Instr: PLLVMValue; const Name: PAnsiChar); cdecl;
   LLVMDisposeBuilder: procedure(Builder: PLLVMBuilder); cdecl;

   { Metadata }
   LLVMSetCurrentDebugLocation: procedure(Builder: PLLVMBuilder; L: PLLVMValue); cdecl;
   LLVMGetCurrentDebugLocation: function(Builder: PLLVMBuilder): PLLVMValue; cdecl;
   LLVMSetInstDebugLocation: procedure(Builder: PLLVMBuilder; Instr: PLLVMValue); cdecl;

   { Terminators }
   LLVMBuildRetVoid: function(Builder: PLLVMBuilder): PLLVMValue; cdecl;
   LLVMBuildRet: function(Builder: PLLVMBuilder; V: PLLVMValue): PLLVMValue; cdecl;
   LLVMBuildAggregateRet: function(Builder: PLLVMBuilder; var RetVals: PLLVMValuePtrArray; N: Cardinal): PLLVMValue; cdecl;
   LLVMBuildBr: function(Builder: PLLVMBuilder; Dest: PLLVMBasicBlock): PLLVMValue; cdecl;
   LLVMBuildCondBr: function(Builder: PLLVMBuilder; &If: PLLVMValue; &Then: PLLVMBasicBlock; &Else: PLLVMBasicBlock): PLLVMValue; cdecl;
   LLVMBuildSwitch: function(Builder: PLLVMBuilder; V: PLLVMValue; &Else: PLLVMBasicBlock; NumCases: Cardinal): PLLVMValue; cdecl;
   LLVMBuildIndirectBr: function(Builder: PLLVMBuilder; Addr: PLLVMValue; NumDests: Cardinal): PLLVMValue; cdecl;
   LLVMBuildInvoke: function(Builder: PLLVMBuilder; Fn: PLLVMValue; var Args: PLLVMValuePtrArray; NumArgs: Cardinal; &Then: PLLVMBasicBlock; Catch: PLLVMBasicBlock; const Name: PAnsiChar): PLLVMValue; cdecl;

   LLVMBuildLandingPad: function(Builder: PLLVMBuilder; Ty: PLLVMType; PersFn: PLLVMValue; NumClauses: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildResume: function(Builder: PLLVMBuilder; Exn: PLLVMValue): PLLVMValue; cdecl;
   LLVMBuildUnreachable: function(Builder: PLLVMBuilder): PLLVMValue; cdecl;

   { Add a case to the switch instruction }
   LLVMAddCase: procedure(Switch: PLLVMValue; OnVal: PLLVMValue; Dest: PLLVMBasicBlock); cdecl;

   { Add a destination to the indirectbr instruction }
   LLVMAddDestination: procedure(IndirectBr: PLLVMValue; Dest: PLLVMBasicBlock); cdecl;

   { Add a catch or filter clause to the landingpad instruction }
   LLVMAddClause: procedure(LandingPad: PLLVMValue; ClauseVal: PLLVMValue); cdecl;

   { Set the 'cleanup' flag in the landingpad instruction }
   LLVMSetCleanup: procedure(LandingPad: PLLVMValue; Val: Boolean); cdecl;

   { Arithmetic }
   LLVMBuildAdd: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNSWAdd: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNUWAdd: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFAdd: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSub: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNSWSub: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNUWSub: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFSub: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildMul: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNSWMul: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNUWMul: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFMul: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildUDiv: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSDiv: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildExactSDiv: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFDiv: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildURem: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSRem: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFRem: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildShl: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildLShr: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildAShr: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildAnd: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildOr: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildXor: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildBinOp: function(B: PLLVMBuilder; Op: TLLVMOpcode; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNeg: function(Builder: PLLVMBuilder; V: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNSWNeg: function(Builder: PLLVMBuilder; V: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNUWNeg: function(Builder: PLLVMBuilder; V: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFNeg: function(Builder: PLLVMBuilder; V: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildNot: function(Builder: PLLVMBuilder; V: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Memory }
   LLVMBuildMalloc: function(Builder: PLLVMBuilder; Ty: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildArrayMalloc: function(Builder: PLLVMBuilder; Ty: PLLVMType; Val: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildAlloca: function(Builder: PLLVMBuilder; Ty: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildArrayAlloca: function(Builder: PLLVMBuilder; Ty: PLLVMType; Val: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFree: function(Builder: PLLVMBuilder; PointerVal: PLLVMValue): PLLVMValue; cdecl;
   LLVMBuildLoad: function(Builder: PLLVMBuilder; PointerVal: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildStore: function(Builder: PLLVMBuilder; Val: PLLVMValue; Ptr: PLLVMValue): PLLVMValue; cdecl;
   LLVMBuildGEP: function(Builder: PLLVMBuilder; Pointer: PLLVMValue; Indices: PLLVMValuePtrArray; NumIndices: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildInBoundsGEP: function(Builder: PLLVMBuilder; Pointer: PLLVMValue; var Indices: PLLVMValuePtrArray; NumIndices: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildStructGEP: function(Builder: PLLVMBuilder; Pointer: PLLVMValue; Idx: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildGlobalString: function(Builder: PLLVMBuilder; Str: PAnsiChar; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildGlobalStringPtr: function(Builder: PLLVMBuilder; Str: PAnsiChar; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMGetVolatile: function(MemoryAccessInst: PLLVMValue): PLLVMValue; cdecl;
   LLVMSetVolatile: procedure(MemoryAccessInst: PLLVMValue; IsVolatile: Boolean); cdecl;

   { Casts }
   LLVMBuildTrunc: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildZExt: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSExt: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFPToUI: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFPToSI: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildUIToFP: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSIToFP: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFPTrunc: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFPExt: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildPtrToInt: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildIntToPtr: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildBitCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildZExtOrBitCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSExtOrBitCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildTruncOrBitCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildPointerCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildIntCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFPCast: function(Builder: PLLVMBuilder; Val: PLLVMValue; DestTy: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Comparisons }
   LLVMBuildICmp: function(Builder: PLLVMBuilder; Op: TLLVMIntPredicate; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildFCmp: function(Builder: PLLVMBuilder; Op: TLLVMRealPredicate; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Miscellaneous instructions }
   LLVMBuildPhi: function(Builder: PLLVMBuilder; Ty: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildCall: function(Builder: PLLVMBuilder; Fn: PLLVMValue; Args: PLLVMValuePtrArray; NumArgs: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildSelect: function(Builder: PLLVMBuilder; &If: PLLVMValue; &Then: PLLVMValue; &Else: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildVAArg: function(Builder: PLLVMBuilder; List: PLLVMValue; Ty: PLLVMType; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildExtractElement: function(Builder: PLLVMBuilder; VecVal: PLLVMValue; Index: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildInsertElement: function(Builder: PLLVMBuilder; VecVal: PLLVMValue; EltVal: PLLVMValue; Index: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildShuffleVector: function(Builder: PLLVMBuilder; V1: PLLVMValue; V2: PLLVMValue; Mask: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildExtractValue: function(Builder: PLLVMBuilder; AggVal: PLLVMValue; Index: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildInsertValue: function(Builder: PLLVMBuilder; AggVal: PLLVMValue; EltVal: PLLVMValue; Index: Cardinal; const Name: PAnsiChar): PLLVMValue; cdecl;

   LLVMBuildIsNull: function(Builder: PLLVMBuilder; Val: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildIsNotNull: function(Builder: PLLVMBuilder; Val: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;
   LLVMBuildPtrDiff: function(Builder: PLLVMBuilder; LHS: PLLVMValue; RHS: PLLVMValue; const Name: PAnsiChar): PLLVMValue; cdecl;

   { Changes the type of M so it can be passed to FunctionPassManagers and the
     JIT. They take ModuleProviders for historical reasons. }
   LLVMCreateModuleProviderForExistingModule: function(M: PLLVMModule): PLLVMModuleProvider; cdecl;

   { Destroys the module M. }
   LLVMDisposeModuleProvider: procedure(M: PLLVMModuleProvider); cdecl;

   LLVMCreateMemoryBufferWithContentsOfFile: function(Path: PAnsiChar; out OutMemBuf: PLLVMMemoryBuffer; out OutMessage: PAnsiChar): Boolean; cdecl;
   LLVMCreateMemoryBufferWithSTDIN: function(out OutMemBuf: PLLVMMemoryBuffer; out OutMessage: PAnsiChar): Boolean; cdecl;
   LLVMDisposeMemoryBuffer: procedure(MemBuf: PLLVMMemoryBuffer); cdecl;

   { Return the global pass registry, for use with initialization functions.}
   LLVMGetGlobalPassRegistry: function: PLLVMPassRegistry; cdecl;


   { Constructs a new whole-module pass pipeline. This type of pipeline is
     suitable for link-time optimization and whole-module transformations. }
   LLVMCreatePassManager: function: PLLVMPassManager; cdecl;

   { Constructs a new function-by-function pass pipeline over the module
     provider. It does not take ownership of the module provider. This type of
     pipeline is suitable for code generation and JIT compilation tasks. }
   LLVMCreateFunctionPassManagerForModule: function(M: PLLVMModule): PLLVMPassManager; cdecl;

   { Deprecated: Use LLVMCreateFunctionPassManagerForModule instead. }
   LLVMCreateFunctionPassManager: function(MP: PLLVMModuleProvider): PLLVMPassManager; cdecl;

   { Initializes, executes on the provided module, and finalizes all of the
     passes scheduled in the pass manager. Returns 1 if any of the passes
     modified the module, 0 otherwise. }
   LLVMRunPassManager: function(FPM: PLLVMPassManager; M: PLLVMModule): Boolean; cdecl;

   { Initializes all of the function passes scheduled in the function pass
     manager. Returns 1 if any of the passes modified the module, 0 otherwise. }
   LLVMInitializeFunctionPassManager: function(FPM: PLLVMPassManager): Boolean; cdecl;

   { Executes all of the function passes scheduled in the function pass manager
     on the provided function. Returns 1 if any of the passes modified the
     function, false otherwise. }
   LLVMRunFunctionPassManager: function(FPM: PLLVMPassManager; F: PLLVMValue): Boolean; cdecl;

   { Finalizes all of the function passes scheduled in in the function pass
     manager. Returns 1 if any of the passes modified the module, 0 otherwise. }
   LLVMFinalizeFunctionPassManager: function(FPM: PLLVMPassManager): Boolean; cdecl;

   { Frees the memory of a pass pipeline. For function pipelines, does not free
     the module provider. }
   LLVMDisposePassManager: procedure(PM: PLLVMPassManager); cdecl;

   { Verifies that a module is valid, taking the specified action if not.
     Optionally returns a human-readable description of any invalid constructs.
     OutMessage must be disposed with LLVMDisposeMessage. }
   LLVMVerifyModule: function(M: PLLVMModule; Action: TLLVMVerifierFailureAction; out OutMessage: PAnsiChar): Boolean; cdecl;

   { Verifies that a single function is valid, taking the specified action. Useful
     for debugging. }
   LLVMVerifyFunction: function(Fn: PLLVMValue; Action: TLLVMVerifierFailureAction): Boolean; cdecl;

   { Open up a ghostview window that displays the CFG of the current function.
     Useful for debugging. }
   LLVMViewFunctionCFG: procedure(Fn: PLLVMValue); cdecl;
   LLVMViewFunctionCFGOnly: procedure(Fn: PLLVMValue); cdecl;

   { Builds a module from the bitcode in the specified memory buffer, returning a
     reference to the module via the OutModule parameter. Returns 0 on success.
     Optionally returns a human-readable error message via OutMessage. }
   LLVMParseBitcode: function(MemBuf: PLLVMMemoryBuffer; out OutModule: PLLVMModule; out OutMessage: PAnsiChar): Boolean; cdecl;

   LLVMParseBitcodeInContext: function(ContextRef: PLLVMContext; MemBuf: PLLVMMemoryBuffer; out OutModule: PLLVMModule; out OutMessage: PAnsiChar): Boolean; cdecl;

   { Reads a module from the specified path, returning via the OutMP parameter
     a module provider which performs lazy deserialization. Returns 0 on success.
     Optionally returns a human-readable error message via OutMessage. }
   LLVMGetBitcodeModuleInContext: function(ContextRef: PLLVMContext; MemBuf: PLLVMMemoryBuffer; out OutM: PLLVMModule; out OutMessage: PAnsiChar): Boolean; cdecl;

   LLVMGetBitcodeModule: function(MemBuf: PLLVMMemoryBuffer; out OutM: PLLVMModule; out OutMessage: PAnsiChar): Boolean; cdecl;

   { Deprecated: Use LLVMGetBitcodeModuleInContext instead. }
   LLVMGetBitcodeModuleProviderInContext: function(ContextRef: PLLVMContext; MemBuf: PLLVMMemoryBuffer; out OutMP: PLLVMModuleProvider; out OutMessage: PAnsiChar): Boolean; cdecl;

   { Deprecated: Use LLVMGetBitcodeModule instead. }
   LLVMGetBitcodeModuleProvider: function(MemBuf: PLLVMMemoryBuffer; out OutMP: PLLVMModuleProvider; out OutMessage: PAnsiChar): Boolean; cdecl;

   { Writes a module to the specified path. Returns 0 on success. }
   LLVMWriteBitcodeToFile: function(M: PLLVMModule; Path: PAnsiChar): Integer; cdecl;

   { Writes a module to an open file descriptor. Returns 0 on success. }
   LLVMWriteBitcodeToFD: function(M: PLLVMModule; FD: Integer; ShouldClose: Integer; Unbuffered: Integer): Integer; cdecl;

   { Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
     descriptor. Returns 0 on success. Closes the Handle. }
   LLVMWriteBitcodeToFileHandle: function(M: PLLVMModule; Handle: Integer): Integer; cdecl;

   { Create a disassembler for the TripleName. Symbolic disassembly is
     supported by passing a block of information in the DisInfo parameter
     and specifying the
     TagType and callback functions as described above. These can all be passed
     as NULL. If successful, this returns a disassembler context.  If not, it
     returns NULL. }
   LLVMCreateDisasm: function(TripleName: PAnsiChar; DisInfo: Pointer; TagType: Integer; GetOpInfo: TLLVMOpInfoCallback; SymbolLookUp: TLLVMSymbolLookupCallback): PLLVMDisasmContext; cdecl;

   { Set the disassembler's options.  Returns 1 if it can set the Options and 0
     otherwise. }
   LLVMSetDisasmOptions: function(DC: PLLVMDisasmContext; Options: TLLVMDisassemblerOptions): Integer; cdecl;

   { Dispose of a disassembler context. }
   LLVMDisasmDispose: procedure(DC: PLLVMDisasmContext); cdecl;

   { Disassemble a single instruction using the disassembler context specified
     in the parameter DC.  The bytes of the instruction are specified in the
     parameter Bytes, and contains at least BytesSize number of bytes.  The
     instruction is at the address specified by the PC parameter.  If a valid
     instruction can be disassembled, its string is returned indirectly in
     OutString whose size is specified in the parameter OutStringSize.  This
     function returns the number of bytes in the instruction or zero if there
     was no valid instruction. }
   LLVMDisasmInstruction: function(DC: PLLVMDisasmContext; Bytes: PByte; BytesSize: UInt64; PC: UInt64; out OutString: PAnsiChar; OutStringSize: NativeUInt): NativeUInt; cdecl;

   { Execution Engine }

   LLVMLinkInJIT: procedure; cdecl;
   LLVMLinkInInterpreter: procedure; cdecl;

   { Operations on generic values }

   LLVMCreateGenericValueOfInt: function(Ty: PLLVMType; N: UInt64; IsSigned: Boolean): PLLVMGenericValue; cdecl;
   LLVMCreateGenericValueOfPointer: function(P: Pointer): PLLVMGenericValue; cdecl;
   LLVMCreateGenericValueOfFloat: function(Ty: PLLVMType; N: Double): PLLVMGenericValue; cdecl;
   LLVMGenericValueIntWidth: function(GenValRef: PLLVMGenericValue): Cardinal; cdecl;
   LLVMGenericValueToInt: function(GenVal: PLLVMGenericValue; IsSigned: Boolean): UInt64; cdecl;
   LLVMGenericValueToPointer: function(GenVal: PLLVMGenericValue): Pointer; cdecl;
   LLVMGenericValueToFloat: function(TyRef: PLLVMType; GenVal: PLLVMGenericValue): Double; cdecl;
   LLVMDisposeGenericValue: procedure(GenVal: PLLVMGenericValue); cdecl;

   { Operations on execution engines }

   LLVMCreateExecutionEngineForModule: function(out OutEE: PLLVMExecutionEngine; M: PLLVMModule; out OutError: PAnsiChar): Boolean; cdecl;
   LLVMCreateInterpreterForModule: function(out OutInterp: PLLVMExecutionEngine; M: PLLVMModule; out OutError: PAnsiChar): Boolean; cdecl;
   LLVMCreateJITCompilerForModule: function(out OutJIT: PLLVMExecutionEngine; M: PLLVMModule; OptLevel: TLLVMCodeGenOptLevel; out OutError: PAnsiChar): Boolean; cdecl;
   LLVMCreateExecutionEngine: function(out OutEE: PLLVMExecutionEngine; MP: PLLVMModuleProvider; out OutError: PAnsiChar): Boolean; cdecl; // -> deprecated, use LLVMCreateExecutionEngineForModule instead
   LLVMCreateInterpreter: function(out OutInterp: PLLVMExecutionEngine; MP: PLLVMModuleProvider; out OutError: PAnsiChar): Boolean; cdecl; // -> deprecated, use LLVMCreateInterpreterForModule instead
   LLVMCreateJITCompiler: function(out OutJIT: PLLVMExecutionEngine; MP: PLLVMModuleProvider; OptLevel: TLLVMCodeGenOptLevel; out OutError: PAnsiChar): Boolean; cdecl; // -> deprecated, use LLVMCreateJITCompilerForModule instead
   LLVMDisposeExecutionEngine: procedure(EE: PLLVMExecutionEngine); cdecl;
   LLVMRunStaticConstructors: procedure(EE: PLLVMExecutionEngine); cdecl;
   LLVMRunStaticDestructors: procedure(EE: PLLVMExecutionEngine); cdecl;
   LLVMRunFunctionAsMain: function(EE: PLLVMExecutionEngine; F: PLLVMValue; ArgC: Cardinal; var ArgV: PAnsiChar; var EnvP: PAnsiChar): Integer; cdecl;
   LLVMRunFunction: function(EE: PLLVMExecutionEngine; F: PLLVMValue; NumArgs: Cardinal; var Args: PLLVMGenericValue): PLLVMGenericValue; cdecl;
   LLVMFreeMachineCodeForFunction: procedure(EE: PLLVMExecutionEngine; F: PLLVMValue); cdecl;
   LLVMAddModule: procedure(EE: PLLVMExecutionEngine; M: PLLVMModule); cdecl;
   LLVMAddModuleProvider: procedure(EE: PLLVMExecutionEngine; MP: PLLVMModuleProvider); cdecl; // -> deprecated, use LLVMAddModule instead
   LLVMRemoveModule: function(EE: PLLVMExecutionEngine; M: PLLVMModule; out OutMod: PLLVMModule; out OutError: PAnsiChar): Boolean; cdecl;
   LLVMRemoveModuleProvider: function(EE: PLLVMExecutionEngine; MP: PLLVMModuleProvider; out OutMod: PLLVMModule; out OutError: PAnsiChar): Boolean; cdecl; // -> deprecated, use LLVMRemoveModule instead
   LLVMFindFunction: function(EE: PLLVMExecutionEngine; const Name: PAnsiChar; out OutFn: PLLVMValue): Boolean; cdecl;
   LLVMRecompileAndRelinkFunction: function(EE: PLLVMExecutionEngine; Fn: PLLVMValue): Pointer; cdecl;
   LLVMGetExecutionEngineTargetData: function(EE: PLLVMExecutionEngine): PLLVMTargetData; cdecl;
   LLVMAddGlobalMapping: procedure(EE: PLLVMExecutionEngine; Global: PLLVMValue; Addr: Pointer); cdecl;
   LLVMGetPointerToGlobal: function(EE: PLLVMExecutionEngine; Global: PLLVMValue): Pointer; cdecl;

   LLVMAddAggressiveDCEPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddCFGSimplificationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddDeadStoreEliminationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddGVNPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddIndVarSimplifyPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddInstructionCombiningPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddJumpThreadingPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLICMPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopDeletionPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopIdiomPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopRotatePass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopUnrollPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopUnswitchPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddMemCpyOptPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddPromoteMemoryToRegisterPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddReassociatePass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddSCCPPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddScalarReplAggregatesPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddScalarReplAggregatesPassSSA: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddScalarReplAggregatesPassWithThreshold: procedure(PM: PLLVMPassManager;
      Threshold: Integer); cdecl;
   LLVMAddSimplifyLibCallsPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddTailCallEliminationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddConstantPropagationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddDemoteMemoryToRegisterPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddVerifierPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddCorrelatedValuePropagationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddEarlyCSEPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLowerExpectIntrinsicPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddTypeBasedAliasAnalysisPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddBasicAliasAnalysisPass: procedure(PM: PLLVMPassManager); cdecl;

   LLVMAddBBVectorizePass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddLoopVectorizePass: procedure(PM: PLLVMPassManager); cdecl;

   LLVMAddArgumentPromotionPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddConstantMergePass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddDeadArgEliminationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddFunctionAttrsPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddFunctionInliningPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddAlwaysInlinerPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddGlobalDCEPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddGlobalOptimizerPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddIPConstantPropagationPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddPruneEHPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddIPSCCPPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddInternalizePass: procedure(PM: PLLVMPassManager; AllButMain: Cardinal); cdecl;
   LLVMAddStripDeadPrototypesPass: procedure(PM: PLLVMPassManager); cdecl;
   LLVMAddStripSymbolsPass: procedure(PM: PLLVMPassManager); cdecl;

   LLVMPassManagerBuilderCreate: function: PLLVMPassManagerBuilder; cdecl;
   LLVMPassManagerBuilderDispose: procedure(PMB: PLLVMPassManagerBuilder); cdecl;
   LLVMPassManagerBuilderSetOptLevel: procedure(PMB: PLLVMPassManagerBuilder; OptLevel: TLLVMCodeGenOptLevel); cdecl;
   LLVMPassManagerBuilderSetSizeLevel: procedure(PMB: PLLVMPassManagerBuilder; SizeLevel: Cardinal); cdecl;
   LLVMPassManagerBuilderSetDisableUnitAtATime: procedure(PMB: PLLVMPassManagerBuilder; Value: Boolean); cdecl;
   LLVMPassManagerBuilderSetDisableUnrollLoops: procedure(PMB: PLLVMPassManagerBuilder; Value: Boolean); cdecl;
   LLVMPassManagerBuilderSetDisableSimplifyLibCalls: procedure(PMB: PLLVMPassManagerBuilder; Value: Boolean); cdecl;
   LLVMPassManagerBuilderUseInlinerWithThreshold: procedure(PMB: PLLVMPassManagerBuilder; Threshold: Cardinal); cdecl;
   LLVMPassManagerBuilderPopulateFunctionPassManager: procedure(PMB: PLLVMPassManagerBuilder; PM: PLLVMPassManager); cdecl;
   LLVMPassManagerBuilderPopulateModulePassManager: procedure(PMB: PLLVMPassManagerBuilder; PM: PLLVMPassManager); cdecl;
   LLVMPassManagerBuilderPopulateLTOPassManager: procedure(PMB: PLLVMPassManagerBuilder; PM: PLLVMPassManager; Internalize, RunInliner: Boolean); cdecl;

   { LLVMInitializeX86* }
   LLVMInitializeX86TargetInfo: procedure; cdecl;
   LLVMInitializeX86Target: procedure; cdecl;
   LLVMInitializeX86TargetMC: procedure; cdecl;
   LLVMInitializeX86AsmPrinter: procedure; cdecl;
   LLVMInitializeX86AsmParser: procedure; cdecl;
   LLVMInitializeX86Disassembler: procedure; cdecl;

   { LLVMInitializeAllTargetInfos - The main program should call this function
     if it wants access to all available targets that LLVM is configured to
     support. }
   LLVMInitializeAllTargetInfos: procedure; cdecl;

   { LLVMInitializeAllTargets - The main program should call this function if it
    wants to link in all available targets that LLVM is configured to support. }
   LLVMInitializeAllTargets: procedure; cdecl;

   { LLVMInitializeAllTargetMCs - The main program should call this function if
    it wants access to all available target MC that LLVM is configured to
    support. }
   LLVMInitializeAllTargetMCs: procedure; cdecl;

   { LLVMInitializeAllAsmPrinters - The main program should call this function
     if it wants all asm printers that LLVM is configured to support, to make
     them available via the TargetRegistry. }
   LLVMInitializeAllAsmPrinters: procedure; cdecl;

   { LLVMInitializeAllAsmParsers - The main program should call this function
     if it wants all asm parsers that LLVM is configured to support, to make
     them available via the TargetRegistry. }
   LLVMInitializeAllAsmParsers: procedure; cdecl;

   { LLVMInitializeAllDisassemblers - The main program should call this function
     if it wants all disassemblers that LLVM is configured to support, to make
     them available via the TargetRegistry. }
   LLVMInitializeAllDisassemblers: procedure; cdecl;

   { LLVMInitializeNativeTarget - The main program should call this function to
    initialize the native target corresponding to the host. This is useful
    for JIT applications to ensure that the target gets linked in correctly. }
   LLVMInitializeNativeTarget: function: Boolean; cdecl;

   { Creates target data from a target layout string. }
   LLVMCreateTargetData: function(StringRep: PAnsiChar): PLLVMTargetData; cdecl;

   { Deallocates a TargetData. }
   LLVMDisposeTargetData: procedure(TargetData: PLLVMTargetData); cdecl;

   { Returns the first llvm::Target in the registered targets list. }
   LLVMGetFirstTarget: function: PLLVMTarget; cdecl;

   { Returns the next llvm::Target given a previous one (or null if there's none) }
   LLVMGetNextTarget: function(T: PLLVMTarget): PLLVMTarget; cdecl;

   { Returns the name of a target. See llvm::Target::getName }
   LLVMGetTargetName: function(T: PLLVMTarget): PAnsiChar; cdecl;

   { Returns the description  of a target. See llvm::Target::getDescription }
   LLVMGetTargetDescription: function (T: PLLVMTarget): PAnsiChar; cdecl;

   { Returns if the target has a JIT }
   LLVMTargetHasJIT: function(T: PLLVMTarget): Boolean; cdecl;

   { Returns if the target has a TargetMachine associated }
   LLVMTargetHasTargetMachine: function(T: PLLVMTarget): Boolean; cdecl;

   { Returns if the target as an ASM backend (required for emitting output) }
   LLVMTargetHasAsmBackend: function(T: PLLVMTarget): Boolean; cdecl;

   { Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine }
   LLVMCreateTargetMachine: function(T: PLLVMTarget;
      Triple: PAnsiChar; CPU: PAnsiChar; Features: PAnsiChar;
      Level: TLLVMCodeGenOptLevel; Reloc: TLLVMRelocMode;
      CodeModel: TLLVMCodeModel): PLLVMTargetMachine; cdecl;

   { Dispose the PLLVMTargetMachine instance generated by
     LLVMCreateTargetMachine. }
   LLVMDisposeTargetMachine: procedure(T: PLLVMTargetMachine); cdecl;

   { Returns the Target used in a TargetMachine }
   LLVMGetTargetMachineTarget: function (T: PLLVMTargetMachine): PLLVMTarget; cdecl;

   { Returns the triple used creating this target machine. See
     llvm::TargetMachine::getTriple. The result needs to be disposed with
     LLVMDisposeMessage. }
   LLVMGetTargetMachineTriple: function(T: PLLVMTargetMachine): PAnsiChar; cdecl;

   { Returns the cpu used creating this target machine. See
     llvm::TargetMachine::getCPU. The result needs to be disposed with
     LLVMDisposeMessage. }
   LLVMGetTargetMachineCPU: function(T: PLLVMTargetMachine): PAnsiChar; cdecl;

   { Returns the feature string used creating this target machine. See
     llvm::TargetMachine::getFeatureString. The result needs to be disposed with
     LLVMDisposeMessage. }
   LLVMGetTargetMachineFeatureString: function(T: PLLVMTargetMachine): PAnsiChar; cdecl;

   { Returns the llvm::DataLayout used for this llvm:TargetMachine. }
   LLVMGetTargetMachineData: function(T: PLLVMTargetMachine): PLLVMTargetData; cdecl;

   { Emits an asm or object file for the given module to the filename. This
   wraps several c++ only classes (among them a file stream). Returns any
   error in ErrorMessage. Use LLVMDisposeMessage to dispose the message. }
   LLVMTargetMachineEmitToFile: function(T: PLLVMTargetMachine; M: PLLVMModule;
      Filename: PAnsiChar; Codegen: TLLVMCodeGenFileType;
      var ErrorMessage: PAnsiChar): PLLVMTargetData; cdecl;

   { ObjectFile creation }
   LLVMCreateObjectFile: function(MemBuf: PLLVMMemoryBuffer): PLLVMObjectFile; cdecl;
   LLVMDisposeObjectFile: procedure(ObjectFile: PLLVMObjectFile); cdecl;

   { ObjectFile Section iterators }
   LLVMGetSections: function(ObjectFile: PLLVMObjectFile): PLLVMSectionIterator; cdecl;
   LLVMDisposeSectionIterator: procedure(SI: PLLVMSectionIterator); cdecl;
   LLVMIsSectionIteratorAtEnd: function(ObjectFile: PLLVMObjectFile; SI: PLLVMSectionIterator): Boolean; cdecl;
   LLVMMoveToNextSection: procedure(SI: PLLVMSectionIterator); cdecl;
   LLVMMoveToContainingSection: procedure(Sect: PLLVMSectionIterator; Sym: PLLVMSymbolIterator); cdecl;

   { ObjectFile Symbol iterators }
   LLVMGetSymbols: function(ObjectFile: PLLVMObjectFile): PLLVMSymbolIterator; cdecl;
   LLVMDisposeSymbolIterator: procedure(SI: PLLVMSymbolIterator); cdecl;
   LLVMIsSymbolIteratorAtEnd: function(ObjectFile: PLLVMObjectFile; SI: PLLVMSymbolIterator): Boolean; cdecl;
   LLVMMoveToNextSymbol: procedure(SI: PLLVMSymbolIterator); cdecl;

   { SectionRef accessors }
   LLVMGetSectionName: function(SI: PLLVMSectionIterator): PAnsiChar; cdecl;
   LLVMGetSectionSize: function(SI: PLLVMSectionIterator): UInt64; cdecl;
   LLVMGetSectionContents: function(SI: PLLVMSectionIterator): PAnsiChar; cdecl;
   LLVMGetSectionAddress: function(SI: PLLVMSectionIterator): UInt64; cdecl;
   LLVMGetSectionContainsSymbol: function(SI: PLLVMSectionIterator; Sym: PLLVMSymbolIterator): Boolean; cdecl;

   { Section Relocation iterators }
   LLVMGetRelocations: function(Section: PLLVMSectionIterator): PLLVMRelocationIterator; cdecl;
   LLVMDisposeRelocationIterator: procedure(RI: PLLVMRelocationIterator); cdecl;
   LLVMIsRelocationIteratorAtEnd: function(Section: PLLVMSectionIterator; RI: PLLVMRelocationIterator): Boolean; cdecl;
   LLVMMoveToNextRelocation: procedure(RI: PLLVMRelocationIterator); cdecl;

   { SymbolRef accessors }
   LLVMGetSymbolName: function(SI: PLLVMSymbolIterator): PAnsiChar; cdecl;
   LLVMGetSymbolAddress: function(SI: PLLVMSymbolIterator): UInt64; cdecl;
   LLVMGetSymbolFileOffset: function(SI: PLLVMSymbolIterator): UInt64; cdecl;
   LLVMGetSymbolSize: function(SI: PLLVMSymbolIterator): UInt64; cdecl;

   { RelocationRef accessors }
   LLVMGetRelocationAddress: function(RI: PLLVMRelocationIterator): UInt64; cdecl;
   LLVMGetRelocationOffset: function(RI: PLLVMRelocationIterator): UInt64; cdecl;
   LLVMGetRelocationSymbol: function(RI: PLLVMRelocationIterator): PLLVMSymbolIterator; cdecl;
   LLVMGetRelocationType: function(RI: PLLVMRelocationIterator): UInt64; cdecl;

   { NOTE: Caller takes ownership of returned string of the two following functions. }
   LLVMGetRelocationTypeName: function(RI: PLLVMRelocationIterator): PAnsiChar; cdecl;
   LLVMGetRelocationValueString: function(RI: PLLVMRelocationIterator): PAnsiChar; cdecl;

   { Adds target data information to a pass manager. This does not take ownership
    of the target data. }
   LLVMAddTargetData: procedure(TargetData: PLLVMTargetData; PassManager: PLLVMPassManager); cdecl;

   { Adds target library information to a pass manager. This does not take
    ownership of the target library info. }
   LLVMAddTargetLibraryInfo: procedure(TargetData: PLLVMTargetLibraryInfo; PassManager: PLLVMPassManager); cdecl;

   { Converts target data to a target layout string. The string must be disposed
    with LLVMDisposeMessage. }
   LLVMCopyStringRepOfTargetData: function(TargetData: PLLVMTargetData): PAnsiChar; cdecl;

   { Returns the byte order of a target, either LLVMBigEndian or
    LLVMLittleEndian. }
   LLVMByteOrder: function(TargetData: PLLVMTargetData): TLLVMByteOrdering; cdecl;

   { Returns the pointer size in bytes for a target. }
   LLVMPointerSize: function(TargetData: PLLVMTargetData): Cardinal; cdecl;

   { Returns the pointer size in bytes for a target for a specified
     address space. }
   LLVMPointerSizeForAS: function(TargetData: PLLVMTargetData; AddressSpace: Cardinal): Cardinal; cdecl;

   { Returns the integer type that is the same size as a pointer on a target. }
   LLVMIntPtrType: function(TargetData: PLLVMTargetData): PLLVMType; cdecl;

   { Returns the integer type that is the same size as a pointer on a target.
     This version allows the address space to be specified. }
   LLVMIntPtrTypeForAS: function(TargetData: PLLVMTargetData; AddressSpace: Cardinal): PLLVMType; cdecl;

   { Computes the size of a type in bytes for a target. }
   LLVMSizeOfTypeInBits: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): UInt64; cdecl;

   { Computes the storage size of a type in bytes for a target. }
   LLVMStoreSizeOfType: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): UInt64; cdecl;

   { Computes the ABI size of a type in bytes for a target. }
   LLVMABISizeOfType: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): UInt64; cdecl;

   { Computes the ABI alignment of a type in bytes for a target. }
   LLVMABIAlignmentOfType: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): Cardinal; cdecl;

   { Computes the call frame alignment of a type in bytes for a target. }
   LLVMCallFrameAlignmentOfType: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): Cardinal; cdecl;

   { Computes the preferred alignment of a type in bytes for a target. }
   LLVMPreferredAlignmentOfType: function(TargetData: PLLVMTargetData; Unknown: PLLVMType): Cardinal; cdecl;

   { Computes the preferred alignment of a global variable in bytes for a
     target. }
   LLVMPreferredAlignmentOfGlobal: function(TargetData: PLLVMTargetData; GlobalVar: PLLVMValue): Cardinal; cdecl;

   { Computes the structure element that contains the byte offset for a
     target. }
   LLVMElementAtOffset: function(TargetData: PLLVMTargetData; StructTy: PLLVMType; Offset: UInt64): Cardinal; cdecl;

   { Computes the byte offset of the indexed struct element for a target. }
   LLVMOffsetOfElement: function(TargetData: PLLVMTargetData; StructTy: PLLVMType; Element: Cardinal): UInt64; cdecl;

   { Returns a printable string. }
   LTOGetVersion: function: PAnsiChar; cdecl;

   { Returns the last error string or NULL if last operation was successful. }
   LTOGetErrorMessage: function: PAnsiChar; cdecl;

   { Checks if a file is a loadable object file. }
   LTOModuleIsObjectFile: function(path: PAnsiChar): Boolean; cdecl;

   { Checks if a file is a loadable object compiled for requested target. }
   LTOModuleIsObjectFileForTarget: function(path: PAnsiChar; targetTriplePrefix: PAnsiChar): Boolean; cdecl;

   { Checks if a buffer is a loadable object file. }
   LTOModuleIsObjectFileInMemory: function(mem: Pointer; length: NativeUInt): Boolean; cdecl;

   { Checks if a buffer is a loadable object compiled for requested target. }
   LTOModuleIsObjectFileInMemoryForTarget: function (mem: Pointer; length: NativeUInt; targetTriplePrefix: PAnsiChar): Boolean; cdecl;

   { Loads an object file from disk.
     Returns NULL on error. }
   LTOModuleCreate: function(path: PAnsiChar): PLTOModule; cdecl;

   { Loads an object file from memory.
     Returns NULL on error. }
   LTOModuleCreateFromMemory: function(mem: Pointer; length: NativeUInt): PLTOModule; cdecl;

   { Loads an object file from disk. The seek point of fd is not preserved.
     Returns NULL on error. }
   LTOModuleCreateFromFd: function(fd: Integer; path: PAnsiChar; fileSize: NativeUInt): PLTOModule; cdecl;

   { Loads an object file from disk. The seek point of fd is not preserved.
     Returns NULL on error. }
   LTOModuleCreateFromFdAtOffset: function(fd: Integer; path: PAnsiChar; fileSize: NativeUInt; mapSize: NativeUInt; offset: Int64): PLTOModule; cdecl;

   { Frees all memory internally allocated by the module.
     Upon return the PLTOModule is no longer valid. }
   LTOModuleDispose: procedure(Module: PLTOModule); cdecl;

   { Returns triple string which the object module was compiled under. }
   LTOModuleGetTargetTriple: function(module: PLTOModule): PAnsiChar; cdecl;

   { Sets triple string with which the object will be codegened. }
   LTOModuleSetTargetTriple: procedure(module: PLTOModule; triple: PAnsiChar); cdecl;

   { Returns the number of symbols in the object module. }
   LTOModuleGetNumSymbols: function(module: PLTOModule): Integer; cdecl;

   { Returns the name of the ith symbol in the object module. }
   LTOModuleGetSymbolName: function(module: PLTOModule; index: Cardinal): PAnsiChar; cdecl;

   { Returns the attributes of the ith symbol in the object module. }
   LTOModuleGetSymbolAttribute: function(module: PLTOModule; index: Cardinal): TLTOSymbolAttributes; cdecl;

   { Instantiates a code generator.
     Returns NULL on error. }
   LTOCodegenCreate: function: PLTOCodeGenerator; cdecl;

   { Frees all code generator and all memory it internally allocated.
     Upon return the PLTOCodeGenerator is no longer valid. }
   LTOCodegenDispose: procedure(Unknown: PLTOCodeGenerator); cdecl;

   { Add an object module to the set of modules for which code will be generated.
     Returns true on error. }
   LTOCodegenAddModule: function(cg: PLTOCodeGenerator; &mod: PLTOModule): Boolean; cdecl;

   { Sets if debug info should be generated.
     Returns true on error. }
   LTOCodegenSetDebugModel: function(cg: PLTOCodeGenerator; Unknown: TLTODebugModel): Boolean; cdecl;

   { Sets which PIC code model to generated.
     Returns true on error. }
   LTOCodegenSetPicModel: function(cg: PLTOCodeGenerator; Unknown: TLTOCodegenModel): Boolean; cdecl;

   { Sets the cpu to generate code for. }
   LTOCodegenSetCpu: procedure(cg: PLTOCodeGenerator; cpu: PAnsiChar); cdecl;

   { Sets the location of the assembler tool to run. If not set, libLTO
     will use gcc to invoke the assembler. }
   LTOCodegenSetAssemblerPath: procedure(cg: PLTOCodeGenerator; path: PAnsiChar); cdecl;

   { Sets extra arguments that libLTO should pass to the assembler. }
   LTOCodegenSetAssemblerArgs: procedure(cg: PLTOCodeGenerator; var args: PAnsiChar; nargs: Integer); cdecl;

   { Adds to a list of all global symbols that must exist in the final
     generated code.  If a function is not listed, it might be
     inlined into every usage and optimized away. }
   LTOCodegenAddMustPreserveSymbol: procedure(cg: PLTOCodeGenerator; symbol: PAnsiChar); cdecl;

   { Writes a new object file at the specified path that contains the merged
     contents of all modules added so far.
     Returns true on error. }
   LTOCodegenWriteMergedModules: function(cg: PLTOCodeGenerator; path: PAnsiChar): Boolean; cdecl;

   { Generates code for all added modules into one native object file.
     On success returns a pointer to a generated mach-o/ELF buffer and
     length set to the buffer size.  The buffer is owned by the
     PLTOCodeGenerator and will be freed when lto_codegen_dispose()
     is called, or lto_codegen_compile() is called again.
     On failure, returns NULL. }
   LTOCodegenCompile: function(cg: PLTOCodeGenerator; length: pNativeUInt): Pointer; cdecl;

   { Generates code for all added modules into one native object file.
     The name of the file is written to name. Returns true on error. }
   LTOCodegenCompileToFile: function(cg: PLTOCodeGenerator; var name: PAnsiChar): Boolean; cdecl;

   { Sets options to help debug codegen bugs. }
   LTOCodegenDebugOptions: procedure(cg: PLTOCodeGenerator; unknown: PAnsiChar); cdecl;

function IsLLVMLoaded: Boolean;
procedure LoadLLVM;
procedure UnloadLLVM;

implementation

resourcestring
  RStrCannotLoadLLVMLib = 'Cannot load LLVM library! (%s)';
  RStrFunctionNotFound = '%s not found!';

var
   GLLVMLibraryName: TFileName;
   GLLVMLibrary: THandle = 0;

function IsLLVMLoaded: Boolean;
begin
   Result := (GLLVMLibrary <> 0);
end;

procedure LoadLLVM;

   function Bind(const aFuncName: string): Pointer;
   begin
      Result := GetProcAddress(GLLVMLibrary, PAnsiChar(AnsiString(aFuncName)));
      if not Assigned(Result) then
      begin
         Result := nil;
         OutputDebugString(PWideChar(Format(RStrFunctionNotFound, [aFuncName])));
      end;
   end;

begin
   GLLVMLibraryName := CLLVMLibraryPrefix + '-' +
        IntToStr(CLLVMLibraryMajorVersion) + '.' +
        IntToStr(CLLVMLibraryMinorVersion) + '-' +
        CLLVMLibraryPlatform + CLLVMLibraryExtension;

   if not IsLLVMLoaded then
      GLLVMLibrary := LoadLibrary(PWideChar(GLLVMLibraryName));

   if (GLLVMLibrary <> 0) then
   begin
      @LLVMInitializeCore := Bind('LLVMInitializeCore');
      @LLVMDisposeMessage := Bind('LLVMDisposeMessage');
      @LLVMContextCreate := Bind('LLVMContextCreate');
      @LLVMGetGlobalContext := Bind('LLVMGetGlobalContext');
      @LLVMContextDispose := Bind('LLVMContextDispose');
      @LLVMGetMDKindIDInContext := Bind('LLVMGetMDKindIDInContext');
      @LLVMGetMDKindID := Bind('LLVMGetMDKindID');
      @LLVMModuleCreateWithName := Bind('LLVMModuleCreateWithName');
      @LLVMModuleCreateWithNameInContext := Bind('LLVMModuleCreateWithNameInContext');
      @LLVMDisposeModule := Bind('LLVMDisposeModule');
      @LLVMGetDataLayout := Bind('LLVMGetDataLayout');
      @LLVMSetDataLayout := Bind('LLVMSetDataLayout');
      @LLVMGetTarget := Bind('LLVMGetTarget');
      @LLVMSetTarget := Bind('LLVMSetTarget');
      @LLVMDumpModule := Bind('LLVMDumpModule');
      @LLVMPrintModuleToFile := Bind('LLVMPrintModuleToFile');
      @LLVMSetModuleInlineAsm := Bind('LLVMSetModuleInlineAsm');
      @LLVMGetModuleContext := Bind('LLVMGetModuleContext');
      @LLVMGetTypeByName := Bind('LLVMGetTypeByName');
      @LLVMGetNamedMetadataNumOperands := Bind('LLVMGetNamedMetadataNumOperands');
      @LLVMGetNamedMetadataOperands := Bind('LLVMGetNamedMetadataOperands');
      @LLVMAddNamedMetadataOperand := Bind('LLVMAddNamedMetadataOperand');
      @LLVMAddFunction := Bind('LLVMAddFunction');
      @LLVMGetNamedFunction := Bind('LLVMGetNamedFunction');
      @LLVMGetFirstFunction := Bind('LLVMGetFirstFunction');
      @LLVMGetLastFunction := Bind('LLVMGetLastFunction');
      @LLVMGetNextFunction := Bind('LLVMGetNextFunction');
      @LLVMGetPreviousFunction := Bind('LLVMGetPreviousFunction');
      @LLVMGetTypeKind := Bind('LLVMGetTypeKind');
      @LLVMTypeIsSized := Bind('LLVMTypeIsSized');
      @LLVMGetTypeContext := Bind('LLVMGetTypeContext');
      @LLVMInt1TypeInContext := Bind('LLVMInt1TypeInContext');
      @LLVMInt8TypeInContext := Bind('LLVMInt8TypeInContext');
      @LLVMInt16TypeInContext := Bind('LLVMInt16TypeInContext');
      @LLVMInt32TypeInContext := Bind('LLVMInt32TypeInContext');
      @LLVMInt64TypeInContext := Bind('LLVMInt64TypeInContext');
      @LLVMIntTypeInContext := Bind('LLVMIntTypeInContext');
      @LLVMInt1Type := Bind('LLVMInt1Type');
      @LLVMInt8Type := Bind('LLVMInt8Type');
      @LLVMInt16Type := Bind('LLVMInt16Type');
      @LLVMInt32Type := Bind('LLVMInt32Type');
      @LLVMInt64Type := Bind('LLVMInt64Type');
      @LLVMIntType := Bind('LLVMIntType');
      @LLVMGetIntTypeWidth := Bind('LLVMGetIntTypeWidth');
      @LLVMHalfTypeInContext := Bind('LLVMHalfTypeInContext');
      @LLVMFloatTypeInContext := Bind('LLVMFloatTypeInContext');
      @LLVMDoubleTypeInContext := Bind('LLVMDoubleTypeInContext');
      @LLVMX86FP80TypeInContext := Bind('LLVMX86FP80TypeInContext');
      @LLVMFP128TypeInContext := Bind('LLVMFP128TypeInContext');
      @LLVMPPCFP128TypeInContext := Bind('LLVMPPCFP128TypeInContext');
      @LLVMHalfType := Bind('LLVMHalfType');
      @LLVMFloatType := Bind('LLVMFloatType');
      @LLVMDoubleType := Bind('LLVMDoubleType');
      @LLVMX86FP80Type := Bind('LLVMX86FP80Type');
      @LLVMFP128Type := Bind('LLVMFP128Type');
      @LLVMPPCFP128Type := Bind('LLVMPPCFP128Type');
      @LLVMFunctionType := Bind('LLVMFunctionType');
      @LLVMIsFunctionVarArg := Bind('LLVMIsFunctionVarArg');
      @LLVMGetReturnType := Bind('LLVMGetReturnType');
      @LLVMCountParamTypes := Bind('LLVMCountParamTypes');
      @LLVMGetParamTypes := Bind('LLVMGetParamTypes');
      @LLVMStructTypeInContext := Bind('LLVMStructTypeInContext');
      @LLVMStructType := Bind('LLVMStructType');
      @LLVMStructCreateNamed := Bind('LLVMStructCreateNamed');
      @LLVMGetStructName := Bind('LLVMGetStructName');
      @LLVMStructSetBody := Bind('LLVMStructSetBody');
      @LLVMCountStructElementTypes := Bind('LLVMCountStructElementTypes');
      @LLVMGetStructElementTypes := Bind('LLVMGetStructElementTypes');
      @LLVMIsPackedStruct := Bind('LLVMIsPackedStruct');
      @LLVMIsOpaqueStruct := Bind('LLVMIsOpaqueStruct');
      @LLVMGetElementType := Bind('LLVMGetElementType');
      @LLVMArrayType := Bind('LLVMArrayType');
      @LLVMGetArrayLength := Bind('LLVMGetArrayLength');
      @LLVMPointerType := Bind('LLVMPointerType');
      @LLVMGetPointerAddressSpace := Bind('LLVMGetPointerAddressSpace');
      @LLVMVectorType := Bind('LLVMVectorType');
      @LLVMGetVectorSize := Bind('LLVMGetVectorSize');
      @LLVMVoidTypeInContext := Bind('LLVMVoidTypeInContext');
      @LLVMLabelTypeInContext := Bind('LLVMLabelTypeInContext');
      @LLVMX86MMXTypeInContext := Bind('LLVMX86MMXTypeInContext');
      @LLVMVoidType := Bind('LLVMVoidType');
      @LLVMLabelType := Bind('LLVMLabelType');
      @LLVMX86MMXType := Bind('LLVMX86MMXType');
      @LLVMTypeOf := Bind('LLVMTypeOf');
      @LLVMGetValueName := Bind('LLVMGetValueName');
      @LLVMSetValueName := Bind('LLVMSetValueName');
      @LLVMDumpValue := Bind('LLVMDumpValue');
      @LLVMReplaceAllUsesWith := Bind('LLVMReplaceAllUsesWith');
      @LLVMIsConstant := Bind('LLVMIsConstant');
      @LLVMIsUndef := Bind('LLVMIsUndef');
      @LLVMIsAArgument := Bind('LLVMIsAArgument');
      @LLVMIsABasicBlock := Bind('LLVMIsABasicBlock');
      @LLVMIsAInlineAsm := Bind('LLVMIsAInlineAsm');
      @LLVMIsAMDNode := Bind('LLVMIsAMDNode');
      @LLVMIsAMDString := Bind('LLVMIsAMDString');
      @LLVMIsAUser := Bind('LLVMIsAUser');
      @LLVMIsAConstant := Bind('LLVMIsAConstant');
      @LLVMIsABlockAddress := Bind('LLVMIsABlockAddress');
      @LLVMIsAConstantAggregateZero := Bind('LLVMIsAConstantAggregateZero');
      @LLVMIsAConstantArray := Bind('LLVMIsAConstantArray');
      @LLVMIsAConstantExpr := Bind('LLVMIsAConstantExpr');
      @LLVMIsAConstantFP := Bind('LLVMIsAConstantFP');
      @LLVMIsAConstantInt := Bind('LLVMIsAConstantInt');
      @LLVMIsAConstantPointerNull := Bind('LLVMIsAConstantPointerNull');
      @LLVMIsAConstantStruct := Bind('LLVMIsAConstantStruct');
      @LLVMIsAConstantVector := Bind('LLVMIsAConstantVector');
      @LLVMIsAGlobalValue := Bind('LLVMIsAGlobalValue');
      @LLVMIsAFunction := Bind('LLVMIsAFunction');
      @LLVMIsAGlobalAlias := Bind('LLVMIsAGlobalAlias');
      @LLVMIsAGlobalVariable := Bind('LLVMIsAGlobalVariable');
      @LLVMIsAUndefValue := Bind('LLVMIsAUndefValue');
      @LLVMIsAInstruction := Bind('LLVMIsAInstruction');
      @LLVMIsABinaryOperator := Bind('LLVMIsABinaryOperator');
      @LLVMIsACallInst := Bind('LLVMIsACallInst');
      @LLVMIsAIntrinsicInst := Bind('LLVMIsAIntrinsicInst');
      @LLVMIsADbgInfoIntrinsic := Bind('LLVMIsADbgInfoIntrinsic');
      @LLVMIsADbgDeclareInst := Bind('LLVMIsADbgDeclareInst');
      @LLVMIsAMemIntrinsic := Bind('LLVMIsAMemIntrinsic');
      @LLVMIsAMemCpyInst := Bind('LLVMIsAMemCpyInst');
      @LLVMIsAMemMoveInst := Bind('LLVMIsAMemMoveInst');
      @LLVMIsAMemSetInst := Bind('LLVMIsAMemSetInst');
      @LLVMIsACmpInst := Bind('LLVMIsACmpInst');
      @LLVMIsAFCmpInst := Bind('LLVMIsAFCmpInst');
      @LLVMIsAICmpInst := Bind('LLVMIsAICmpInst');
      @LLVMIsAExtractElementInst := Bind('LLVMIsAExtractElementInst');
      @LLVMIsAGetElementPtrInst := Bind('LLVMIsAGetElementPtrInst');
      @LLVMIsAInsertElementInst := Bind('LLVMIsAInsertElementInst');
      @LLVMIsAInsertValueInst := Bind('LLVMIsAInsertValueInst');
      @LLVMIsALandingPadInst := Bind('LLVMIsALandingPadInst');
      @LLVMIsAPHINode := Bind('LLVMIsAPHINode');
      @LLVMIsASelectInst := Bind('LLVMIsASelectInst');
      @LLVMIsAShuffleVectorInst := Bind('LLVMIsAShuffleVectorInst');
      @LLVMIsAStoreInst := Bind('LLVMIsAStoreInst');
      @LLVMIsATerminatorInst := Bind('LLVMIsATerminatorInst');
      @LLVMIsABranchInst := Bind('LLVMIsABranchInst');
      @LLVMIsAIndirectBrInst := Bind('LLVMIsAIndirectBrInst');
      @LLVMIsAInvokeInst := Bind('LLVMIsAInvokeInst');
      @LLVMIsAReturnInst := Bind('LLVMIsAReturnInst');
      @LLVMIsASwitchInst := Bind('LLVMIsASwitchInst');
      @LLVMIsAUnreachableInst := Bind('LLVMIsAUnreachableInst');
      @LLVMIsAResumeInst := Bind('LLVMIsAResumeInst');
      @LLVMIsAUnaryInstruction := Bind('LLVMIsAUnaryInstruction');
      @LLVMIsAAllocaInst := Bind('LLVMIsAAllocaInst');
      @LLVMIsACastInst := Bind('LLVMIsACastInst');
      @LLVMIsABitCastInst := Bind('LLVMIsABitCastInst');
      @LLVMIsAFPExtInst := Bind('LLVMIsAFPExtInst');
      @LLVMIsAFPToSIInst := Bind('LLVMIsAFPToSIInst');
      @LLVMIsAFPToUIInst := Bind('LLVMIsAFPToUIInst');
      @LLVMIsAFPTruncInst := Bind('LLVMIsAFPTruncInst');
      @LLVMIsAIntToPtrInst := Bind('LLVMIsAIntToPtrInst');
      @LLVMIsAPtrToIntInst := Bind('LLVMIsAPtrToIntInst');
      @LLVMIsASExtInst := Bind('LLVMIsASExtInst');
      @LLVMIsASIToFPInst := Bind('LLVMIsASIToFPInst');
      @LLVMIsATruncInst := Bind('LLVMIsATruncInst');
      @LLVMIsAUIToFPInst := Bind('LLVMIsAUIToFPInst');
      @LLVMIsAZExtInst := Bind('LLVMIsAZExtInst');
      @LLVMIsAExtractValueInst := Bind('LLVMIsAExtractValueInst');
      @LLVMIsALoadInst := Bind('LLVMIsALoadInst');
      @LLVMIsAVAArgInst := Bind('LLVMIsAVAArgInst');
      @LLVMGetFirstUse := Bind('LLVMGetFirstUse');
      @LLVMGetNextUse := Bind('LLVMGetNextUse');
      @LLVMGetUser := Bind('LLVMGetUser');
      @LLVMGetUsedValue := Bind('LLVMGetUsedValue');
      @LLVMGetOperand := Bind('LLVMGetOperand');
      @LLVMSetOperand := Bind('LLVMSetOperand');
      @LLVMGetNumOperands := Bind('LLVMGetNumOperands');
      @LLVMConstNull := Bind('LLVMConstNull');
      @LLVMConstAllOnes := Bind('LLVMConstAllOnes');
      @LLVMGetUndef := Bind('LLVMGetUndef');
      @LLVMIsNull := Bind('LLVMIsNull');
      @LLVMConstPointerNull := Bind('LLVMConstPointerNull');
      @LLVMConstInt := Bind('LLVMConstInt');
      @LLVMConstIntOfArbitraryPrecision := Bind('LLVMConstIntOfArbitraryPrecision');
      @LLVMConstIntOfString := Bind('LLVMConstIntOfString');
      @LLVMConstIntOfStringAndSize := Bind('LLVMConstIntOfStringAndSize');
      @LLVMConstReal := Bind('LLVMConstReal');
      @LLVMConstRealOfString := Bind('LLVMConstRealOfString');
      @LLVMConstRealOfStringAndSize := Bind('LLVMConstRealOfStringAndSize');
      @LLVMConstIntGetZExtValue := Bind('LLVMConstIntGetZExtValue');
      @LLVMConstIntGetSExtValue := Bind('LLVMConstIntGetSExtValue');
      @LLVMConstStringInContext := Bind('LLVMConstStringInContext');
      @LLVMConstString := Bind('LLVMConstString');
      @LLVMConstStructInContext := Bind('LLVMConstStructInContext');
      @LLVMConstStruct := Bind('LLVMConstStruct');
      @LLVMConstArray := Bind('LLVMConstArray');
      @LLVMConstNamedStruct := Bind('LLVMConstNamedStruct');
      @LLVMConstVector := Bind('LLVMConstVector');
      @LLVMGetConstOpcode := Bind('LLVMGetConstOpcode');
      @LLVMAlignOf := Bind('LLVMAlignOf');
      @LLVMSizeOf := Bind('LLVMSizeOf');
      @LLVMConstNeg := Bind('LLVMConstNeg');
      @LLVMConstNSWNeg := Bind('LLVMConstNSWNeg');
      @LLVMConstNUWNeg := Bind('LLVMConstNUWNeg');
      @LLVMConstFNeg := Bind('LLVMConstFNeg');
      @LLVMConstNot := Bind('LLVMConstNot');
      @LLVMConstAdd := Bind('LLVMConstAdd');
      @LLVMConstNSWAdd := Bind('LLVMConstNSWAdd');
      @LLVMConstNUWAdd := Bind('LLVMConstNUWAdd');
      @LLVMConstFAdd := Bind('LLVMConstFAdd');
      @LLVMConstSub := Bind('LLVMConstSub');
      @LLVMConstNSWSub := Bind('LLVMConstNSWSub');
      @LLVMConstNUWSub := Bind('LLVMConstNUWSub');
      @LLVMConstFSub := Bind('LLVMConstFSub');
      @LLVMConstMul := Bind('LLVMConstMul');
      @LLVMConstNSWMul := Bind('LLVMConstNSWMul');
      @LLVMConstNUWMul := Bind('LLVMConstNUWMul');
      @LLVMConstFMul := Bind('LLVMConstFMul');
      @LLVMConstUDiv := Bind('LLVMConstUDiv');
      @LLVMConstSDiv := Bind('LLVMConstSDiv');
      @LLVMConstExactSDiv := Bind('LLVMConstExactSDiv');
      @LLVMConstFDiv := Bind('LLVMConstFDiv');
      @LLVMConstURem := Bind('LLVMConstURem');
      @LLVMConstSRem := Bind('LLVMConstSRem');
      @LLVMConstFRem := Bind('LLVMConstFRem');
      @LLVMConstAnd := Bind('LLVMConstAnd');
      @LLVMConstOr := Bind('LLVMConstOr');
      @LLVMConstXor := Bind('LLVMConstXor');
      @LLVMConstICmp := Bind('LLVMConstICmp');
      @LLVMConstFCmp := Bind('LLVMConstFCmp');
      @LLVMConstShl := Bind('LLVMConstShl');
      @LLVMConstLShr := Bind('LLVMConstLShr');
      @LLVMConstAShr := Bind('LLVMConstAShr');
      @LLVMConstGEP := Bind('LLVMConstGEP');
      @LLVMConstInBoundsGEP := Bind('LLVMConstInBoundsGEP');
      @LLVMConstTrunc := Bind('LLVMConstTrunc');
      @LLVMConstSExt := Bind('LLVMConstSExt');
      @LLVMConstZExt := Bind('LLVMConstZExt');
      @LLVMConstFPTrunc := Bind('LLVMConstFPTrunc');
      @LLVMConstFPExt := Bind('LLVMConstFPExt');
      @LLVMConstUIToFP := Bind('LLVMConstUIToFP');
      @LLVMConstSIToFP := Bind('LLVMConstSIToFP');
      @LLVMConstFPToUI := Bind('LLVMConstFPToUI');
      @LLVMConstFPToSI := Bind('LLVMConstFPToSI');
      @LLVMConstPtrToInt := Bind('LLVMConstPtrToInt');
      @LLVMConstIntToPtr := Bind('LLVMConstIntToPtr');
      @LLVMConstBitCast := Bind('LLVMConstBitCast');
      @LLVMConstZExtOrBitCast := Bind('LLVMConstZExtOrBitCast');
      @LLVMConstSExtOrBitCast := Bind('LLVMConstSExtOrBitCast');
      @LLVMConstTruncOrBitCast := Bind('LLVMConstTruncOrBitCast');
      @LLVMConstPointerCast := Bind('LLVMConstPointerCast');
      @LLVMConstIntCast := Bind('LLVMConstIntCast');
      @LLVMConstFPCast := Bind('LLVMConstFPCast');
      @LLVMConstSelect := Bind('LLVMConstSelect');
      @LLVMConstExtractElement := Bind('LLVMConstExtractElement');
      @LLVMConstInsertElement := Bind('LLVMConstInsertElement');
      @LLVMConstShuffleVector := Bind('LLVMConstShuffleVector');
      @LLVMConstExtractValue := Bind('LLVMConstExtractValue');
      @LLVMConstInsertValue := Bind('LLVMConstInsertValue');
      @LLVMConstInlineAsm := Bind('LLVMConstInlineAsm');
      @LLVMBlockAddress := Bind('LLVMBlockAddress');
      @LLVMGetGlobalParent := Bind('LLVMGetGlobalParent');
      @LLVMIsDeclaration := Bind('LLVMIsDeclaration');
      @LLVMGetLinkage := Bind('LLVMGetLinkage');
      @LLVMSetLinkage := Bind('LLVMSetLinkage');
      @LLVMGetSection := Bind('LLVMGetSection');
      @LLVMSetSection := Bind('LLVMSetSection');
      @LLVMGetVisibility := Bind('LLVMGetVisibility');
      @LLVMSetVisibility := Bind('LLVMSetVisibility');
      @LLVMGetAlignment := Bind('LLVMGetAlignment');
      @LLVMSetAlignment := Bind('LLVMSetAlignment');
      @LLVMAddGlobal := Bind('LLVMAddGlobal');
      @LLVMAddGlobalInAddressSpace := Bind('LLVMAddGlobalInAddressSpace');
      @LLVMGetNamedGlobal := Bind('LLVMGetNamedGlobal');
      @LLVMGetFirstGlobal := Bind('LLVMGetFirstGlobal');
      @LLVMGetLastGlobal := Bind('LLVMGetLastGlobal');
      @LLVMGetNextGlobal := Bind('LLVMGetNextGlobal');
      @LLVMGetPreviousGlobal := Bind('LLVMGetPreviousGlobal');
      @LLVMDeleteGlobal := Bind('LLVMDeleteGlobal');
      @LLVMGetInitializer := Bind('LLVMGetInitializer');
      @LLVMSetInitializer := Bind('LLVMSetInitializer');
      @LLVMIsThreadLocal := Bind('LLVMIsThreadLocal');
      @LLVMSetThreadLocal := Bind('LLVMSetThreadLocal');
      @LLVMIsGlobalConstant := Bind('LLVMIsGlobalConstant');
      @LLVMSetGlobalConstant := Bind('LLVMSetGlobalConstant');
      @LLVMAddAlias := Bind('LLVMAddAlias');
      @LLVMDeleteFunction := Bind('LLVMDeleteFunction');
      @LLVMGetIntrinsicID := Bind('LLVMGetIntrinsicID');
      @LLVMGetFunctionCallConv := Bind('LLVMGetFunctionCallConv');
      @LLVMSetFunctionCallConv := Bind('LLVMSetFunctionCallConv');
      @LLVMGetGC := Bind('LLVMGetGC');
      @LLVMSetGC := Bind('LLVMSetGC');
      @LLVMAddFunctionAttr := Bind('LLVMAddFunctionAttr');
      @LLVMGetFunctionAttr := Bind('LLVMGetFunctionAttr');
      @LLVMRemoveFunctionAttr := Bind('LLVMRemoveFunctionAttr');
      @LLVMCountParams := Bind('LLVMCountParams');
      @LLVMGetParams := Bind('LLVMGetParams');
      @LLVMGetParam := Bind('LLVMGetParam');
      @LLVMGetParamParent := Bind('LLVMGetParamParent');
      @LLVMGetFirstParam := Bind('LLVMGetFirstParam');
      @LLVMGetLastParam := Bind('LLVMGetLastParam');
      @LLVMGetNextParam := Bind('LLVMGetNextParam');
      @LLVMGetPreviousParam := Bind('LLVMGetPreviousParam');
      @LLVMAddAttribute := Bind('LLVMAddAttribute');
      @LLVMRemoveAttribute := Bind('LLVMRemoveAttribute');
      @LLVMGetAttribute := Bind('LLVMGetAttribute');
      @LLVMSetParamAlignment := Bind('LLVMSetParamAlignment');
      @LLVMMDStringInContext := Bind('LLVMMDStringInContext');
      @LLVMMDString := Bind('LLVMMDString');
      @LLVMMDNodeInContext := Bind('LLVMMDNodeInContext');
      @LLVMMDNode := Bind('LLVMMDNode');
      @LLVMGetMDString := Bind('LLVMGetMDString');
      @LLVMGetMDNodeNumOperands := Bind('LLVMGetMDNodeNumOperands');
      @LLVMGetMDNodeOperands := Bind('LLVMGetMDNodeOperands');
      @LLVMBasicBlockAsValue := Bind('LLVMBasicBlockAsValue');
      @LLVMValueIsBasicBlock := Bind('LLVMValueIsBasicBlock');
      @LLVMValueAsBasicBlock := Bind('LLVMValueAsBasicBlock');
      @LLVMGetBasicBlockParent := Bind('LLVMGetBasicBlockParent');
      @LLVMGetBasicBlockTerminator := Bind('LLVMGetBasicBlockTerminator');
      @LLVMCountBasicBlocks := Bind('LLVMCountBasicBlocks');
      @LLVMGetBasicBlocks := Bind('LLVMGetBasicBlocks');
      @LLVMGetFirstBasicBlock := Bind('LLVMGetFirstBasicBlock');
      @LLVMGetLastBasicBlock := Bind('LLVMGetLastBasicBlock');
      @LLVMGetNextBasicBlock := Bind('LLVMGetNextBasicBlock');
      @LLVMGetPreviousBasicBlock := Bind('LLVMGetPreviousBasicBlock');
      @LLVMGetEntryBasicBlock := Bind('LLVMGetEntryBasicBlock');
      @LLVMAppendBasicBlockInContext := Bind('LLVMAppendBasicBlockInContext');
      @LLVMAppendBasicBlock := Bind('LLVMAppendBasicBlock');
      @LLVMInsertBasicBlockInContext := Bind('LLVMInsertBasicBlockInContext');
      @LLVMInsertBasicBlock := Bind('LLVMInsertBasicBlock');
      @LLVMDeleteBasicBlock := Bind('LLVMDeleteBasicBlock');
      @LLVMRemoveBasicBlockFromParent := Bind('LLVMRemoveBasicBlockFromParent');
      @LLVMMoveBasicBlockBefore := Bind('LLVMMoveBasicBlockBefore');
      @LLVMMoveBasicBlockAfter := Bind('LLVMMoveBasicBlockAfter');
      @LLVMGetFirstInstruction := Bind('LLVMGetFirstInstruction');
      @LLVMGetLastInstruction := Bind('LLVMGetLastInstruction');
      @LLVMHasMetadata := Bind('LLVMHasMetadata');
      @LLVMGetMetadata := Bind('LLVMGetMetadata');
      @LLVMSetMetadata := Bind('LLVMSetMetadata');
      @LLVMGetInstructionParent := Bind('LLVMGetInstructionParent');
      @LLVMGetNextInstruction := Bind('LLVMGetNextInstruction');
      @LLVMGetPreviousInstruction := Bind('LLVMGetPreviousInstruction');
      @LLVMInstructionEraseFromParent := Bind('LLVMInstructionEraseFromParent');
      @LLVMGetInstructionOpcode := Bind('LLVMGetInstructionOpcode');
      @LLVMGetICmpPredicate := Bind('LLVMGetICmpPredicate');
      @LLVMSetInstructionCallConv := Bind('LLVMSetInstructionCallConv');
      @LLVMGetInstructionCallConv := Bind('LLVMGetInstructionCallConv');
      @LLVMAddInstrAttribute := Bind('LLVMAddInstrAttribute');
      @LLVMRemoveInstrAttribute := Bind('LLVMRemoveInstrAttribute');
      @LLVMSetInstrParamAlignment := Bind('LLVMSetInstrParamAlignment');
      @LLVMIsTailCall := Bind('LLVMIsTailCall');
      @LLVMSetTailCall := Bind('LLVMSetTailCall');
      @LLVMGetSwitchDefaultDest := Bind('LLVMGetSwitchDefaultDest');
      @LLVMAddIncoming := Bind('LLVMAddIncoming');
      @LLVMCountIncoming := Bind('LLVMCountIncoming');
      @LLVMGetIncomingValue := Bind('LLVMGetIncomingValue');
      @LLVMGetIncomingBlock := Bind('LLVMGetIncomingBlock');
      @LLVMCreateBuilderInContext := Bind('LLVMCreateBuilderInContext');
      @LLVMCreateBuilder := Bind('LLVMCreateBuilder');
      @LLVMPositionBuilder := Bind('LLVMPositionBuilder');
      @LLVMPositionBuilderBefore := Bind('LLVMPositionBuilderBefore');
      @LLVMPositionBuilderAtEnd := Bind('LLVMPositionBuilderAtEnd');
      @LLVMGetInsertBlock := Bind('LLVMGetInsertBlock');
      @LLVMClearInsertionPosition := Bind('LLVMClearInsertionPosition');
      @LLVMInsertIntoBuilder := Bind('LLVMInsertIntoBuilder');
      @LLVMInsertIntoBuilderWithName := Bind('LLVMInsertIntoBuilderWithName');
      @LLVMDisposeBuilder := Bind('LLVMDisposeBuilder');
      @LLVMSetCurrentDebugLocation := Bind('LLVMSetCurrentDebugLocation');
      @LLVMGetCurrentDebugLocation := Bind('LLVMGetCurrentDebugLocation');
      @LLVMSetInstDebugLocation := Bind('LLVMSetInstDebugLocation');
      @LLVMBuildRetVoid := Bind('LLVMBuildRetVoid');
      @LLVMBuildRet := Bind('LLVMBuildRet');
      @LLVMBuildAggregateRet := Bind('LLVMBuildAggregateRet');
      @LLVMBuildBr := Bind('LLVMBuildBr');
      @LLVMBuildCondBr := Bind('LLVMBuildCondBr');
      @LLVMBuildSwitch := Bind('LLVMBuildSwitch');
      @LLVMBuildIndirectBr := Bind('LLVMBuildIndirectBr');
      @LLVMBuildInvoke := Bind('LLVMBuildInvoke');
      @LLVMBuildLandingPad := Bind('LLVMBuildLandingPad');
      @LLVMBuildResume := Bind('LLVMBuildResume');
      @LLVMBuildUnreachable := Bind('LLVMBuildUnreachable');
      @LLVMAddCase := Bind('LLVMAddCase');
      @LLVMAddDestination := Bind('LLVMAddDestination');
      @LLVMAddClause := Bind('LLVMAddClause');
      @LLVMSetCleanup := Bind('LLVMSetCleanup');
      @LLVMBuildAdd := Bind('LLVMBuildAdd');
      @LLVMBuildNSWAdd := Bind('LLVMBuildNSWAdd');
      @LLVMBuildNUWAdd := Bind('LLVMBuildNUWAdd');
      @LLVMBuildFAdd := Bind('LLVMBuildFAdd');
      @LLVMBuildSub := Bind('LLVMBuildSub');
      @LLVMBuildNSWSub := Bind('LLVMBuildNSWSub');
      @LLVMBuildNUWSub := Bind('LLVMBuildNUWSub');
      @LLVMBuildFSub := Bind('LLVMBuildFSub');
      @LLVMBuildMul := Bind('LLVMBuildMul');
      @LLVMBuildNSWMul := Bind('LLVMBuildNSWMul');
      @LLVMBuildNUWMul := Bind('LLVMBuildNUWMul');
      @LLVMBuildFMul := Bind('LLVMBuildFMul');
      @LLVMBuildUDiv := Bind('LLVMBuildUDiv');
      @LLVMBuildSDiv := Bind('LLVMBuildSDiv');
      @LLVMBuildExactSDiv := Bind('LLVMBuildExactSDiv');
      @LLVMBuildFDiv := Bind('LLVMBuildFDiv');
      @LLVMBuildURem := Bind('LLVMBuildURem');
      @LLVMBuildSRem := Bind('LLVMBuildSRem');
      @LLVMBuildFRem := Bind('LLVMBuildFRem');
      @LLVMBuildShl := Bind('LLVMBuildShl');
      @LLVMBuildLShr := Bind('LLVMBuildLShr');
      @LLVMBuildAShr := Bind('LLVMBuildAShr');
      @LLVMBuildAnd := Bind('LLVMBuildAnd');
      @LLVMBuildOr := Bind('LLVMBuildOr');
      @LLVMBuildXor := Bind('LLVMBuildXor');
      @LLVMBuildBinOp := Bind('LLVMBuildBinOp');
      @LLVMBuildNeg := Bind('LLVMBuildNeg');
      @LLVMBuildNSWNeg := Bind('LLVMBuildNSWNeg');
      @LLVMBuildNUWNeg := Bind('LLVMBuildNUWNeg');
      @LLVMBuildFNeg := Bind('LLVMBuildFNeg');
      @LLVMBuildNot := Bind('LLVMBuildNot');
      @LLVMBuildMalloc := Bind('LLVMBuildMalloc');
      @LLVMBuildArrayMalloc := Bind('LLVMBuildArrayMalloc');
      @LLVMBuildAlloca := Bind('LLVMBuildAlloca');
      @LLVMBuildArrayAlloca := Bind('LLVMBuildArrayAlloca');
      @LLVMBuildFree := Bind('LLVMBuildFree');
      @LLVMBuildLoad := Bind('LLVMBuildLoad');
      @LLVMBuildStore := Bind('LLVMBuildStore');
      @LLVMBuildGEP := Bind('LLVMBuildGEP');
      @LLVMBuildInBoundsGEP := Bind('LLVMBuildInBoundsGEP');
      @LLVMBuildStructGEP := Bind('LLVMBuildStructGEP');
      @LLVMBuildGlobalString := Bind('LLVMBuildGlobalString');
      @LLVMBuildGlobalStringPtr := Bind('LLVMBuildGlobalStringPtr');
      @LLVMGetVolatile := Bind('LLVMGetVolatile');
      @LLVMSetVolatile := Bind('LLVMSetVolatile');
      @LLVMBuildTrunc := Bind('LLVMBuildTrunc');
      @LLVMBuildZExt := Bind('LLVMBuildZExt');
      @LLVMBuildSExt := Bind('LLVMBuildSExt');
      @LLVMBuildFPToUI := Bind('LLVMBuildFPToUI');
      @LLVMBuildFPToSI := Bind('LLVMBuildFPToSI');
      @LLVMBuildUIToFP := Bind('LLVMBuildUIToFP');
      @LLVMBuildSIToFP := Bind('LLVMBuildSIToFP');
      @LLVMBuildFPTrunc := Bind('LLVMBuildFPTrunc');
      @LLVMBuildFPExt := Bind('LLVMBuildFPExt');
      @LLVMBuildPtrToInt := Bind('LLVMBuildPtrToInt');
      @LLVMBuildIntToPtr := Bind('LLVMBuildIntToPtr');
      @LLVMBuildBitCast := Bind('LLVMBuildBitCast');
      @LLVMBuildZExtOrBitCast := Bind('LLVMBuildZExtOrBitCast');
      @LLVMBuildSExtOrBitCast := Bind('LLVMBuildSExtOrBitCast');
      @LLVMBuildTruncOrBitCast := Bind('LLVMBuildTruncOrBitCast');
      @LLVMBuildCast := Bind('LLVMBuildCast');
      @LLVMBuildPointerCast := Bind('LLVMBuildPointerCast');
      @LLVMBuildIntCast := Bind('LLVMBuildIntCast');
      @LLVMBuildFPCast := Bind('LLVMBuildFPCast');
      @LLVMBuildICmp := Bind('LLVMBuildICmp');
      @LLVMBuildFCmp := Bind('LLVMBuildFCmp');
      @LLVMBuildPhi := Bind('LLVMBuildPhi');
      @LLVMBuildCall := Bind('LLVMBuildCall');
      @LLVMBuildSelect := Bind('LLVMBuildSelect');
      @LLVMBuildVAArg := Bind('LLVMBuildVAArg');
      @LLVMBuildExtractElement := Bind('LLVMBuildExtractElement');
      @LLVMBuildInsertElement := Bind('LLVMBuildInsertElement');
      @LLVMBuildShuffleVector := Bind('LLVMBuildShuffleVector');
      @LLVMBuildExtractValue := Bind('LLVMBuildExtractValue');
      @LLVMBuildInsertValue := Bind('LLVMBuildInsertValue');
      @LLVMBuildIsNull := Bind('LLVMBuildIsNull');
      @LLVMBuildIsNotNull := Bind('LLVMBuildIsNotNull');
      @LLVMBuildPtrDiff := Bind('LLVMBuildPtrDiff');
      @LLVMCreateModuleProviderForExistingModule := Bind('LLVMCreateModuleProviderForExistingModule');
      @LLVMDisposeModuleProvider := Bind('LLVMDisposeModuleProvider');
      @LLVMCreateMemoryBufferWithContentsOfFile := Bind('LLVMCreateMemoryBufferWithContentsOfFile');
      @LLVMCreateMemoryBufferWithSTDIN := Bind('LLVMCreateMemoryBufferWithSTDIN');
      @LLVMDisposeMemoryBuffer := Bind('LLVMDisposeMemoryBuffer');
      @LLVMGetGlobalPassRegistry := Bind('LLVMGetGlobalPassRegistry');
      @LLVMCreatePassManager := Bind('LLVMCreatePassManager');
      @LLVMCreateFunctionPassManagerForModule := Bind('LLVMCreateFunctionPassManagerForModule');
      @LLVMCreateFunctionPassManager := Bind('LLVMCreateFunctionPassManager');
      @LLVMRunPassManager := Bind('LLVMRunPassManager');
      @LLVMInitializeFunctionPassManager := Bind('LLVMInitializeFunctionPassManager');
      @LLVMRunFunctionPassManager := Bind('LLVMRunFunctionPassManager');
      @LLVMFinalizeFunctionPassManager := Bind('LLVMFinalizeFunctionPassManager');
      @LLVMDisposePassManager := Bind('LLVMDisposePassManager');
      @LLVMVerifyModule := Bind('LLVMVerifyModule');
      @LLVMVerifyFunction := Bind('LLVMVerifyFunction');
      @LLVMViewFunctionCFG := Bind('LLVMViewFunctionCFG');
      @LLVMViewFunctionCFGOnly := Bind('LLVMViewFunctionCFGOnly');
      @LLVMParseBitcode := Bind('LLVMParseBitcode');
      @LLVMParseBitcodeInContext := Bind('LLVMParseBitcodeInContext');
      @LLVMGetBitcodeModuleInContext := Bind('LLVMGetBitcodeModuleInContext');
      @LLVMGetBitcodeModule := Bind('LLVMGetBitcodeModule');
      @LLVMGetBitcodeModuleProviderInContext := Bind('LLVMGetBitcodeModuleProviderInContext');
      @LLVMGetBitcodeModuleProvider := Bind('LLVMGetBitcodeModuleProvider');
      @LLVMWriteBitcodeToFile := Bind('LLVMWriteBitcodeToFile');
      @LLVMWriteBitcodeToFD := Bind('LLVMWriteBitcodeToFD');
      @LLVMWriteBitcodeToFileHandle := Bind('LLVMWriteBitcodeToFileHandle');
      @LLVMCreateDisasm := Bind('LLVMCreateDisasm');
      @LLVMSetDisasmOptions := Bind('LLVMSetDisasmOptions');
      @LLVMDisasmDispose := Bind('LLVMDisasmDispose');
      @LLVMDisasmInstruction := Bind('LLVMDisasmInstruction');
      @LLVMLinkInJIT := Bind('LLVMLinkInJIT');
      @LLVMLinkInInterpreter := Bind('LLVMLinkInInterpreter');
      @LLVMCreateGenericValueOfInt := Bind('LLVMCreateGenericValueOfInt');
      @LLVMCreateGenericValueOfPointer := Bind('LLVMCreateGenericValueOfPointer');
      @LLVMCreateGenericValueOfFloat := Bind('LLVMCreateGenericValueOfFloat');
      @LLVMGenericValueIntWidth := Bind('LLVMGenericValueIntWidth');
      @LLVMGenericValueToInt := Bind('LLVMGenericValueToInt');
      @LLVMGenericValueToPointer := Bind('LLVMGenericValueToPointer');
      @LLVMGenericValueToFloat := Bind('LLVMGenericValueToFloat');
      @LLVMDisposeGenericValue := Bind('LLVMDisposeGenericValue');
      @LLVMCreateExecutionEngineForModule := Bind('LLVMCreateExecutionEngineForModule');
      @LLVMCreateInterpreterForModule := Bind('LLVMCreateInterpreterForModule');
      @LLVMCreateJITCompilerForModule := Bind('LLVMCreateJITCompilerForModule');
      @LLVMCreateExecutionEngine := Bind('LLVMCreateExecutionEngine');
      @LLVMCreateInterpreter := Bind('LLVMCreateInterpreter');
      @LLVMCreateJITCompiler := Bind('LLVMCreateJITCompiler');
      @LLVMDisposeExecutionEngine := Bind('LLVMDisposeExecutionEngine');
      @LLVMRunStaticConstructors := Bind('LLVMRunStaticConstructors');
      @LLVMRunStaticDestructors := Bind('LLVMRunStaticDestructors');
      @LLVMRunFunctionAsMain := Bind('LLVMRunFunctionAsMain');
      @LLVMRunFunction := Bind('LLVMRunFunction');
      @LLVMFreeMachineCodeForFunction := Bind('LLVMFreeMachineCodeForFunction');
      @LLVMAddModule := Bind('LLVMAddModule');
      @LLVMAddModuleProvider := Bind('LLVMAddModuleProvider');
      @LLVMRemoveModule := Bind('LLVMRemoveModule');
      @LLVMRemoveModuleProvider := Bind('LLVMRemoveModuleProvider');
      @LLVMFindFunction := Bind('LLVMFindFunction');
      @LLVMRecompileAndRelinkFunction := Bind('LLVMRecompileAndRelinkFunction');
      @LLVMGetExecutionEngineTargetData := Bind('LLVMGetExecutionEngineTargetData');
      @LLVMAddGlobalMapping := Bind('LLVMAddGlobalMapping');
      @LLVMGetPointerToGlobal := Bind('LLVMGetPointerToGlobal');
      @LLVMAddAggressiveDCEPass := Bind('LLVMAddAggressiveDCEPass');
      @LLVMAddCFGSimplificationPass := Bind('LLVMAddCFGSimplificationPass');
      @LLVMAddDeadStoreEliminationPass := Bind('LLVMAddDeadStoreEliminationPass');
      @LLVMAddGVNPass := Bind('LLVMAddGVNPass');
      @LLVMAddIndVarSimplifyPass := Bind('LLVMAddIndVarSimplifyPass');
      @LLVMAddInstructionCombiningPass := Bind('LLVMAddInstructionCombiningPass');
      @LLVMAddJumpThreadingPass := Bind('LLVMAddJumpThreadingPass');
      @LLVMAddLICMPass := Bind('LLVMAddLICMPass');
      @LLVMAddLoopDeletionPass := Bind('LLVMAddLoopDeletionPass');
      @LLVMAddLoopIdiomPass := Bind('LLVMAddLoopIdiomPass');
      @LLVMAddLoopRotatePass := Bind('LLVMAddLoopRotatePass');
      @LLVMAddLoopUnrollPass := Bind('LLVMAddLoopUnrollPass');
      @LLVMAddLoopUnswitchPass := Bind('LLVMAddLoopUnswitchPass');
      @LLVMAddMemCpyOptPass := Bind('LLVMAddMemCpyOptPass');
      @LLVMAddPromoteMemoryToRegisterPass := Bind('LLVMAddPromoteMemoryToRegisterPass');
      @LLVMAddReassociatePass := Bind('LLVMAddReassociatePass');
      @LLVMAddSCCPPass := Bind('LLVMAddSCCPPass');
      @LLVMAddScalarReplAggregatesPass := Bind('LLVMAddScalarReplAggregatesPass');
      @LLVMAddScalarReplAggregatesPassSSA := Bind('LLVMAddScalarReplAggregatesPassSSA');
      @LLVMAddScalarReplAggregatesPassWithThreshold := Bind('LLVMAddScalarReplAggregatesPassWithThreshold');
      @LLVMAddSimplifyLibCallsPass := Bind('LLVMAddSimplifyLibCallsPass');
      @LLVMAddTailCallEliminationPass := Bind('LLVMAddTailCallEliminationPass');
      @LLVMAddConstantPropagationPass := Bind('LLVMAddConstantPropagationPass');
      @LLVMAddDemoteMemoryToRegisterPass := Bind('LLVMAddDemoteMemoryToRegisterPass');
      @LLVMAddVerifierPass := Bind('LLVMAddVerifierPass');
      @LLVMAddCorrelatedValuePropagationPass := Bind('LLVMAddCorrelatedValuePropagationPass');
      @LLVMAddEarlyCSEPass := Bind('LLVMAddEarlyCSEPass');
      @LLVMAddLowerExpectIntrinsicPass := Bind('LLVMAddLowerExpectIntrinsicPass');
      @LLVMAddTypeBasedAliasAnalysisPass := Bind('LLVMAddTypeBasedAliasAnalysisPass');
      @LLVMAddBasicAliasAnalysisPass := Bind('LLVMAddBasicAliasAnalysisPass');
      @LLVMAddBBVectorizePass := Bind('LLVMAddBBVectorizePass');
      @LLVMAddLoopVectorizePass := Bind('LLVMAddLoopVectorizePass');
      @LLVMAddArgumentPromotionPass := Bind('LLVMAddArgumentPromotionPass');
      @LLVMAddConstantMergePass := Bind('LLVMAddConstantMergePass');
      @LLVMAddDeadArgEliminationPass := Bind('LLVMAddDeadArgEliminationPass');
      @LLVMAddFunctionAttrsPass := Bind('LLVMAddFunctionAttrsPass');
      @LLVMAddFunctionInliningPass := Bind('LLVMAddFunctionInliningPass');
      @LLVMAddAlwaysInlinerPass := Bind('LLVMAddAlwaysInlinerPass');
      @LLVMAddGlobalDCEPass := Bind('LLVMAddGlobalDCEPass');
      @LLVMAddGlobalOptimizerPass := Bind('LLVMAddGlobalOptimizerPass');
      @LLVMAddIPConstantPropagationPass := Bind('LLVMAddIPConstantPropagationPass');
      @LLVMAddPruneEHPass := Bind('LLVMAddPruneEHPass');
      @LLVMAddIPSCCPPass := Bind('LLVMAddIPSCCPPass');
      @LLVMAddInternalizePass := Bind('LLVMAddInternalizePass');
      @LLVMAddStripDeadPrototypesPass := Bind('LLVMAddStripDeadPrototypesPass');
      @LLVMAddStripSymbolsPass := Bind('LLVMAddStripSymbolsPass');
      @LLVMPassManagerBuilderCreate := Bind('LLVMPassManagerBuilderCreate');
      @LLVMPassManagerBuilderDispose := Bind('LLVMPassManagerBuilderDispose');
      @LLVMPassManagerBuilderSetOptLevel := Bind('LLVMPassManagerBuilderSetOptLevel');
      @LLVMPassManagerBuilderSetSizeLevel := Bind('LLVMPassManagerBuilderSetSizeLevel');
      @LLVMPassManagerBuilderSetDisableUnitAtATime := Bind('LLVMPassManagerBuilderSetDisableUnitAtATime');
      @LLVMPassManagerBuilderSetDisableUnrollLoops := Bind('LLVMPassManagerBuilderSetDisableUnrollLoops');
      @LLVMPassManagerBuilderSetDisableSimplifyLibCalls := Bind('LLVMPassManagerBuilderSetDisableSimplifyLibCalls');
      @LLVMPassManagerBuilderUseInlinerWithThreshold := Bind('LLVMPassManagerBuilderUseInlinerWithThreshold');
      @LLVMPassManagerBuilderPopulateFunctionPassManager := Bind('LLVMPassManagerBuilderPopulateFunctionPassManager');
      @LLVMPassManagerBuilderPopulateModulePassManager := Bind('LLVMPassManagerBuilderPopulateModulePassManager');
      @LLVMPassManagerBuilderPopulateLTOPassManager := Bind('LLVMPassManagerBuilderPopulateLTOPassManager');
      @LLVMInitializeX86TargetInfo := Bind('LLVMInitializeX86TargetInfoX');
      @LLVMInitializeX86Target := Bind('LLVMInitializeX86TargetX');
      @LLVMInitializeX86TargetMC := Bind('LLVMInitializeX86TargetMCX');
      @LLVMInitializeX86AsmPrinter := Bind('LLVMInitializeX86AsmPrinterX');
      @LLVMInitializeX86AsmParser := Bind('LLVMInitializeX86AsmParserX');
      @LLVMInitializeX86Disassembler := Bind('LLVMInitializeX86DisassemblerX');
      @LLVMInitializeAllTargetInfos := Bind('LLVMInitializeAllTargetInfosX');
      @LLVMInitializeAllTargets := Bind('LLVMInitializeAllTargetsX');
      @LLVMInitializeAllTargetMCs := Bind('LLVMInitializeAllTargetMCsX');
      @LLVMInitializeAllAsmPrinters := Bind('LLVMInitializeAllAsmPrintersX');
      @LLVMInitializeAllAsmParsers := Bind('LLVMInitializeAllAsmParsersX');
      @LLVMInitializeAllDisassemblers := Bind('LLVMInitializeAllDisassemblersX');
      @LLVMInitializeNativeTarget := Bind('LLVMInitializeNativeTargetX');
      @LLVMCreateTargetData := Bind('LLVMCreateTargetData');
      @LLVMAddTargetData := Bind('LLVMAddTargetData');
      @LLVMAddTargetLibraryInfo := Bind('LLVMAddTargetLibraryInfo');
      @LLVMCopyStringRepOfTargetData := Bind('LLVMCopyStringRepOfTargetData');
      @LLVMByteOrder := Bind('LLVMByteOrder');
      @LLVMPointerSize := Bind('LLVMPointerSize');
      @LLVMPointerSizeForAS := Bind('LLVMPointerSizeForAS');
      @LLVMIntPtrType := Bind('LLVMIntPtrType');
      @LLVMIntPtrTypeForAS := Bind('LLVMIntPtrTypeForAS');
      @LLVMSizeOfTypeInBits := Bind('LLVMSizeOfTypeInBits');
      @LLVMStoreSizeOfType := Bind('LLVMStoreSizeOfType');
      @LLVMABISizeOfType := Bind('LLVMABISizeOfType');
      @LLVMABIAlignmentOfType := Bind('LLVMABIAlignmentOfType');
      @LLVMCallFrameAlignmentOfType := Bind('LLVMCallFrameAlignmentOfType');
      @LLVMPreferredAlignmentOfType := Bind('LLVMPreferredAlignmentOfType');
      @LLVMPreferredAlignmentOfGlobal := Bind('LLVMPreferredAlignmentOfGlobal');
      @LLVMElementAtOffset := Bind('LLVMElementAtOffset');
      @LLVMOffsetOfElement := Bind('LLVMOffsetOfElement');
      @LLVMDisposeTargetData := Bind('LLVMDisposeTargetData');
      @LLVMGetFirstTarget := Bind('LLVMGetFirstTarget');
      @LLVMGetNextTarget := Bind('LLVMGetNextTarget');
      @LLVMGetTargetName := Bind('LLVMGetTargetName');
      @LLVMGetTargetDescription := Bind('LLVMGetTargetDescription');
      @LLVMTargetHasJIT := Bind('LLVMTargetHasJIT');
      @LLVMTargetHasTargetMachine := Bind('LLVMTargetHasTargetMachine');
      @LLVMTargetHasAsmBackend := Bind('LLVMTargetHasAsmBackend');
      @LLVMCreateTargetMachine := Bind('LLVMCreateTargetMachine');
      @LLVMDisposeTargetMachine := Bind('LLVMDisposeTargetMachine');
      @LLVMGetTargetMachineTarget := Bind('LLVMGetTargetMachineTarget');
      @LLVMGetTargetMachineTriple := Bind('LLVMGetTargetMachineTriple');
      @LLVMGetTargetMachineCPU := Bind('LLVMGetTargetMachineCPU');
      @LLVMGetTargetMachineFeatureString := Bind('LLVMGetTargetMachineFeatureString');
      @LLVMGetTargetMachineData := Bind('LLVMGetTargetMachineData');
      @LLVMTargetMachineEmitToFile := Bind('LLVMTargetMachineEmitToFile');
      @LLVMCreateObjectFile := Bind('LLVMCreateObjectFile');
      @LLVMDisposeObjectFile := Bind('LLVMDisposeObjectFile');
      @LLVMGetSections := Bind('LLVMGetSections');
      @LLVMDisposeSectionIterator := Bind('LLVMDisposeSectionIterator');
      @LLVMIsSectionIteratorAtEnd := Bind('LLVMIsSectionIteratorAtEnd');
      @LLVMMoveToNextSection := Bind('LLVMMoveToNextSection');
      @LLVMMoveToContainingSection := Bind('LLVMMoveToContainingSection');
      @LLVMGetSymbols := Bind('LLVMGetSymbols');
      @LLVMDisposeSymbolIterator := Bind('LLVMDisposeSymbolIterator');
      @LLVMIsSymbolIteratorAtEnd := Bind('LLVMIsSymbolIteratorAtEnd');
      @LLVMMoveToNextSymbol := Bind('LLVMMoveToNextSymbol');
      @LLVMGetSectionName := Bind('LLVMGetSectionName');
      @LLVMGetSectionSize := Bind('LLVMGetSectionSize');
      @LLVMGetSectionContents := Bind('LLVMGetSectionContents');
      @LLVMGetSectionAddress := Bind('LLVMGetSectionAddress');
      @LLVMGetSectionContainsSymbol := Bind('LLVMGetSectionContainsSymbol');
      @LLVMGetRelocations := Bind('LLVMGetRelocations');
      @LLVMDisposeRelocationIterator := Bind('LLVMDisposeRelocationIterator');
      @LLVMIsRelocationIteratorAtEnd := Bind('LLVMIsRelocationIteratorAtEnd');
      @LLVMMoveToNextRelocation := Bind('LLVMMoveToNextRelocation');
      @LLVMGetSymbolName := Bind('LLVMGetSymbolName');
      @LLVMGetSymbolAddress := Bind('LLVMGetSymbolAddress');
      @LLVMGetSymbolFileOffset := Bind('LLVMGetSymbolFileOffset');
      @LLVMGetSymbolSize := Bind('LLVMGetSymbolSize');
      @LLVMGetRelocationAddress := Bind('LLVMGetRelocationAddress');
      @LLVMGetRelocationOffset := Bind('LLVMGetRelocationOffset');
      @LLVMGetRelocationSymbol := Bind('LLVMGetRelocationSymbol');
      @LLVMGetRelocationType := Bind('LLVMGetRelocationType');
      @LLVMGetRelocationTypeName := Bind('LLVMGetRelocationTypeName');
      @LLVMGetRelocationValueString := Bind('LLVMGetRelocationValueString');
      @LTOGetVersion := Bind('LTOGetVersion');
      @LTOGetErrorMessage := Bind('LTOGetErrorMessage');
      @LTOModuleIsObjectFile := Bind('LTOModuleIsObjectFile');
      @LTOModuleIsObjectFileForTarget := Bind('LTOModuleIsObjectFileForTarget');
      @LTOModuleIsObjectFileInMemory := Bind('LTOModuleIsObjectFileInMemory');
      @LTOModuleIsObjectFileInMemoryForTarget := Bind('LTOModuleIsObjectFileInMemoryForTarget');
      @LTOModuleCreate := Bind('LTOModuleCreate');
      @LTOModuleCreateFromMemory := Bind('LTOModuleCreateFromMemory');
      @LTOModuleCreateFromFd := Bind('LTOModuleCreateFromFd');
      @LTOModuleCreateFromFdAtOffset := Bind('LTOModuleCreateFromFdAtOffset');
      @LTOModuleDispose := Bind('LTOModuleDispose');
      @LTOModuleGetTargetTriple := Bind('LTOModuleGetTargetTriple');
      @LTOModuleSetTargetTriple := Bind('LTOModuleSetTargetTriple');
      @LTOModuleGetNumSymbols := Bind('LTOModuleGetNumSymbols');
      @LTOModuleGetSymbolName := Bind('LTOModuleGetSymbolName');
      @LTOModuleGetSymbolAttribute := Bind('LTOModuleGetSymbolAttribute');
      @LTOCodegenCreate := Bind('LTOCodegenCreate');
      @LTOCodegenDispose := Bind('LTOCodegenDispose');
      @LTOCodegenAddModule := Bind('LTOCodegenAddModule');
      @LTOCodegenSetDebugModel := Bind('LTOCodegenSetDebugModel');
      @LTOCodegenSetPicModel := Bind('LTOCodegenSetPicModel');
      @LTOCodegenSetCpu := Bind('LTOCodegenSetCpu');
      @LTOCodegenSetAssemblerPath := Bind('LTOCodegenSetAssemblerPath');
      @LTOCodegenSetAssemblerArgs := Bind('LTOCodegenSetAssemblerArgs');
      @LTOCodegenAddMustPreserveSymbol := Bind('LTOCodegenAddMustPreserveSymbol');
      @LTOCodegenWriteMergedModules := Bind('LTOCodegenWriteMergedModules');
      @LTOCodegenCompile := Bind('LTOCodegenCompile');
      @LTOCodegenCompileToFile := Bind('LTOCodegenCompileToFile');
      @LTOCodegenDebugOptions := Bind('LTOCodegenDebugOptions');
   end
   else
      raise Exception.CreateFmt(RStrCannotLoadLLVMLib, [GLLVMLibraryName]);
end;

procedure UnloadLLVM;
begin
   if IsLLVMLoaded then
   begin
      FreeLibrary(GLLVMLibrary);
      GLLVMLibrary := 0;
   end;
end;


initialization
   LoadLLVM;

finalization
   UnloadLLVM;

end.
