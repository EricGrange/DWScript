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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsLanguageExtension;

{$I dws.inc}

interface

uses
  Classes, Types,
  dwsCompiler, dwsExprs, dwsErrors, dwsSymbols, dwsUnitSymbols, dwsScriptSource;

type

   // TdwsLanguageExtension
   //
   {: Base class for DWS language extensions. }
   TdwsLanguageExtension = class
      public
         constructor Create; virtual;

         function CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol; virtual;
         procedure CreateSystemSymbols(table : TSystemSymbolTable); virtual;
         function StaticSymbols : Boolean; virtual;
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; virtual;
         function ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr; virtual;
         function ReadInstrSwitch(compiler : TdwsCompiler) : Boolean; virtual;
         function FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol; virtual;
         procedure SectionChanged(compiler : TdwsCompiler); virtual;
         procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                              scriptType : TScriptSourceType); virtual;
         procedure GetDefaultEnvironment(var enviro : IdwsEnvironment); virtual;
         function RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol; virtual;
         procedure ApplyConditionalDefines(defines : TStrings); virtual;
   end;

   // TdwsLanguageExtensionAggregator
   //
   {: Aggregate multiple language extensions (referred, not owned). }
   TdwsLanguageExtensionAggregator = class (TdwsLanguageExtension)
      private
         FList : TList;

      public
         constructor Create; override;
         destructor Destroy; override;

         procedure Add(anExtension : TdwsLanguageExtension);
         procedure Remove(anExtension : TdwsLanguageExtension);
         function Count : Integer;
         procedure Clear;

         function CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol; override;
         procedure CreateSystemSymbols(table : TSystemSymbolTable); override;
         function StaticSymbols : Boolean; override;
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;
         function ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr; override;
         function ReadInstrSwitch(compiler : TdwsCompiler) : Boolean; override;
         function FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol; override;
         procedure SectionChanged(compiler : TdwsCompiler); override;
         procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                              scriptType : TScriptSourceType); override;
         procedure GetDefaultEnvironment(var enviro : IdwsEnvironment); override;
         function DefaultEnvironment : IdwsEnvironment;
         function RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol; override;
         procedure ApplyConditionalDefines(defines : TStrings); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsLanguageExtension ------------------
// ------------------

// Create
//
constructor TdwsLanguageExtension.Create;
begin
   inherited;
end;

// CreateBaseVariantSymbol
//
function TdwsLanguageExtension.CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol;
begin
   Result:=TBaseVariantSymbol.Create;
   table.AddSymbol(Result);
   table.TypVariant:=Result;
end;

// CreateSystemSymbols
//
procedure TdwsLanguageExtension.CreateSystemSymbols(table : TSystemSymbolTable);
begin
   // nothing
end;

// StaticSymbols
//
function TdwsLanguageExtension.StaticSymbols : Boolean;
begin
   Result:=False;
end;

// ReadInstrSwitch
//
function TdwsLanguageExtension.ReadUnknownName(compiler: TdwsCompiler) : TTypedExpr;
begin
   Result:=nil;
end;

// ReadInstr
//
function TdwsLanguageExtension.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
begin
   Result:=nil;
end;

// ReadInstrSwitch
//
function TdwsLanguageExtension.ReadInstrSwitch(compiler : TdwsCompiler) : Boolean;
begin
   Result:=False;
end;

// FindUnknownName
//
function TdwsLanguageExtension.FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol;
begin
   Result:=nil;
end;

// SectionChanged
//
procedure TdwsLanguageExtension.SectionChanged(compiler : TdwsCompiler);
begin
   // nothing
end;

// ReadScript
//
procedure TdwsLanguageExtension.ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                                           scriptType : TScriptSourceType);
begin
   // nothing
end;

// GetDefaultEnvironment
//
procedure TdwsLanguageExtension.GetDefaultEnvironment(var enviro : IdwsEnvironment);
begin
   // nothing
end;

// RootExternalClass
//
function TdwsLanguageExtension.RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol;
begin
   Result := compiler.CompilerContext. TypObject;
end;

// ApplyConditionalDefines
//
procedure TdwsLanguageExtension.ApplyConditionalDefines(defines : TStrings);
begin
   // nothing
end;

// ------------------
// ------------------ TdwsLanguageExtensionAggregator ------------------
// ------------------

// Create
//
constructor TdwsLanguageExtensionAggregator.Create;
begin
   inherited;
   FList:=TList.Create;
end;

// Destroy
//
destructor TdwsLanguageExtensionAggregator.Destroy;
begin
   inherited;
   FList.Free;
end;

// Add
//
procedure TdwsLanguageExtensionAggregator.Add(anExtension : TdwsLanguageExtension);
begin
   FList.Add(anExtension);
end;

// Remove
//
procedure TdwsLanguageExtensionAggregator.Remove(anExtension : TdwsLanguageExtension);
begin
   FList.Remove(anExtension);
end;

// Count
//
function TdwsLanguageExtensionAggregator.Count : Integer;
begin
   Result:=FList.Count;
end;

// Clear
//
procedure TdwsLanguageExtensionAggregator.Clear;
begin
   FList.Clear;
end;

// CreateBaseVariantSymbol
//
function TdwsLanguageExtensionAggregator.CreateBaseVariantSymbol(table : TSystemSymbolTable) : TBaseVariantSymbol;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=ext.CreateBaseVariantSymbol(table);
      if Result<>nil then Exit;
   end;
   Result:=nil;
end;

// CreateSystemSymbols
//
procedure TdwsLanguageExtensionAggregator.CreateSystemSymbols(table : TSystemSymbolTable);
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      ext.CreateSystemSymbols(table);
   end;
end;

// StaticSymbols
//
function TdwsLanguageExtensionAggregator.StaticSymbols : Boolean;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   Result:=True;
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=Result and ext.StaticSymbols;
   end;
end;

// ReadUnknownName
//
function TdwsLanguageExtensionAggregator.ReadUnknownName(compiler: TdwsCompiler): TTypedExpr;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=ext.ReadUnknownName(compiler);
      if Result<>nil then Exit;
   end;
   Result:=nil;
end;

// ReadInstr
//
function TdwsLanguageExtensionAggregator.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=ext.ReadInstr(compiler);
      if Result<>nil then Exit;
   end;
   Result:=nil;
end;

// ReadInstrSwitch
//
function TdwsLanguageExtensionAggregator.ReadInstrSwitch(compiler : TdwsCompiler) : Boolean;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      if ext.ReadInstrSwitch(compiler) then
         Exit(True);
   end;
   Result:=False;
end;

// FindUnknownName
//
function TdwsLanguageExtensionAggregator.FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=ext.FindUnknownName(compiler, name);
      if Result<>nil then Exit;
   end;
   Result:=inherited FindUnknownName(compiler, name);
end;

// SectionChanged
//
procedure TdwsLanguageExtensionAggregator.SectionChanged(compiler : TdwsCompiler);
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      ext.SectionChanged(compiler);
   end;
end;

// ReadScript
//
procedure TdwsLanguageExtensionAggregator.ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                                                     scriptType : TScriptSourceType);
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      ext.ReadScript(compiler, sourceFile, scriptType);
   end;
end;

// GetDefaultEnvironment
//
procedure TdwsLanguageExtensionAggregator.GetDefaultEnvironment(var enviro : IdwsEnvironment);
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      ext.GetDefaultEnvironment(enviro);
   end;
end;

// DefaultEnvironment
//
function TdwsLanguageExtensionAggregator.DefaultEnvironment : IdwsEnvironment;
begin
   Result:=nil;
   GetDefaultEnvironment(Result);
end;

// RootExternalClass
//
function TdwsLanguageExtensionAggregator.RootExternalClass(compiler : TdwsCompiler; const externalName : String) : TClassSymbol;
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   Result:=nil;
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      Result:=ext.RootExternalClass(compiler, externalName);
      if Result<>nil then Exit;
   end;
end;

// ApplyConditionalDefines
//
procedure TdwsLanguageExtensionAggregator.ApplyConditionalDefines(defines : TStrings);
var
   i : Integer;
   ext : TdwsLanguageExtension;
begin
   for i:=0 to FList.Count-1 do begin
      ext:=TdwsLanguageExtension(FList.List[i]);
      ext.ApplyConditionalDefines(defines);
   end;
end;

end.
