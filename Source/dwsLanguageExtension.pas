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
{$I dws.inc}
unit dwsLanguageExtension;

interface

uses Classes, dwsCompiler, dwsExprs, dwsErrors;

type

   // TdwsLanguageExtension
   //
   {: Base class for DWS language extensions. }
   TdwsLanguageExtension = class
      public
         constructor Create; virtual;

         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; virtual;
         procedure SectionChanged(compiler : TdwsCompiler); virtual;
         procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                              scriptType : TScriptSourceType); virtual;
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

         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;
         procedure SectionChanged(compiler : TdwsCompiler); override;
         procedure ReadScript(compiler : TdwsCompiler; sourceFile : TSourceFile;
                              scriptType : TScriptSourceType); override;
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

// ReadInstr
//
function TdwsLanguageExtension.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
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

end.
