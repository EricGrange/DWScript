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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsCodeGenWriters;

{$I dws.inc}

interface

uses
   SysUtils,
   dwsUtils, dwsSymbols, dwsDataContext, dwsCodeGen;

type
   TdwsCodeGenSymbolWriters = class;

   TdwsCodeGenSymbolWriter = class
      private
         FSymbolClass : TSymbolClass;
         FCodeGen : TdwsCodeGen;
         FOwner : TdwsCodeGenSymbolWriters;

      protected

      public
         constructor Create(aSymbolClass : TSymbolClass); virtual;

         property SymbolClass : TSymbolClass read FSymbolClass;
         property CodeGen : TdwsCodeGen read FCodeGen;
         property Owner : TdwsCodeGenSymbolWriters read FOwner;

         procedure WriteDefaultValue(sym : TSymbol); virtual; abstract;
         procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); virtual; abstract;

   end;

   TdwsCodeGenSymbolClassWriter = record
      SymbolClass : TClass;
      Writer : TdwsCodeGenSymbolWriter;
   end;

   TdwsCodeGenSymbolWriterLookup = class(TSimpleHash<TdwsCodeGenSymbolClassWriter>)
      protected
         function SameItem(const item1, item2 : TdwsCodeGenSymbolClassWriter) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsCodeGenSymbolClassWriter) : Cardinal; override;
   end;

   TdwsCodeGenSymbolWriters = class
      private
         FRegisteredWriters : array of TdwsCodeGenSymbolWriter;
         FWriterBySymbolClass : TdwsCodeGenSymbolWriterLookup;
         FCodeGen : TdwsCodeGen;

      protected
         function FindBestMatch(aSymbolClass : TClass) : TdwsCodeGenSymbolWriter;

      public
         constructor Create(aCodeGen : TdwsCodeGen);
         destructor Destroy; override;

         procedure Clean;

         procedure RegisterWriter(writer : TdwsCodeGenSymbolWriter);

         function Writer(aSymbolClass : TClass) : TdwsCodeGenSymbolWriter;

         procedure WriteDefaultValue(typ : TTypeSymbol);
         procedure WriteValue(typ : TTypeSymbol; const dataPtr : IDataContext);

         property CodeGen : TdwsCodeGen read FCodeGen;
   end;

implementation

// ------------------
// ------------------ TdwsCodeGenSymbolWriter ------------------
// ------------------

// Create
//
constructor TdwsCodeGenSymbolWriter.Create(aSymbolClass : TSymbolClass);
begin
   inherited Create;
   FSymbolClass := aSymbolClass;
end;

// ------------------
// ------------------ TdwsCodeGenSymbolWriterLookup ------------------
// ------------------

// SameItem
//
function TdwsCodeGenSymbolWriterLookup.SameItem(const item1, item2 : TdwsCodeGenSymbolClassWriter) : Boolean;
begin
   Result := item1.SymbolClass = item2.SymbolClass;
end;

// GetItemHashCode
//
function TdwsCodeGenSymbolWriterLookup.GetItemHashCode(const item1 : TdwsCodeGenSymbolClassWriter) : Cardinal;
begin
   Result := SimplePointerHash(item1.SymbolClass);
end;

// ------------------
// ------------------ TdwsCodeGenSymbolWriters ------------------
// ------------------

// Create
//
constructor TdwsCodeGenSymbolWriters.Create(aCodeGen : TdwsCodeGen);
begin
   inherited Create;
   FCodeGen := aCodeGen;
end;

// Destroy
//
destructor TdwsCodeGenSymbolWriters.Destroy;
begin
   inherited;
   Clean;
end;

// Clean
//
procedure TdwsCodeGenSymbolWriters.Clean;
var
   i : Integer;
begin
   FreeAndNil(FWriterBySymbolClass);
   for i := 0 to High(FRegisteredWriters) do
      FRegisteredWriters[i].Free;
   SetLength(FRegisteredWriters, 0);
end;

// RegisterWriter
//
procedure TdwsCodeGenSymbolWriters.RegisterWriter(writer : TdwsCodeGenSymbolWriter);
var
   i, n : Integer;
begin
   Assert(writer.FOwner = nil);
   n := Length(FRegisteredWriters);
   for i := 0 to n-1 do
      Assert(FRegisteredWriters[i].SymbolClass <> writer.SymbolClass);
   SetLength(FRegisteredWriters, n+1);
   FRegisteredWriters[n] := writer;
   FreeAndNil(FWriterBySymbolClass);
   writer.FCodeGen := CodeGen;
   writer.FOwner := Self;
end;

// FindBestMatch
//
function TdwsCodeGenSymbolWriters.FindBestMatch(aSymbolClass : TClass) : TdwsCodeGenSymbolWriter;
var
   i : Integer;
   parent : TClass;
begin
   while aSymbolClass <> nil do begin
      for i := 0 to High(FRegisteredWriters) do begin
         Result := FRegisteredWriters[i];
         if Result.SymbolClass = aSymbolClass then
            Exit;
      end;
      parent := aSymbolClass.ClassParent;
      if parent.InheritsFrom(TSymbol) then
         aSymbolClass := parent
      else Break;
   end;
   Result := nil;
end;

// Writer
//
function TdwsCodeGenSymbolWriters.Writer(aSymbolClass : TClass) : TdwsCodeGenSymbolWriter;
var
   temp : TdwsCodeGenSymbolClassWriter;
begin
   temp.SymbolClass := aSymbolClass;
   if FWriterBySymbolClass = nil then
      FWriterBySymbolClass := TdwsCodeGenSymbolWriterLookup.Create
   else if FWriterBySymbolClass.Match(temp) then
      Exit(temp.Writer);

   temp.Writer := FindBestMatch(aSymbolClass);
   FWriterBySymbolClass.Add(temp);
   Result := temp.Writer;
end;

// WriteDefaultValue
//
procedure TdwsCodeGenSymbolWriters.WriteDefaultValue(typ : TTypeSymbol);
var
   symbolWriter : TdwsCodeGenSymbolWriter;
begin
   typ := typ.UnAliasedType;
   symbolWriter := Self.Writer(typ.ClassType);
   if symbolWriter <> nil then
      symbolWriter.WriteDefaultValue(typ)
   else raise ECodeGenUnsupportedSymbol.CreateFmt('Default value of type %s', [typ.ClassName]);
end;

// WriteValue
//
procedure TdwsCodeGenSymbolWriters.WriteValue(typ : TTypeSymbol; const dataPtr : IDataContext);
var
   symbolWriter : TdwsCodeGenSymbolWriter;
begin
   typ := typ.UnAliasedType;
   symbolWriter := Self.Writer(typ.ClassType);
   if symbolWriter <> nil then
      symbolWriter.WriteValue(typ, dataPtr)
   else raise ECodeGenUnsupportedSymbol.CreateFmt('Value of type %s', [typ.ClassName]);
end;

end.
