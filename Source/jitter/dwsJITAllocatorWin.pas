{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsJITAllocatorWin;

{$I ../dws.inc}

interface

uses
   Windows, Classes, SysUtils,
   dwsUtils, dwsXPlatform, dwsJIT;

const
   cJITSubAllocatorSize = 64*1024; // 64 kB

type
   TdwsJITCodeSubAllocator = class (TInterfacedObject, IdwsJITCodeSubAllocator)
      private
         FBlock : Pointer;
         FCurrent : PByte;
         FBlockSize : Integer;
         FRemaining : Integer;
         FNext : IdwsJITCodeSubAllocator;

      protected
         function GetNext : IdwsJITCodeSubAllocator;
         procedure SetNext(const aNext : IdwsJITCodeSubAllocator);

      public
         constructor Create(aSize : Integer);
         destructor Destroy; override;

         property Remaining : Integer read FRemaining;

         function Allocate(aSize : Integer) : Pointer;

         procedure Protect;
   end;

   TdwsJITAllocatorWin = class
      private
         FSubAllocators : IdwsJITCodeSubAllocator;
         FAllocatorSize : Integer;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         function Allocate(const code : TBytes; var aSubAllocator : IdwsJITCodeSubAllocator) : Pointer;

         procedure DetachSubAllocators;

         property AllocatorSize : Integer read FAllocatorSize write FAllocatorSize;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJITCodeSubAllocator ------------------
// ------------------

// Create
//
constructor TdwsJITCodeSubAllocator.Create(aSize : Integer);
begin
   FBlockSize:=aSize;
   FRemaining:=aSize;
   FBlock:=VirtualAlloc(nil, aSize, MEM_COMMIT, PAGE_READWRITE);
   FCurrent:=FBlock;
end;

// Destroy
//
destructor TdwsJITCodeSubAllocator.Destroy;
begin
   inherited;
   VirtualFree(FBlock, 0, MEM_RELEASE);
end;

// Allocate
//
function TdwsJITCodeSubAllocator.Allocate(aSize : Integer) : Pointer;
begin
   // round up to multiple of 16
   aSize := ((aSize + 15) shr 4) shl 4;

   if aSize>FRemaining then
      Exit(nil);

   Result:=FCurrent;
   Inc(FCurrent, aSize);
   Dec(FRemaining, aSize);
   if FRemaining<64 then
      Protect;
end;

// Protect
//
procedure TdwsJITCodeSubAllocator.Protect;
var
   oldProtect : Integer;
   n : Integer;
begin
   if FRemaining>16 then
      n:=16
   else n:=FRemaining;
   // pad a small range with nops after last code
   FillChar(FCurrent^, n, $90);
   FRemaining:=0;
   VirtualProtect(FBlock, FBlockSize, PAGE_EXECUTE_READWRITE, @oldProtect);
end;

// GetNext
//
function TdwsJITCodeSubAllocator.GetNext : IdwsJITCodeSubAllocator;
begin
   Result := FNext;
end;

// SetNext
//
procedure TdwsJITCodeSubAllocator.SetNext(const aNext : IdwsJITCodeSubAllocator);
begin
   FNext := aNext;
end;

// ------------------
// ------------------ TdwsJITAllocatorWin ------------------
// ------------------

// Create
//
constructor TdwsJITAllocatorWin.Create;
begin
   inherited;
   FAllocatorSize:=cJITSubAllocatorSize;
end;

// Destroy
//
destructor TdwsJITAllocatorWin.Destroy;
begin
   inherited;
   DetachSubAllocators;
end;

// Allocate
//
function TdwsJITAllocatorWin.Allocate(const code : TBytes; var aSubAllocator : IdwsJITCodeSubAllocator) : Pointer;
var
   sub : IdwsJITCodeSubAllocator;
   n, s : Integer;
   p : Pointer;
begin
   n:=Length(code);
   Assert(n>0);
   p:=nil;
   sub:=FSubAllocators;
   while sub<>nil do begin
      p:=sub.Allocate(n);
      if p<>nil then break;
   end;
   if p=nil then begin
      if n>AllocatorSize then
         s:=n+16
      else s:=AllocatorSize;
      sub := TdwsJITCodeSubAllocator.Create(s);
      sub.Next:=FSubAllocators;
      FSubAllocators:=sub;
      p:=sub.Allocate(n);
   end;

   System.Move(code[0], p^, n);
   Result := p;
   aSubAllocator := sub;
end;

// DetachSubAllocators
//
procedure TdwsJITAllocatorWin.DetachSubAllocators;
var
   sub : IdwsJITCodeSubAllocator;
begin
   while FSubAllocators<>nil do begin
      sub := FSubAllocators;
      FSubAllocators := sub.Next;
      sub.Protect;
   end;
end;

end.
