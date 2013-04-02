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

   TdwsJITCodeSubAllocator = class (TRefCountedObject)
      private
         FBlock : Pointer;
         FCurrent : PByte;
         FBlockSize : Integer;
         FRemaining : Integer;
         FNext : TdwsJITCodeSubAllocator;

      protected
         property Next : TdwsJITCodeSubAllocator read FNext write FNext;

      public
         constructor Create(aSize : Integer);
         destructor Destroy; override;

         property Remaining : Integer read FRemaining;

         function Allocate(aSize : Integer) : Pointer;

         procedure Protect;
   end;

   TdwsJITAllocatorWin = class
      private
         FSubAllocators : TdwsJITCodeSubAllocator;
         FAllocatorSize : Integer;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         function Allocate(const code : TBytes) : TdwsJITCodeBlock;

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
function TdwsJITAllocatorWin.Allocate(const code : TBytes) : TdwsJITCodeBlock;
var
   sub : TdwsJITCodeSubAllocator;
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
      sub:=TdwsJITCodeSubAllocator.Create(s);
      sub.IncRefCount;
      sub.Next:=FSubAllocators;
      FSubAllocators:=sub;
      p:=sub.Allocate(n);
   end;

   Result.Code:=p;
   Result.SubAllocator:=sub;
   Result.Steppable:=False;
   sub.IncRefCount;
   System.Move(code[0], p^, n);
end;

// DetachSubAllocators
//
procedure TdwsJITAllocatorWin.DetachSubAllocators;
var
   sub : TdwsJITCodeSubAllocator;
begin
   while FSubAllocators<>nil do begin
      sub:=FSubAllocators;
      FSubAllocators:=sub.Next;
      sub.Protect;
      sub.DecRefCount;
      sub.Free;
   end;
end;

end.
