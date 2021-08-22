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
unit dwsJITFixups;

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsJITx86Intrinsics;

type
   TFixup = class;
   TFixupRelativeOffset = class;
   TFixupTarget = class;

   TFixupNeedLocationEvent = function : Integer of object;

   TFixupLogic = class
      private
         FBase : TFixup;
         FOnNeedLocation : TFixupNeedLocationEvent;

      protected
         procedure OptimizeFixups;
         procedure SortFixups;
         procedure ResolveFixups;

         procedure AfterResolve; virtual;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddFixup(aFixup : TFixup);
         function  NewRelativeOffset(size : Integer) : TFixupRelativeOffset;
         function  NewTarget(align : Boolean) : TFixupTarget;
         function  NewHangingTarget(align : Boolean) : TFixupTarget; virtual;

         procedure FlushFixups(outStream : Tx86BaseWriteOnlyStream); virtual;
         procedure ClearFixups; virtual;

         property Base : TFixup read FBase write FBase;
         property OnNeedLocation : TFixupNeedLocationEvent read FOnNeedLocation write FOnNeedLocation;
   end;

   TFixupOptimizeAction = (foaNone, foaRemove);

   TFixup = class (TObject)
      private
         FLogic : TFixupLogic;
         FNext : TFixup;
         FLocation : Integer;
         FFixedLocation : Integer;
         FTargetCount : Integer;

      public
         function  GetSize : Integer; virtual; abstract;
         procedure Write(output : TWriteOnlyBlockStream); virtual; abstract;

         function JumpLocation : Integer; virtual;
         function Optimize : TFixupOptimizeAction; virtual;

         property Logic : TFixupLogic read FLogic write FLogic;
         property Next : TFixup read FNext write FNext;
         property Location : Integer read FLocation write FLocation;
         property FixedLocation : Integer read FFixedLocation write FFixedLocation;
         property TargetCount : Integer read FTargetCount write FTargetCount;
   end;

   TFixupTarget = class(TFixup)
      public
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

   TFixupTargeting = class(TFixup)
      private
         FTarget : TFixup;

      protected
         procedure SetTarget(const aTarget : TFixup);

      public
         function NewTarget(align : Boolean) : TFixupTarget;

         property Target : TFixup read FTarget write SetTarget;
   end;


   TFixupRelativeOffset = class(TFixupTargeting)
      private
         FSize : Integer;

      public
         constructor Create(aSize : Integer);

         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TFixup ------------------
// ------------------

// JumpLocation
//
function TFixup.JumpLocation : Integer;
begin
   Result:=FixedLocation;
end;

// Optimize
//
function TFixup.Optimize : TFixupOptimizeAction;
begin
   Result:=foaNone;
end;

// ------------------
// ------------------ TFixupLogic ------------------
// ------------------

// Create
//
constructor TFixupLogic.Create;
begin
   inherited;
end;

// Destroy
//
destructor TFixupLogic.Destroy;
begin
   inherited;
   ClearFixups;
end;

// AddFixup
//
procedure TFixupLogic.AddFixup(aFixup : TFixup);
begin
   aFixup.FLogic:=Self;
   aFixup.Next:=FBase;
   aFixup.Location:=OnNeedLocation;
   FBase:=aFixup;
end;

// NewRelativeOffset
//
function TFixupLogic.NewRelativeOffset(size : Integer) : TFixupRelativeOffset;
begin
   Result:=TFixupRelativeOffset.Create(size);
   AddFixup(Result);
end;

// NewTarget
//
function TFixupLogic.NewTarget(align : Boolean) : TFixupTarget;
begin
   Result:=NewHangingTarget(align);
   AddFixup(Result);
end;

// NewHangingTarget
//
function TFixupLogic.NewHangingTarget(align : Boolean) : TFixupTarget;
begin
   Result:=TFixupTarget.Create;
   Result.Logic:=Self;
end;

// SortFixups
//
procedure TFixupLogic.SortFixups;
var
   next, candidate, iterator, iteratorPrev : TFixup;
begin
   // sort fixups in ascending location order
   if FBase = nil then Exit;
   candidate := FBase;
   FBase := nil;
   while candidate <> nil do begin
      next := candidate.Next;
      candidate.FNext := nil;

      iterator := FBase;
      iteratorPrev := nil;
      while (iterator <> nil) and (candidate.Location > iterator.Location) do begin
         iteratorPrev := iterator;
         iterator := iterator.Next;
      end;

      if iteratorPrev = nil then begin
         candidate.FNext := FBase;
         FBase := candidate;
      end else begin
         candidate.FNext := iteratorPrev.Next;
         iteratorPrev.Next := candidate;
      end;

      candidate := next;
   end;
end;

// OptimizeFixups
//
procedure TFixupLogic.OptimizeFixups;
var
   fixup, next, prev : TFixup;
begin
   prev:=nil;
   fixup:=FBase;
   while fixup<>nil do begin
      next:=fixup.Next;
      case fixup.Optimize of
         foaRemove : begin
            if fixup=FBase then
               FBase:=next
            else prev.Next:=next;
            fixup.Free;
         end;
      else
         prev:=fixup;
      end;
      fixup:=next;
   end;
end;

// ResolveFixups
//
procedure TFixupLogic.ResolveFixups;
var
   fixup : TFixup;
   changed : Boolean;
   prevLocation, fixedLocation : Integer;
begin
   // brute-force, need improvements
   repeat
      changed:=False;
      prevLocation:=0;
      fixedLocation:=0;
      fixup:=FBase;
      while fixup<>nil do begin
         Inc(fixedLocation, fixup.Location-prevLocation);
         prevLocation:=fixup.Location;
         if fixup.FixedLocation<>fixedLocation then begin
            changed:=True;
            fixup.FixedLocation:=fixedLocation;
         end;
         Inc(fixedLocation, fixup.GetSize);
         fixup:=fixup.Next;
      end;
   until not changed;

   AfterResolve;
end;

// AfterResolve
//
procedure TFixupLogic.AfterResolve;
begin
   // nothing here
end;

// FlushFixups
//
procedure TFixupLogic.FlushFixups(outStream : Tx86BaseWriteOnlyStream);
var
   fixup : TFixup;
   prevLocation, n, rawCodeLength : Integer;
   rawCode : PAnsiChar;
begin
   if FBase = nil then
      Exit;

   rawCodeLength := outStream.Size;
   GetMem(rawCode, rawCodeLength);
   try
      outStream.StoreData(rawCode^);
      outStream.Clear;

      SortFixups;

      OptimizeFixups;

      ResolveFixups;

      if FBase=nil then Exit;

      fixup:=FBase;
      prevLocation:=0;
      while fixup<>nil do begin
         n:=fixup.Location-prevLocation;
         if n>0 then
            outStream.Write(rawCode[prevLocation], n);
         prevLocation:=fixup.Location;
         fixup.Write(outStream);
         fixup:=fixup.Next;
      end;
      if prevLocation<Length(rawCode) then
         outStream.Write(rawCode[prevLocation], rawCodeLength-prevLocation);
   finally
      FreeMem(rawCode);
   end;
end;

// ClearFixups
//
procedure TFixupLogic.ClearFixups;
var
   fixup : TFixup;
begin
   while FBase<>nil do begin
      fixup:=FBase.Next;
      FBase.Free;
      FBase:=fixup;
   end;
end;

// ------------------
// ------------------ TFixupTarget ------------------
// ------------------

// GetSize
//
function TFixupTarget.GetSize : Integer;
begin
   Result:=0;
end;

// Write
//
procedure TFixupTarget.Write(output : TWriteOnlyBlockStream);
begin
   // nothing
end;

// ------------------
// ------------------ TFixupTargeting ------------------
// ------------------

// NewTarget
//
function TFixupTargeting.NewTarget(align : Boolean) : TFixupTarget;
begin
   Result:=Logic.NewTarget(align);
   Target:=Result;
end;

// SetTarget
//
procedure TFixupTargeting.SetTarget(const aTarget : TFixup);
begin
   if FTarget<>nil then
      FTarget.TargetCount:=FTarget.TargetCount-1;
   FTarget:=aTarget;
   if FTarget<>nil then
      FTarget.TargetCount:=FTarget.TargetCount+1;
end;

// ------------------
// ------------------ TFixupRelativeOffset ------------------
// ------------------

// Create
//
constructor TFixupRelativeOffset.Create(aSize : Integer);
begin
   FSize:=aSize;
end;

// GetSize
//
function TFixupRelativeOffset.GetSize : Integer;
begin
   Result:=FSize;
end;

// Write
//
procedure TFixupRelativeOffset.Write(output : TWriteOnlyBlockStream);
var
   offset : Integer;
begin
   offset:=Target.FixedLocation-FixedLocation;
   case FSize of
      1 : output.Write(offset, 1);
      4 : output.Write(offset, 4);
   else
      Assert(False);
   end;
end;

end.
