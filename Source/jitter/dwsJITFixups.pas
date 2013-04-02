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
   dwsUtils;

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
         procedure SortFixups;
         procedure ResolveFixups;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddFixup(aFixup : TFixup);
         function  NewRelativeOffset(size : Integer) : TFixupRelativeOffset;
         function  NewTarget : TFixupTarget;
         function  NewHangingTarget : TFixupTarget;

         procedure FlushFixups(const rawCode : TBytes; outStream : TWriteOnlyBlockStream);
         procedure ClearFixups;

         property Base : TFixup read FBase write FBase;
         property OnNeedLocation : TFixupNeedLocationEvent read FOnNeedLocation write FOnNeedLocation;
   end;

   TFixup = class (TRefCountedObject)
      private
         FLogic : TFixupLogic;
         FNext : TFixup;
         FLocation : Integer;
         FFixedLocation : Integer;
         FOrder : Integer;

      public
         function  GetSize : Integer; virtual; abstract;
         procedure Write(output : TWriteOnlyBlockStream); virtual; abstract;

         property Logic : TFixupLogic read FLogic write FLogic;
         property Next : TFixup read FNext write FNext;
         property Location : Integer read FLocation write FLocation;
         property FixedLocation : Integer read FFixedLocation write FFixedLocation;
   end;

   TSortedFixupList = class (TSortedList<TFixup>)
      protected
         function Compare(const item1, item2 : TFixup) : Integer; override;
   end;

   TFixupTarget = class(TFixup)
      public
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

   TFixupTargeting = class(TFixup)
      private
         FTarget : TFixup;

      public
         function NewTarget : TFixupTarget;

         property Target : TFixup read FTarget write FTarget;
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
function TFixupLogic.NewTarget : TFixupTarget;
begin
   Result:=TFixupTarget.Create;
   AddFixup(Result);
end;

// NewHangingTarget
//
function TFixupLogic.NewHangingTarget : TFixupTarget;
begin
   Result:=TFixupTarget.Create;
   Result.Logic:=Self;
end;

// SortFixups
//
procedure TFixupLogic.SortFixups;
var
   fixup : TFixup;
   ordered : TSortedFixupList;
   i : Integer;
begin
   // sort fixups in ascending location order
   ordered:=TSortedFixupList.Create;
   try
      fixup:=FBase;
      i:=0;
      while fixup<>nil do begin
         fixup.FOrder:=i;
         Inc(i);
         ordered.Add(fixup);
         fixup:=fixup.Next;
      end;
      fixup:=ordered[0];
      FBase:=fixup;
      for i:=1 to ordered.Count-1 do begin
         fixup.Next:=ordered[i];
         fixup:=fixup.Next;
      end;
      fixup.Next:=nil;
   finally
      ordered.Free;
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
end;

// FlushFixups
//
procedure TFixupLogic.FlushFixups(const rawCode : TBytes; outStream : TWriteOnlyBlockStream);
var
   fixup : TFixup;
   prevLocation : Integer;
begin
   outStream.Clear;

   if FBase=nil then begin
      outStream.WriteBytes(rawCode);
      Exit;
   end;

   SortFixups;

   ResolveFixups;

   if FBase=nil then Exit;

   fixup:=FBase;
   prevLocation:=0;
   while fixup<>nil do begin
      outStream.Write(rawCode[prevLocation], fixup.Location-prevLocation);
      prevLocation:=fixup.Location;
      fixup.Write(outStream);
      fixup:=fixup.Next;
   end;
   if prevLocation<Length(rawCode) then
      outStream.Write(rawCode[prevLocation], Length(rawCode)-prevLocation);

   ClearFixups;
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
// ------------------ TSortedFixupList ------------------
// ------------------

// Compare
//
function TSortedFixupList.Compare(const item1, item2 : TFixup) : Integer;
begin
   Result:=item1.Location-item2.Location;
   if Result=0 then
      Result:=item2.FOrder-item1.FOrder;
end;

// ------------------
// ------------------ TFixup ------------------
// ------------------



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
function TFixupTargeting.NewTarget : TFixupTarget;
begin
   Assert(Target=nil);
   Result:=TFixupTarget.Create;
   Logic.AddFixup(Result);
   Target:=Result;
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
