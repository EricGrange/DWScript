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
unit UdwsCustomDataTests;

interface

uses
   System.Variants, System.SysUtils,
   dwsXPlatformTests, dwsCustomData, dwsUtils;

type

   TdwsCustomDataTests = class (TTestCase)
      private

      protected
         function PurgeCustomState(const item : TdwsCustomState) : TSimpleHashAction;

      published
         procedure VariantStates;
         procedure TypedQueries;
         procedure CustomInterfaces;
         procedure CloneTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cTestID1 : TGUID = '{C7762FF4-98D2-4A9A-8301-DC02557606C5}';
   cTestID2 : TGUID = '{A5F63DCB-C7CF-4946-A6A2-665FF3B8181B}';

// ------------------
// ------------------ TdwsCustomDataTests ------------------
// ------------------


// VariantStates
//
procedure TdwsCustomDataTests.VariantStates;
var
   states : TdwsCustomStates;
begin
   states := TdwsCustomStates.Create;
   try
      CheckEquals(0, states.Count);

      CheckTrue(VarIsEmpty(states.States[cTestID1]));

      states.States[cTestID1] := 'hello';

      CheckEquals('hello', states.States[cTestID1]);
      CheckTrue(VarIsEmpty(states.States[cTestID2]));

      states.States[cTestID2] := 1234;

      CheckEquals('hello', states.States[cTestID1]);
      CheckEquals(1234, states.States[cTestID2]);

      CheckEquals(2, states.Count);

      states.States[cTestID1] := 5678;

      CheckEquals(5678, states.States[cTestID1]);
      CheckEquals(1234, states.States[cTestID2]);

      states.Clear;

      CheckEquals(0, states.Count);
   finally
      states.Free;
   end;
end;

// TypedQueries
//
procedure TdwsCustomDataTests.TypedQueries;
var
   states : TdwsCustomStates;
begin
   states := TdwsCustomStates.Create;
   try
      CheckEquals(True, states.BooleanStateDef(cTestID1, True));
      CheckEquals(-1, states.IntegerStateDef(cTestID1, -1));
      CheckEquals('foo', states.StringStateDef(cTestID1, 'foo'));

      states.States[cTestID1] := Null;
      states.States[cTestID2] := Null;

      CheckEquals(True, states.BooleanStateDef(cTestID1, True));
      CheckEquals(-1, states.IntegerStateDef(cTestID1, -1));
      CheckEquals('foo', states.StringStateDef(cTestID1, 'foo'));

      states.States[cTestID1] := 0;
      states.States[cTestID2] := 1;

      CheckEquals(False, states.BooleanStateDef(cTestID1, True));
      CheckEquals(True, states.BooleanStateDef(cTestID2, True));
      CheckEquals(False, states.BooleanStateDef(cTestID1, False));
      CheckEquals(True, states.BooleanStateDef(cTestID2, False));

      CheckEquals(0, states.IntegerStateDef(cTestID1, 2));
      CheckEquals(1, states.IntegerStateDef(cTestID2, 2));

      CheckEquals('0', states.StringStateDef(cTestID1, 'foo'));
      CheckEquals('1', states.StringStateDef(cTestID2, 'bar'));

      states.States[cTestID1] := False;
      states.States[cTestID2] := True;

      CheckEquals(False, states.BooleanStateDef(cTestID1, True));
      CheckEquals(True, states.BooleanStateDef(cTestID2, True));
      CheckEquals(False, states.BooleanStateDef(cTestID1, False));
      CheckEquals(True, states.BooleanStateDef(cTestID2, False));

      CheckEquals(0, states.IntegerStateDef(cTestID1, 2));
      CheckEquals(-1, states.IntegerStateDef(cTestID2, 2));

      CheckEquals('False', states.StringStateDef(cTestID1, 'foo'));
      CheckEquals('True', states.StringStateDef(cTestID2, 'bar'));

      states.States[cTestID1] := 'hello';
      states.States[cTestID2] := 'true';

      CheckEquals(False, states.BooleanStateDef(cTestID1, True));
      CheckEquals(True, states.BooleanStateDef(cTestID2, True));
      CheckEquals(False, states.BooleanStateDef(cTestID1, False));
      CheckEquals(True, states.BooleanStateDef(cTestID2, False));

      CheckEquals(2, states.IntegerStateDef(cTestID1, 2));
      CheckEquals(2, states.IntegerStateDef(cTestID2, 2));

      CheckEquals('hello', states.StringStateDef(cTestID1, 'foo'));
      CheckEquals('true', states.StringStateDef(cTestID2, 'bar'));
   finally
      states.Free;
   end;
end;

// CustomInterfaces
//
procedure TdwsCustomDataTests.CustomInterfaces;
var
   intfs : TdwsCustomInterfaces;
   i : IInterface;
begin
   i := TInterfacedObject.Create;
   intfs := TdwsCustomInterfaces.Create;
   try
      intfs.Interfaces[cTestID1] := i;

      Check(intfs.Interfaces[cTestID1] = i, 'id1 set');
      Check(intfs.Interfaces[cTestID2] = nil, 'id2');

      intfs.Interfaces[cTestID1] := nil;

      Check(intfs.Interfaces[cTestID1] = nil, 'id1 clear');
   finally
      intfs.Free;
   end;
end;

// CloneTest
//
procedure TdwsCustomDataTests.CloneTest;
const
   cVal1 : TGUID = '{EC8DB0C4-AD8D-4008-A45E-7A54CB12B12C}';
   cVal2 : TGUID = '{44ABDD6B-E6E0-4D67-BFEC-36A441990A56}';
begin
   var states1 := TdwsCustomStates.Create;
   var states2 : TdwsCustomStates := nil;
   try
      states1[cVal1] := 123456;
      states1[cVal2] := 'hello';

      states2 := states1.Clone;

      states1.Enumerate(PurgeCustomState);

      FreeAndNil(states1);

      CheckEquals(2, states2.Count);
      CheckEquals(123456, states2[cVal1]);
      CheckEquals('hello', states2[cVal2]);

      states2.Clear(False);
      CheckEquals(0, states2.Count);
      CheckTrue(states2.Capacity > 0);
   finally
      states1.Free;
      states2.Free;
   end;
end;

// PurgeCustomState
//
function TdwsCustomDataTests.PurgeCustomState(const item : TdwsCustomState) : TSimpleHashAction;
begin
   Result := shaRemove;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CustomData', TdwsCustomDataTests);

end.
