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
   dwsXPlatformTests, dwsCustomData, Variants;

type

   TdwsCustomDataTests = class (TTestCase)
      private

      protected

      published
         procedure VariantStates;
         procedure TypedQueries;
         procedure CustomInterfaces;
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CustomData', TdwsCustomDataTests);

end.
