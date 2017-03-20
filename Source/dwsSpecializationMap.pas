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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsSpecializationMap;

interface

uses dwsUtils;

type

   TSpecializationMap = class
      private
         FMap : TSimpleObjectObjectHash<TRefCountedObject, TRefCountedObject>;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         function SpecializationOf(obj : TRefCountedObject) : TRefCountedObject;
         procedure RegisterSpecialization(generic, specialized : TRefCountedObject);

         procedure Clean;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSpecializationMap ------------------
// ------------------

// Create
//
constructor TSpecializationMap.Create;
begin
   inherited;
   FMap := TSimpleObjectObjectHash<TRefCountedObject, TRefCountedObject>.Create;
end;

// Destroy
//
destructor TSpecializationMap.Destroy;
begin
   inherited;
   FMap.Clear;
   FMap.Free;
end;

// SpecializationOf
//
function TSpecializationMap.SpecializationOf(obj : TRefCountedObject) : TRefCountedObject;
begin
   Result := FMap.GetValue(obj);
end;

// RegisterSpecialization
//
procedure TSpecializationMap.RegisterSpecialization(generic, specialized : TRefCountedObject);
var
   prevSpec : TRefCountedObject;
begin
   prevSpec := FMap.GetValue(generic);
   if prevSpec <> nil then begin
      Assert(specialized = prevSpec);
   end;
   FMap.SetValue(generic, specialized);
end;

// Clean
//
procedure TSpecializationMap.Clean;
begin
   FMap.CleanValues;
end;

end.
