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
unit UModel3DTests;

interface

uses
   System.Classes, System.SysUtils,
   dwsXPlatformTests;

type

   TdwsModel3DTests = class (TTestCase)
      protected

      published
         procedure TestCube1;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   dwsModel3DFilePLY, dwsXPlatform, dwsUtils;

// ------------------
// ------------------ TdwsModel3DTests ------------------
// ------------------

// TestCube1
//
procedure TdwsModel3DTests.TestCube1;
begin
   var model := TModel3DFilePLY.Create;
   var errors := TStringList.Create;
   try
      model.LoadFromData(
         LoadRawBytesFromFile(ExtractFilePath(ParamStr(0)) + 'Model3D\cube1.ply'),
         errors
      );
      CheckEquals(0, errors.Count, errors.Text);

      CheckEquals(2, model.ElementCount);

      var vertex := model.Elements[0];
      CheckEquals('vertex', vertex.Name);
      CheckEquals(8, vertex.Count);
      CheckEquals(3, vertex.PropertyCount);
      CheckEquals('x,y,z', vertex.PropertyNames.Join(','));
      CheckEquals(2, vertex.IndexOfProperty('z'));

      var face := model.Elements[1];
      CheckEquals('face', face.Name);
      CheckEquals(6, face.Count);
      CheckEquals(1, face.PropertyCount);
      CheckEquals('vertex_index', face.PropertyNames.Join(','));

   finally
      errors.Free;
      model.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('Model3D', TdwsModel3DTests);

end.