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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsFilter;

{$I dws.inc}

interface

uses
   Classes,
   dwsErrors;

type
   TdwsFilter = class;

   TdwsFilter = class(TComponent)
      private
         FSubFilter : TdwsFilter;
         FDependencies : TStrings;
         FPrivateDependencies : TStrings;
         FEditorMode : Integer;

         function GetDependencies : TStrings;

      protected
         procedure SetSubFilter(const filter : TdwsFilter); virtual;
         procedure Notification(aComponent : TComponent; operation: TOperation); override;

         property PrivateDependencies : TStrings read FPrivateDependencies;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         function Process(const aText : UnicodeString; aMsgs : TdwsMessageList) : UnicodeString; virtual;

         property Dependencies : TStrings read GetDependencies;

         (*
           EditorMode means the filter should strive to keep source locations unchanged
           as the compilation will be used of Code Editor support features rather than execution
         *)
         procedure BeginEditorMode;
         procedure EndEditorMode;
         function  EditorMode : Boolean; inline;

      published
         property SubFilter : TdwsFilter read FSubFilter write SetSubFilter;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsFilter ------------------
// ------------------

constructor TdwsFilter.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TStringList.Create;
  FPrivateDependencies := TStringList.Create;
end;

destructor TdwsFilter.Destroy;
begin
  inherited;
  FDependencies.Free;
  FPrivateDependencies.Free;
end;

function TdwsFilter.GetDependencies: TStrings;
begin
  FDependencies.Clear;
  FDependencies.AddStrings(FPrivateDependencies);

  // Merge dependencies with subfilter dependencies
  if Assigned(FSubFilter) then
    FDependencies.AddStrings(FSubFilter.Dependencies);

  Result := FDependencies;
end;

procedure TdwsFilter.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSubFilter) then
    SetSubFilter(nil);
end;

// BeginEditorMode
//
procedure TdwsFilter.BeginEditorMode;
begin
   if Self<>nil then
      Inc(FEditorMode);
end;

// EndEditorMode
//
procedure TdwsFilter.EndEditorMode;
begin
   if Self<>nil then begin
      Assert(FEditorMode>0, 'Unbalanced EndEditorMode');
      Dec(FEditorMode);
   end;
end;

// EditorMode
//
function TdwsFilter.EditorMode : Boolean;
begin
   Result:=(FEditorMode>0);
end;

// Process
//
function TdwsFilter.Process(const aText : UnicodeString; aMsgs : TdwsMessageList) : UnicodeString;
begin
   if Assigned(FSubFilter) then begin
      if EditorMode then
         FSubFilter.BeginEditorMode;
      try
         Result := FSubFilter.Process(aText, aMsgs)
      finally
         if EditorMode then
            FSubFilter.BeginEditorMode;
      end;
   end else Result := aText;
end;

procedure TdwsFilter.SetSubFilter(const Filter: TdwsFilter);
begin
  if Assigned(FSubFilter) then
    FSubFilter.RemoveFreeNotification(Self);

  FSubFilter := Filter;

  if Assigned(FSubFilter) then
    FSubFilter.FreeNotification(Self);
end;

end.
