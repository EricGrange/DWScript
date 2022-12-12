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
unit dwsAutoFormat;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenizer;

type

   TdwsAutoFormat = class
      private
         FIndent : String;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         function Process(const sourceCode : String) : String;

         property Indent : String read FIndent write FIndent;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsAutoFormat ------------------
// ------------------

// Create
//
constructor TdwsAutoFormat.Create;
begin
   inherited;
   Indent := #9;
end;

// Destroy
//
destructor TdwsAutoFormat.Destroy;
begin
   inherited;
end;

// Process
//
function TdwsAutoFormat.Process(const sourceCode : String) : String;
begin
   Result := sourceCode;
end;

end.
