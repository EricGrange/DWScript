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
unit dwsSpecialKeywords;

{$I dws.inc}

interface

uses SysUtils;

type
   TSpecialKeywordKind = (skNone, skAssert, skAssigned, skDefault,
                          skHigh, skLength, skLow,
                          skOrd, skSizeOf, skDefined, skDeclared,
                          skInc, skDec, skSucc, skPred,
                          skInclude, skExclude,
                          skSwap,
                          skConditionalDefined,
                          skDebugBreak);

const
   cSpecialKeywords : array [TSpecialKeywordKind] of String = (
      '', 'Assert', 'Assigned', 'Default', 'High', 'Length', 'Low',
      'Ord', 'SizeOf', 'Defined', 'Declared', 'Inc', 'Dec', 'Succ', 'Pred',
      'Include', 'Exclude', 'Swap', 'ConditionalDefined', 'DebugBreak'
   );

function IdentifySpecialKeyword(const name : String) : TSpecialKeywordKind;

implementation

uses dwsXPlatform;

function IdentifySpecialKeyword(const name : String) : TSpecialKeywordKind;
var
   n : Integer;
begin
   n:=Length(name);
   case n of
      3 : case name[1] of
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDec]) then Exit(skDec);
         'i', 'I' : if ASCIISameText(name, cSpecialKeywords[skInc]) then Exit(skInc);
         'l', 'L' : if ASCIISameText(name, cSpecialKeywords[skLow]) then Exit(skLow);
         'o', 'O' : if ASCIISameText(name, cSpecialKeywords[skOrd]) then Exit(skOrd);
      end;
      4 : case name[1] of
         'h', 'H' : if ASCIISameText(name, cSpecialKeywords[skHigh]) then Exit(skHigh);
         'p', 'P' : if ASCIISameText(name, cSpecialKeywords[skPred]) then Exit(skPred);
         's', 'S' : case name[2] of
            'u', 'U' : if ASCIISameText(name, cSpecialKeywords[skSucc]) then Exit(skSucc);
            'w', 'W' : if ASCIISameText(name, cSpecialKeywords[skSwap]) then Exit(skSwap);
         end;
      end;
      6 : case name[1] of
         'a', 'A' : if ASCIISameText(name, cSpecialKeywords[skAssert]) then Exit(skAssert);
         'l', 'L' : if ASCIISameText(name, cSpecialKeywords[skLength]) then Exit(skLength);
         's', 'S' : if ASCIISameText(name, cSpecialKeywords[skSizeOf]) then Exit(skSizeOf);
      end;
      7 : case name[1] of
         'd', 'D' :
            if ASCIISameText(name, cSpecialKeywords[skDefined]) then  Exit(skDefined)
            else if ASCIISameText(name, cSpecialKeywords[skDefault]) then  Exit(skDefault)
            ;
         'i', 'I' : if ASCIISameText(name, cSpecialKeywords[skInclude]) then Exit(skInclude);
         'e', 'E' : if ASCIISameText(name, cSpecialKeywords[skExclude]) then Exit(skExclude);
      end;
      8 : case name[1] of
         'a', 'A' : if ASCIISameText(name, cSpecialKeywords[skAssigned]) then Exit(skAssigned);
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDeclared]) then Exit(skDeclared);
      end;
      10 : case name[1] of
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDebugBreak]) then Exit(skDebugBreak);
      end;
      18 : case name[1] of
         'c', 'C' : if ASCIISameText(name, cSpecialKeywords[skConditionalDefined]) then Exit(skConditionalDefined);
      end;
   end;
   Result := skNone;
end;

end.
