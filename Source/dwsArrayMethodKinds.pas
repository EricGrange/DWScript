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
unit dwsArrayMethodKinds;

{$I dws.inc}

interface

type

   TArrayMethodKind = (
      amkNone,
      amkAdd, amkPush, amkIndexOf, amkRemove, amkSort, amkMap, amkHigh, amkLow,
      amkLength, amkCount, amkPop, amkPeek, amkDelete, amkInsert, amkSetLength,
      amkClear, amkSwap, amkCopy, amkReverse, amkDimCount, amkKeys, amkMove,
      amkFilter
   );

implementation

end.
