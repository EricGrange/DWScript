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
unit dwsICMP;

interface

function PingIPv4(const hostName : String; timeoutMs : Integer) : Integer;

implementation

uses Windows, SysUtils, WinSock, dwsXPlatform, dwsRandom;

function IcmpCreateFile: THandle; stdcall; external 'iphlpapi.dll';
function IcmpCloseHandle(icmpHandle : THandle) : Boolean; stdcall; external 'iphlpapi.dll';
function IcmpSendEcho(
   icmpHandle : THandle; destinationAddress : In_Addr;
   requestData : Pointer; requestSize : SmallInt; requestOptions : Pointer;
   replyBuffer : Pointer; replySize : DWORD; timeout : DWORD
   ): DWORD; stdcall; external 'iphlpapi.dll';

type
   TEchoReply = packed record
      Addr : In_Addr;
      Status : DWORD;
      RoundTripTime : DWORD;
   end;
   PEchoReply = ^TEchoReply;

var
   vMessageInt64 : UInt64;

function PingIPv4(const hostName : String; timeoutMs : Integer) : Integer;
var
   e : PHostEnt;
   a : PInAddr;
   h : THandle;
   d : UInt64;
   r : array [0 .. $400 - 1] of Byte;
   i : Cardinal;
   hostNameA : RawByteString;
begin
   if timeoutMs <= 0 then
      raise Exception.Create('Timeout should be greater than zero');

   hostNameA := UTF8Encode(hostName);
   e := gethostbyname(PAnsiChar(hostNameA));
   if e = nil then
      RaiseLastOSError;
   if e.h_addrtype = AF_INET then
      Pointer(a) := e.h_addr^
   else raise Exception.CreateFmt('Could not resolve "%" to an IPv4 address', [ hostName ]);

   d := InterlockedIncrement64(Int64(vMessageInt64));
   d := SplitMix64(d);

   h := IcmpCreateFile;
   if h = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
   try
      i := IcmpSendEcho(h, a^, @d, SizeOf(d), nil, @r[0], SizeOf(r), timeoutMs);
      if (i <> 0) and (PEchoReply(@r[0]).Status = 0) then
         Result := PEchoReply(@r[0]).RoundTripTime
      else Result := -1;
   finally
      IcmpCloseHandle(h);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vMessageInt64 := GetTickCount64 xor RDTSC xor GetCurrentProcessId;
   vMessageInt64 := SplitMix64(vMessageInt64);

end.
