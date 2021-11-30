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

{$I dws.inc}

interface

function PingIPv4(const hostName : String; timeoutMs : Integer) : Integer;
function PingIPv6(const hostName : String; timeoutMs : Integer) : Integer;

implementation

uses Windows, SysUtils, WinSock, dwsXPlatform, dwsRandom;

type
   PIO_APC_ROUTINE = Pointer;
   PIP_OPTION_INFORMATION = Pointer;

   TIn6Addr = record
      case Integer of
         0: (bytes: array [0..15] of Byte);
         1: (addr: array [0..15] of AnsiChar);
         2: (words: array[0..7] of Word);
   end;

   TSockAddrIn6 = record
      sin6_family: short;    // AF_INET6
      sin6_port: u_short;    // Transport level port number
      sin6_flowinfo: u_long; // IPv6 flow information
      sin6_addr: TIn6Addr;   // IPv6 address
      sin6_scope_id: u_long; // set of interfaces for a scope
   end;
   PSockAddrIn6 = ^TSockAddrIn6;

   PAddrInfoW = ^TAddrInfoW;
   TAddrInfoW = record
      ai_flags: Integer;
      ai_family: Integer;
      ai_socktype: Integer;
      ai_protocol: Integer;
      ai_addrlen: LongWord;
      ai_canonname: PWideChar;
      ai_addr: PSOCKADDR;
      ai_next: PAddrInfoW;
   end;

const
   AF_INET6 = 23;


function IcmpCreateFile: THandle; stdcall; external 'iphlpapi.dll';
function IcmpCloseHandle(icmpHandle : THandle) : Boolean; stdcall; external 'iphlpapi.dll';
function IcmpSendEcho(
   icmpHandle : THandle; destinationAddress : In_Addr;
   requestData : Pointer; requestSize : SmallInt; requestOptions : Pointer;
   replyBuffer : Pointer; replySize : DWORD; timeout : DWORD
   ): DWORD; stdcall; external 'iphlpapi.dll';

function Icmp6CreateFile: THandle; stdcall; external 'iphlpapi.dll';
function Icmp6SendEcho2(
   icmpHandle: THandle; event: THandle;
   apcRoutine: PIO_APC_ROUTINE; apcContext: Pointer;
   sourceAddress: PSockAddrIn6; destinationAddress: PSockAddrIn6;
   requestData: Pointer; requestSize: WORD; requestOptions: PIP_OPTION_INFORMATION;
   replyBuffer: Pointer; replySize: DWORD; timeout: DWORD
   ): DWORD; stdcall; external 'iphlpapi.dll';

function GetAddrInfoW(pNodeName, pServiceName: PWideChar;
                      pHints : PADDRINFOW; var ppResult : PADDRINFOW
   ): Integer; stdcall; external 'ws2_32.dll';

type
   TV4EchoReply = packed record
      Addr : In_Addr;
      Status : DWORD;
      RoundTripTime : DWORD;
   end;
   PV4EchoReply = ^TV4EchoReply;

   IPV6_ADDRESS_EX = packed record
      sin6_port : USHORT;
      sin6_flowinfo : ULONG;
      sin6_addr : array [0..7] of USHORT;
      sin6_scope_id : ULONG;
   end;
   TV6EchoReply = packed record
      Addr : IPV6_ADDRESS_EX;
      Status : DWORD;
      RoundTripTime : DWORD;
   end;
   PV6EchoReply = ^TV6EchoReply;

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
   else raise Exception.CreateFmt('Could not resolve "%s" to an IPv4 address', [ hostName ]);

   d := InterlockedIncrement64(Int64(vMessageInt64));
   d := SplitMix64(d);

   h := IcmpCreateFile;
   if h = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
   try
      i := IcmpSendEcho(h, a^, @d, SizeOf(d), nil, @r[0], SizeOf(r), timeoutMs);
      if (i <> 0) and (PV4EchoReply(@r[0]).Status = 0) then
         Result := PV4EchoReply(@r[0]).RoundTripTime
      else Result := -1;
   finally
      IcmpCloseHandle(h);
   end;
end;

function PingIPv6(const hostName : String; timeoutMs : Integer) : Integer;
var
   pInfo : PADDRINFOW;
   hint : TAddrInfoW;
   r : array [0 .. $400 - 1] of Byte;
begin
   if timeoutMs <= 0 then
      raise Exception.Create('Timeout should be greater than zero');
   FillChar(hint, SizeOf(TAddrInfoW), 0);
   hint.ai_family := AF_INET6;

   pInfo := nil;
   var status := GetAddrInfoW(PChar(hostName), nil, @hint, pInfo);
   if (status <> 0) or (pInfo = nil) or (pInfo.ai_family <> AF_INET6) then
      raise Exception.CreateFmt('Could not resolve "%s" to an IPv6 address', [ hostName ]);

   var addr : TSockAddrIn6 := PSockAddrIn6(pInfo.ai_addr)^;
   var loopBack : TSockAddrIn6;
   FillChar(loopBack, SizeOf(loopBack), 0);
   loopBack.sin6_family := AF_INET6;
   loopBack.sin6_addr.bytes[15] := 1;

   var d : UInt64 := InterlockedIncrement64(Int64(vMessageInt64));
   d := SplitMix64(d);

   var h := Icmp6CreateFile;
   if h = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
   try
      var i := Icmp6SendEcho2(h, 0, nil, nil,
                              @loopBack, @addr,
                              @d, SizeOf(d), nil,
                              @r[0], SizeOf(r), timeoutMs);
      if (i <> 0) and (PV6EchoReply(@r[0]).Status = 0) then
         Result := PV6EchoReply(@r[0]).RoundTripTime
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
