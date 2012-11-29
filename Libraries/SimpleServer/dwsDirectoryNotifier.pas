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
unit dwsDirectoryNotifier;

interface

uses Windows, Classes, SysUtils;

type

   TdwsDirectoryNotifier = class;

   TdwsDirectoryNotifierMode = (dnoDirectoryOnly, dnoDirectoryAndSubTree);

   TdwsDirectoryChangedEvent = procedure (sender : TdwsDirectoryNotifier) of object;

	// TdwsDirectoryNotifier
	//
   TdwsDirectoryNotifier = class (TThread)
	   private
	      { Private Declarations }
         FDirectory : String;
         FOnDirectoryChanged : TdwsDirectoryChangedEvent;
         FMode : TdwsDirectoryNotifierMode;
         FNotifyHandle : array [0..1] of THandle;
         FLastChange : TDateTime;

	   protected
	      { Protected Declarations }
         procedure Shutdown;

	   public
	      { Public Declarations }
         constructor Create(const aDirectory : String; aMode : TdwsDirectoryNotifierMode);
         destructor Destroy; override;

         procedure Execute; override;

         property Directory : String read FDirectory;
         property Mode : TdwsDirectoryNotifierMode read FMode;
         property LastChange : TDateTime read FLastChange write FLastChange;

         property OnDirectoryChanged : TdwsDirectoryChangedEvent read FOnDirectoryChanged write FOnDirectoryChanged;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsDirectoryNotifier ------------------
// ------------------

// Create
//
constructor TdwsDirectoryNotifier.Create(const aDirectory : String; aMode : TdwsDirectoryNotifierMode);
begin
	inherited Create(False);
   NameThreadForDebugging('DirectoryNotifier');
   FDirectory:=aDirectory;
   FMode:=aMode;
end;

// Destroy
//
destructor TdwsDirectoryNotifier.Destroy;
begin
   Shutdown;
	inherited Destroy;
end;

// Shutdown
//
procedure TdwsDirectoryNotifier.Shutdown;
begin
   Terminate;
   SetEvent(FNotifyHandle[1]);
   WaitFor;
   Free;
end;

// Execute
//
procedure TdwsDirectoryNotifier.Execute;
var
   status : Cardinal;
begin
   FNotifyHandle[0]:=FindFirstChangeNotification(
            PChar(FDirectory),
            (Mode=dnoDirectoryAndSubTree),
             FILE_NOTIFY_CHANGE_FILE_NAME+FILE_NOTIFY_CHANGE_DIR_NAME
            +FILE_NOTIFY_CHANGE_ATTRIBUTES+FILE_NOTIFY_CHANGE_SIZE
            +FILE_NOTIFY_CHANGE_LAST_WRITE+FILE_NOTIFY_CHANGE_SECURITY);
   if FNotifyHandle[0]=INVALID_HANDLE_VALUE then Exit;
   FNotifyHandle[1]:=CreateEvent(nil, True, False, nil);
   try
      while not Terminated do begin
         status:=WaitForMultipleObjects(2, @FNotifyHandle[0], False, 1000);
         case status of
            WAIT_TIMEOUT : ; // nothing (= continue)
            WAIT_OBJECT_0 : if not Terminated then begin // notification signal
               FLastChange:=Now;
               if Assigned(FOnDirectoryChanged) then
                  FOnDirectoryChanged(Self);
               if not Terminated then
                  FindNextChangeNotification(FNotifyHandle[0]);
            end;
         else
            // Termination signal
            Break;
         end;
      end;
   finally
      FindCloseChangeNotification(FNotifyHandle[0]);
      CloseHandle(FNotifyHandle[1]);
   end;
end;

end.

