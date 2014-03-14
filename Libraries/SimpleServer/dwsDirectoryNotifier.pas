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
   TdwsFileNotifier = class;

   TdwsDirectoryNotifierMode = (dnoDirectoryOnly, dnoDirectoryAndSubTree);

   {$MINENUMSIZE 4}
   TFileNotificationAction = (
      FILE_ACTION_ADDED = 1,
      FILE_ACTION_REMOVED,
      FILE_ACTION_MODIFIED,
      FILE_ACTION_RENAMED_OLD_NAME,
      FILE_ACTION_RENAMED_NEW_NAME
   );

   TdwsDirectoryChangedEvent = procedure (sender : TdwsDirectoryNotifier) of object;
   TdwsFileChangedEvent = procedure (sender : TdwsFileNotifier; const fileName : String;
                                     changeAction : TFileNotificationAction) of object;

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

   // TdwsFileNotifier
   //
   TdwsFileNotifier = class (TThread)
      private
         { Private Declarations }
         FDirectory : String;
         FOnFileChanged : TdwsFileChangedEvent;
         FMode : TdwsDirectoryNotifierMode;
         FDirectoryHandle : THandle;
         FNotificationBuffer : array[0..4096] of Byte;
         FNotifyFilter : DWORD;
         FOverlapped : TOverlapped;
         FPOverlapped : POverlapped;
         FBytesWritten : DWORD;
         FCompletionPort : THandle;
         FLastChange : TDateTime;
         FActive : Boolean;

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

         property OnFileChanged : TdwsFileChangedEvent read FOnFileChanged write FOnFileChanged;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TFileNotifyInformation = record
      NextEntryOffset : DWORD;
      Action : TFileNotificationAction;
      FileNameLength : DWORD;
      FileName : array[0..0] of WideChar;
   end;
   PFileNotifyInformation = ^TFileNotifyInformation;

// ------------------
// ------------------ TdwsDirectoryNotifier ------------------
// ------------------

// Create
//
constructor TdwsDirectoryNotifier.Create(const aDirectory : String; aMode : TdwsDirectoryNotifierMode);
begin
   inherited Create(False);
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
end;

// Execute
//
procedure TdwsDirectoryNotifier.Execute;
var
   status : Cardinal;
begin
   NameThreadForDebugging('DirectoryNotifier');
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

// ------------------
// ------------------ TdwsFileNotifier ------------------
// ------------------

// Create
//
constructor TdwsFileNotifier.Create(const aDirectory : String; aMode : TdwsDirectoryNotifierMode);
begin
   inherited Create(False);
   FDirectory:=IncludeTrailingPathDelimiter(aDirectory);
   FMode:=aMode;
   FNotifyFilter:=   FILE_NOTIFY_CHANGE_FILE_NAME  or FILE_NOTIFY_CHANGE_DIR_NAME
                  or FILE_NOTIFY_CHANGE_ATTRIBUTES
                  or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_SECURITY;
   FDirectoryHandle:=CreateFile(PChar(FDirectory), FILE_LIST_DIRECTORY,
         FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
         OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
   if FDirectoryHandle=INVALID_HANDLE_VALUE then
      RaiseLastOSError;
   FCompletionPort:=CreateIoCompletionPort(FDirectoryHandle, 0, 1, 0);
   FBytesWritten:=0;
   if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer),
                               (FMode=dnoDirectoryAndSubTree), FNotifyFilter, @FBytesWritten,
                               @FOverlapped, nil) then
      RaiseLastOSError;
end;

// Destroy
//
destructor TdwsFileNotifier.Destroy;
begin
   if FActive then
      Shutdown;
   if FDirectoryHandle<>0 then
      CloseHandle(FDirectoryHandle);
   if FCompletionPort<>0 then
      CloseHandle(FCompletionPort);
   inherited;
end;

// Execute
//
procedure TdwsFileNotifier.Execute;
var
   numBytes : DWORD;
   {$if CompilerVersion < 23.0} // XE2
   completionKey : DWORD;
   {$else}
   completionKey : ULONG_PTR;
   {$ifend}
   fileOpNotification : PFileNotifyInformation;
   offset : Longint;
   fileName : String;
begin
   FActive:=True;
   NameThreadForDebugging('FileNotifier');
   while not Terminated do begin
      GetQueuedCompletionStatus(FCompletionPort, numBytes, completionKey, FPOverlapped, INFINITE);
      if completionKey<>0 then begin
         fileOpNotification:=@FNotificationBuffer;
         repeat
            offset:=fileOpNotification^.NextEntryOffset;
            if Assigned(FOnFileChanged) then begin
               SetString(fileName, fileOpNotification^.FileName,
                         fileOpNotification^.FileNameLength div SizeOf(Char));
               FOnFileChanged(Self, FDirectory+fileName, fileOpNotification^.Action);
            end;
            fileOpNotification:=@PAnsiChar(fileOpNotification)[offset];
         until offset=0;
         FBytesWritten:=0;
         FillChar(FNotificationBuffer, 0, SizeOf(FNotificationBuffer));
         if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer),
                                     (FMode=dnoDirectoryAndSubTree), FNotifyFilter,
                                     @FBytesWritten, @FOverlapped, nil) then
            Terminate;
      end else Terminate;
   end;
   FActive:=False;
end;

// Shutdown
//
procedure TdwsFileNotifier.Shutdown;
begin
   if FActive then begin
      Terminate;
      if FCompletionPort<>0 then
         PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
      WaitFor;
   end;
end;

end.

