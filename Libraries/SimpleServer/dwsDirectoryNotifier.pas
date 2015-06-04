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

uses Windows, Classes, SysUtils, dwsXPlatform;

type

   TdwsDirectoryNotifier = class;
   TdwsFileNotifier = class;
   TdwsFileNotifierQueue = class;

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

   TdwsFileNotifierBuffer = array [0..63*1024-1] of Byte; // limit at 64kb for network drives

   TdwsFileNotifierPaths = array of String;

   // TdwsFileNotifier
   //
   TdwsFileNotifier = class (TThread)
      private
         { Private Declarations }
         FDirectory : String;
         FOnFileChanged : TdwsFileChangedEvent;
         FMode : TdwsDirectoryNotifierMode;
         FDirectoryHandle : THandle;
         FNotificationBuffer : TdwsFileNotifierBuffer;
         FNotifyFilter : DWORD;
         FOverlapped : TOverlapped;
         FPOverlapped : POverlapped;
         FBytesWritten : DWORD;
         FChangeCP : THandle;
         FLastChange : TDateTime;
         FActive : Boolean;
         FIgnoredPaths : TdwsFileNotifierPaths;
         FQueue : TdwsFileNotifierQueue;

      protected
         { Protected Declarations }
         procedure Shutdown;
         procedure SetIgnoredPaths(const val : TdwsFileNotifierPaths);

      public
         { Public Declarations }
         constructor Create(const aDirectory : String; aMode : TdwsDirectoryNotifierMode);
         destructor Destroy; override;

         procedure Execute; override;

         property Directory : String read FDirectory;
         property IgnoredPaths : TdwsFileNotifierPaths read FIgnoredPaths write SetIgnoredPaths;
         property Mode : TdwsDirectoryNotifierMode read FMode;
         property LastChange : TDateTime read FLastChange write FLastChange;

         property OnFileChanged : TdwsFileChangedEvent read FOnFileChanged write FOnFileChanged;
   end;

   // TdwsFileNotifierQueue
   //
   TdwsFileNotifierQueue = class (TThread)
      private
         { Private Declarations }
         FOwner : TdwsFileNotifier;
         FOwnerThreadID : Integer;
         FQueueCP : THandle;
         FLock : TMultiReadSingleWrite;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Execute; override;

         procedure QueueNotification(const fileName : String; action : TFileNotificationAction);
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

   FQueue:=TdwsFileNotifierQueue.Create;
   FQueue.FOwner:=Self;
   FQueue.FOwnerThreadID:=ThreadID;

   FNotifyFilter:=   FILE_NOTIFY_CHANGE_FILE_NAME  or FILE_NOTIFY_CHANGE_DIR_NAME
                  or FILE_NOTIFY_CHANGE_ATTRIBUTES
                  or FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_SECURITY;
   FDirectoryHandle:=CreateFile(PChar(FDirectory), FILE_LIST_DIRECTORY,
         FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
         OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
   if FDirectoryHandle=INVALID_HANDLE_VALUE then
      RaiseLastOSError;
   FChangeCP:=CreateIoCompletionPort(FDirectoryHandle, 0, 1, 0);
   FBytesWritten:=0;
   if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer, SizeOf(TdwsFileNotifierBuffer),
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
   FQueue.Free;
   if FDirectoryHandle<>0 then
      CloseHandle(FDirectoryHandle);
   if FChangeCP<>0 then
      CloseHandle(FChangeCP);
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
   i : Integer;
   notify : Boolean;
begin
   FActive:=True;
   NameThreadForDebugging('FileNotifier '+AnsiString(FDirectory));
   while not Terminated do begin
      GetQueuedCompletionStatus(FChangeCP, numBytes, completionKey, FPOverlapped, INFINITE);
      if completionKey<>0 then begin

         if not ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer,
                                     SizeOf(TdwsFileNotifierBuffer),
                                     (FMode=dnoDirectoryAndSubTree), FNotifyFilter,
                                     @FBytesWritten, @FOverlapped, nil) then
            Terminate;

         fileOpNotification:=@FNotificationBuffer;
         repeat
            offset:=fileOpNotification^.NextEntryOffset;
            if Assigned(FOnFileChanged) then begin
               notify:=True;
               for i:=0 to High(FIgnoredPaths) do begin
                  if StrLComp(@fileOpNotification^.FileName[0],
                              PWideChar(Pointer(FIgnoredPaths[i])),
                              Length(FIgnoredPaths[i]))=0 then begin
                     notify:=False;
                     Break;
                  end;
               end;
               if notify then begin
                  SetString(fileName, fileOpNotification^.FileName,
                            fileOpNotification^.FileNameLength div SizeOf(Char));
                  FQueue.QueueNotification(fileName, fileOpNotification^.Action);
               end;
            end;
            fileOpNotification:=@PAnsiChar(fileOpNotification)[offset];
         until offset=0;
         FBytesWritten:=0;
         FillChar(FNotificationBuffer, 0, SizeOf(TdwsFileNotifierBuffer));

      end else Terminate;
   end;
   FActive:=False;
end;

// Shutdown
//
procedure TdwsFileNotifier.Shutdown;
begin
   if FActive then begin
      if FChangeCP<>0 then
         PostQueuedCompletionStatus(FChangeCP, 0, 0, nil);
      Terminate;
      WaitFor;
   end;
end;

// SetIgnoredPaths
//
procedure TdwsFileNotifier.SetIgnoredPaths(const val : TdwsFileNotifierPaths);
begin
   FIgnoredPaths:=Copy(val, 0, Length(val));
end;

// ------------------
// ------------------ TdwsFileNotifier ------------------
// ------------------

// Create
//
constructor TdwsFileNotifierQueue.Create;
begin
   inherited Create;
   FQueueCP:=CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 1, 0);
   FreeOnTerminate:=False;
   FLock:=TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsFileNotifierQueue.Destroy;
begin
   PostQueuedCompletionStatus(FQueueCP, 0, 0, nil);
   Terminate;
   FLock.BeginWrite;
   try
      FOwner:=nil;
   finally
      FLock.EndWrite;
   end;
   WaitFor;

   FLock.Free;
   CloseHandle(FQueueCP);
   inherited;
end;

// Execute
//
procedure TdwsFileNotifierQueue.Execute;
var
   numBytes : DWORD;
   {$if CompilerVersion < 23.0} // XE2
   completionKey : DWORD;
   {$else}
   completionKey : ULONG_PTR;
   {$ifend}
   overlapped : POverlapped;
   fileName : String;
begin
   NameThreadForDebugging('FileNotifierQueue for '+AnsiString(IntToStr(FOwnerThreadID)));
   while not Terminated do begin
      GetQueuedCompletionStatus(FQueueCP, numBytes, completionKey, overlapped, INFINITE);
      case numBytes of
         0 : Terminate;
      else
         fileName := FOwner.FDirectory + String(overlapped);
         PString(@overlapped)^:='';
         FLock.BeginRead;
         try
            if Assigned(FOwner.FOnFileChanged) then
               FOwner.FOnFileChanged(FOwner, fileName, TFileNotificationAction(completionKey));
         finally
            FLock.EndRead;
         end;
      end;
   end;
end;

// QueueNotification
//
procedure TdwsFileNotifierQueue.QueueNotification(const fileName : String; action : TFileNotificationAction);
var
   p : POverlapped;
begin
   p := nil;
   PString(@p)^ := fileName;
   PostQueuedCompletionStatus(FQueueCP, 1, Ord(action), p);
end;

end.

