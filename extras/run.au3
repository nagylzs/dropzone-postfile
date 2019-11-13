;
; This example AutoIt script shows how to encrypt and compress a file, and then
; upload it to a dropzone-backup server. Look for parameters below.
;
; To use this script, create a folder "util" and copy PostFile.exe and a full 7-zip distribution under that directory.
;

#include <MsgBoxConstants.au3>
#include <Date.au3>


Func _DateFormat($DateTime, $sFormat)
    If StringRegExp($DateTime, "\d{4}/\d{1,2}/\d{1,2}") = 0 Then $DateTime = _NowCalcDate() & " " & $DateTime
    GUICreate("DateTimePick Set Format", 400, 300)
    Local $idDate = GUICtrlCreateDate($DateTime, 2, 6, 190)
    GUICtrlSendMsg($idDate, 0x1032, 0, $sFormat)
    Return GUICtrlRead($idDate)
EndFunc   ;==>_DateFormat


Func _TodaysDate()
    Return _DateFormat(_NowCalc(), "yyyy-MM-ddThhmmss")
EndFunc   ;==>_TodaysDate

; Parameters below
Local $dbFile = @ScriptDir & "\database.s3db";
Local $backupFile = @ScriptDir & "\backup_" & _TodaysDate() & ".7z";
Local $sevenZip = @ScriptDir & "\util\7zG.exe";
Local $postFile = @ScriptDir & "\util\PostFile.exe";
Local $zipPassword = "your_aes256_7zip_password";
Local $dropzoneServer = "https://your.dropzone.server.com/upload"
Local $dropzoneUser = "your_dropzone_user_name";
Local $dropzonePassword = "your_dropzone_password";
; Parameters above

If not FileExists($dbFile) Then
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONERROR, "No such file", $dbFile);
   Exit(1);
EndIf

If not FileExists($sevenZip) Then
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONERROR, "No such file", $sevenZip);
   Exit(1);
EndIf

If not FileExists($postFile) Then
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONERROR, "No such file", $postFile);
   Exit(1);
EndIf



If FileExists($backupFile) Then
   FileDelete($backupFile);
EndIf

; See here: https://sevenzip.osdn.jp/chm/cmdline/switches/method.htm#7Z
Local $compressCmd = $sevenZip & " a -p" & $zipPassword & " -t7z -mx=9 -ms=on -mhc=on -mhe=on -m0=lzma2 -mtm=on " & $backupFile & " " & $dbFile

;MsgBox($MB_APPLMODAL or $MB_OK, "Holvagyok", $compressCmd);

Local $iReturn = RunWait($compressCmd);
If ($iReturn <> 0) Then
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONERROR, "Error", "Could not compress, return code: " & $iReturn);
   Exit($iReturn);
EndIf

Local $backzpCmd = $postFile & " -u " & $dropzoneServer & _
   " -f " & $backupFile & _
   " --add-header=UserName="&$dropzoneUser & _
   " --add-header=Password="&$dropzonePassword & _
   " --auto-start --auto-close ";

Local $iReturn2 = RunWait($backzpCmd);

FileDelete($backupFile);

If ($iReturn2 <> 0) Then
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONERROR, "Error", "Could not upload, return code: " & $iReturn2);
   Exit($iReturn);
Else
   MsgBox($MB_APPLMODAL + $MB_SETFOREGROUND + $MB_OK + $MB_ICONINFORMATION, "Success", "Database backup was successful.");
   Exit($iReturn);
EndIf
