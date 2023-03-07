'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2023 Samuel Gomes
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
'$Include:'./Common.bi'
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
$Console:Only
$Unstable:Http
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Const APP_NAME = "Brainfuck64"
Const INTERPRETER_MEMORY_DEFAULT = 30000
Const UPDATES_PER_SECOND = 60
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' EXTERNAL LIBRARIES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Declare CustomType Library
    Function getchar&
    Sub putchar (ByVal ch As Long)
    Function GetTicks~&&
End Declare
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Dim As Unsigned Integer64 startTick, deltaTicks

Do
    ConsoleTitle APP_NAME

    Dim As String programFile: programFile = OpenFileDialog$("Open", "", "*.bf|*.BF|*.Bf|*.bF", "Brainfuck Program Files")
    If Not FileExists(programFile) Then Exit Do

    putchar 13: putchar 10 ' start on a new line

    startTick = GetTicks
    RunBrainfuckProgram LoadFile(programFile), GetFileNameFromPath(programFile)
    deltaTicks = GetTicks - startTick

    ConsoleTitle "Run time =" + Str$(deltaTicks / 1000) + "s. Press any key to run another file...": Sleep 3600
Loop

System
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' FUNCTIONS AND SUBROUTINES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Sub RunBrainfuckProgram (programString As String, programName As String)
    ReDim As Unsigned Byte memory(0 To INTERPRETER_MEMORY_DEFAULT - 1)
    Dim As Unsigned Byte instruction
    Dim As Long instructionPointer, memoryPointer, programLength, bracketOpenCount, bracketCloseCount
    Dim As String program

    ConsoleTitle programName ' set the window title

    ' Optimize program stream
    programLength = Len(programString)
    For instructionPointer = 1 To programLength
        instruction = Asc(programString, instructionPointer)

        ' Only accept supported commands and discard the rest
        Select Case instruction
            Case KEY_GREATER_THAN, KEY_LESS_THAN, KEY_PLUS, KEY_MINUS, KEY_DOT, KEY_COMMA ' regular commands
                program = program + Chr$(instruction)

            Case KEY_OPEN_BRACKET ' basic loop check
                bracketOpenCount = bracketOpenCount + 1
                program = program + Chr$(instruction)

            Case KEY_CLOSE_BRACKET ' basic loop check
                bracketCloseCount = bracketCloseCount + 1
                program = program + Chr$(instruction)

        End Select
    Next

    If bracketOpenCount <> bracketCloseCount Then Error ERROR_SYNTAX_ERROR ' brackets are not matched

    ' Re-initialize stuff based on the optimized stream
    instructionPointer = 1
    bracketOpenCount = 0
    bracketCloseCount = 0
    programLength = Len(program)

    Do Until instructionPointer > programLength
        instruction = Asc(program, instructionPointer)

        Select Case instruction
            Case KEY_GREATER_THAN
                memoryPointer = memoryPointer + 1 ' increment the memory pointer

                Select Case memoryPointer
                    Case Is > UBound(memory) ' if we moved pass the memory address space
                        ReDim Preserve As Unsigned Byte memory(0 To memoryPointer) ' dynamically grow the memory space preserving contents

                    Case Is < 0 ' can happen if we move past the max value of long
                        Error ERROR_CANNOT_CONTINUE ' throw an error
                End Select

            Case KEY_LESS_THAN
                memoryPointer = memoryPointer - 1 ' decrement the memory pointer

                If memoryPointer < 0 Then Error ERROR_CANNOT_CONTINUE ' cannot go to negative address space

            Case KEY_PLUS
                memory(memoryPointer) = memory(memoryPointer) + 1

            Case KEY_MINUS
                memory(memoryPointer) = memory(memoryPointer) - 1

            Case KEY_DOT
                putchar memory(memoryPointer)

            Case KEY_COMMA
                ' Get the current window title and then tell the user that we need keyboard input
                ConsoleTitle "[WAITING FOR INPUT] " + programName

                memory(memoryPointer) = getchar

                ConsoleTitle programName ' set the window title the way it was

            Case KEY_OPEN_BRACKET
                If memory(memoryPointer) = 0 Then
                    bracketOpenCount = 0 ' reset bracket count
                    instructionPointer = instructionPointer + 1 ' move past this bracket

                    Do Until instructionPointer > programLength
                        instruction = Asc(program, instructionPointer)

                        Select Case instruction
                            Case KEY_OPEN_BRACKET ' we found a nested bracket
                                bracketOpenCount = bracketOpenCount + 1 ' increment bracket counter

                            Case KEY_CLOSE_BRACKET
                                If bracketOpenCount = 0 Then ' we found the matching bracket
                                    Exit Do
                                Else
                                    bracketOpenCount = bracketOpenCount - 1 ' decrement bracket counter
                                End If

                        End Select

                        instructionPointer = instructionPointer + 1
                    Loop
                End If

            Case KEY_CLOSE_BRACKET
                If memory(memoryPointer) <> 0 Then
                    bracketCloseCount = 0
                    instructionPointer = instructionPointer - 1

                    Do While instructionPointer > 0
                        instruction = Asc(program, instructionPointer)

                        Select Case instruction
                            Case KEY_CLOSE_BRACKET
                                bracketCloseCount = bracketCloseCount + 1

                            Case KEY_OPEN_BRACKET
                                If bracketCloseCount = 0 Then ' we found the matching bracket
                                    Exit Do
                                Else
                                    bracketCloseCount = bracketCloseCount - 1
                                End If
                        End Select

                        instructionPointer = instructionPointer - 1
                    Loop
                End If

        End Select

        instructionPointer = instructionPointer + 1
    Loop
End Sub


' Gets the filename portion from a file path
Function GetFileNameFromPath$ (pathName As String)
    Dim i As Unsigned Long

    ' Retrieve the position of the first / or \ in the parameter from the
    For i = Len(pathName) To 1 Step -1
        If Asc(pathName, i) = KEY_SLASH Or Asc(pathName, i) = KEY_BACKSLASH Then Exit For
    Next

    ' Return the full string if pathsep was not found
    If i = 0 Then
        GetFileNameFromPath = pathName
    Else
        GetFileNameFromPath = Right$(pathName, Len(pathName) - i)
    End If
End Function


' Loads a whole file in memory
Function LoadFile$ (pathString As String)
    If FileExists(pathString) Then
        Dim As Long fh: fh = FreeFile
        Open pathString For Binary Access Read As fh
        LoadFile$ = Input$(LOF(fh), fh)
        Close fh
    End If
End Function


' Loads a whole file from a URL into memory
Function DownloadFile$ (url As String)
    Dim h As Long, content As String, s As String

    h = OpenClient("HTTP:" + url)

    While Not EOF(h)
        Limit UPDATES_PER_SECOND
        Get h, , s
        content = content + s
    Wend

    Close h

    DownloadFile = content
End Function
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

