'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2023 Samuel Gomes
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'$Include:'./ANSIPrint.bi'

$Unstable:Http

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' EXTERNAL LIBRARIES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Declare CustomType Library
    Function GetTicks~&&
End Declare
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

Dim As Unsigned Integer64 startTick, deltaTick

Screen NewImage(8 * 80, 16 * 30, 32)

Do
    Title "Brainfuck64"

    Dim As String programFile: programFile = OpenFileDialog$("Open", "", "*.bf|*.BF|*.Bf|*.bF", "Brainfuck Program Files")
    If Not FileExists(programFile) Then Exit Do

    Cls
    ResetANSIEmulator

    startTick = GetTicks
    RunBrainfuckProgram LoadFile(programFile)
    deltaTick = GetTicks - startTick

    Title "Run time =" + Str$(deltaTick / 1000) + "s. Press any key to run another file...": Sleep 3600
Loop

End

Sub RunBrainfuckProgram (programString As String)
    ReDim As Unsigned Byte memory(0 To 29999)
    Dim As Unsigned Byte instruction
    Dim As Long instructionPointer, memoryPointer, programLength, bracketOpenCount, bracketCloseCount
    Dim As String program, windowTitle

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

    If bracketOpenCount - bracketCloseCount <> 0 Then Error ERROR_SYNTAX_ERROR ' brackets are not matched

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
                        Error ERROR_OVERFLOW ' throw an error
                End Select

            Case KEY_LESS_THAN
                memoryPointer = memoryPointer - 1 ' decrement the memory pointer

                If memoryPointer < 0 Then Error ERROR_MEMORY_REGION_OUT_OF_RANGE ' cannot go to negative address space

            Case KEY_PLUS
                memory(memoryPointer) = memory(memoryPointer) + 1

            Case KEY_MINUS
                memory(memoryPointer) = memory(memoryPointer) - 1

            Case KEY_DOT
                If Not PrintANSICharacter(memory(memoryPointer)) Then
                    Print
                    ResetANSIEmulator
                End If

            Case KEY_COMMA
                ' Get the current window title and then tell the user that we need keyboard input
                windowTitle = Title$
                Title "[WAITING FOR INPUT] " + windowTitle

                memory(memoryPointer) = Asc(Input$(1))

                Title windowTitle ' set the window title the way it was

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
        Limit 60
        Get h, , s
        content = content + s
    Wend

    Close h

    DownloadFile = content
End Function


'$Include:'./ANSIPrint.bas'

