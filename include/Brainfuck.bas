'$Include:'./Common.bi'

Do
    Title "Brainfuck"

    Dim As String programFile: programFile = OpenFileDialog$("Open", "", "*.bf|*.BF|*.Bf|*.bF", "Brainfuck Program Files")
    If Not FileExists(programFile) Then Exit Do

    Cls

    RunBrainfuckProgram LoadFile(programFile)

    Title "Press any key to run another file...": Sleep 3600
Loop

End

Sub RunBrainfuckProgram (programString As String)
    ReDim As Unsigned Byte memory(0 To 0)
    ReDim As Long loopStack(0 To 0)
    Dim As Unsigned Byte instruction
    Dim As Long instructionPointer, dataPointer, programLength, loopStackPointer
    Dim As String program, windowTitle

    ' Optimize program stream
    programLength = Len(programString)
    For instructionPointer = 1 To programLength
        instruction = Asc(programString, instructionPointer)

        ' Only accept supported commands and discard the rest
        Select Case instruction
            Case 62, 60, 43, 45, 46, 44 ' regular commands
                program = program + Chr$(instruction)

            Case 91 ' basic nested loop check
                loopStackPointer = loopStackPointer + 1
                program = program + Chr$(instruction)

            Case 93 ' basic nested loop check
                loopStackPointer = loopStackPointer - 1
                program = program + Chr$(instruction)

        End Select
    Next

    If loopStackPointer <> 0 Then Error 17

    ' Re-initialize stuff based on the optimizied stream
    instructionPointer = 1
    programLength = Len(program)

    Do Until instructionPointer > programLength
        instruction = Asc(program, instructionPointer)

        Select Case instruction
            Case 62 ' >
                dataPointer = dataPointer + 1

                ' Dynamically grow the memory space as needed
                If dataPointer > UBound(memory) Then ReDim Preserve As Unsigned Byte memory(0 To dataPointer)

            Case 60 ' <
                dataPointer = dataPointer - 1 ' we'll let the data pointer move lower than zero (obviously this is a bug in the BF program)

            Case 43 ' +
                memory(dataPointer) = memory(dataPointer) + 1

            Case 45 ' -
                memory(dataPointer) = memory(dataPointer) - 1

            Case 46 ' .
                Print Chr$(memory(dataPointer));

            Case 44 ' ,
                ' Get the current window title and then tell the user that we need keyboard input
                windowTitle = Title$
                Title "[WAITING FOR INPUT] " + windowTitle

                memory(dataPointer) = Asc(Input$(1))

                Title windowTitle ' set the window title the way it was

            Case 91 ' [
                If memory(dataPointer) = 0 Then
                    loopStackPointer = loopStackPointer + 1

                    ' Dynamically increase the loop stack
                    If loopStackPointer > UBound(loopStack) Then ReDim Preserve As Long loopStack(0 To loopStackPointer)
                    loopStack(loopStackPointer) = instructionPointer

                    Do
                        instructionPointer = instructionPointer + 1
                        instruction = Asc(program, instructionPointer)
                        If instruction = 91 Then loopStackPointer = loopStackPointer + 1
                        If instruction = 93 Then loopStackPointer = loopStackPointer - 1
                    Loop Until loopStackPointer = 0
                Else
                    loopStackPointer = loopStackPointer + 1

                    ' Dynamically increase the loop stack
                    If loopStackPointer > UBound(loopStack) Then ReDim Preserve As Long loopStack(0 To loopStackPointer)
                    loopStack(loopStackPointer) = instructionPointer
                End If

            Case 93 ' ]
                If memory(dataPointer) <> 0 Then
                    instructionPointer = loopStack(loopStackPointer)
                Else
                    loopStackPointer = loopStackPointer - 1
                End If

        End Select

        instructionPointer = instructionPointer + 1

        ' Echo "IP =" + Str$(instructionPointer) + " DP =" + Str$(dataPointer) + " LSP =" + Str$(loopStackPointer)
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

