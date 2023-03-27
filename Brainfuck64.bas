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

    Dim As String programFile: programFile = OpenFileDialog$("Open", "", "*.bf|*.BF|*.Bf|*.bF", "Brainfuck Source Files")
    If Not FileExists(programFile) Then Exit Do

    putchar 13: putchar 10 ' start on a new line

    startTick = GetTicks
    RunBrainfuckProgram LoadFile(programFile), GetFileNameFromPathOrURL(programFile)
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
    Dim As Long instructionPointer, memoryPointer, programLength, bracketOpenCount
    ReDim As Long bracketPosition(0 To 0) ' matching bracket positions
    ReDim As Long stack(0 To 0)
    Dim As String program

    ' Optimize program stream
    ConsoleTitle "Optimizing..."

    programLength = Len(programString)
    program = Space$(programLength) ' allocate memory assuming we'll use the entire lenght of programString

    For instructionPointer = 1 To programLength
        instruction = Asc(programString, instructionPointer)

        ' Only accept supported commands and discard the rest
        Select Case instruction
            Case KEY_GREATER_THAN, KEY_LESS_THAN, KEY_PLUS, KEY_MINUS, KEY_DOT, KEY_COMMA, KEY_OPEN_BRACKET, KEY_CLOSE_BRACKET ' regular commands
                memoryPointer = memoryPointer + 1
                Asc(program, memoryPointer) = instruction
        End Select
    Next

    program = Left$(program, memoryPointer) ' compact the program stream

    ' Build jump table
    ConsoleTitle "Building jump table..."

    programLength = Len(program)
    For instructionPointer = 0 To programLength - 1
        instruction = Asc(program, instructionPointer + 1)

        Select Case instruction
            Case KEY_OPEN_BRACKET
                stack(bracketOpenCount) = instructionPointer

                bracketOpenCount = bracketOpenCount + 1
                If bracketOpenCount > UBound(stack) Then ' we moved past the stack upper bound
                    ReDim Preserve As Long stack(0 To bracketOpenCount) ' dynamically grow the stack space preserving contents
                End If

            Case KEY_CLOSE_BRACKET
                If bracketOpenCount < 1 Then Error ERROR_SYNTAX_ERROR ' brackets are not matched

                If instructionPointer > UBound(bracketPosition) Then
                    ReDim Preserve As Long bracketPosition(0 To instructionPointer)
                End If

                bracketPosition(instructionPointer) = stack(bracketOpenCount - 1)
                bracketPosition(stack(bracketOpenCount - 1)) = instructionPointer

                bracketOpenCount = bracketOpenCount - 1
        End Select
    Next

    If bracketOpenCount > 0 Then Error ERROR_SYNTAX_ERROR ' brackets are not matched

    ' Execute the program
    ConsoleTitle programName ' set the window title

    ' Re-initialize stuff based on the optimized stream
    instructionPointer = 0
    memoryPointer = 0
    programLength = Len(program)

    Do While instructionPointer < programLength
        instruction = Asc(program, instructionPointer + 1)

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
                If memory(memoryPointer) = 0 Then instructionPointer = bracketPosition(instructionPointer)

            Case KEY_CLOSE_BRACKET
                If memory(memoryPointer) <> 0 Then instructionPointer = bracketPosition(instructionPointer)

        End Select

        instructionPointer = instructionPointer + 1
    Loop
End Sub


' Gets the filename portion from a file path
Function GetFileNameFromPathOrURL$ (PathOrURL As String)
    Dim As Unsigned Long i, j: j = Len(PathOrURL)

    ' Retrieve the position of the first / or \ in the parameter from the
    For i = j To 1 Step -1
        Select Case Asc(PathOrURL, i)
            Case KEY_SLASH, KEY_BACKSLASH
                Exit For
        End Select
    Next

    ' Return the full string if pathsep was not found
    If i = NULL Then
        GetFileNameFromPathOrURL = PathOrURL
    Else
        GetFileNameFromPathOrURL = Right$(PathOrURL, j - i)
    End If
End Function


' Get the file extension from a path name (ex. .doc, .so etc.)
' Note this will return anything after a dot if the URL/path is just a directory name
Function GetFileExtensionFromPathOrURL$ (PathOrURL As String)
    Dim fileName As String: fileName = GetFileNameFromPathOrURL(PathOrURL)
    Dim i As Unsigned Long: i = InStrRev(fileName, Chr$(KEY_DOT))

    If i <> NULL Then
        GetFileExtensionFromPathOrURL = Right$(fileName, Len(fileName) - i + 1)
    End If
End Function


' Gets the drive or scheme from a path name (ex. C:, HTTPS: etc.)
Function GetDriveOrSchemeFromPathOrURL$ (PathOrURL As String)
    Dim i As Unsigned Long: i = InStr(PathOrURL, Chr$(KEY_COLON))

    If i <> NULL Then
        GetDriveOrSchemeFromPathOrURL = Left$(PathOrURL, i)
    End If
End Function


' Load a file from a file or URL
Function LoadFile$ (PathOrURL As String)
    Select Case UCase$(GetDriveOrSchemeFromPathOrURL(PathOrURL))
        Case "HTTP:", "HTTPS:", "FTP:"
            LoadFile = LoadFileFromURL(PathOrURL)

        Case Else
            LoadFile = LoadFileFromDisk(PathOrURL)
    End Select
End Function


' Loads a whole file from disk into memory
Function LoadFileFromDisk$ (path As String)
    If FileExists(path) Then
        Dim As Long fh: fh = FreeFile

        Open path For Binary Access Read As fh

        LoadFileFromDisk = Input$(LOF(fh), fh)

        Close fh
    End If
End Function


' Loads a whole file from a URL into memory
Function LoadFileFromURL$ (url As String)
    Dim h As Long: h = OpenClient("HTTP:" + url)

    If h <> NULL Then
        Dim As String content, buffer

        While Not EOF(h)
            Limit UPDATES_PER_SECOND
            Get h, , buffer
            content = content + buffer
        Wend

        Close h

        LoadFileFromURL = content
    End If
End Function
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

