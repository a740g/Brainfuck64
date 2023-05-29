'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2023 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'-----------------------------------------------------------------------------------------------------------------------
'$Include:'include/CRTLib.bi'
'$Include:'include/FileOps.bi'
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'-----------------------------------------------------------------------------------------------------------------------
$NoPrefix
$Console:Only
$ExeIcon:'./Brainfuck64.ico'
$VersionInfo:ProductName=Brainfuck64
$VersionInfo:CompanyName=Samuel Gomes
$VersionInfo:LegalCopyright=Copyright (c) 2023 Samuel Gomes
$VersionInfo:LegalTrademarks=All trademarks are property of their respective owners
$VersionInfo:Web=https://github.com/a740g
$VersionInfo:Comments=https://github.com/a740g
$VersionInfo:InternalName=Brainfuck64
$VersionInfo:OriginalFilename=Brainfuck64.exe
$VersionInfo:FileDescription=Brainfuck64 executable
$VersionInfo:FILEVERSION#=1,0,0,0
$VersionInfo:PRODUCTVERSION#=1,0,0,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
Const APP_NAME = "Brainfuck64"
Const INTERPRETER_MEMORY_DEFAULT = 30000
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
ChDir StartDir$

' If there are no command line parameters just show some info and exit
If CommandCount < 1 Or GetProgramArgumentIndex(KEY_QUESTION_MARK) > 0 Then
    Print
    Print "Brainfuck64: A Brainfuck interpreter written in QB64-PE"
    Print "Copyright (c) 2023 Samuel Gomes"
    Print
    Print "https://github.com/a740g"
    Print
    Print "Usage: Brainfuck64 [program1.bf] [program2.bf] ..."
    Print
    Print "Note:"
    Print " * Wildcards (*, ?) are supported"
    Print " * URLs are supported"
    Print " * On Windows, use Terminal for best results"
    Print
    System
End If

Dim i As Unsigned Long

For i = 1 To CommandCount
    RunBrainfuckProgram LoadFile(Command$(i)), GetFileNameFromPathOrURL(Command$(i))
    If i < CommandCount Then Print ' move to a new line if we are running more than one program
Next

System
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' FUNCTIONS AND SUBROUTINES
'-----------------------------------------------------------------------------------------------------------------------
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
    program = Space$(programLength) ' allocate memory assuming we'll use the entire length of programString

    For instructionPointer = 0 To programLength - 1
        instruction = PeekString(programString, instructionPointer)

        ' Only accept supported commands and discard the rest
        Select Case instruction
            Case KEY_GREATER_THAN, KEY_LESS_THAN, KEY_PLUS, KEY_MINUS, KEY_DOT, KEY_COMMA, KEY_OPEN_BRACKET, KEY_CLOSE_BRACKET ' regular commands
                PokeString program, memoryPointer, instruction
                memoryPointer = memoryPointer + 1
        End Select
    Next

    program = Left$(program, memoryPointer) ' compact the program stream

    ' Build jump table
    ConsoleTitle "Building jump table..."

    programLength = Len(program)
    For instructionPointer = 0 To programLength - 1
        instruction = PeekString(program, instructionPointer)

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
        instruction = PeekString(program, instructionPointer)

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
                PutChar memory(memoryPointer)

            Case KEY_COMMA
                ' Get the current window title and then tell the user that we need keyboard input
                ConsoleTitle "[WAITING FOR INPUT] " + programName

                memory(memoryPointer) = GetChar

                ConsoleTitle programName ' set the window title the way it was

            Case KEY_OPEN_BRACKET
                If memory(memoryPointer) = 0 Then instructionPointer = bracketPosition(instructionPointer)

            Case KEY_CLOSE_BRACKET
                If memory(memoryPointer) <> 0 Then instructionPointer = bracketPosition(instructionPointer)

        End Select

        instructionPointer = instructionPointer + 1
    Loop
End Sub
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'-----------------------------------------------------------------------------------------------------------------------
'$Include:'include/ProgramArgs.bas'
'$Include:'include/FileOps.bas'
'-----------------------------------------------------------------------------------------------------------------------
'-----------------------------------------------------------------------------------------------------------------------
