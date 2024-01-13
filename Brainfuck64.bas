'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2023 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/ConsoleOps.bi'
'$INCLUDE:'include/PointerOps.bi'
'$INCLUDE:'include/FileOps.bi'
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'-----------------------------------------------------------------------------------------------------------------------
$NOPREFIX
$CONSOLE:ONLY
$EXEICON:'./Brainfuck64.ico'
$VERSIONINFO:ProductName='Brainfuck64'
$VERSIONINFO:CompanyName='Samuel Gomes'
$VERSIONINFO:LegalCopyright='Copyright (c) 2023 Samuel Gomes'
$VERSIONINFO:LegalTrademarks='All trademarks are property of their respective owners'
$VERSIONINFO:Web='https://github.com/a740g'
$VERSIONINFO:Comments='https://github.com/a740g'
$VERSIONINFO:InternalName='Brainfuck64'
$VERSIONINFO:OriginalFilename='Brainfuck64.exe'
$VERSIONINFO:FileDescription='Brainfuck64 executable'
$VERSIONINFO:FILEVERSION#=1,0,2,0
$VERSIONINFO:PRODUCTVERSION#=1,0,2,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
CONST APP_NAME = "Brainfuck64"
CONST INTERPRETER_MEMORY_DEFAULT = 30000
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
CHDIR STARTDIR$

' If there are no command line parameters just show some info and exit
IF COMMANDCOUNT < 1 THEN
    ECHO STRING_EMPTY
    ECHO "Brainfuck64: A Brainfuck interpreter written in QB64-PE"
    ECHO "Copyright (c) 2023 Samuel Gomes"
    ECHO STRING_EMPTY
    ECHO "https://github.com/a740g"
    ECHO STRING_EMPTY
    ECHO "Usage: Brainfuck64 [program1.bf] [program2.bf] ..."
    ECHO STRING_EMPTY
    ECHO "Note:"
    ECHO " * Wildcards (*, ?) are supported"
    ECHO " * URLs are supported"
    ECHO " * On Windows, use Terminal for best results"
    ECHO STRING_EMPTY

    DO
        DIM fileName AS STRING: fileName = _OPENFILEDIALOG$("Open Brainfuck File", , "*.bf|*.BF|*.Bf|*.bF", "Brainfuck files")

        IF LEN(fileName) > 0 THEN
            ECHO STRING_EMPTY ' move to a new line if we are running more than one program
            RunBrainfuckProgram LoadFile(fileName), GetFileNameFromPathOrURL(fileName)
        END IF
    LOOP UNTIL LEN(fileName) = NULL
ELSE
    DIM i AS UNSIGNED LONG: FOR i = 1 TO COMMANDCOUNT
        RunBrainfuckProgram LoadFile(COMMAND$(i)), GetFileNameFromPathOrURL(COMMAND$(i))

        IF i < COMMANDCOUNT THEN ECHO STRING_EMPTY ' move to a new line if we are running more than one program
    NEXT
END IF

SYSTEM
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' FUNCTIONS AND SUBROUTINES
'-----------------------------------------------------------------------------------------------------------------------
SUB RunBrainfuckProgram (programString AS STRING, programName AS STRING)
    REDIM AS UNSIGNED BYTE memory(0 TO INTERPRETER_MEMORY_DEFAULT - 1)
    DIM AS UNSIGNED BYTE instruction
    DIM AS LONG instructionPointer, memoryPointer, programLength, bracketOpenCount
    REDIM AS LONG bracketPosition(0 TO 0) ' matching bracket positions
    REDIM AS LONG stack(0 TO 0)
    DIM AS STRING program

    ' Optimize program stream
    CONSOLETITLE "Optimizing..."

    programLength = LEN(programString)
    program = SPACE$(programLength) ' allocate memory assuming we'll use the entire length of programString

    instructionPointer = 0
    WHILE instructionPointer < programLength
        instruction = PeekStringByte(programString, instructionPointer)

        ' Only accept supported commands and discard the rest
        SELECT CASE instruction
            CASE KEY_GREATER_THAN, KEY_LESS_THAN, KEY_PLUS, KEY_MINUS, KEY_DOT, KEY_COMMA, KEY_OPEN_BRACKET, KEY_CLOSE_BRACKET ' regular commands
                PokeStringByte program, memoryPointer, instruction
                memoryPointer = memoryPointer + 1
        END SELECT

        instructionPointer = instructionPointer + 1
    WEND

    program = LEFT$(program, memoryPointer) ' compact the program stream

    ' Build jump table
    CONSOLETITLE "Building jump table..."

    programLength = LEN(program)
    instructionPointer = 0
    WHILE instructionPointer < programLength
        instruction = PeekStringByte(program, instructionPointer)

        SELECT CASE instruction
            CASE KEY_OPEN_BRACKET
                stack(bracketOpenCount) = instructionPointer

                bracketOpenCount = bracketOpenCount + 1
                IF bracketOpenCount > UBOUND(stack) THEN ' we moved past the stack upper bound
                    REDIM PRESERVE AS LONG stack(0 TO bracketOpenCount) ' dynamically grow the stack space preserving contents
                END IF

            CASE KEY_CLOSE_BRACKET
                IF bracketOpenCount < 1 THEN ERROR ERROR_SYNTAX_ERROR ' brackets are not matched

                IF instructionPointer > UBOUND(bracketPosition) THEN
                    REDIM PRESERVE AS LONG bracketPosition(0 TO instructionPointer)
                END IF

                bracketPosition(instructionPointer) = stack(bracketOpenCount - 1)
                bracketPosition(stack(bracketOpenCount - 1)) = instructionPointer

                bracketOpenCount = bracketOpenCount - 1
        END SELECT

        instructionPointer = instructionPointer + 1
    WEND

    IF bracketOpenCount > 0 THEN ERROR ERROR_SYNTAX_ERROR ' brackets are not matched

    ' Execute the program
    CONSOLETITLE programName ' set the window title

    ' Re-initialize stuff based on the optimized stream
    instructionPointer = 0
    memoryPointer = 0
    programLength = LEN(program)

    DO WHILE instructionPointer < programLength
        instruction = PeekStringByte(program, instructionPointer)

        SELECT CASE instruction
            CASE KEY_GREATER_THAN
                memoryPointer = memoryPointer + 1 ' increment the memory pointer

                SELECT CASE memoryPointer
                    CASE IS > UBOUND(memory) ' if we moved pass the memory address space
                        REDIM PRESERVE AS UNSIGNED BYTE memory(0 TO memoryPointer) ' dynamically grow the memory space preserving contents

                    CASE IS < 0 ' can happen if we move past the max value of long
                        ERROR ERROR_CANNOT_CONTINUE ' throw an error
                END SELECT

            CASE KEY_LESS_THAN
                memoryPointer = memoryPointer - 1 ' decrement the memory pointer

                IF memoryPointer < 0 THEN ERROR ERROR_CANNOT_CONTINUE ' cannot go to negative address space

            CASE KEY_PLUS
                memory(memoryPointer) = memory(memoryPointer) + 1

            CASE KEY_MINUS
                memory(memoryPointer) = memory(memoryPointer) - 1

            CASE KEY_DOT
                Console_PutCharacter memory(memoryPointer)

            CASE KEY_COMMA
                ' Get the current window title and then tell the user that we need keyboard input
                CONSOLETITLE "[WAITING FOR INPUT] " + programName

                memory(memoryPointer) = Console_GetCharacter

                CONSOLETITLE programName ' set the window title the way it was

            CASE KEY_OPEN_BRACKET
                IF memory(memoryPointer) = 0 THEN instructionPointer = bracketPosition(instructionPointer)

            CASE KEY_CLOSE_BRACKET
                IF memory(memoryPointer) <> 0 THEN instructionPointer = bracketPosition(instructionPointer)

        END SELECT

        instructionPointer = instructionPointer + 1
    LOOP
END SUB
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/ProgramArgs.bas'
'$INCLUDE:'include/FileOps.bas'
'-----------------------------------------------------------------------------------------------------------------------
'-----------------------------------------------------------------------------------------------------------------------
