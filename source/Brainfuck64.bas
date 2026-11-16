'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2026 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

$CONSOLE:ONLY

_DEFINE A-Z AS LONG
OPTION _EXPLICIT
'$STATIC
OPTION BASE 1

'$INCLUDE:'Pathname.bi'
'$INCLUDE:'File.bi'
'$INCLUDE:'Console.bi'

$EXEICON:'./Brainfuck64.ico'
$VERSIONINFO:ProductName='Brainfuck64'
$VERSIONINFO:CompanyName='Samuel Gomes'
$VERSIONINFO:LegalCopyright='Copyright (c) 2026 Samuel Gomes'
$VERSIONINFO:LegalTrademarks='All trademarks are property of their respective owners'
$VERSIONINFO:Web='https://github.com/a740g'
$VERSIONINFO:Comments='https://github.com/a740g'
$VERSIONINFO:InternalName='Brainfuck64'
$VERSIONINFO:OriginalFilename='Brainfuck64.exe'
$VERSIONINFO:FileDescription='Brainfuck64 executable'
$VERSIONINFO:FILEVERSION#=1,1,7,0
$VERSIONINFO:PRODUCTVERSION#=1,1,7,0

CONST APP_NAME = "Brainfuck64"
CONST MEMORY_CHUNK_SIZE = 1024 * 1024
CONST OP_ADD = 1
CONST OP_MOVE = 2
CONST OP_OUT = 3
CONST OP_IN = 4
CONST OP_JZ = 5
CONST OP_JNZ = 6
CONST OP_SET = 7
CONST OP_SCAN_ZERO = 8
CONST OP_ADD_REL_MULTI = 9
CONST OP_NOP = 10

TYPE RelativeAddition
    offset AS LONG
    multiplier AS LONG
END TYPE

SUB Brainfuck_Execute (sourceCode AS STRING, sourceName AS STRING)
    DIM cleanedCode AS STRING: cleanedCode = Brainfuck_GetCleanedCode(sourceCode)
    IF LEN(cleanedCode) = 0 THEN EXIT SUB

    DIM initialCapacity AS LONG: initialCapacity = LEN(cleanedCode)
    REDIM opCodes(0 TO initialCapacity) AS _UNSIGNED _BYTE
    REDIM opValues(0 TO initialCapacity) AS LONG
    REDIM opAuxValues(0 TO initialCapacity) AS LONG

    DIM instructionCount AS LONG: instructionCount = Brainfuck_GenerateInitialIR(cleanedCode, opCodes(), opValues())

    Brainfuck_ResolveJumps instructionCount, opCodes(), opValues()

    REDIM multiAddTable(0 TO instructionCount * 2) AS RelativeAddition
    DIM multiAddCount AS LONG: multiAddCount = Brainfuck_OptimizePatterns(instructionCount, opCodes(), opValues(), opAuxValues(), multiAddTable())

    instructionCount = Brainfuck_CompactInstructions(instructionCount, opCodes(), opValues(), opAuxValues())

    Brainfuck_RunInterpreter instructionCount, opCodes(), opValues(), opAuxValues(), multiAddTable(), sourceName
END SUB

FUNCTION Brainfuck_GetCleanedCode$ (source AS STRING)
    _CONSOLETITLE "Cleaning code..."

    DIM length AS LONG: length = LEN(source)
    IF length = 0 THEN EXIT FUNCTION

    DIM buffer AS STRING: buffer = SPACE$(length)
    DIM char AS _UNSIGNED _BYTE
    DIM AS LONG i, writeIdx

    STATIC isBrainfuckChar(0 TO 255) AS _BYTE
    STATIC lutInitialized AS _BYTE

    IF NOT lutInitialized THEN
        isBrainfuckChar(_ASC_GREATERTHAN) = _TRUE
        isBrainfuckChar(_ASC_LESSTHAN) = _TRUE
        isBrainfuckChar(_ASC_PLUS) = _TRUE
        isBrainfuckChar(_ASC_MINUS) = _TRUE
        isBrainfuckChar(_ASC_FULLSTOP) = _TRUE
        isBrainfuckChar(_ASC_COMMA) = _TRUE
        isBrainfuckChar(_ASC_LEFTSQUAREBRACKET) = _TRUE
        isBrainfuckChar(_ASC_RIGHTSQUAREBRACKET) = _TRUE
        lutInitialized = _TRUE
    END IF

    FOR i = 1 TO length
        char = ASC(source, i)
        IF isBrainfuckChar(char) THEN
            writeIdx = writeIdx + 1
            ASC(buffer, writeIdx) = char
        END IF
    NEXT i

    Brainfuck_GetCleanedCode = LEFT$(buffer, writeIdx)
END FUNCTION

FUNCTION Brainfuck_GenerateInitialIR& (source AS STRING, codes() AS _UNSIGNED _BYTE, values() AS LONG)
    _CONSOLETITLE "Generating IR..."

    DIM length AS LONG: length = LEN(source)
    DIM char AS _UNSIGNED _BYTE
    DIM AS LONG i, count

    i = 1
    WHILE i <= length
        char = ASC(source, i)

        SELECT CASE char
            CASE _ASC_PLUS, _ASC_MINUS
                DIM delta AS LONG: delta = 0

                DO WHILE i <= length
                    char = ASC(source, i)
                    IF char = _ASC_PLUS THEN
                        delta = delta + 1
                    ELSEIF char = _ASC_MINUS THEN
                        delta = delta - 1
                    ELSE
                        EXIT DO
                    END IF
                    i = i + 1
                LOOP

                i = i - 1

                IF delta <> 0 THEN
                    IF count > 0 _ANDALSO codes(count - 1) = OP_SET THEN
                        values(count - 1) = (values(count - 1) + delta) AND 255
                    ELSE
                        codes(count) = OP_ADD
                        values(count) = delta
                        count = count + 1
                    END IF
                END IF

            CASE _ASC_GREATERTHAN, _ASC_LESSTHAN
                DIM move AS LONG: move = 0

                DO WHILE i <= length
                    char = ASC(source, i)
                    IF char = _ASC_GREATERTHAN THEN
                        move = move + 1
                    ELSEIF char = _ASC_LESSTHAN THEN
                        move = move - 1
                    ELSE
                        EXIT DO
                    END IF
                    i = i + 1
                LOOP

                i = i - 1

                IF move <> 0 THEN
                    codes(count) = OP_MOVE
                    values(count) = move
                    count = count + 1
                END IF

            CASE _ASC_LEFTSQUAREBRACKET
                DIM handled AS LONG: handled = _FALSE

                IF i + 2 <= length THEN
                    DIM next1 AS _UNSIGNED _BYTE: next1 = ASC(source, i + 1)
                    DIM next2 AS _UNSIGNED _BYTE: next2 = ASC(source, i + 2)
                    IF (next1 = _ASC_MINUS _ORELSE next1 = _ASC_PLUS) _ANDALSO next2 = _ASC_RIGHTSQUAREBRACKET THEN
                        codes(count) = OP_SET
                        values(count) = 0
                        count = count + 1
                        i = i + 2
                        handled = _TRUE
                    END IF
                END IF

                IF NOT handled THEN
                    codes(count) = OP_JZ
                    count = count + 1
                END IF

            CASE _ASC_RIGHTSQUAREBRACKET
                codes(count) = OP_JNZ
                count = count + 1

            CASE _ASC_FULLSTOP
                codes(count) = OP_OUT
                count = count + 1

            CASE _ASC_COMMA
                codes(count) = OP_IN
                count = count + 1
        END SELECT

        i = i + 1
    WEND

    Brainfuck_GenerateInitialIR = count
END FUNCTION

SUB Brainfuck_ResolveJumps (count AS LONG, codes() AS _UNSIGNED _BYTE, values() AS LONG)
    _CONSOLETITLE "Linking jumps..."

    REDIM stack(0 TO count) AS LONG
    DIM AS LONG i, ptr

    WHILE i < count
        IF codes(i) = OP_JZ THEN
            stack(ptr) = i
            ptr = ptr + 1
        ELSEIF codes(i) = OP_JNZ THEN
            ptr = ptr - 1
            IF ptr < 0 THEN ERROR _ERR_SYNTAX_ERROR
            DIM target AS LONG: target = stack(ptr)
            values(i) = target
            values(target) = i
        END IF

        i = i + 1
    WEND

    IF ptr THEN ERROR _ERR_SYNTAX_ERROR
END SUB

FUNCTION Brainfuck_OptimizePatterns& (count AS LONG, codes() AS _UNSIGNED _BYTE, values() AS LONG, aux() AS LONG, table() AS RelativeAddition)
    _CONSOLETITLE "Optimizing patterns..."

    DIM AS LONG i, k, target, tableIdx
    DIM AS LONG curOffset, counterDelta, numDeltas, foundIdx, j, finalEntries
    DIM loopOk AS _BYTE
    DIM deltaOffsets(1 TO 64) AS LONG
    DIM deltaMults(1 TO 64) AS LONG

    WHILE i < count
        IF codes(i) = OP_JZ THEN
            target = values(i)

            IF target = i + 2 _ANDALSO codes(i + 1) = OP_MOVE THEN
                ' [>] or [<] scan loop
                codes(i) = OP_SCAN_ZERO
                values(i) = values(i + 1)
                codes(i + 1) = OP_NOP
                codes(target) = OP_NOP
            ELSE
                ' Check for copy / multiply loop: [ ... ]
                ' Loop must contain only OP_MOVE and OP_ADD, net offset must be 0, counter delta must be -1
                loopOk = _TRUE
                curOffset = 0
                counterDelta = 0
                numDeltas = 0

                FOR k = i + 1 TO target - 1
                    IF codes(k) = OP_MOVE THEN
                        curOffset = curOffset + values(k)
                    ELSEIF codes(k) = OP_ADD THEN
                        IF curOffset = 0 THEN
                            counterDelta = counterDelta + values(k)
                        ELSE
                            foundIdx = 0
                            FOR j = 1 TO numDeltas
                                IF deltaOffsets(j) = curOffset THEN
                                    foundIdx = j
                                    EXIT FOR
                                END IF
                            NEXT j

                            IF foundIdx > 0 THEN
                                deltaMults(foundIdx) = deltaMults(foundIdx) + values(k)
                            ELSE
                                IF numDeltas < 64 THEN
                                    numDeltas = numDeltas + 1
                                    deltaOffsets(numDeltas) = curOffset
                                    deltaMults(numDeltas) = values(k)
                                ELSE
                                    loopOk = _FALSE
                                    EXIT FOR
                                END IF
                            END IF
                        END IF
                    ELSE
                        loopOk = _FALSE
                        EXIT FOR
                    END IF
                NEXT k

                IF loopOk _ANDALSO curOffset = 0 _ANDALSO counterDelta = -1 THEN
                    ' Count how many non-zero deltas we have
                    finalEntries = 0
                    FOR j = 1 TO numDeltas
                        IF deltaMults(j) <> 0 THEN finalEntries = finalEntries + 1
                    NEXT j

                    IF finalEntries > 0 THEN
                        ' Make sure table has enough space
                        IF tableIdx + finalEntries > UBOUND(table) THEN
                            REDIM _PRESERVE table(0 TO UBOUND(table) + count + finalEntries) AS RelativeAddition
                        END IF

                        codes(i) = OP_ADD_REL_MULTI
                        values(i) = tableIdx
                        aux(i) = finalEntries

                        FOR j = 1 TO numDeltas
                            IF deltaMults(j) <> 0 THEN
                                table(tableIdx).offset = deltaOffsets(j)
                                table(tableIdx).multiplier = deltaMults(j)
                                tableIdx = tableIdx + 1
                            END IF
                        NEXT j
                    ELSE
                        ' Counter was decremented to 0 and no other cells modified: equivalent to [-]
                        codes(i) = OP_SET
                        values(i) = 0
                    END IF

                    FOR k = i + 1 TO target
                        codes(k) = OP_NOP
                    NEXT k
                END IF
            END IF
        END IF

        i = i + 1
    WEND

    Brainfuck_OptimizePatterns = tableIdx
END FUNCTION

FUNCTION Brainfuck_CompactInstructions& (count AS LONG, codes() AS _UNSIGNED _BYTE, values() AS LONG, aux() AS LONG)
    _CONSOLETITLE "Compacting IR..."

    REDIM mapping(0 TO count) AS LONG
    DIM AS LONG i, writeIdx

    WHILE i < count
        IF codes(i) <> OP_NOP THEN
            mapping(i) = writeIdx
            codes(writeIdx) = codes(i)
            values(writeIdx) = values(i)
            aux(writeIdx) = aux(i)
            writeIdx = writeIdx + 1
        ELSE
            mapping(i) = -1
        END IF

        i = i + 1
    WEND

    i = 0
    WHILE i < writeIdx
        IF codes(i) = OP_JZ _ORELSE codes(i) = OP_JNZ THEN
            values(i) = mapping(values(i)) + 1
        END IF

        i = i + 1
    WEND

    Brainfuck_CompactInstructions = writeIdx
END FUNCTION

SUB Brainfuck_RunInterpreter (count AS LONG, codes() AS _UNSIGNED _BYTE, values() AS LONG, aux() AS LONG, table() AS RelativeAddition, programName AS STRING)
    _CONSOLETITLE programName

    REDIM memory(0 TO MEMORY_CHUNK_SIZE - 1) AS _UNSIGNED _BYTE

    DIM AS LONG pc, dataPtr, mult, stride, irTableBase, entries, i, target
    DIM AS LONG maxMem: maxMem = UBOUND(memory)

    WHILE pc < count
        SELECT CASE codes(pc)
            CASE OP_ADD
                memory(dataPtr) = memory(dataPtr) + values(pc)

            CASE OP_MOVE
                dataPtr = dataPtr + values(pc)

                IF dataPtr > maxMem THEN
                    REDIM _PRESERVE memory(0 TO dataPtr + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
                    maxMem = UBOUND(memory)
                ELSEIF dataPtr < 0 THEN
                    ERROR _ERR_CANNOT_CONTINUE
                END IF

            CASE OP_JZ
                IF memory(dataPtr) = 0 THEN
                    pc = values(pc)
                    _CONTINUE
                END IF

            CASE OP_JNZ
                IF memory(dataPtr) <> 0 THEN
                    pc = values(pc)
                    _CONTINUE
                END IF

            CASE OP_SET
                memory(dataPtr) = values(pc)

            CASE OP_SCAN_ZERO
                stride = values(pc)

                DO WHILE memory(dataPtr) <> 0
                    dataPtr = dataPtr + stride

                    IF dataPtr > maxMem THEN
                        REDIM _PRESERVE memory(0 TO dataPtr + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
                        maxMem = UBOUND(memory)
                    ELSEIF dataPtr < 0 THEN
                        ERROR _ERR_CANNOT_CONTINUE
                    END IF
                LOOP

            CASE OP_ADD_REL_MULTI
                mult = memory(dataPtr)

                IF mult <> 0 THEN
                    irTableBase = values(pc)
                    entries = aux(pc)

                    FOR i = irTableBase TO irTableBase + entries - 1
                        target = dataPtr + table(i).offset

                        IF target > maxMem THEN
                            REDIM _PRESERVE memory(0 TO target + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
                            maxMem = UBOUND(memory)
                        ELSEIF target < 0 THEN
                            ERROR _ERR_CANNOT_CONTINUE
                        END IF

                        memory(target) = memory(target) + mult * table(i).multiplier
                    NEXT i

                    memory(dataPtr) = 0
                END IF

            CASE OP_OUT
                Console_WriteChar memory(dataPtr)

            CASE OP_IN
                _CONSOLETITLE "[WAITING FOR INPUT] " + programName
                memory(dataPtr) = Console_ReadChar
                _CONSOLETITLE programName
        END SELECT

        pc = pc + 1
    WEND
END SUB

' ENTRY POINT

ON ERROR GOTO error_handler

CHDIR _STARTDIR$

DIM isInteractive AS _BYTE

IF _COMMANDCOUNT < 1 THEN
    isInteractive = _TRUE

    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Brainfuck64: A Brainfuck interpreter written in QB64-PE"
    Console_WriteLine "Copyright (c) 2026 Samuel Gomes"
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "https://github.com/a740g"
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Usage: Brainfuck64 [program1.bf] [program2.bf] ..."
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Notes:"
    Console_WriteLine " * Wildcards (* and ?) are supported for file names."
    Console_WriteLine " * HTTP/HTTPS URLs are supported for remote programs."
    Console_WriteLine " * On Windows, use Windows Terminal for the best experience."
    Console_WriteLine _STR_EMPTY

    DO
        DIM selectedFile AS STRING: selectedFile = _OPENFILEDIALOG$("Open Brainfuck File", , "*.bf|*.BF|*.Bf|*.bF", "Brainfuck files")

        IF LEN(selectedFile) THEN
            Console_WriteLine _STR_EMPTY
            Brainfuck_Execute File_Load(selectedFile), Pathname_GetFileName(selectedFile)
            _ECHO _STR_EMPTY ' flush
        END IF
    LOOP WHILE LEN(selectedFile)
ELSE
    DIM argIndex AS LONG
    FOR argIndex = 1 TO _COMMANDCOUNT
        Brainfuck_Execute File_Load(COMMAND$(argIndex)), Pathname_GetFileName(COMMAND$(argIndex))

        IF argIndex < _COMMANDCOUNT THEN _ECHO _STR_EMPTY ' flush
    NEXT
END IF

SYSTEM

error_handler:
DIM errorLine AS _UNSIGNED _INTEGER64: errorLine = _INCLERRORLINE
DIM errorFile AS STRING

IF errorLine THEN
    errorFile = _INCLERRORFILE$
ELSE
    errorLine = _ERRORLINE
    IF errorLine = 0 THEN errorLine = ERL

    errorFile = _INCLERRORFILE$
    IF LEN(errorFile) = 0 THEN errorFile = Pathname_RemoveFileExtension(Pathname_GetFileName(COMMAND$(0))) + ".bas"
END IF

Console_WriteLine _STR_EMPTY
Console_WriteLine "Runtime error" + STR$(ERR) + " (" + LCASE$(_ERRORMESSAGE$) + ") on line" + STR$(errorLine) + " in " + errorFile

IF isInteractive THEN
    END ERR
ELSE
    SYSTEM ERR
END IF
