'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2025 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

$CONSOLE:ONLY

$LET TOOLBOX64_STRICT = TRUE
'$INCLUDE:'include/Core/PointerOps.bi'
'$INCLUDE:'include/CLI/Args.bi'
'$INCLUDE:'include/FS/Pathname.bi'
'$INCLUDE:'include/IO/Console.bi'
'$INCLUDE:'include/IO/File.bi'

$EXEICON:'./Brainfuck64.ico'
$VERSIONINFO:ProductName='Brainfuck64'
$VERSIONINFO:CompanyName='Samuel Gomes'
$VERSIONINFO:LegalCopyright='Copyright (c) 2025 Samuel Gomes'
$VERSIONINFO:LegalTrademarks='All trademarks are property of their respective owners'
$VERSIONINFO:Web='https://github.com/a740g'
$VERSIONINFO:Comments='https://github.com/a740g'
$VERSIONINFO:InternalName='Brainfuck64'
$VERSIONINFO:OriginalFilename='Brainfuck64.exe'
$VERSIONINFO:FileDescription='Brainfuck64 executable'
$VERSIONINFO:FILEVERSION#=1,1,6,0
$VERSIONINFO:PRODUCTVERSION#=1,1,6,0

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

ON ERROR GOTO error_handler

CHDIR _STARTDIR$

DIM isInteractive AS _BYTE

IF _COMMANDCOUNT < 1 _ORELSE Args_GetArgumentIndex("help") > 0 THEN
    isInteractive = _TRUE

    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Brainfuck64: A Brainfuck interpreter written in QB64-PE"
    Console_WriteLine "Copyright (c) 2025 Samuel Gomes"
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
        END IF
    LOOP WHILE LEN(selectedFile)
ELSE
    DIM argIndex AS LONG
    FOR argIndex = 1 TO _COMMANDCOUNT
        Brainfuck_Execute File_Load(COMMAND$(argIndex)), Pathname_GetFileName(COMMAND$(argIndex))

        IF argIndex < _COMMANDCOUNT THEN Console_WriteLine _STR_EMPTY
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
    IF LEN(errorFile) = 0 THEN errorFile = Pathname_RemoveFileExtension(Pathname_GetFileName(Args_GetExecutablePathName)) + ".bas"
END IF

Console_WriteLine _STR_EMPTY
Console_WriteLine "Runtime error" + STR$(ERR) + " (" + LCASE$(_ERRORMESSAGE$) + ") on line" + STR$(errorLine) + " in " + errorFile

IF isInteractive THEN
    END ERR
ELSE
    SYSTEM ERR
END IF

SUB Brainfuck_Execute (sourceCode AS STRING, sourceName AS STRING)
    DIM cleanedCode AS STRING: cleanedCode = Brainfuck_GetCleanedCode(sourceCode)

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
    DIM buffer AS STRING: buffer = SPACE$(length)
    DIM char AS _UNSIGNED _BYTE
    DIM AS LONG i, writeIdx

    WHILE i < length
        char = PeekStringByte(source, i)

        SELECT CASE char
            CASE _ASC_GREATERTHAN, _ASC_LESSTHAN, _ASC_PLUS, _ASC_MINUS, _ASC_FULLSTOP, _ASC_COMMA, _ASC_LEFTSQUAREBRACKET, _ASC_RIGHTSQUAREBRACKET
                PokeStringByte buffer, writeIdx, char
                writeIdx = writeIdx + 1
        END SELECT

        i = i + 1
    WEND

    Brainfuck_GetCleanedCode = LEFT$(buffer, writeIdx)
END FUNCTION

FUNCTION Brainfuck_GenerateInitialIR& (source AS STRING, codes() AS _UNSIGNED _BYTE, values() AS LONG)
    _CONSOLETITLE "Generating IR..."

    DIM length AS LONG: length = LEN(source)
    DIM char AS _UNSIGNED _BYTE
    DIM AS LONG i, count

    WHILE i < length
        char = PeekStringByte(source, i)

        SELECT CASE char
            CASE _ASC_PLUS, _ASC_MINUS
                DIM delta AS LONG: delta = 0

                DO WHILE i < length
                    char = PeekStringByte(source, i)
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
                    codes(count) = OP_ADD
                    values(count) = delta
                    count = count + 1
                END IF

            CASE _ASC_GREATERTHAN, _ASC_LESSTHAN
                DIM move AS LONG: move = 0

                DO WHILE i < length
                    char = PeekStringByte(source, i)
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

                IF i + 2 < length THEN
                    DIM next1 AS _UNSIGNED _BYTE: next1 = PeekStringByte(source, i + 1)
                    DIM next2 AS _UNSIGNED _BYTE: next2 = PeekStringByte(source, i + 2)
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

    DIM AS LONG i, k, offset, entries, tableIdx
    DIM ok AS _BYTE

    WHILE i < count
        IF codes(i) = OP_JZ THEN
            DIM target AS LONG: target = values(i)

            IF target = i + 2 _ANDALSO codes(i + 1) = OP_MOVE THEN
                codes(i) = OP_SCAN_ZERO
                values(i) = values(i + 1)
                codes(i + 1) = OP_NOP
                codes(target) = OP_NOP
            ELSEIF codes(i + 1) = OP_ADD _ANDALSO values(i + 1) = -1 THEN
                offset = 0
                ok = _TRUE

                FOR k = i + 2 TO target - 1
                    IF codes(k) <> OP_MOVE _ANDALSO codes(k) <> OP_ADD THEN
                        ok = _FALSE
                        EXIT FOR
                    END IF
                    IF codes(k) = OP_MOVE THEN offset = offset + values(k)
                NEXT

                IF ok _ANDALSO offset = 0 THEN
                    codes(i) = OP_ADD_REL_MULTI
                    values(i) = tableIdx
                    offset = 0
                    entries = 0

                    FOR k = i + 2 TO target - 1
                        IF codes(k) = OP_MOVE THEN
                            offset = offset + values(k)
                        ELSEIF codes(k) = OP_ADD THEN
                            table(tableIdx).offset = offset
                            table(tableIdx).multiplier = values(k)
                            tableIdx = tableIdx + 1
                            entries = entries + 1
                        END IF

                        codes(k) = OP_NOP
                    NEXT

                    aux(i) = entries
                    codes(i + 1) = OP_SET: values(i + 1) = 0
                    codes(target) = OP_NOP
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
            values(i) = mapping(values(i))
        END IF

        i = i + 1
    WEND

    Brainfuck_CompactInstructions = writeIdx
END FUNCTION

SUB Brainfuck_RunInterpreter (count AS LONG, codes() AS _UNSIGNED _BYTE, values() AS LONG, aux() AS LONG, table() AS RelativeAddition, programName AS STRING)
    _CONSOLETITLE programName

    REDIM memory(0 TO MEMORY_CHUNK_SIZE - 1) AS _UNSIGNED _BYTE

    DIM AS LONG pc, dataPtr, mult, stride, irTableBase, entries, i, target

    WHILE pc < count
        SELECT CASE codes(pc)
            CASE OP_ADD
                memory(dataPtr) = memory(dataPtr) + values(pc)

            CASE OP_MOVE
                dataPtr = dataPtr + values(pc)

                IF dataPtr < 0 THEN ERROR _ERR_CANNOT_CONTINUE

                IF dataPtr > UBOUND(memory) THEN
                    REDIM _PRESERVE memory(0 TO dataPtr + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
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

                    IF dataPtr < 0 THEN ERROR _ERR_CANNOT_CONTINUE

                    IF dataPtr > UBOUND(memory) THEN
                        REDIM _PRESERVE memory(0 TO dataPtr + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
                    END IF
                LOOP

            CASE OP_ADD_REL_MULTI
                mult = memory(dataPtr)

                IF mult <> 0 THEN
                    irTableBase = values(pc)
                    entries = aux(pc)

                    i = 0
                    DO WHILE i < entries
                        target = dataPtr + table(irTableBase + i).offset

                        IF target < 0 THEN ERROR _ERR_CANNOT_CONTINUE

                        IF target > UBOUND(memory) THEN
                            REDIM _PRESERVE memory(0 TO target + MEMORY_CHUNK_SIZE) AS _UNSIGNED _BYTE
                        END IF

                        memory(target) = memory(target) + mult * table(irTableBase + i).multiplier

                        i = i + 1
                    LOOP
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
