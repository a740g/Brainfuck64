'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Brainfuck interpreter
' Copyright (c) 2024 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

$LET TOOLBOX64_STRICT = TRUE
'$INCLUDE:'include/Console/Console.bi'
'$INCLUDE:'include/Core/PointerOps.bi'
'$INCLUDE:'include/FS/Pathname.bi'
'$INCLUDE:'include/IO/File.bi'

$CONSOLE:ONLY
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
$VERSIONINFO:FILEVERSION#=1,1,0,0
$VERSIONINFO:PRODUCTVERSION#=1,1,0,0

CONST APP_NAME = "Brainfuck64"
CONST INITIAL_MEMORY_SIZE = 30000

CHDIR _STARTDIR$

IF _COMMANDCOUNT < 1 THEN
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Brainfuck64: A Brainfuck interpreter written in QB64-PE"
    Console_WriteLine "Copyright (c) 2025 Samuel Gomes"
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "https://github.com/a740g"
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Usage: Brainfuck64 [program1.bf] [program2.bf] ..."
    Console_WriteLine _STR_EMPTY
    Console_WriteLine "Note:"
    Console_WriteLine " * Wildcards (*, ?) are supported"
    Console_WriteLine " * URLs are supported"
    Console_WriteLine " * On Windows, use Terminal for best results"
    Console_WriteLine _STR_EMPTY

    DO
        DIM selectedFile AS STRING: selectedFile = _OPENFILEDIALOG$("Open Brainfuck File", , "*.bf|*.BF|*.Bf|*.bF", "Brainfuck files")

        IF LEN(selectedFile) THEN
            Console_WriteLine _STR_EMPTY
            ExecuteBrainfuck File_Load(selectedFile), Pathname_GetFileName(selectedFile)
        END IF
    LOOP WHILE LEN(selectedFile)
ELSE
    DIM argIndex AS _UNSIGNED LONG
    FOR argIndex = 1 TO _COMMANDCOUNT
        ExecuteBrainfuck File_Load(COMMAND$(argIndex)), Pathname_GetFileName(COMMAND$(argIndex))

        IF argIndex < _COMMANDCOUNT THEN Console_WriteLine _STR_EMPTY
    NEXT
END IF

SYSTEM

SUB ExecuteBrainfuck (sourceCode AS STRING, sourceName AS STRING)
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

    _CONSOLETITLE "Optimizing..."
    DIM sourceLength AS LONG: sourceLength = LEN(sourceCode)
    DIM cleanedCode AS STRING: cleanedCode = SPACE$(sourceLength)
    DIM cleanedLength AS LONG: cleanedLength = 0
    DIM charIndex AS LONG, commandByte AS LONG

    FOR charIndex = 0 TO sourceLength - 1
        commandByte = PeekStringByte(sourceCode, charIndex)
        SELECT CASE commandByte
            CASE _ASC_GREATERTHAN, _ASC_LESSTHAN, _ASC_PLUS, _ASC_MINUS, _ASC_FULLSTOP, _ASC_COMMA, _ASC_LEFTSQUAREBRACKET, _ASC_RIGHTSQUAREBRACKET
                PokeStringByte cleanedCode, cleanedLength, commandByte
                cleanedLength = cleanedLength + 1
        END SELECT
    NEXT
    cleanedCode = LEFT$(cleanedCode, cleanedLength)

    _CONSOLETITLE "Building IR..."
    REDIM opCodes(0 TO cleanedLength) AS _UNSIGNED _BYTE
    REDIM opValues(0 TO cleanedLength) AS LONG
    REDIM opAuxValues(0 TO cleanedLength) AS LONG
    DIM instructionCount AS LONG: instructionCount = 0

    charIndex = 0
    WHILE charIndex < cleanedLength
        commandByte = PeekStringByte(cleanedCode, charIndex)

        SELECT CASE commandByte
            CASE _ASC_PLUS, _ASC_MINUS
                DIM delta AS LONG: delta = 0
                DO WHILE charIndex < cleanedLength
                    commandByte = PeekStringByte(cleanedCode, charIndex)
                    IF commandByte = _ASC_PLUS THEN
                        delta = delta + 1
                    ELSEIF commandByte = _ASC_MINUS THEN
                        delta = delta - 1
                    ELSE
                        EXIT DO
                    END IF
                    charIndex = charIndex + 1
                LOOP
                charIndex = charIndex - 1
                IF delta <> 0 THEN
                    opCodes(instructionCount) = OP_ADD
                    opValues(instructionCount) = delta
                    instructionCount = instructionCount + 1
                END IF

            CASE _ASC_GREATERTHAN, _ASC_LESSTHAN
                DIM moveDelta AS LONG: moveDelta = 0
                DO WHILE charIndex < cleanedLength
                    commandByte = PeekStringByte(cleanedCode, charIndex)
                    IF commandByte = _ASC_GREATERTHAN THEN
                        moveDelta = moveDelta + 1
                    ELSEIF commandByte = _ASC_LESSTHAN THEN
                        moveDelta = moveDelta - 1
                    ELSE
                        EXIT DO
                    END IF
                    charIndex = charIndex + 1
                LOOP
                charIndex = charIndex - 1
                IF moveDelta <> 0 THEN
                    opCodes(instructionCount) = OP_MOVE
                    opValues(instructionCount) = moveDelta
                    instructionCount = instructionCount + 1
                END IF

            CASE _ASC_LEFTSQUAREBRACKET
                IF charIndex + 2 < cleanedLength THEN
                    DIM next1 AS _UNSIGNED _BYTE: next1 = PeekStringByte(cleanedCode, charIndex + 1)
                    DIM next2 AS _UNSIGNED _BYTE: next2 = PeekStringByte(cleanedCode, charIndex + 2)
                    IF (next1 = _ASC_MINUS OR next1 = _ASC_PLUS) AND next2 = _ASC_RIGHTSQUAREBRACKET THEN
                        opCodes(instructionCount) = OP_SET
                        opValues(instructionCount) = 0
                        instructionCount = instructionCount + 1
                        charIndex = charIndex + 2
                        GOTO PrepareNextInstruction
                    END IF
                END IF
                opCodes(instructionCount) = OP_JZ
                instructionCount = instructionCount + 1

            CASE _ASC_RIGHTSQUAREBRACKET
                opCodes(instructionCount) = OP_JNZ
                instructionCount = instructionCount + 1

            CASE _ASC_FULLSTOP
                opCodes(instructionCount) = OP_OUT
                instructionCount = instructionCount + 1

            CASE _ASC_COMMA
                opCodes(instructionCount) = OP_IN
                instructionCount = instructionCount + 1
        END SELECT

        PrepareNextInstruction:
        charIndex = charIndex + 1
    WEND

    _CONSOLETITLE "Resolving jumps..."
    REDIM jumpStack(0 TO instructionCount) AS LONG
    DIM stackPointer AS LONG: stackPointer = 0
    FOR charIndex = 0 TO instructionCount - 1
        IF opCodes(charIndex) = OP_JZ THEN
            jumpStack(stackPointer) = charIndex
            stackPointer = stackPointer + 1
        ELSEIF opCodes(charIndex) = OP_JNZ THEN
            stackPointer = stackPointer - 1
            IF stackPointer < 0 THEN ERROR _ERR_SYNTAX_ERROR
            DIM openBracketIndex AS LONG: openBracketIndex = jumpStack(stackPointer)
            opValues(charIndex) = openBracketIndex
            opValues(openBracketIndex) = charIndex
        END IF
    NEXT
    IF stackPointer <> 0 THEN ERROR _ERR_SYNTAX_ERROR

    _CONSOLETITLE "Optimizing IR..."
    REDIM multiAddTable(0 TO instructionCount * 2) AS RelativeAddition
    DIM multiAddTableCount AS LONG: multiAddTableCount = 0
    DIM loopIdx AS LONG, innerIdx AS LONG, relativeOffset AS LONG, isSimpleLoop AS LONG, entriesInLoop AS LONG

    FOR loopIdx = 0 TO instructionCount - 1
        IF opCodes(loopIdx) = OP_JZ THEN
            DIM closeBracketIndex AS LONG: closeBracketIndex = opValues(loopIdx)

            IF closeBracketIndex = loopIdx + 2 AND opCodes(loopIdx + 1) = OP_MOVE THEN
                opCodes(loopIdx) = OP_SCAN_ZERO
                opValues(loopIdx) = opValues(loopIdx + 1)
                opCodes(loopIdx + 1) = OP_NOP
                opCodes(closeBracketIndex) = OP_NOP
            ELSEIF opCodes(loopIdx + 1) = OP_ADD AND opValues(loopIdx + 1) = -1 THEN
                relativeOffset = 0
                isSimpleLoop = _TRUE
                FOR innerIdx = loopIdx + 2 TO closeBracketIndex - 1
                    IF opCodes(innerIdx) <> OP_MOVE AND opCodes(innerIdx) <> OP_ADD THEN
                        isSimpleLoop = _FALSE: EXIT FOR
                    END IF
                    IF opCodes(innerIdx) = OP_MOVE THEN relativeOffset = relativeOffset + opValues(innerIdx)
                NEXT
                IF isSimpleLoop AND relativeOffset = 0 THEN
                    opCodes(loopIdx) = OP_ADD_REL_MULTI
                    opValues(loopIdx) = multiAddTableCount
                    relativeOffset = 0
                    entriesInLoop = 0
                    FOR innerIdx = loopIdx + 2 TO closeBracketIndex - 1
                        IF opCodes(innerIdx) = OP_MOVE THEN
                            relativeOffset = relativeOffset + opValues(innerIdx)
                        ELSEIF opCodes(innerIdx) = OP_ADD THEN
                            multiAddTable(multiAddTableCount).offset = relativeOffset
                            multiAddTable(multiAddTableCount).multiplier = opValues(innerIdx)
                            multiAddTableCount = multiAddTableCount + 1
                            entriesInLoop = entriesInLoop + 1
                        END IF
                        opCodes(innerIdx) = OP_NOP
                    NEXT
                    opAuxValues(loopIdx) = entriesInLoop
                    opCodes(loopIdx + 1) = OP_SET: opValues(loopIdx + 1) = 0
                    opCodes(closeBracketIndex) = OP_NOP
                END IF
            END IF
        END IF
    NEXT

    DIM writePointer AS LONG: writePointer = 0
    REDIM indexMap(0 TO instructionCount) AS LONG
    FOR charIndex = 0 TO instructionCount - 1
        IF opCodes(charIndex) <> OP_NOP THEN
            indexMap(charIndex) = writePointer
            opCodes(writePointer) = opCodes(charIndex)
            opValues(writePointer) = opValues(charIndex)
            opAuxValues(writePointer) = opAuxValues(charIndex)
            writePointer = writePointer + 1
        ELSE
            indexMap(charIndex) = -1
        END IF
    NEXT
    instructionCount = writePointer

    FOR charIndex = 0 TO instructionCount - 1
        IF opCodes(charIndex) = OP_JZ OR opCodes(charIndex) = OP_JNZ THEN
            IF indexMap(opValues(charIndex)) = -1 THEN ERROR _ERR_INTERNAL_ERROR
            opValues(charIndex) = indexMap(opValues(charIndex))
        END IF
    NEXT

    _CONSOLETITLE sourceName
    
    REDIM memoryBuffer(0 TO 1048575) AS _UNSIGNED _BYTE
    DIM memoryHandle AS _MEM: memoryHandle = _MEM(memoryBuffer())
    DIM memoryStartOffset AS _OFFSET: memoryStartOffset = memoryHandle.OFFSET
    DIM dataPointer AS LONG: dataPointer = 0
    DIM programPointer AS LONG: programPointer = 0
    DIM loopMultiplier AS LONG, scanStride AS LONG, tableBase AS LONG, entryCount AS LONG, tableIdx AS LONG
    
    DO WHILE programPointer < instructionCount
        SELECT CASE opCodes(programPointer)
            CASE OP_ADD
                _MEMPUT memoryHandle, memoryStartOffset + dataPointer, _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE) + opValues(programPointer) AS _UNSIGNED _BYTE

            CASE OP_MOVE
                dataPointer = dataPointer + opValues(programPointer)
                IF dataPointer < 0 THEN ERROR _ERR_CANNOT_CONTINUE
                IF dataPointer >= memoryHandle.SIZE THEN
                    _MEMFREE memoryHandle
                    REDIM _PRESERVE memoryBuffer(0 TO dataPointer + 1048576) AS _UNSIGNED _BYTE
                    memoryHandle = _MEM(memoryBuffer())
                    memoryStartOffset = memoryHandle.OFFSET
                END IF

            CASE OP_JZ
                IF _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE) = 0 THEN programPointer = opValues(programPointer)

            CASE OP_JNZ
                IF _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE) <> 0 THEN programPointer = opValues(programPointer)

            CASE OP_SET
                _MEMPUT memoryHandle, memoryStartOffset + dataPointer, opValues(programPointer) AS _UNSIGNED _BYTE

            CASE OP_SCAN_ZERO
                scanStride = opValues(programPointer)
                DO WHILE _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE) <> 0
                    dataPointer = dataPointer + scanStride
                    IF dataPointer < 0 THEN ERROR _ERR_CANNOT_CONTINUE
                    IF dataPointer >= memoryHandle.SIZE THEN
                        _MEMFREE memoryHandle
                        REDIM _PRESERVE memoryBuffer(0 TO dataPointer + 1048576) AS _UNSIGNED _BYTE
                        memoryHandle = _MEM(memoryBuffer())
                        memoryStartOffset = memoryHandle.OFFSET
                    END IF
                LOOP

            CASE OP_ADD_REL_MULTI
                loopMultiplier = _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE)
                IF loopMultiplier <> 0 THEN
                    tableBase = opValues(programPointer)
                    entryCount = opAuxValues(programPointer)
                    FOR tableIdx = 0 TO entryCount - 1
                        DIM targetCellOffset AS LONG: targetCellOffset = dataPointer + multiAddTable(tableBase + tableIdx).offset
                        IF targetCellOffset < 0 THEN ERROR _ERR_CANNOT_CONTINUE
                        IF targetCellOffset >= memoryHandle.SIZE THEN
                            _MEMFREE memoryHandle
                            REDIM _PRESERVE memoryBuffer(0 TO targetCellOffset + 1048576) AS _UNSIGNED _BYTE
                            memoryHandle = _MEM(memoryBuffer())
                            memoryStartOffset = memoryHandle.OFFSET
                        END IF
                        _MEMPUT memoryHandle, memoryStartOffset + targetCellOffset, _MEMGET(memoryHandle, memoryStartOffset + targetCellOffset, _UNSIGNED _BYTE) + loopMultiplier * multiAddTable(tableBase + tableIdx).multiplier AS _UNSIGNED _BYTE
                    NEXT
                END IF

            CASE OP_OUT
                Console_WriteChar _MEMGET(memoryHandle, memoryStartOffset + dataPointer, _UNSIGNED _BYTE)

            CASE OP_IN
                _CONSOLETITLE "[WAITING FOR INPUT] " + sourceName
                _MEMPUT memoryHandle, memoryStartOffset + dataPointer, Console_ReadChar AS _UNSIGNED _BYTE
                _CONSOLETITLE sourceName

        END SELECT
        programPointer = programPointer + 1
    LOOP
    _MEMFREE memoryHandle
END SUB

'$INCLUDE:'include/FS/Pathname.bas'
'$INCLUDE:'include/IO/File.bas'
