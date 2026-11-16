'-----------------------------------------------------------------------------------------------------------------------
' Console Input/Output functions
' Copyright (c) 2026 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

$INCLUDEONCE

DECLARE LIBRARY
    FUNCTION Console_ReadChar& ALIAS "::getchar"
    SUB Console_WriteChar ALIAS "::putchar" (BYVAL ch AS LONG)
END DECLARE

SUB Console_WriteLine (text AS STRING)
    DECLARE LIBRARY
        SUB __Console_WriteLine ALIAS "::puts" (text AS STRING)
    END DECLARE

    __Console_WriteLine text + _CHR_NUL
END SUB

SUB Console_Write (text AS STRING)
    DECLARE LIBRARY
        SUB __Console_Write ALIAS "::printf" (text AS STRING)
    END DECLARE

    __Console_Write text + _CHR_NUL
END SUB
