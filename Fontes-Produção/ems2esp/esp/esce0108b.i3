/*********************** INICIO INCLUDE ESCE0108B.I3 *****************************/

PROCEDURE pi-gera-temp-erro:
    define input parameter i-coderro  as integer                        no-undo.
    define input parameter c-complem  as character                      no-undo.
    RUN pi-erro-item IN THIS-PROCEDURE (INPUT ITEM.it-codigo, INPUT i-coderro, INPUT c-complem).
END PROCEDURE. /* pi_gera-temp-erro */

PROCEDURE pi-erro-item:
    DEFINE INPUT PARAMETER p-it-codigo    AS CHAR    NO-UNDO.
    DEFINE INPUT PARAMETER p-cod-erro     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-complemento  AS CHAR    NO-UNDO.

    DEFINE VARIABLE lc-c-aux  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lc-c-aux1 AS CHARACTER   NO-UNDO.

    IF p-cod-erro <> 17006 THEN DO:
        FIND FIRST cad-msgs WHERE cad-msgs.cd-msg = p-cod-erro NO-LOCK NO-ERROR. 
        IF AVAIL cad-msgs THEN DO:

            ASSIGN lc-c-aux   = "".
            IF INDEX(p-complemento, "~~") > 0  THEN DO:
                ASSIGN lc-c-aux   = cad-msgs.texto-msg.
                assign lc-c-aux1 = p-complemento + "~~~~~~~~~~~~~~~~":U.
                assign lc-c-aux = substitute(lc-c-aux,
                                              entry(1, lc-c-aux1, "~~":U),
                                              entry(2, lc-c-aux1, "~~":U),
                                              entry(3, lc-c-aux1, "~~":U),
                                              entry(4, lc-c-aux1, "~~":U),
                                              entry(5, lc-c-aux1, "~~":U),
                                              entry(6, lc-c-aux1, "~~":U),
                                              entry(7, lc-c-aux1, "~~":U),
                                              entry(8, lc-c-aux1, "~~":U),
                                              entry(9, lc-c-aux1, "~~":U)).
            END.

            CREATE tt-er-item.
            ASSIGN tt-er-item.it-codigo   = p-it-codigo 
                   tt-er-item.cd-msg      = cad-msgs.cd-msg     
                   tt-er-item.tipo-msg    = cad-msgs.tipo-msg   
                   tt-er-item.cd-msg-subs = cad-msgs.cd-msg-subs
                   tt-er-item.texto-msg   = IF TRIM(lc-c-aux) = "" THEN cad-msgs.texto-msg  ELSE lc-c-aux
                   tt-er-item.help-msg    = IF TRIM(lc-c-aux) = "" THEN cad-msgs.help-msg + p-complemento   ELSE cad-msgs.help-msg
                .
        END.
    END.
    ELSE DO:
        CREATE tt-er-item.
        ASSIGN tt-er-item.it-codigo   = p-it-codigo 
               tt-er-item.cd-msg      = p-cod-erro     
               tt-er-item.tipo-msg    = 1   
               tt-er-item.cd-msg-subs = 0
               tt-er-item.texto-msg   = p-complemento  
               tt-er-item.help-msg    = p-complemento 
            .
    END.
    RETURN "OK".
END PROCEDURE.

/*********************** FIM INCLUDE ESCE0108B.I3 *****************************/
