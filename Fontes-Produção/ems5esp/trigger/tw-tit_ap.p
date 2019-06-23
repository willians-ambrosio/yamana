DEF PARAM BUFFER bf-tit_ap-new FOR tit_ap.
DEF PARAM BUFFER bf-tit_ap-old FOR tit_ap.

DEFINE TEMP-TABLE tt-tit_ap-new LIKE tit_ap.
DEFINE TEMP-TABLE tt-tit_ap-old LIKE tit_ap.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO. 

DEFINE VARIABLE cAlteracao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cValor      AS CHARACTER   NO-UNDO.

/* ===> Main Block <=== */
IF NOT NEW bf-tit_ap-new THEN DO:
    CREATE es-his-tit-ap.
    ASSIGN es-his-tit-ap.cod-estabel   = bf-tit_ap-new.cod_estab
           es-his-tit-ap.cod-esp       = bf-tit_ap-new.cod_espec_docto
           es-his-tit-ap.cod-fornec    = bf-tit_ap-new.cdn_fornecedor
           es-his-tit-ap.ep-codigo     = bf-tit_ap-new.cod_empresa
           es-his-tit-ap.nr-docto      = bf-tit_ap-new.cod_tit_ap
           es-his-tit-ap.parcela       = bf-tit_ap-new.cod_parcela
           es-his-tit-ap.serie         = bf-tit_ap-new.cod_ser_docto
           es-his-tit-ap.num-id-mov-ap = 0
           es-his-tit-ap.usuario       = REPLACE (c-seg-usuario,"/","")
           es-his-tit-ap.dt-transacao  = TODAY
           es-his-tit-ap.hr-transacao  = TIME.

    ASSIGN cAlteracao = "".
    BUFFER-COMPARE bf-tit_ap-old TO bf-tit_ap-new SAVE RESULT IN cAlteracao NO-ERROR.

    IF cAlteracao <> "" THEN DO:
        ASSIGN cAlteracao = REPLACE(cAlteracao,","," ").

        EMPTY TEMP-TABLE tt-tit_ap-old.
        CREATE tt-tit_ap-old.
        BUFFER-COPY bf-tit_ap-old TO tt-tit_ap-old.

        EMPTY TEMP-TABLE tt-tit_ap-new.
        CREATE tt-tit_ap-new.
        BUFFER-COPY bf-tit_ap-new TO tt-tit_ap-new.

        ASSIGN es-his-tit-ap.hist-alterado = "CAMPOS ALTERADOS : " + CHR(10).

        DO iCont = 1 TO NUM-ENTRIES (cAlteracao, " "):
            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + ENTRY (iCont, cAlteracao, " ") + " - DE: ".

            RUN pi-Query (INPUT 'tt-tit_ap-old',
                          INPUT ENTRY (iCont, cAlteracao, " "),
                          OUTPUT cValor).

            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + cValor + " PARA: ".

            RUN pi-Query (INPUT 'tt-tit_ap-new',
                          INPUT ENTRY (iCont, cAlteracao, " "),
                          OUTPUT cValor).

            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + cValor + CHR(10).
        END.
    END.
END.

RETURN "OK":U.

/* ===> Procedures <=== */

PROCEDURE pi-Query:
    DEFINE INPUT  PARAM pcTable  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAM pcField  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAM pcValor  AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hQuery  AS HANDLE      NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE pcTable.
    CREATE QUERY hQuery.   

    hQuery:SET-BUFFERS(hBuffer).

    hQuery:QUERY-PREPARE("FOR EACH " + pcTable + " FIELDS (" + pcField + ")").

    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().

    REPEAT:
      IF hBuffer:AVAILABLE THEN DO:
          ASSIGN pcValor = STRING (hBuffer:BUFFER-FIELD(pcField):BUFFER-VALUE).

          IF ERROR-STATUS:ERROR THEN DO:
              MESSAGE "Failed" VIEW-AS ALERT-BOX ERROR.
          END.
      END.

      hQuery:GET-NEXT().
      IF hQuery:QUERY-OFF-END THEN LEAVE.
    END.

    DELETE OBJECT hBuffer.
    DELETE OBJECT hQuery.
END PROCEDURE.

