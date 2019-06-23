DEF PARAM BUFFER bf-movto_tit_ap-new FOR movto_tit_ap.
DEF PARAM BUFFER bf-movto_tit_ap-old FOR movto_tit_ap.

DEFINE TEMP-TABLE tt-movto_tit_ap-new NO-UNDO LIKE movto_tit_ap.
DEFINE TEMP-TABLE tt-movto_tit_ap-old NO-UNDO LIKE movto_tit_ap.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO. 

DEFINE VARIABLE cAlteracao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cValor      AS CHARACTER   NO-UNDO.

/* ===> Main Block <=== */
/* IF NOT NEW bf-movto_tit_ap-new THEN DO: */
    FIND FIRST tit_ap NO-LOCK WHERE
               tit_ap.cod_estab     = bf-movto_tit_ap-new.cod_estab
           AND tit_ap.num_id_tit_ap = bf-movto_tit_ap-new.num_id_tit_ap NO-ERROR.

    CREATE es-his-tit-ap.
    ASSIGN es-his-tit-ap.cod-estabel   = tit_ap.cod_estab
           es-his-tit-ap.cod-esp       = tit_ap.cod_espec_docto
           es-his-tit-ap.cod-fornec    = tit_ap.cdn_fornecedor
           es-his-tit-ap.ep-codigo     = tit_ap.cod_empresa
           es-his-tit-ap.nr-docto      = tit_ap.cod_tit_ap
           es-his-tit-ap.parcela       = tit_ap.cod_parcela
           es-his-tit-ap.serie         = tit_ap.cod_ser_docto
           es-his-tit-ap.num-id-mov-ap = bf-movto_tit_ap-new.num_id_movto_tit_ap
           es-his-tit-ap.usuario       = REPLACE (c-seg-usuario,"/","")
           es-his-tit-ap.dt-transacao  = TODAY
           es-his-tit-ap.hr-transacao  = TIME.

    ASSIGN cAlteracao = "".
    BUFFER-COMPARE bf-movto_tit_ap-old TO bf-movto_tit_ap-new SAVE RESULT IN cAlteracao NO-ERROR.

    IF cAlteracao <> "" THEN DO:
        ASSIGN cAlteracao = REPLACE(cAlteracao,","," ").

        EMPTY TEMP-TABLE tt-movto_tit_ap-old.
        CREATE tt-movto_tit_ap-old.
        BUFFER-COPY bf-movto_tit_ap-old TO tt-movto_tit_ap-old.

        EMPTY TEMP-TABLE tt-movto_tit_ap-new.
        CREATE tt-movto_tit_ap-new.
        BUFFER-COPY bf-movto_tit_ap-new TO tt-movto_tit_ap-new.

        ASSIGN es-his-tit-ap.hist-alterado = "CAMPOS ALTERADOS : " + CHR(10).

        DO iCont = 1 TO NUM-ENTRIES (cAlteracao, " "):
            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + ENTRY (iCont, cAlteracao, " ") + " - DE: ".

            RUN pi-Query (INPUT 'tt-movto_tit_ap-old',
                          INPUT ENTRY (iCont, cAlteracao, " "),
                          OUTPUT cValor).

            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + cValor + " PARA: ".

            RUN pi-Query (INPUT 'tt-movto_tit_ap-new',
                          INPUT ENTRY (iCont, cAlteracao, " "),
                          OUTPUT cValor).

            ASSIGN es-his-tit-ap.hist-alterado = es-his-tit-ap.hist-alterado + cValor + CHR(10).
        END.
    END.
/* END. */

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

