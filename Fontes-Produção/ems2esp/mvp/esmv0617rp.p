&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0617RP 2.06.00.000}  /*** 010006 ***/

/** Defini‡Æo da Temp-table de parƒmetros **/
define temp-table tt-param
        field destino              as integer
        field arquivo              as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field classifica           as integer
        field desc-classifica      as char format "x(40)"
        field desc-considera       as char format "x(40)"
        field i-ordem-ini          as integer
        field i-ordem-fim          as integer
        field i-empresa-ini        as char
        field i-empresa-fim        as char
        field c-equipamento-ini    as character format "x(16)":U 
        field c-equipamento-fim    as character format "x(16)":U     
        FIELD c-especialidade-ini  as character format "x(12)":U 
        FIELD c-especialidade-fim  as character format "x(12)":U 
        field i-tipo-manut-ini     as integer
        field i-tipo-manut-fim     as integer
        FIELD i-planejador-ini     as character format "x(12)":U
        FIELD i-planejador-fim     as character format "x(12)":U
        field c-oficina-ini        as character format "x(8)":U
        field c-oficina-fim        as character format "x(8)":U
        field dat-evento-ini       as date      format "99/99/9999":U
        field dat-evento-fim       as date      format "99/99/9999":U
        field dat-base             as date      format "99/99/9999":U
        field dat-tendencia-ini    as date      format "99/99/9999":U
        field dat-tendencia-fim    as date      format "99/99/9999":U
        FIELD l-naoIniciada        AS LOGICAL
        FIELD l-Terminada          AS LOGICAL
        FIELD l-iniciada           AS LOGICAL
        FIELD l-equipamento        AS LOGICAL
        FIELD l-componente         AS LOGICAL
        FIELD l-naoIniciada2       AS LOGICAL
        FIELD l-iniciada2          AS logical
        field l-save               as logical
        field i-tipo               as integer.

DEFINE TEMP-TABLE tt-tendencia
    FIELD cod-especialid LIKE mmv-tecnico-tarefa-om.cod-especialid 
    FIELD lin-ini        AS INTEGER
    FIELD lin-fim        AS INTEGER
    FIELD col-ini        AS INTEGER
    FIELD col-fim        AS INTEGER.
    
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita as raw.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEFINE VARIABLE chExcel      AS COM-HANDLE               NO-UNDO.
DEFINE VARIABLE chArquivo    AS COM-HANDLE               NO-UNDO.
DEFINE VARIABLE chPlanilha   AS COM-HANDLE               NO-UNDO.
DEFINE VARIABLE hacomp       AS HANDLE                   NO-UNDO.
                                                         
DEFINE VARIABLE linhaTres    AS INTEGER                  NO-UNDO.
DEFINE VARIABLE linhaCinco   AS INTEGER                  NO-UNDO.
DEFINE VARIABLE linhaUltima  AS INTEGER                  NO-UNDO.
                                                         
DEFINE VARIABLE l-verifica   AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE Inicial      AS INTEGER                  NO-UNDO.
DEFINE VARIABLE Final        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iCont        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cDescTar     AS CHAR FORMAT "x(500)"     NO-UNDO.
DEFINE VARIABLE deAux        AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE deAux2       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE iAux3        AS INT                      NO-UNDO.
DEFINE VARIABLE iCapMob      AS INT                      NO-UNDO.
DEFINE VARIABLE deHorasEspec AS DECIMAL                  NO-UNDO.

DEFINE TEMP-TABLE tempo-total 
    FIELD data  AS DATE
    FIELD tempo AS INTEGER.

{include/i-rpvar.i}

&SCOPED-DEFINE xlCenter -4108

DEFINE VARIABLE d-disponivel-disp           AS DECIMAL FORMAT "->>>>>>,>>9.9999"    NO-UNDO.
DEFINE VARIABLE d-saldo-calculado           AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999" NO-UNDO.
DEFINE VARIABLE qt-disponivel               AS DECIMAL FORMAT "->>>>>,>>>,>>9.9999" NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR g-saldo-zerado AS LOGICAL INITIAL NO     NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR g-dt-saldo     LIKE movto-estoq.dt-trans NO-UNDO.
DEFINE VARIABLE d-tot-disponivel AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE d-tot-saldo-calc AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE dt-saldo AS DATE FORMAT "99/99/9999":U NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fn-qtd-data) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-data Procedure 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnQtd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnQtd Procedure 
FUNCTION fnQtd RETURNS DECIMAL
  (pItem AS CHAR, pEstab AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.33
         WIDTH              = 33.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN piPrincipal.

RETURN "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calcExcecao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcExcecao Procedure 
PROCEDURE calcExcecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pDataMov AS DATE    NO-UNDO.
    DEFINE INPUT  PARAMETER pHoras   AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER pTotal   AS DECIMAL NO-UNDO.

    ASSIGN deAux  = 0
           deAux2 = 0
           iAux3  = 0.

    FOR EACH  mmv-exc-esp-data
        WHERE mmv-exc-esp-data.cod-especialid = mmv-cap-esp-turno.cod-especialid
        AND   mmv-exc-esp-data.cod-turno      = mmv-cap-esp-turno.cod-turno
        AND   mmv-exc-esp-data.data           = pDataMov NO-LOCK:

        ASSIGN deAux = (INT(SUBSTRING(mmv-exc-esp-data.hora-inicial,1,2))  * 3600 +
                        INT(SUBSTRING(mmv-exc-esp-data.hora-inicial,4,2))  * 60   +
                        INT(SUBSTRING(mmv-exc-esp-data.hora-inicial,7,2))) / 3600.

        ASSIGN deAux2 = (INT(SUBSTRING(mmv-exc-esp-data.hora-termino,1,2))  * 3600 +     
                         INT(SUBSTRING(mmv-exc-esp-data.hora-termino,4,2))  * 60   +
                         INT(SUBSTRING(mmv-exc-esp-data.hora-termino,4,2))) / 3600.

        ASSIGN iAux3 = iAux3 + ((deAux2 - deAux) * mmv-exc-esp-data.nr-tecnico).
    END.

    ASSIGN pTotal = pHoras - iAux3.

    IF pTotal < 0 THEN
        ASSIGN pTotal = 0.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcHoras) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcHoras Procedure 
PROCEDURE calcHoras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pDataMov AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER iAux3    AS INT  NO-UNDO.

    ASSIGN deAux  = 0
           deAux2 = 0
           iAux3  = 0.

    FOR EACH  data-turno-calen
        WHERE data-turno-calen.cd-calen = mmv-turno-esp.cd-calen
        AND   data-turno-calen.cd-turno = mmv-cap-esp-turno.cod-turno
        AND   data-turno-calen.data     = pDataMov NO-LOCK:

        ASSIGN deAux = (INT(SUBSTRING(data-turno-calen.hora-inicio,1,2))  * 3600 +
                        INT(SUBSTRING(data-turno-calen.hora-inicio,4,2))  * 60   +
                        INT(SUBSTRING(data-turno-calen.hora-inicio,7,2))) / 3600.

        ASSIGN deAux2 = (INT(SUBSTRING(data-turno-calen.hora-termino,1,2))  * 3600 +     
                         INT(SUBSTRING(data-turno-calen.hora-termino,4,2))  * 60   +
                         INT(SUBSTRING(data-turno-calen.hora-termino,4,2))) / 3600.

        ASSIGN iAux3 = iAux3 + (deAux2 - deAux).
    END.

    ASSIGN iAux3 = iAux3 * mmv-cap-esp-turno.nr-tecnico.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piCabecalho) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCabecalho Procedure 
PROCEDURE piCabecalho :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCont      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCabecalho AS CHARACTER NO-UNDO.

    /** Preencher Nome da Empresa **/
    FIND FIRST param-global NO-LOCK NO-ERROR.
    IF AVAILABLE param-global THEN DO:
        FIND FIRST empresa WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR.
        IF AVAILABLE empresa THEN
            ASSIGN cCabecalho = empresa.razao-social.
    END.

    ASSIGN chPlanilha:Range("A1:J2"):MergeCells    = TRUE
           chPlanilha:cells(1,1):VALUE             = "  YAMANA":U + "                                            Backlog de Frotas":U
           chPlanilha:cells(1,1):FONT:Bold         = TRUE
           chPlanilha:cells(1,1):FONT:SIZE         = 14
           chPlanilha:cells(1,1):VerticalAlignment = {&xlCenter}
           chPlanilha:cells(2,12):VALUE            = TODAY
           chPlanilha:cells(2,11):VALUE            = STRING(TIME,"HH:MM:SS")
           chPlanilha:Range("K1:L2"):FONT:Bold     = TRUE
           chPlanilha:Range("K1:L1"):MergeCells    = TRUE
           chPlanilha:Range("K1:L1"):VALUE         = cCabecalho.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piCarregaTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaTable Procedure 
PROCEDURE piCarregaTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
 
    IF CAN-FIND(FIRST mmv-backlog
                WHERE mmv-backlog.periodo = TODAY) THEN DO:
        FOR EACH  mmv-backlog
            WHERE mmv-backlog.periodo = TODAY EXCLUSIVE-LOCK:
            DELETE mmv-backlog.
        END.
    END.

    /**---- Filtra os dados conforme sele‡Æo de especialidade / Ordem -----**/
    FOR EACH  mmv-especialid-func
        WHERE mmv-especialid-func.cod-especialid >= tt-param.c-especialidade-ini
        AND   mmv-especialid-func.cod-especialid <= tt-param.c-especialidade-fim NO-LOCK:

        FOR EACH  mmv-tecnico-tarefa-om
            WHERE mmv-tecnico-tarefa-om.cod-especialid  = mmv-especialid-func.cod-especialid
            AND   mmv-tecnico-tarefa-om.nr-ord-produ   >= tt-param.i-ordem-ini
            AND   mmv-tecnico-tarefa-om.nr-ord-produ   <= tt-param.i-ordem-fim NO-LOCK:

            FOR FIRST mmv-ord-manut
                WHERE mmv-ord-manut.nr-ord-produ   = mmv-tecnico-tarefa-om.nr-ord-produ
                AND   mmv-ord-manut.dat-term       = ?
                AND   mmv-ord-manut.dat-prev-term  < TODAY
                AND   mmv-ord-manut.ep-codigo     >= tt-param.i-empresa-ini
                AND   mmv-ord-manut.ep-codigo     <= tt-param.i-empresa-fim
                AND   mmv-ord-manut.cod-eqpto     >= tt-param.c-equipamento-ini
                AND   mmv-ord-manut.cod-eqpto     <= tt-param.c-equipamento-fim
                AND   mmv-ord-manut.cd-tipo       >= tt-param.i-tipo-manut-ini
                AND   mmv-ord-manut.cd-tipo       <= tt-param.i-tipo-manut-fim
                AND   mmv-ord-manut.cod-plandor   >= tt-param.i-planejador-ini
                AND   mmv-ord-manut.cod-plandor   <= tt-param.i-planejador-fim
                AND   mmv-ord-manut.cod-ofici     >= tt-param.c-oficina-ini
                AND   mmv-ord-manut.cod-ofici     <= tt-param.c-oficina-fim NO-LOCK:

                FOR FIRST mmv-tar-ord-manut
                    WHERE mmv-tar-ord-manut.num-seq = mmv-tecnico-tarefa-om.num-seq NO-LOCK:

                    FOR FIRST mab-eqpto
                        WHERE mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto NO-LOCK:
                    END.

                    /*-------------- valida‡äes do fPage4 ------------------*/
                    IF NOT (tt-param.l-naoIniciada)  AND  mmv-ord-manut.estado      = 1 THEN NEXT.
                    IF NOT (tt-param.l-Iniciada)     AND  mmv-ord-manut.estado      = 6 THEN NEXT.
                    IF NOT (tt-param.l-equipamento)  AND  mmv-ord-manut.idi-tip-ord = 1 THEN NEXT.
                    IF NOT (tt-param.l-componente)   AND  mmv-ord-manut.idi-tip-ord = 2 THEN NEXT.
                    IF NOT (tt-param.l-naoIniciada2) AND  mmv-ord-manut.idi-tip-ord = 1 THEN NEXT.
                    IF NOT (tt-param.l-iniciada2)    AND  mmv-ord-manut.idi-tip-ord = 2 THEN NEXT.

                    IF NOT CAN-FIND(FIRST mmv-backlog
                                    WHERE mmv-backlog.cod-especialid = mmv-especialid-func.cod-especialid
                                    AND   mmv-backlog.periodo        = mmv-ord-manut.dat-prev-term) THEN DO:
                        CREATE mmv-backlog.
                        ASSIGN mmv-backlog.cod-especialid = mmv-especialid-func.cod-especialid
                               mmv-backlog.periodo        = mmv-ord-manut.dat-prev-term
                               mmv-backlog.horas          = mmv-tecnico-tarefa-om.tempo-previsto.
                    END.
                    ELSE DO:
                        FOR FIRST mmv-backlog
                            WHERE mmv-backlog.cod-especialid = mmv-especialid-func.cod-especialid
                            AND   mmv-backlog.periodo        = mmv-ord-manut.dat-prev-term EXCLUSIVE-LOCK:
                            ASSIGN mmv-backlog.horas         = mmv-backlog.horas + mmv-tecnico-tarefa-om.tempo-previsto.
                        END.
                    END.
                END.
            END.
        END.
    END.
    FOR EACH  mab-evento
        WHERE mab-evento.cod-especialid >= tt-param.c-especialidade-ini
        AND   mab-evento.cod-especialid <= tt-param.c-especialidade-fim NO-LOCK:

        FOR FIRST mab-movto-event
            WHERE mab-movto-event.num-docto    = mab-evento.num-docto
            AND   mab-movto-event.dat-final    = ?
            AND   mab-movto-event.ep-codigo   >= tt-param.i-empresa-ini
            AND   mab-movto-event.ep-codigo   <= tt-param.i-empresa-fim
            AND   mab-movto-event.cod-eqpto   >= tt-param.c-equipamento-ini
            AND   mab-movto-event.cod-eqpto   <= tt-param.c-equipamento-fim
            AND   mab-movto-event.cd-tipo     >= tt-param.i-tipo-manut-ini
            AND   mab-movto-event.cd-tipo     <= tt-param.i-tipo-manut-fim
            AND   mab-movto-event.cod-ofici   >= tt-param.c-oficina-ini
            AND   mab-movto-event.cod-ofici   <= tt-param.c-oficina-fim
            AND   mab-movto-event.dat-inicial >= tt-param.dat-evento-ini
            AND   mab-movto-event.dat-inicial <= tt-param.dat-evento-fim NO-LOCK:        
    
            FOR FIRST mab-eqpto
                WHERE mab-eqpto.cod-eqpto = mab-movto-event.cod-eqpto NO-LOCK:
            END.
            IF NOT AVAIL mab-eqpto THEN NEXT.

            IF NOT CAN-FIND(FIRST mmv-backlog
                            WHERE mmv-backlog.cod-especialid = mab-evento.cod-especialid
                            AND   mmv-backlog.periodo        = mab-movto-event.dat-inicial) THEN DO:
                CREATE mmv-backlog.
                ASSIGN mmv-backlog.cod-especialid = mab-evento.cod-especialid
                       mmv-backlog.periodo        = mab-movto-event.dat-inicial
                       mmv-backlog.horas          = mab-evento.horas-espec.
            END.
            ELSE DO:
                FOR FIRST mmv-backlog
                    WHERE mmv-backlog.cod-especialid = mab-evento.cod-especialid
                    AND   mmv-backlog.periodo        = mab-movto-event.dat-inicial EXCLUSIVE-LOCK:
                    ASSIGN mmv-backlog.horas         = mmv-backlog.horas + mab-evento.horas-espec.
                END.
            END.
        END.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piExportaExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExportaExcel Procedure 
PROCEDURE piExportaExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-linha  AS INTEGER NO-UNDO.
DEFINE VARIABLE l-coluna AS INTEGER NO-UNDO.
DEFINE VARIABLE l-first  AS LOGICAL NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET hacomp.
{utp/ut-liter.i Exporta_Excel *}
RUN pi-inicializar IN hacomp (INPUT RETURN-VALUE).

RUN piCabecalho.

ASSIGN l-linha = 3.

{utp/ut-liter.i "Grupo_de_Equipamento" *}
chPlanilha:cells(l-linha,1):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Modelo" *}
chPlanilha:cells(l-linha,2):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Equipamento" *}
chPlanilha:cells(l-linha,3):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Especialidade" *}
chPlanilha:cells(l-linha,4):VALUE  = RETURN-VALUE.    
{utp/ut-liter.i "Dt._Manuten‡Æo" *}
chPlanilha:cells(l-linha,5):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Ordem" *}
chPlanilha:cells(l-linha,6):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Tarefa" *}
chPlanilha:cells(l-linha,7):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Oficina" *}
chPlanilha:cells(l-linha,8):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Nro_Doc_Evento" *}
chPlanilha:cells(l-linha,9):VALUE  = RETURN-VALUE.
{utp/ut-liter.i "Evento" *}
chPlanilha:cells(l-linha,10):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Sub-Sistema" *}
chPlanilha:cells(l-linha,11):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Tp._Ordem" *}
chPlanilha:cells(l-linha,12):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Prev._T‚rmino" *}
chPlanilha:cells(l-linha,13):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Tempo" *}
chPlanilha:cells(l-linha,14):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Capacidade_MOB" *}
chPlanilha:cells(l-linha,15):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Material" *}
chPlanilha:cells(l-linha,16):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Descri‡Æo" *}
chPlanilha:cells(l-linha,17):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Quantidade" *}
chPlanilha:cells(l-linha,18):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Saldo em Estoque" *}
chPlanilha:cells(l-linha,19):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Qtd Saldo em Estoque" *}
chPlanilha:cells(l-linha,20):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Status" *}
chPlanilha:cells(l-linha,21):VALUE = RETURN-VALUE.
{utp/ut-liter.i "Criticidade " *}
chPlanilha:cells(l-linha,22):VALUE = RETURN-VALUE.
ASSIGN l-linha = l-linha + 1.

/** Negrito **/
chPlanilha:Range("A3:U3"):FONT:Bold = TRUE.

/**---- Filtra os dados conforme sele‡Æo de especialidade / Ordem -----**/
FOR EACH  mmv-especialid-func
    WHERE mmv-especialid-func.cod-especialid >= tt-param.c-especialidade-ini
    AND   mmv-especialid-func.cod-especialid <= tt-param.c-especialidade-fim NO-LOCK:

    FOR EACH  mmv-tecnico-tarefa-om
        WHERE mmv-tecnico-tarefa-om.cod-especialid  = mmv-especialid-func.cod-especialid
        AND   mmv-tecnico-tarefa-om.nr-ord-produ   >= tt-param.i-ordem-ini
        AND   mmv-tecnico-tarefa-om.nr-ord-produ   <= tt-param.i-ordem-fim NO-LOCK
        BREAK BY mmv-tecnico-tarefa-om.cod-especialid:

        FOR FIRST mmv-ord-manut
            WHERE mmv-ord-manut.nr-ord-produ   = mmv-tecnico-tarefa-om.nr-ord-produ
            AND   mmv-ord-manut.dat-term       = ?
            AND   mmv-ord-manut.dat-prev-term  < tt-param.dat-base
            AND   mmv-ord-manut.ep-codigo     >= tt-param.i-empresa-ini
            AND   mmv-ord-manut.ep-codigo     <= tt-param.i-empresa-fim
            AND   mmv-ord-manut.cod-eqpto     >= tt-param.c-equipamento-ini
            AND   mmv-ord-manut.cod-eqpto     <= tt-param.c-equipamento-fim
            AND   mmv-ord-manut.cd-tipo       >= tt-param.i-tipo-manut-ini
            AND   mmv-ord-manut.cd-tipo       <= tt-param.i-tipo-manut-fim
            AND   mmv-ord-manut.cod-plandor   >= tt-param.i-planejador-ini
            AND   mmv-ord-manut.cod-plandor   <= tt-param.i-planejador-fim
            AND   mmv-ord-manut.cod-ofici     >= tt-param.c-oficina-ini
            AND   mmv-ord-manut.cod-ofici     <= tt-param.c-oficina-fim NO-LOCK:

            /*-------------- valida‡äes do fPage 4 ------------------*/
            IF NOT (tt-param.l-naoIniciada)  AND  mmv-ord-manut.estado      = 1 THEN NEXT.
            IF NOT (tt-param.l-Iniciada)     AND  mmv-ord-manut.estado      = 6 THEN NEXT.
            IF NOT (tt-param.l-equipamento)  AND  mmv-ord-manut.idi-tip-ord = 1 THEN NEXT.
            IF NOT (tt-param.l-componente)   AND  mmv-ord-manut.idi-tip-ord = 2 THEN NEXT.
            IF NOT (tt-param.l-naoIniciada2) AND  mmv-ord-manut.idi-tip-ord = 1 THEN NEXT.
            IF NOT (tt-param.l-iniciada2)    AND  mmv-ord-manut.idi-tip-ord = 2 THEN NEXT.

            FOR FIRST mmv-tar-ord-manut
                WHERE mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                AND   mmv-tar-ord-manut.num-seq      = mmv-tecnico-tarefa-om.num-seq NO-LOCK:

                FOR FIRST mab-eqpto
                    WHERE mab-eqpto.ep-codigo = mmv-ord-manut.ep-codigo
                    AND   mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto NO-LOCK:
                END.
                IF NOT AVAIL mab-eqpto THEN NEXT.

                FOR FIRST mmv-ofici
                    WHERE mmv-ofici.cod-ofici = mmv-ord-manut.cod-ofici NO-LOCK:
                END.
                FOR FIRST mab-model
                    WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
                END.
                FOR FIRST mab-grp-eqpto
                    WHERE mab-grp-eqpto.cod-grp-eqpto = mab-eqpto.cod-grp-eqpto NO-LOCK:
                END.
                FOR FIRST mmv-equipto-esp
                    WHERE mmv-equipto-esp.cod-equipto = mab-eqpto.cod-eqpto NO-LOCK:
                END.
                FOR FIRST mab-sub-sist
                    WHERE mab-sub-sist.cod-sub-sist = mmv-tar-ord-manut.cod-sub-sist NO-LOCK:
                END.
                FOR FIRST mab-event FIELDS(cod-evento des-evento)
                    WHERE mab-event.cod-evento = mmv-tar-ord-manut.cod-evento NO-LOCK:
                END.
                ASSIGN cDescTar = STRING(mmv-tecnico-tarefa-om.num-seq) + " - ":U + REPLACE(SUBSTRING(mmv-tar-ord-manut.dsl-obs,1,400),CHR(10)," ":U).

                FOR FIRST mmv-ord-status
                    WHERE mmv-ord-status.nr-ord-produ = mmv-ord-manut.nr-ord-produ NO-LOCK:
                END.
                FOR LAST  mmv-cap-esp-turno
                    WHERE mmv-cap-esp-turno.cod-especialid  = mmv-especialid-func.cod-especialid
                    AND   mmv-cap-esp-turno.dt-efetivacao  <= TODAY NO-LOCK:
                END.
                IF AVAIL mmv-cap-esp-turno THEN DO:
                    FOR FIRST mmv-turno-esp
                        WHERE mmv-turno-esp.cod-especialid = mmv-cap-esp-turno.cod-especialid NO-LOCK:
                    END.
                    RUN calcHoras   IN THIS-PROCEDURE (INPUT mmv-ord-manut.dat-entr,
                                                       OUTPUT iAux3).
                    RUN calcExcecao IN THIS-PROCEDURE (INPUT mmv-ord-manut.dat-entr,
                                                       INPUT iAux3,
                                                       OUTPUT iCapMob).
                END.

                chPlanilha:cells(l-linha,1):VALUE  = mab-eqpto.cod-grp-eqpto + " - ":U + IF AVAIL mab-grp-eqpto THEN mab-grp-eqpto.des-grp-eqpto ELSE "":U.
                chPlanilha:cells(l-linha,2):VALUE  = mab-eqpto.cod-model     + " - ":U + IF AVAIL mab-model     THEN mab-model.des-model         ELSE "":U.
                chPlanilha:cells(l-linha,3):VALUE  = mmv-ord-manut.cod-eqpto.
                chPlanilha:cells(l-linha,4):VALUE  = mmv-especialid-func.cod-especialid + " - ":U + mmv-especialid-func.descricao.
                chPlanilha:cells(l-linha,5):VALUE  = mmv-ord-manut.dat-entr.
                chPlanilha:cells(l-linha,6):VALUE  = mmv-tecnico-tarefa-om.nr-ord-produ.
                chPlanilha:cells(l-linha,7):VALUE  = cDescTar.
                chPlanilha:cells(l-linha,8):VALUE  = mmv-ord-manut.cod-ofici + " - ":U + IF AVAIL mmv-ofici THEN mmv-ofici.des-ofici ELSE "":U.
                chPlanilha:cells(l-linha,9):VALUE  = "":U.
                chPlanilha:cells(l-linha,10):VALUE = mmv-tar-ord-manut.cod-evento   + " - ":U + IF AVAIL mab-event    THEN mab-event.des-evento      ELSE "":U.
                chPlanilha:cells(l-linha,11):VALUE = mmv-tar-ord-manut.cod-sub-sist + " - ":U + IF AVAIL mab-sub-sist THEN mab-sub-sist.des-sub-sist ELSE "":U.
                chPlanilha:cells(l-linha,12):VALUE = {frinc/i00fr072.i 4 mmv-ord-manut.idi-tip-ord}.
                chPlanilha:cells(l-linha,13):VALUE = mmv-ord-manut.dat-prev-term.
                chPlanilha:cells(l-linha,14):VALUE = mmv-tecnico-tarefa-om.tempo-previsto.
                chPlanilha:cells(l-linha,15):VALUE = IF AVAIL mmv-cap-esp-turno THEN STRING(iCapMob) ELSE "":U.
                chPlanilha:cells(l-linha,21):VALUE = IF AVAIL mmv-ord-status    THEN {ydminc/i00ydm001.i 04 mmv-ord-status.idi-status-ord} ELSE {ydminc/i00ydm001.i 04 1}.
                IF AVAIL mmv-equipto-esp THEN
                    CASE mmv-equipto-esp.critic:
                        WHEN 1 THEN
                            chPlanilha:cells(l-linha,22):VALUE = "A":U.
                        WHEN 2 THEN
                            chPlanilha:cells(l-linha,22):VALUE = "B":U.
                        WHEN 3 THEN
                            chPlanilha:cells(l-linha,22):VALUE = "C":U.
                        OTHERWISE
                            chPlanilha:cells(l-linha,22):VALUE = "C":U.
                    END CASE.
                ELSE
                    chPlanilha:cells(l-linha,22):VALUE = "C":U.

                ASSIGN l-linha = l-linha + 1.
            END.
        END.
    END.
END.
FOR EACH  mab-evento
    WHERE mab-evento.cod-especialid >= tt-param.c-especialidade-ini
    AND   mab-evento.cod-especialid <= tt-param.c-especialidade-fim NO-LOCK:

    FOR FIRST mab-movto-event
        WHERE mab-movto-event.num-docto    = mab-evento.num-docto
        AND   mab-movto-event.dat-final    = ?
        AND   mab-movto-event.ep-codigo   >= tt-param.i-empresa-ini
        AND   mab-movto-event.ep-codigo   <= tt-param.i-empresa-fim
        AND   mab-movto-event.cod-eqpto   >= tt-param.c-equipamento-ini
        AND   mab-movto-event.cod-eqpto   <= tt-param.c-equipamento-fim
        AND   mab-movto-event.cd-tipo     >= tt-param.i-tipo-manut-ini
        AND   mab-movto-event.cd-tipo     <= tt-param.i-tipo-manut-fim
        AND   mab-movto-event.cod-ofici   >= tt-param.c-oficina-ini
        AND   mab-movto-event.cod-ofici   <= tt-param.c-oficina-fim
        AND   mab-movto-event.dat-inicial >= tt-param.dat-evento-ini
        AND   mab-movto-event.dat-inicial <= tt-param.dat-evento-fim NO-LOCK:        

        FOR FIRST mab-eqpto
            WHERE mab-eqpto.ep-codigo = mab-movto-event.ep-codigo
            AND   mab-eqpto.cod-eqpto = mab-movto-event.cod-eqpto NO-LOCK:
        END.
        IF NOT AVAIL mab-eqpto THEN NEXT.

        FOR FIRST mmv-ofici
            WHERE mmv-ofici.cod-ofici = mab-movto-event.cod-ofici NO-LOCK:
        END.
        FOR FIRST mab-model
            WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
        END.
        FOR FIRST mab-grp-eqpto
            WHERE mab-grp-eqpto.cod-grp-eqpto = mab-eqpto.cod-grp-eqpto NO-LOCK:
        END.
        FOR FIRST mab-sub-sist
            WHERE mab-sub-sist.cod-sub-sist = mab-movto-event.cod-sub-sist NO-LOCK:
        END.
        FOR FIRST mmv-equipto-esp
            WHERE mmv-equipto-esp.cod-equipto = mab-eqpto.cod-eqpto NO-LOCK:
        END.
        FOR FIRST mab-event
            WHERE mab-event.cod-evento = mab-movto-event.cod-evento NO-LOCK:
        END.
        FOR FIRST mab-event-status
            WHERE mab-event-status.num-docto = mab-movto-event.num-docto NO-LOCK:
        END.
        FOR FIRST mmv-especialid-func
            WHERE mmv-especialid-func.cod-especialid = mab-evento.cod-especialid NO-LOCK:
        END.

        ASSIGN iCont = IF AVAIL mab-event-status THEN mab-event-status.idi-status-ord ELSE 1.
        IF iCont = 0 THEN
            ASSIGN iCont = 1.
        
        FOR LAST  mmv-cap-esp-turno
            WHERE mmv-cap-esp-turno.cod-especialid  = mab-evento.cod-especialid
            AND   mmv-cap-esp-turno.dt-efetivacao  <= TODAY NO-LOCK:
        END.
        IF AVAIL mmv-cap-esp-turno THEN DO:
            FOR FIRST mmv-turno-esp
                WHERE mmv-turno-esp.cod-especialid = mmv-cap-esp-turno.cod-especialid NO-LOCK:
            END.
            RUN calcHoras   IN THIS-PROCEDURE (INPUT mab-movto-event.dat-inicial,
                                               OUTPUT iAux3).
            RUN calcExcecao IN THIS-PROCEDURE (INPUT mab-movto-event.dat-inicial,
                                               INPUT iAux3,
                                               OUTPUT iCapMob).
        END.

        ASSIGN l-first = YES.
        /** Reservas de Materiais **/
        FOR EACH mab-evento-mat
            WHERE mab-evento-mat.num-docto = mab-movto-event.num-docto NO-LOCK:

            chPlanilha:cells(l-linha,1):VALUE  = mab-eqpto.cod-grp-eqpto + " - ":U + IF AVAIL mab-grp-eqpto THEN mab-grp-eqpto.des-grp-eqpto ELSE "":U.
            chPlanilha:cells(l-linha,2):VALUE  = mab-eqpto.cod-model     + " - ":U + IF AVAIL mab-model     THEN mab-model.des-model         ELSE "":U.
            chPlanilha:cells(l-linha,3):VALUE  = mab-eqpto.cod-eqpto.
            chPlanilha:cells(l-linha,4):VALUE  = mab-evento.cod-especialid + " - ":U + IF AVAIL mmv-especialid-func THEN mmv-especialid-func.descricao ELSE "":U.
            chPlanilha:cells(l-linha,5):VALUE  = mab-movto-event.dat-inicial.
            chPlanilha:cells(l-linha,6):VALUE  = "":U.
            chPlanilha:cells(l-linha,7):VALUE  = "":U.
            chPlanilha:cells(l-linha,8):VALUE  = mab-movto-event.cod-ofici    + " - ":U + IF AVAIL mmv-ofici    THEN mmv-ofici.des-ofici       ELSE "":U.
            chPlanilha:cells(l-linha,9):VALUE  = STRING(mab-movto-event.num-docto) + " - ":U + REPLACE(SUBSTRING(mab-movto-event.dsl-obs,1,400),CHR(10)," ":U).
            chPlanilha:cells(l-linha,10):VALUE = mab-movto-event.cod-evento   + " - ":U + IF AVAIL mab-event    THEN mab-event.des-evento      ELSE "":U.
            chPlanilha:cells(l-linha,11):VALUE = mab-movto-event.cod-sub-sist + " - ":U + IF AVAIL mab-sub-sist THEN mab-sub-sist.des-sub-sist ELSE "":U.
            chPlanilha:cells(l-linha,12):VALUE = "":U.
            chPlanilha:cells(l-linha,13):VALUE = "":U.

            /** Exibi Tempo e Capacidade somente para o primeiro Material do Evento **/
            IF l-first THEN DO:
                /** Criada essa variavel pois o codigo "IF AVAIL mab-evento THEN mab-evento.horas-espec" 
                atribuido direto a celula gera erro de tipo de dado, pois as horas sao inteiras e ‚ necessaria ser informada assim. **/
                ASSIGN deHorasEspec = IF AVAIL mab-evento THEN mab-evento.horas-espec ELSE 0.
                chPlanilha:cells(l-linha,14):VALUE = deHorasEspec.
                chPlanilha:cells(l-linha,15):VALUE = IF AVAIL mmv-cap-esp-turno THEN STRING(iCapMob) ELSE "":U.
                ASSIGN l-first = NO.
            END.

            /** Reservas de Materiais **/
            chPlanilha:cells(l-linha,16):VALUE = IF AVAIL mab-evento-mat   THEN mab-evento-mat.it-codigo               ELSE "":U.
            chPlanilha:cells(l-linha,17):VALUE = IF AVAIL mab-evento-mat   THEN mab-evento-mat.descricao               ELSE "":U.
            chPlanilha:cells(l-linha,18):VALUE = IF AVAIL mab-evento-mat   THEN STRING(mab-evento-mat.quantidade)      ELSE "":U. 
            chPlanilha:cells(l-linha,19):VALUE = IF AVAIL mab-evento-mat   THEN STRING(mab-evento-mat.saldo,"Sim/NÆo") ELSE "":U. 
            IF AVAIL mab-evento-mat THEN chPlanilha:cells(l-linha,20):VALUE = fnQtd(mab-evento-mat.it-codigo,mab-eqpto.cod-estabel). ELSE
            chPlanilha:cells(l-linha,20):VALUE = "":U.
            chPlanilha:cells(l-linha,21):VALUE = IF AVAIL mab-event-status THEN {ydminc/i00ydm001.i 04 iCont}          ELSE {ydminc/i00ydm001.i 04 1}.                
            IF AVAIL mmv-equipto-esp THEN
                CASE mmv-equipto-esp.critic:
                    WHEN 1 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "A":U.
                    WHEN 2 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "B":U.
                    WHEN 3 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "C":U.
                    OTHERWISE
                        chPlanilha:cells(l-linha,22):VALUE = "C":U.
                END CASE.
            ELSE
                chPlanilha:cells(l-linha,22):VALUE = "C":U.

            ASSIGN l-linha = l-linha + 1.
        END.

        /** Se nao encontrar nenhuma Reserva de Material **/
        IF NOT CAN-FIND(FIRST mab-evento-mat
                        WHERE mab-evento-mat.num-docto = mab-movto-event.num-docto NO-LOCK) THEN DO:

            /** Criada essa variavel pois o codigo "IF AVAIL mab-evento THEN mab-evento.horas-espec" 
                atribuido direto a celula gera erro de tipo de dado, pois as horas sao inteiras e ‚ necessaria ser informada assim. **/
            ASSIGN deHorasEspec = IF AVAIL mab-evento THEN mab-evento.horas-espec ELSE 0.

            chPlanilha:cells(l-linha,1):VALUE  = mab-eqpto.cod-grp-eqpto + " - ":U + IF AVAIL mab-grp-eqpto THEN mab-grp-eqpto.des-grp-eqpto ELSE "":U.
            chPlanilha:cells(l-linha,2):VALUE  = mab-eqpto.cod-model     + " - ":U + IF AVAIL mab-model     THEN mab-model.des-model         ELSE "":U. 
            chPlanilha:cells(l-linha,3):VALUE  = mab-eqpto.cod-eqpto.
            chPlanilha:cells(l-linha,4):VALUE  = mab-evento.cod-especialid + " - ":U + IF AVAIL mmv-especialid-func THEN mmv-especialid-func.descricao ELSE "":U.
            chPlanilha:cells(l-linha,5):VALUE  = mab-movto-event.dat-inicial.
            chPlanilha:cells(l-linha,6):VALUE  = "":U.
            chPlanilha:cells(l-linha,7):VALUE  = "":U.
            chPlanilha:cells(l-linha,8):VALUE  = mab-movto-event.cod-ofici    + " - ":U + IF AVAIL mmv-ofici    THEN mmv-ofici.des-ofici       ELSE "":U.            
            chPlanilha:cells(l-linha,9):VALUE  = STRING(mab-movto-event.num-docto) + " - ":U + REPLACE(SUBSTRING(mab-movto-event.dsl-obs,1,400),CHR(10)," ":U).
            chPlanilha:cells(l-linha,10):VALUE = mab-movto-event.cod-evento   + " - ":U + IF AVAIL mab-event    THEN mab-event.des-evento      ELSE "":U.
            chPlanilha:cells(l-linha,11):VALUE = mab-movto-event.cod-sub-sist + " - ":U + IF AVAIL mab-sub-sist THEN mab-sub-sist.des-sub-sist ELSE "":U.
            chPlanilha:cells(l-linha,12):VALUE = "":U.
            chPlanilha:cells(l-linha,13):VALUE = "":U.
            chPlanilha:cells(l-linha,14):VALUE = deHorasEspec.
            chPlanilha:cells(l-linha,15):VALUE = IF AVAIL mmv-cap-esp-turno THEN STRING(iCapMob)               ELSE "":U.            
            chPlanilha:cells(l-linha,21):VALUE = IF AVAIL mab-event-status  THEN {ydminc/i00ydm001.i 04 iCont} ELSE {ydminc/i00ydm001.i 04 1}.

            IF AVAIL mmv-equipto-esp THEN
                CASE mmv-equipto-esp.critic:
                    WHEN 1 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "A":U.
                    WHEN 2 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "B":U.
                    WHEN 3 THEN
                        chPlanilha:cells(l-linha,22):VALUE = "C":U.
                    OTHERWISE
                        chPlanilha:cells(l-linha,22):VALUE = "C":U.
                 END CASE.
            ELSE
                chPlanilha:cells(l-linha,22):VALUE = "C":U.

            ASSIGN l-linha = l-linha + 1.
        END.            
    END.
END.

IF VALID-HANDLE(hAcomp) THEN
    RUN pi-finalizar IN hAcomp.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piFimExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFimExcel Procedure 
PROCEDURE piFimExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    chPlanilha:Range("A1:IV635"):SELECT.
    chExcel:SELECTION:EntireColumn:Autofit.
    chExcel:SELECTION:HorizontalAlignment = -4108.

    chExcel:Range("A1"):SELECT.
    chExcel:CURSOR =-4143.
    chExcel:ScreenUpdating = TRUE.
    chExcel:WindowState = -4137.

    /** Diminui tamanho da coluna "Nro_Doc_Evento" **/
    chExcel:COLUMNS("I:I"):ColumnWidth = 50.43.
    /** Diminui tamanho da coluna "Tarefa" **/
    chExcel:COLUMNS("G:G"):ColumnWidth = 50.43.

    /** Abre o excel com o arquivo **/
    chExcel:VISIBLE = TRUE.

    /** Libera da mem¢ria e fecha a aplica‡Æo do excell **/
     IF VALID-HANDLE(chPlanilha) THEN DO:
         RELEASE OBJECT chPlanilha.
     END.

     IF VALID-HANDLE(chArquivo) THEN DO:
         RELEASE OBJECT chArquivo.
     END.

     /** Libera da mem¢ria e fecha a aplica‡Æo do excell **/
     IF VALID-HANDLE(chExcel) THEN DO:
         RELEASE OBJECT chExcel.
     END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piInicioExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piInicioExcel Procedure 
PROCEDURE piInicioExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE "Excel.Application" chExcel NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        CREATE "Excel.Application" chExcel.
    END.

    /* Inicia o excel com 1 planilha */
    chExcel:SheetsInNewWorkBook = 1.

    ASSIGN chArquivo  = chExcel:WorkBooks:ADD.

    /* Esconde as linhas de grade */
    chExcel:ActiveWindow:DisplayGridLines = TRUE.

    ASSIGN chPlanilha = chArquivo:Sheets:ITEM(1).
    chPlanilha:Activate().

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piPrincipal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPrincipal Procedure 
PROCEDURE piPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     piPrincipal
  Parameters:  <none>
  Notes:       Corpo princiapl da aplica‡Æo
------------------------------------------------------------------------------*/
    IF tt-param.i-tipo = 1 THEN DO:
        RUN piInicioExcel.

        RUN piExportaExcel.

        RUN piFimExcel.

        IF tt-param.l-save THEN
            RUN piCarregaTable.
    END.
    ELSE DO:
        RUN piTendencias.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piTendencias) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTendencias Procedure 
PROCEDURE piTendencias :
/*------------------------------------------------------------------------------
  Purpose:     piExcel
  Parameters:  <none>
  Notes:       Exporta os dados do programa para o Excel
------------------------------------------------------------------------------*/
    /** Variÿveis de arquivos **/
    DEFINE VARIABLE c-arq-mod           AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-caminho           AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-arquivo           AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-novo-arquivo      AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-novo-arquivo-resp AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE i-linha-aux         AS INTEGER              NO-UNDO.
    DEFINE VARIABLE i-col-aux           AS INTEGER              NO-UNDO.
    /** Variÿveis para Excel **/
    DEFINE VARIABLE chPlanilha          AS COM-HANDLE           NO-UNDO.
    DEFINE VARIABLE chPlanilha2         AS COM-HANDLE           NO-UNDO.
    DEFINE VARIABLE ch-Arquivo          AS COM-HANDLE           NO-UNDO.
    DEFINE VARIABLE ch-excel            AS COM-HANDLE           NO-UNDO.
    DEFINE VARIABLE c-alfabeto          AS CHAR INIT "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z" NO-UNDO.
    DEFINE VARIABLE h-prog              AS HANDLE               NO-UNDO.
    DEFINE VARIABLE i-alf               AS INTEGER              NO-UNDO.
    DEFINE VARIABLE i-alf2              AS INTEGER              NO-UNDO.
    DEFINE VARIABLE c-range             AS CHAR                 NO-UNDO.
    DEFINE VARIABLE i-linhagraf         AS INTEGER              NO-UNDO.

    REPEAT i-alf = 1 TO 26:
        REPEAT i-alf2 = 1 TO 26:
            ASSIGN c-alfabeto = c-alfabeto + "," + ENTRY(i-alf,c-alfabeto,",") + ENTRY(i-alf2,c-alfabeto,",").
        END.
    END.

    RUN utp/ut-acomp.p PERSISTENT SET h-prog.
    {utp/ut-liter.i Exportando_Excel *}
    RUN pi-inicializar IN h-prog (INPUT RETURN-VALUE).

    /** Salva o arquivo temporÿrio e nome do arquivo modelo 
    Valida»’o da existencia do arquivo modelo ² feita no .w  **/
    ASSIGN c-caminho = SESSION:TEMP-DIRECTORY
           c-arq-mod = "mvp~\esmv0617.xls"
           c-novo-arquivo = SESSION:TEMP-DIRECTORY + "TEND_YAMANA-Frotas.xls".

    /*** Busca planilha padr’o para copiar formata»’o ****/
    ASSIGN c-novo-arquivo-resp = SEARCH(c-arq-mod).
    IF c-novo-arquivo-resp <> ? THEN DO:
       OS-COPY value(c-novo-arquivo-resp) VALUE(c-novo-arquivo).
    END.
    ELSE DO:
       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 1332  ,
                          INPUT c-arq-mod).
       RETURN "NOK":U.
    END.

    /* create a new Excel Application object */
    CREATE "Excel.Application" ch-excel.

    /* launch Excel so it is visible to the user */
    ch-excel:VISIBLE = FALSE.

    /* create a new Workbook */
    ch-Arquivo = ch-excel:Workbooks:OPEN(c-novo-arquivo).

    chPlanilha  = ch-excel:Sheets:ITEM(2).
    chPlanilha2 = ch-excel:Sheets:ITEM(1).

    ASSIGN i-linha-aux = 0
           i-col-aux   = 0.

    FOR EACH mmv-backlog NO-LOCK
        BREAK BY mmv-backlog.cod-especialid
              BY mmv-backlog.periodo:

        ASSIGN i-col-aux = i-col-aux + 1.
        IF FIRST-OF(mmv-backlog.cod-especialid) THEN DO:
            ASSIGN i-linha-aux = i-linha-aux + 3
                   i-col-aux   =             + 1.

            {utp/ut-liter.i "Especialidade" *}
            ASSIGN chPlanilha:cells(i-linha-aux,i-col-aux):VALUE = RETURN-VALUE + " ":U + mmv-backlog.cod-especialid.
        END.

        ASSIGN chPlanilha:cells(i-linha-aux + 1,i-col-aux):VALUE = STRING(mmv-backlog.periodo,"99/99/99")
               chPlanilha:cells(i-linha-aux + 2,i-col-aux):VALUE = mmv-backlog.horas.

        IF NOT CAN-FIND(FIRST tt-tendencia
                        WHERE tt-tendencia.cod-especialid = mmv-backlog.cod-especialid) THEN DO:
            CREATE tt-tendencia.
            ASSIGN tt-tendencia.cod-especialid = mmv-backlog.cod-especialid
                   tt-tendencia.col-ini = i-col-aux
                   tt-tendencia.col-fim = i-col-aux
                   tt-tendencia.lin-ini = i-linha-aux + 1
                   tt-tendencia.lin-fim = i-linha-aux + 2.
        END.
        ELSE DO:
            FOR FIRST tt-tendencia
                WHERE tt-tendencia.cod-especialid = mmv-backlog.cod-especialid:
                ASSIGN tt-tendencia.col-fim = i-col-aux
                       tt-tendencia.lin-fim = i-linha-aux + 2.
            END.
        END.
    END.

    ASSIGN i-linhagraf = 18.
    FOR EACH mmv-backlog NO-LOCK
        BREAK BY mmv-backlog.cod-especialid:

        IF FIRST-OF(mmv-backlog.cod-especialid) THEN DO:
            FOR FIRST tt-tendencia
                WHERE tt-tendencia.cod-especialid = mmv-backlog.cod-especialid:
            END.

            ch-excel:Charts:ADD().
            ch-excel:ActiveChart:Location(2,"Plan1").
            ch-excel:ActiveChart:ChartType = 51.
            ch-excel:ActiveChart:SetSourceData(chPlanilha:Range(ENTRY(tt-tendencia.col-ini,c-alfabeto,",") + string(tt-tendencia.lin-ini) + ":" + ENTRY(tt-tendencia.col-fim,c-alfabeto,",") + STRING(tt-tendencia.lin-fim))).
            ch-excel:ActiveChart:HasTitle = TRUE.
            {utp/ut-liter.i "Especialidade" *}
            ch-excel:ActiveChart:ChartTitle:Characters:TEXT = RETURN-VALUE + " ":U + tt-tendencia.cod-especialid.
            ch-excel:ActiveChart:HasLegend = FALSE.

            ASSIGN i-linhagraf = i-linhagraf + 29.
            chPlanilha2:cells(i-linhagraf,1):SELECT.
            /* Parƒmetros do SmallScroll (down,up,right,left)
            So funciona no office 2003 */
            /*ch-excel:ActiveWindow:SmallScroll(24,0,0,0).*/           
        END.
    END.

    IF c-range = "" THEN
        ASSIGN c-range = ENTRY(1,c-alfabeto,",") + ":" + ENTRY(256,c-alfabeto,",").
    chPlanilha:COLUMNS(c-range):EntireColumn:AutoFit NO-ERROR.

    /** Seleciona a pasta planilha para exibir para a o usu rio**/
    chPlanilha = ch-excel:Sheets:ITEM(1).

    /** Elimina linhas 1 a 7 **/
    chPlanilha:Rows("1:5"):SELECT.
    ch-excel:SELECTION:DELETE().

    /** Mescla celulas do titulo **/
    chPlanilha:Range("G1:N1"):SELECT.
    ch-excel:SELECTION:MergeCells = TRUE.
    ch-excel:SELECTION:FONT:Bold  = TRUE. /** Negrito **/
    ch-excel:SELECTION:FONT:SIZE = 18.    /** Tamanho Font **/
    chPlanilha:Range("G1:N1"):HorizontalAlignment = -4108.
    chPlanilha:Range("G1:N1"):VerticalAlignment   = -4108.
    {utp/ut-liter.i "An lise_de_Tendˆncia_Especialidade" *}
    chPlanilha:cells(1,7):VALUE = RETURN-VALUE.
    
    RUN pi-finalizar IN h-prog.

    /** Deixa vis¡vel para o usu rio **/
    ch-excel:VISIBLE = TRUE. 
    
    RELEASE OBJECT chPlanilha NO-ERROR.
    RELEASE OBJECT ch-Arquivo NO-ERROR.
    RELEASE OBJECT ch-excel   NO-ERROR.        

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fn-qtd-data) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-data Procedure 
FUNCTION fn-qtd-data RETURNS DECIMAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

ASSIGN d-tot-disponivel = 0
       d-tot-saldo-calc = 0
       dt-saldo = g-dt-saldo.
ASSIGN d-tot-disponivel  = d-tot-disponivel + (saldo-estoq.qtidade-atu - saldo-estoq.qt-alocada -
                                               saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped)
       d-saldo-calculado = saldo-estoq.qtidade-atu.

IF ITEM.tipo-con-est = 1
    OR  g-dt-saldo = TODAY /* se for a data que vem padrÆo (today) nao ira lcoalizar nenhum movimento
                              neste caso o melhor indice e' por data mesmo que o controle seja por lote */
    THEN
        OPEN QUERY qr-movto
        FOR EACH movto-estoq USE-INDEX item-data WHERE
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    AND
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    AND
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  AND
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    AND
                             movto-estoq.lote        = saldo-estoq.lote         AND
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  AND
                             movto-estoq.esp-docto  <> 37                       AND
                             movto-estoq.dt-trans    > g-dt-saldo NO-LOCK.
    ELSE
        OPEN QUERY qr-movto
        FOR EACH movto-estoq USE-INDEX item-estab WHERE
                             movto-estoq.it-codigo   = saldo-estoq.it-codigo    AND
                             movto-estoq.cod-refer   = saldo-estoq.cod-refer    AND
                             movto-estoq.cod-estabel = saldo-estoq.cod-estabel  AND
                             movto-estoq.cod-depos   = saldo-estoq.cod-depos    AND
                             movto-estoq.lote        = saldo-estoq.lote         AND
                             movto-estoq.cod-localiz = saldo-estoq.cod-localiz  AND
                             movto-estoq.esp-docto  <> 37                       AND
                             movto-estoq.dt-trans    > g-dt-saldo NO-LOCK.


GET FIRST qr-movto.
DO WHILE AVAIL (movto-estoq):
    IF movto-estoq.tipo-trans = 1 THEN
        ASSIGN d-saldo-calculado = d-saldo-calculado - movto-estoq.quantidade.
    ELSE
        ASSIGN d-saldo-calculado = d-saldo-calculado + movto-estoq.quantidade.
    GET NEXT qr-movto.
END. /* while */ 

  RETURN d-saldo-calculado.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnQtd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnQtd Procedure 
FUNCTION fnQtd RETURNS DECIMAL
  (pItem AS CHAR, pEstab AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    ASSIGN d-disponivel-disp = 0.

    FOR FIRST ITEM
        WHERE ITEM.it-codigo = pItem NO-LOCK:
        FOR EACH  saldo-estoq OF ITEM
            WHERE saldo-estoq.cod-estabel = pEstab NO-LOCK:
            ASSIGN d-saldo-calculado = fn-qtd-data().
            IF d-saldo-calculado = 0 AND NOT g-saldo-zerado THEN NEXT.
    
            ASSIGN  qt-disponivel = saldo-estoq.qtidade-atu  -
                                    saldo-estoq.qt-alocada   -
                                    saldo-estoq.qt-aloc-prod -
                                    saldo-estoq.qt-aloc-ped.
    
            ASSIGN d-disponivel-disp = d-disponivel-disp + qt-disponivel.        
        END.
    END.

    RETURN d-disponivel-disp.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

