/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFP0010RP 1.02.00.000}  /*** 010000 ***/

/********************************************************************************************************************************************
**       Programa: YMFP0009RP
**       Data....: 02/09/2016
**       Autor...: Natanael Silva - DSC
**       Objetivo: Importaá∆o de Gestores
*********************************************************************************************************************************************/
/* ---------------------------------- Definiá‰es -------------------------------- */
/* ----------------------------------- Includes ----------------------------------*/
/* --- Global --- */
{utp/ut-glob.i}
{include/i-rpvar.i}

define temp-table tt-param NO-UNDO
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field data-exec            as date
    field hora-exec            as integer
    field classific            as integer
    FIELD arquivo-entrada    AS CHAR.

DEFINE TEMP-TABLE tt_gestor_hist
    FIELD cdn_gestor    LIKE es_HistGestor.cdn_gestor
    FIELD cdn_empresa   LIKE es_HistGestor.cdn_empresa
    FIELD cdn_estab     LIKE es_HistGestor.cdn_estab
    FIELD cdn_funciona  LIKE es_HistGestor.cdn_funcionario
    FIELD nom_funciona  LIKE es_gestor.nom_gestor.

/* Inicializaá∆o da Temp-table tt-param carregada */
define temp-table tt-raw-digita field raw-digita as raw.
DEFINE VARIABLE v_han_acomp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-status-imp AS CHARACTER   FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE l-reg-ok     AS LOGICAL     FORMAT "SIM/NAO" NO-UNDO.
DEFINE VARIABLE da-data-inicio AS DATE        NO-UNDO.

define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

define stream str-imp.
define stream str-rp.

{include/i-rpcab.i &stream="str-rp"}
{include/i-rpout.i &stream="stream str-rp"}

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run pi-importa.

{include/i-rpclo.i &stream="stream str-rp"}
return "OK".

procedure pi-importa:

    find empresa  no-lock where empresa.ep-codigo = v_cdn_empres_usuar no-error.
         assign c-programa     = "YMFP0010"
                c-versao       = "0.00"
                c-revisao      = "01"
                c-empresa      = if avail empresa then empresa.nome else ""
                c-sistema      = "CARGOS E SALµRIOS"
                c-titulo-relat = "Importaá∆o de Historico de Gestor".

    run utp/ut-acomp.p persistent set v_han_acomp.
    RUN pi-inicializar IN v_han_acomp (INPUT "Importando Historico de Gestores").


    INPUT STREAM str-imp FROM VALUE(tt-param.arquivo-entrada).
    REPEAT:
        CREATE tt_gestor_hist.
        IMPORT STREAM str-imp DELIMITER ";" tt_gestor_hist NO-ERROR.
    END.
    INPUT STREAM str-imp CLOSE.

    RUN pi-inicializar IN v_han_acomp (INPUT "Atualizando Historico Gestores").
    FOR EACH tt_gestor_hist
        WHERE tt_gestor_hist.cdn_gestor <> 0:

        RUN pi-acompanhar IN v_han_acomp (INPUT STRING(tt_gestor_hist.cdn_gestor,"99999999") + "-" + tt_gestor_hist.nom_funciona).

        RUN pi-valida-registro (OUTPUT c-status-imp,
                                OUTPUT l-reg-ok).
                                

        IF l-reg-ok THEN DO:

            ASSIGN da-data-inicio = TODAY.

            FIND es_HistGestor
                WHERE es_HistGestor.cdn_empresa  = tt_gestor_hist.cdn_empresa
                  AND es_HistGestor.cdn_estab    = tt_gestor_hist.cdn_estab
                  AND es_HistGestor.cdn_funciona = tt_gestor_hist.cdn_funciona
                  AND es_HistGestor.cdn_gestor   = tt_gestor_hist.cdn_gestor 
                  AND es_HistGestor.da-inicio    = da-data-inicio NO-ERROR.

            IF NOT AVAIL es_HistGestor THEN DO:
                CREATE es_HistGestor.
                ASSIGN 
                    es_HistGestor.cdn_gestor       = tt_gestor_hist.cdn_gestor  
                    es_HistGestor.cdn_funcionario  = tt_gestor_hist.cdn_funciona
                    es_HistGestor.cdn_estab        = tt_gestor_hist.cdn_estab   
                    es_HistGestor.cdn_empresa      = tt_gestor_hist.cdn_empresa 
                    es_HistGestor.da-inicio        = da-data-inicio
                    es_HistGestor.da-final         = DATE(12/31/9999).

            END.
        
            ASSIGN c-status-imp = "Historico Gestor IMPORTADO !!!".

        END.

        DISP STREAM str-rp 
            tt_gestor_hist.cdn_gestor   
            tt_gestor_hist.cdn_empresa  
            tt_gestor_hist.cdn_estab    
            tt_gestor_hist.cdn_funciona 
            l-reg-ok                    COLUMN-LABEL "Reg OK"
            c-status-imp                COLUMN-LABEL "Situacao"
            WITH WIDTH 300 STREAM-IO FRAME f-log-imp-hist 60 DOWN.
        DOWN STREAM str-rp WITH FRAME f-log-imp-hist.
    END.

    run pi-finalizar in v_han_acomp.

end procedure.


/*-------------------------------------------------------------------*/
PROCEDURE pi-valida-registro:
DEFINE OUTPUT PARAMETER c-erro AS CHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER l-ok   AS LOGICAL NO-UNDO.

ASSIGN l-ok = YES.

    FIND mgcad.empresa
        WHERE empresa.ep-codigo = tt_gestor_hist.cdn_empresa NO-LOCK NO-ERROR.
    IF NOT AVAIL empresa THEN DO:
        ASSIGN c-erro = "Empresa n∆o Cadastrada"
               l-ok   = NO.
        NEXT.
    END.

     FIND rh_estab 
         WHERE rh_estab.cdn_empresa = tt_gestor_hist.cdn_empresa
           AND rh_estab.cdn_estab   = tt_gestor_hist.cdn_estab NO-LOCK NO-ERROR.
               
     IF NOT AVAIL rh_estab THEN DO:
         ASSIGN c-erro = "Estabelecimento n∆o Cadastrado"
                l-ok   = NO.
         NEXT.
     END.

    FIND funcionario
        WHERE funcionario.cdn_empresa      = tt_gestor_hist.cdn_empresa
          AND funcionario.cdn_estab        = tt_gestor_hist.cdn_estab
          AND funcionario.cdn_funcionario  = tt_gestor_hist.cdn_funciona
        NO-LOCK NO-ERROR.
    IF NOT AVAIL funcionario THEN DO:
        ASSIGN c-erro = "Funcionario n∆o Cadastrado"
               l-ok   = NO.

        NEXT.
    END.

    FIND es_gestor
        WHERE es_gestor.cdn_gestor = tt_gestor_hist.cdn_gestor NO-LOCK NO-ERROR.
    IF NOT AVAIL es_gestor THEN DO:
        ASSIGN c-erro = "Gestor n∆o Cadastrado"
               l-ok   = NO.
        NEXT.
    END.

    FIND es_HistGestor 
        WHERE es_HistGestor.cdn_empresa  = tt_gestor_hist.cdn_empresa
          AND es_HistGestor.cdn_estab    = tt_gestor_hist.cdn_estab
          AND es_HistGestor.cdn_gestor   = tt_gestor_hist.cdn_gestor
          AND es_HistGestor.cdn_funciona = tt_gestor_hist.cdn_funciona
          AND es_HistGestor.da-inici     = da-data-inicio
        NO-LOCK NO-ERROR.
   IF AVAIL es_HistGestor THEN DO:
       ASSIGN c-erro = "Historico ja Cadastrado com Inicio: " + STRING(da-data-inicio)
              l-ok   = NO.
       NEXT.
   END.

END PROCEDURE.
