/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFP0009RP 1.02.00.000}  /*** 010000 ***/

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

DEFINE TEMP-TABLE tt_gestor
    FIELD cdn_gestor    LIKE es_gestor.cdn_gestor
    FIELD cdn_empresa   LIKE es_gestor.cdn_empresa
    FIELD cdn_estab     LIKE es_gestor.cdn_estab
    FIELD cdn_funciona  AS CHAR 
    FIELD nom_gestor    LIKE es_gestor.nom_gestor
    FIELD niv_hier_funcnal LIKE es_gestor.niv_hier_funcnal
    FIELD tipo  AS CHAR
    INDEX ch-gestor cdn_gestor.
    .

/* Inicializaá∆o da Temp-table tt-param carregada */
define temp-table tt-raw-digita field raw-digita as raw.
DEFINE VARIABLE v_han_acomp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-status-imp AS CHARACTER   FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE l-reg-ok     AS LOGICAL     FORMAT "SIM/NAO" NO-UNDO.

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
         assign c-programa     = "YMFP0009"
                c-versao       = "0.00"
                c-revisao      = "01"
                c-empresa      = if avail empresa then empresa.nome else ""
                c-sistema      = "CARGOS E SALµRIOS"
                c-titulo-relat = "Importaá∆o de Gestor".

    run utp/ut-acomp.p persistent set v_han_acomp.
    RUN pi-inicializar IN v_han_acomp (INPUT "Importando Gestores").


    INPUT STREAM str-imp FROM VALUE(tt-param.arquivo-entrada).
    REPEAT:
        CREATE tt_gestor.
        IMPORT STREAM str-imp DELIMITER ";" tt_gestor NO-ERROR.
    END.
    INPUT STREAM str-imp CLOSE.

    RUN pi-inicializar IN v_han_acomp (INPUT "Atualizando Gestores").
    FOR EACH tt_gestor
        WHERE tt_gestor.cdn_gestor <> 0:

        RUN pi-acompanhar IN v_han_acomp (INPUT STRING(tt_gestor.cdn_gestor,"99999999") + "-" + tt_gestor.nom_gestor).

        RUN pi-valida-registro (OUTPUT c-status-imp,
                                OUTPUT l-reg-ok).
                                
        IF l-reg-ok THEN DO:

            FIND es_gestor
                WHERE es_gestor.cdn_empresa = tt_gestor.cdn_empresa
                  AND es_gestor.cdn_estab   = tt_gestor.cdn_estab
                  AND es_gestor.cdn_funciona = int(tt_gestor.cdn_funciona)
                  AND es_gestor.cdn_gestor   = tt_gestor.cdn_gestor NO-ERROR.

            IF NOT AVAIL es_gestor THEN DO:
                CREATE es_gestor.
                ASSIGN 
                    es_Gestor.origem            = IF tt_gestor.tipo = "Interno" THEN 1 ELSE 2
                    es_Gestor.nom_gestor        = tt_gestor.nom_gestor
                    es_Gestor.niv_hier_funcnal  = tt_gestor.niv_hier_funcnal
                    es_Gestor.cdn_gestor        = tt_gestor.cdn_gestor.

                IF tt_gestor.tipo = "Interno" THEN
                    ASSIGN 
                        es_Gestor.cdn_funcionario   = int(tt_gestor.cdn_funciona)
                        es_Gestor.cdn_estab         = tt_gestor.cdn_estab       
                        es_Gestor.cdn_empresa       = tt_gestor.cdn_empresa.  
                        .
            END.
        
            ASSIGN c-status-imp = "Gestor IMPORTADO !!!".

        END.

        DISP STREAM str-rp 
            tt_gestor.cdn_gestor   
            tt_gestor.cdn_empresa  
            tt_gestor.cdn_estab    
            tt_gestor.cdn_funciona 
            tt_gestor.nom_gestor 
            tt_gestor.niv_hier_funcnal
            l-reg-ok                    COLUMN-LABEL "Reg OK"
            c-status-imp                COLUMN-LABEL "Situacao"
            WITH WIDTH 300 STREAM-IO FRAME f-log-imp 60 DOWN.
        DOWN STREAM str-rp WITH FRAME f-log-imp.


    END.

    run pi-finalizar in v_han_acomp.

end procedure.


/*-------------------------------------------------------------------*/
PROCEDURE pi-valida-registro:
DEFINE OUTPUT PARAMETER c-erro AS CHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER l-ok   AS LOGICAL NO-UNDO.

ASSIGN l-ok = YES.

    IF tt_gestor.tipo = "Interno" THEN DO:
        FIND mgcad.empresa
            WHERE empresa.ep-codigo = tt_gestor.cdn_empresa NO-LOCK NO-ERROR.
        IF NOT AVAIL empresa THEN DO:
            ASSIGN c-erro = "Empresa n∆o Cadastrada"
                   l-ok   = NO.
            NEXT.
        END.
    
         FIND rh_estab 
             WHERE rh_estab.cdn_empresa = tt_gestor.cdn_empresa
               AND rh_estab.cdn_estab   = tt_gestor.cdn_estab NO-LOCK NO-ERROR.
                   
         IF NOT AVAIL rh_estab THEN DO:
             ASSIGN c-erro = "Estabelecimento n∆o Cadastrado"
                    l-ok   = NO.
             NEXT.
         END.
    
        FIND funcionario
            WHERE funcionario.cdn_empresa      = tt_gestor.cdn_empresa
              AND funcionario.cdn_estab        = tt_gestor.cdn_estab
              AND funcionario.cdn_funcionario  = int(tt_gestor.cdn_funciona)
            NO-LOCK NO-ERROR.
        IF NOT AVAIL funcionario THEN DO:
            ASSIGN c-erro = "Funcionario n∆o Cadastrado"
                   l-ok   = NO.
    
            NEXT.
        END.
    END.

    FIND niv_hier_funcnal
        WHERE niv_hier_funcnal.cdn_niv_hier_funcnal = tt_gestor.niv_hier_funcnal
        NO-LOCK NO-ERROR.
    IF NOT AVAIL niv_hier_funcnal THEN DO:
        ASSIGN c-erro = "Hierarquia nao Cadastrada"
               l-ok   = NO.

        NEXT.
    END.

    FIND FIRST es_gestor
        WHERE es_gestor.cdn_gestor   = tt_gestor.cdn_gestor NO-LOCK NO-ERROR.

    IF  AVAIL es_gestor THEN DO:
        IF es_gestor.nom_gestor <> tt_gestor.nom_gestor THEN
            ASSIGN c-erro = "Gestor DUPLICADO"
                   l-ok   = NO.
        ELSE 
            ASSIGN c-erro = "Gestor ja Cadastrado"
               l-ok   = NO.
        NEXT.
    END.



END PROCEDURE.
