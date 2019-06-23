/*****************************************************************************************
**    Programa: esmen013rp.p
**       Autor: RDE
**        Data: 13/04/2018
**    Objetivo: Relatorio validaá∆o corretiva - conflito de acessos
******************************************************************************************
**  Data            Autor           Requisiá∆o          Alteraá∆o
**  03/05/2018      DPC-DKP         -                   Reorganizaá∆o total do relat¢rio para 
**                                                      melhorar performance
**
******************************************************************************************/
{include/i-prgvrs.i esmen013rp 1.00.00.000} 
{utp/ut-glob.i}

DEF VAR h-acomp AS HANDLE NO-UNDO.

DEFINE BUFFER bf-prog_dtsul_segur FOR prog_dtsul_segur.
DEFINE VARIABLE c-nom_usuario AS CHARACTER   NO-UNDO.
DEF BUFFER bf-usuar_grp_usuar FOR usuar_grp_usuar.

DEF TEMP-TABLE tt-conflito
    FIELD cod_grp_usuar  LIKE bf-usuar_grp_usuar.cod_grp_usuar    
    FIELD cod_usuario    LIKE bf-usuar_grp_usuar.cod_usuario      
    FIELD cod_prog_dtsul LIKE bf-prog_dtsul_segur.cod_prog_dtsul.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD usuario-ini      AS CHAR
    FIELD usuario-fim      AS CHAR
    FIELD grupo-ini        AS CHAR
    FIELD grupo-fim        AS CHAR
    FIELD tipo             AS INT
    FIELD tp-usu           AS INT.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param no-error. 

{include/i-rpvar.i}

DEF TEMP-TABLE tt-report
    FIELD chave               AS CHAR 
    FIELD cod_usuario         LIKE usuar_grp_usuar.cod_usuario                
    FIELD cod_grp_usuar_base  LIKE prog_dtsul_segur.cod_grp_usuar             
    FIELD cod_prog_dtsul_base LIKE ctrl-conflito-it.cod_prog_dtsul_base
    FIELD cod_grp_usuar       LIKE prog_dtsul_segur.cod_grp_usuar      
    FIELD cod_prog_dtsul      LIKE ctrl-conflito-it.cod_prog_dtsul     
    FIELD observacao          LIKE ctrl-conflito-it.observacao         
    FIELD razao               LIKE ctrl-conflito-it.razao
    INDEX idx0 cod_usuario        
               cod_grp_usuar_base 
               cod_prog_dtsul_base
               cod_grp_usuar      
               cod_prog_dtsul
    INDEX idx1 chave.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Relat¢rio Ctrl Conflito").
           

/* pesquisa conflitos - base */
FOR EACH ctrl-conflito-it NO-LOCK:

    /* pesquisa os grupos de conflito dos programas relacionados ao conflito */
    FOR EACH prog_dtsul_segur NO-LOCK 
        WHERE prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul
         AND prog_dtsul_segur.cod_grp_usuar >= tt-param.grupo-ini  
         AND prog_dtsul_segur.cod_grp_usuar <= tt-param.grupo-fim, 
        EACH  usuar_grp_usuar NO-LOCK 
           WHERE usuar_grp_usuar.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar
             AND usuar_grp_usuar.cod_usuario >= tt-param.usuario-ini
             AND usuar_grp_usuar.cod_usuario <= tt-param.usuario-fim:
        
            /* Localiza conflito do usu†rio */
            FOR EACH bf-prog_dtsul_segur NO-LOCK 
                WHERE bf-prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul_base 
                AND   ROWID(bf-prog_dtsul_segur) <> ROWID(prog_dtsul_segur)
                AND   bf-prog_dtsul_segur.cod_grp_usuar <> prog_dtsul_segur.cod_grp_usuar,
                EACH bf-usuar_grp_usuar NO-LOCK 
                WHERE bf-usuar_grp_usuar.cod_grp_usuar = bf-prog_dtsul_segur.cod_grp_usuar
                  AND bf-usuar_grp_usuar.cod_usuario = usuar_grp_usuar.cod_usuario:  

                    RUN pi-acompanhar in h-acomp (input "Conflito: " + CAPS(usuar_grp_usuar.cod_grp_usuar ) + ' X ' + CAPS(prog_dtsul_segur.cod_grp_usuar)).

                    FIND tt-report 
                       WHERE tt-report.cod_usuario          = usuar_grp_usuar.cod_usuario      
                        AND  tt-report.cod_grp_usuar_base   = bf-prog_dtsul_segur.cod_grp_usuar
                        AND  tt-report.cod_prog_dtsul_base  = bf-prog_dtsul_segur.cod_prog_dtsul
                        AND  tt-report.cod_grp_usuar        = usuar_grp_usuar.cod_grp_usuar 
                        AND  tt-report.cod_prog_dtsul       = ctrl-conflito-it.cod_prog_dtsul NO-ERROR.
                    IF NOT AVAIL tt-report THEN DO:
                        CREATE tt-report.
                        ASSIGN tt-report.chave                = (IF tt-param.tipo = 1 THEN usuar_grp_usuar.cod_usuario ELSE usuar_grp_usuar.cod_grp_usuar)
                               tt-report.cod_usuario          = usuar_grp_usuar.cod_usuario             
                               tt-report.cod_grp_usuar_base   = bf-prog_dtsul_segur.cod_grp_usuar
                               tt-report.cod_prog_dtsul_base  = bf-prog_dtsul_segur.cod_prog_dtsul
                               tt-report.cod_grp_usuar        = usuar_grp_usuar.cod_grp_usuar      
                               tt-report.cod_prog_dtsul       = ctrl-conflito-it.cod_prog_dtsul
                               tt-report.observacao           = ctrl-conflito-it.observacao 
                               tt-report.razao                = ctrl-conflito-it.razao.
                    END.
            END.
    END.
END.

/* Gera Relat¢rio - CSV */
{include/i-rpout.i}

PUT UNFORMATTED "Usu†rio;Nome;Grupo Base;Programa Base;Grupo Conflito;Programa Conflito;Observaá∆o;Raz∆o/Motivo" SKIP.

FOR EACH tt-report BREAK BY chave:

    ASSIGN c-nom_usuario = "".

    FIND usuar_mestre NO-LOCK WHERE
         usuar_mestre.cod_usuario = tt-report.cod_usuario NO-ERROR.

     /* Lista Apenas usu†rios Ativos */
    IF AVAIL usuar_mestre AND usuar_mestre.dat_fim_valid < TODAY AND tt-param.tp-usu = 1 THEN NEXT.

    /* Lista Apenas usu†rios Inativos */
    IF AVAIL usuar_mestre AND (usuar_mestre.dat_fim_valid >= TODAY OR usuar_mestre.dat_fim_valid = ?) AND tt-param.tp-usu = 2 THEN NEXT.

    ASSIGN c-nom_usuario = usuar_mestre.nom_usuario WHEN AVAIL usuar_mestre.

    PUT UNFORMATTED
        tt-report.cod_usuario          ";"
        c-nom_usuario                  ";" 
        tt-report.cod_grp_usuar_base   ";"
        tt-report.cod_prog_dtsul_base  ";"
        tt-report.cod_grp_usuar        ";"
        tt-report.cod_prog_dtsul       ";"
        tt-report.observacao           ";" 
        tt-report.razao                SKIP.

END.

{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.

RETURN "OK":U.
/* fim */
