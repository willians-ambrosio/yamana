/**============================================================**
** Programa....: 
** Empresa.....: Cleilton / DSC
** Data........: 16/02/2015
** Objetivo....: Replicar os itens para as demais empresas
** Alteraá∆o...: 
** ............:  
**=============================================================**/
{include/i_dbvers.i}
{include/i-prgvrs.i YMCD0203 11.5.11.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMCD0203 MCD}
&ENDIF

/*** Definiá∆o de Vari†veis Globais ***/
{utp/ut-glob.i}


{cdp/cdcfgman.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cdcfgcex.i}

/* Definiá∆o de vari†veis locais */
DEFINE VARIABLE c-param-cad       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-param-esp       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-conectado       AS LOGICAL     NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
/* Main-Block */

EMPTY TEMP-TABLE tt-param.



CREATE tt-param.
ASSIGN tt-param.usuario         = c-seg-usuario
       tt-param.destino         = 2 /* Arquivo */
       tt-param.data-exec       = TODAY 
       tt-param.hora-exec       = TIME.
       tt-param.arquivo         = SESSION:TEMP-DIRECTORY + "ymcd0203.tmp":U 
       .
IF i-num-ped-exec-rpw <> 0 THEN
    ASSIGN tt-param.arquivo = "ymcd0203.tmp":U .

RAW-TRANSFER tt-param TO raw-param.
RUN esp/ymcd0202rp.p (INPUT raw-param,
                      INPUT TABLE tt-raw-digita).

/* /* COMENTADO C‡DIGO, PORQUE A IMPLANTAÄ«O DEVERµ SER EXECUTADA POR EMPRESA CORRENTE */ */
/* FOR EACH ems2cadme.empresa FIELDS(ep-codigo) NO-LOCK:                                  */
/*                                                                                        */
/*     ASSIGN l-conectado = NO.                                                           */
/*     IF CONNECTED("REPEMS2CAD") THEN                                                    */
/*         DISCONNECT REPEMS2CAD NO-ERROR.                                                */
/*     IF CONNECTED("REPEMS2ESP") THEN                                                    */
/*         DISCONNECT REPEMS2ESP NO-ERROR.                                                */
/*                                                                                        */
/*     ASSIGN c-param-cad = ""                                                            */
/*            c-param-esp = "".                                                           */
/*                                                                                        */
/*     FIND FIRST bco_empres                                                              */
/*         WHERE bco_empres.cod_bco_logic = "ems2cademp"                                  */
/*           AND bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK NO-ERROR.           */
/*     IF AVAIL bco_empres THEN DO:                                                       */
/*         ASSIGN c-param-cad = "-db " + bco_empres.cod_bco_logic +                       */
/*                              " -ld " + "REPEMS2CAD" +                                  */
/*                              bco_empres.cod_param_conex.                               */
/*         CONNECT VALUE(c-param-cad) NO-ERROR.                                           */
/*         IF CONNECTED ("REPEMS2CAD") THEN DO:                                           */
/*             FIND FIRST bco_empres                                                      */
/*                 WHERE bco_empres.cod_bco_logic = "mgesp"                               */
/*                   AND bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK NO-ERROR.   */
/*             IF AVAIL bco_empres THEN DO:                                               */
/*                 ASSIGN c-param-esp = "-db " + bco_empres.cod_bco_logic +               */
/*                                      " -ld " + "REPEMS2ESP" +                          */
/*                                      bco_empres.cod_param_conex.                       */
/*                 CONNECT VALUE(c-param-esp) NO-ERROR.                                   */
/*                 ASSIGN l-conectado = CONNECTED("REPEMS2ESP").                          */
/*             END. /* AVAIL bco_empres */                                                */
/*         END.                                                                           */
/*     END. /* AVAIL bco_empres */                                                        */
/*                                                                                        */
/*     /* Se tudo conectado faz a replicaá∆o da empresa */                                */
/*     IF l-conectado THEN DO:                                                            */
/*         RUN esp/ymcd0203a.p (INPUT empresa.ep-codigo) "REPEMS2CAD." "REPEMS2ESP." .    */
/*     END.                                                                               */
/*                                                                                        */
/* END.                                                                                   */
/*                                                                                        */
                                                                                                         
