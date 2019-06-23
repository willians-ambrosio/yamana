/* include de controle de versÆo */
{include/i-prgvrs.i ESIN0520RP 1.00.00.000}

{utp/ut-glob.i} 

/* pr‚processador para ativar ou nÆo a sa¡da para RTF */
&GLOBAL-DEFINE RTF NO
/* pr‚processador para setar o tamanho da p gina */
&SCOPED-DEFINE pagesize 88 
/* defini‡Æo das temp-tables para recebimento de parƒmetros */
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)":U
    FIELD usuario          AS CHAR FORMAT "x(12)":U
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)":U
    FIELD modelo           AS CHAR FORMAT "x(35)":U
    FIELD cod-ep-ini       AS CHAR
    FIELD cod-ep-fim       AS CHAR
    FIELD cod-estab-ini    LIKE estabelec.cod-estabel
    FIELD cod-estab-fim    LIKE estabelec.cod-estabel
    FIELD num-proj-ini     LIKE proj-inv.num-projeto
    FIELD num-proj-fim     LIKE proj-inv.num-projeto
    FIELD num-ord-inv-ini  LIKE mat-rat-contr-inv.num-ord-inv
    FIELD num-ord-inv-fim  LIKE mat-rat-contr-inv.num-ord-inv
    FIELD i-moeda          AS   INTEGER
    FIELD arquivo-excel    AS   CHAR FORMAT "x(60)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    FIELD l-habilitaRtf    AS LOG.
    /*Fim alteracao 15/02/2005*/

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FORM /*PAR¶METRO*/
     SKIP(5)
     "SELE€ÇO"                          AT 09      
     SKIP(1)
     "Empresa:"                             AT 15
     tt-param.cod-ep-ini          NO-LABEL  AT 35
     "|< >|":U                              AT 60
     tt-param.cod-ep-fim          NO-LABEL  AT 70 SKIP
     "Estabelecimento:"                     AT 15
     tt-param.cod-estab-ini       NO-LABEL  AT 35
     "|< >|":U                              AT 60
     tt-param.cod-estab-fim       NO-LABEL  AT 70 SKIP
     "Projeto:"                             AT 15
     tt-param.num-proj-ini        NO-LABEL  AT 35
     "|< >|":U                              AT 60
     tt-param.num-proj-fim        NO-LABEL  AT 70 SKIP
     "Ordem Invest:"                        AT 15
     tt-param.num-ord-inv-ini     NO-LABEL  AT 35
     "|< >|":U                              AT 60
     tt-param.num-ord-inv-fim     NO-LABEL  AT 70 SKIP
     SKIP(1)
     "PAR¶METROS"                           AT 09 
     SKIP(1)
     "Moeda:"                               AT 15
     tt-param.i-moeda             NO-LABEL  AT 47 SKIP
     SKIP(1)
     "IMPRESSÇO"                            AT 09
     SKIP(1)
     "Destino:"                             AT 15 " - " tt-param.arquivo NO-LABEL
     "Usu rio:"                             AT 15 " - " tt-param.usuario NO-LABEL
     SKIP(1)
     "EXCEL"                                AT 09
     SKIP(1)
     "Destino:"                             AT 15 " - " tt-param.arquivo-excel NO-LABEL
     WITH WIDTH 188 SIDE-LABELS FRAME fParam STREAM-IO. 

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}


/* bloco principal do programa */
ASSIGN  c-programa 	    = "ESIN0520RP"
	    c-versao	    = "1.00"
	    c-revisao	    = "1.00.000"
	    c-empresa       = string(v_nom_razao_social)
        c-titulo-relat  = "Relat¢rio Investimentos Compromissados".


/* para nÆo visualizar cabe‡alho/rodap‚ em sa¡da RTF */
IF tt-param.destino <> 4 THEN DO:
    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.
END.

/* corpo do relat¢rio */

RUN inp/esp/esin0520a.p (INPUT-OUTPUT TABLE tt-param).

FIND FIRST tt-param NO-ERROR.

DISPLAY
     tt-param.cod-ep-ini   
     tt-param.cod-ep-fim   
     tt-param.cod-estab-ini  
     tt-param.cod-estab-fim  
     tt-param.num-proj-ini   
     tt-param.num-proj-fim   
     tt-param.num-ord-inv-ini
     tt-param.num-ord-inv-fim
     tt-param.i-moeda        
     tt-param.arquivo
     tt-param.usuario
     tt-param.arquivo-excel
     WITH FRAME fParam.

/*fechamento do output do relat¢rio*/
{include/i-rpclo.i}

RETURN "OK":U.
