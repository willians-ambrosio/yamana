/****************************************************************************************** 
** 	   Programa: cc0300-u03.p
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: Grupo DKP
**    	 Data: 15/11/2018
** Change/Chamado: REQ01
**    Objetivo: Criar pedido de execuá∆o para Enviar e-mail de pedidos de compras
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: tt-param
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp/ut-glob.i}
{btb/btb912zc.i3}
{include/i_dbinst.i}
{include/i-prgvrs.i cc0300-u03 12.00.00.000}

/* definiá∆o das temp-tables para recebimento de parametros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as INTEGER.
                                                                        
DEF TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita  AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEFINE VARIABLE raw-param2 AS RAW NO-UNDO.
DEFINE VARIABLE raw-param3 AS RAW NO-UNDO.
define temp-table tt-param2 no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

CREATE tt-param2.
ASSIGN 
    tt-param2.destino          = 3
    tt-param2.arquivo          = "cc002-w01.lst"
    tt-param2.usuario          = c-seg-usuario
    tt-param2.data-exec        = TODAY
    tt-param2.hora-exec        = TIME
    tt-param2.classifica       = 0
    tt-param2.desc-classifica  = ""
    tt-param2.modelo-rtf       = ""
    tt-param2.l-habilitaRtf    = NO.

RAW-TRANSFER tt-param2 TO raw-param2.
CREATE tt-raw-digita.
RAW-TRANSFER tt-raw-digita TO raw-param3.

define temp-table tt-log no-undo
    FIELD num_ped_exec     LIKE ped_exec.num_ped_exec
    FIELD dt-exec-ped      AS DATE
    field hr-exec          as CHAR
    FIELD nom_prog_ext     LIKE ped_exec.nom_prog_ext.

DEFINE TEMP-TABLE tt-mes_estab NO-UNDO
    FIELD mensagem        AS CHAR
    FIELD cod-estab       AS CHAR.

{include/i-rpvar.i}

DEFINE VARIABLE hr-exec    AS CHAR        NO-UNDO.
DEFINE VARIABLE hr-cria    AS CHAR        NO-UNDO.
DEFINE VARIABLE i-num_ped_exec AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-num_ped_espelho AS INTEGER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR i-ep-codigo-usuario AS CHARACTER NO-UNDO.

DEFINE BUFFER b-ped_exec           FOR ped_exec.
DEFINE BUFFER b-ped_exec_param     FOR ped_exec_param.
DEFINE BUFFER b-ped_exec_param_aux FOR ped_exec_param_aux.
DEFINE BUFFER b-ped_exec_sel       FOR ped_exec_sel.

DEFINE TEMP-TABLE tt-ped_exec LIKE ped_exec.
DEFINE TEMP-TABLE tt-ped_exec_param LIKE ped_exec_param.
DEFINE TEMP-TABLE tt-ped_exec_param_aux LIKE ped_exec_param_aux.
DEFINE TEMP-TABLE tt-ped_exec_sel LIKE ped_exec_sel.

ASSIGN
    c-programa 	= "cc0300-u03"
    c-versao	= "12.01"
    c-revisao	= ".00.001"
    c-empresa 	= "YAMANA"
    c-sistema	= "MATERIAIS"
    c-titulo-relat = "RELAT‡RIO DE CRIAÄ«O DE PEDIDOS"
    /*
    da-iniper-x = tt-param.dt-carteira
    da-fimper-x = tt-param.dt-carteira */.

{include/i-rpout.i &STREAM="stream str-rp"}
{include/i-rpcab.i &STREAM="str-rp"}

/*view stream str-rp frame f-cabper.*/
view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

ASSIGN
    /*hr-exec = SUBSTRING(STRING(TIME, "HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME, "HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME, "HH:MM:SS"),7,2)*/
    hr-exec = "210000".
    hr-cria = hr-exec.

FIND FIRST ped_exec NO-LOCK USE-INDEX pedexec_01
     WHERE ped_exec.dat_exec_ped_exec      = TODAY
       AND ped_exec.cod_prog_dtsul         = "cc002-w01"
       AND ped_exec.nom_prog_ext           = "ccp\cc002-rp.r"
       AND ped_exec.cod_release_prog_dtsul = "12.00.01" NO-ERROR.
IF NOT AVAIL ped_exec THEN
DO:
    FIND FIRST servid_exec NO-LOCK
         WHERE servid_exec.cdn_empres_servid <> ""
           AND servid_exec.log_servid_exec_ativ
           AND servid_exec.ind_tip_fila_exec = "Windows NT"
           AND servid_exec.cdn_empres_servid = i-ep-codigo-usuario NO-ERROR.

    FIND LAST ped_exec USE-INDEX pedexec_05 NO-LOCK NO-ERROR.
    i-num_ped_exec = ped_exec.num_ped_exec + 1.

    FIND ped_exec_param WHERE
         ped_exec_param.num_ped_exec = ped_exec.num_ped_exec NO-LOCK NO-ERROR.

    FIND ped_exec_param_aux WHERE
         ped_exec_param_aux.num_ped_exec = ped_exec.num_ped_exec NO-LOCK NO-ERROR.

    FIND FIRST ped_exec_sel WHERE
         ped_exec_sel.num_ped_exec = ped_exec.num_ped_exec NO-LOCK NO-ERROR.

    CREATE tt-ped_exec.
    BUFFER-COPY ped_exec TO tt-ped_exec.
    ASSIGN
        tt-ped_exec.num_ped_exec                = i-num_ped_exec
        tt-ped_exec.dat_exec_ped_exec           = TODAY
        tt-ped_exec.hra_exec_ped_exec           = hr-exec
        tt-ped_exec.cod_usuario                 = c-seg-usuario
        tt-ped_exec.cod_prog_dtsul              = "cc002-w01"      
        tt-ped_exec.nom_prog_ext                = "ccp\cc002-rp.r"
        tt-ped_exec.cod_release_prog_dtsul      = "12.00.01"
        tt-ped_exec.dat_criac_ped_exec          = TODAY
        tt-ped_exec.hra_criac_ped_exec          = SUBSTRING(STRING(TIME, "HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME, "HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME, "HH:MM:SS"),7,2)
        tt-ped_exec.dat_inic_exec_servid_exec   = ?
        tt-ped_exec.hra_inic_exec_servid_exec   = ""
        tt-ped_exec.dat_fim_exec_servid_exec    = ?
        tt-ped_exec.hra_fim_exec_servid_exec    = ""
        tt-ped_exec.ind_sit_ped_exec            = "1"
        tt-ped_exec.cod_ult_obj_procesdo        = ""
        tt-ped_exec.ind_motiv_sit_ped_exec      = "14"
        tt-ped_exec.cod_servid_exec             = IF AVAIL servid_exec THEN servid_exec.cod_servid_exec ELSE "jmc_lx"
        tt-ped_exec.des_text_erro               = ""
        tt-ped_exec.dat_ult_atualiz_servid_exec = ?
        tt-ped_exec.hra_ult_atualiz_servid_exec = ""
        tt-ped_exec.cod_ult_obj_procesdo        = ""
        tt-ped_exec.cdn_estil_dwb               = 97
        tt-ped_exec.des_inf_aplic               = "Enviar E-mail pedido com cond pagto < 21 dias".

    CREATE b-ped_exec.
    BUFFER-COPY tt-ped_exec TO b-ped_exec.

    CREATE tt-ped_exec_param.
    BUFFER-COPY ped_exec_param TO tt-ped_exec_param.
    ASSIGN
        tt-ped_exec_param.num_ped_exec =     i-num_ped_exec
        tt-ped_exec_param.nom_dwb_printer    = ""
        tt-ped_exec_param.raw_param_ped_exec = raw-param2.

    CREATE b-ped_exec_param.
    BUFFER-COPY tt-ped_exec_param TO b-ped_exec_param.

    CREATE ped_exec_param_aux.
    ASSIGN
        ped_exec_param_aux.num_ped_exec  = i-num_ped_exec
        ped_exec_param_aux.num_dwb_order = 1.
        ped_exec_param_aux.raw_param_ped_exec = raw-param3.

    PUT STREAM str-rp
       i-num_ped_exec           AT 01 FORMAT ">>>>>>9"
       TODAY                    AT 10 FORMAT "99/99/9999"
       hr-exec                  AT 22 FORMAT "99:99:99" 
       tt-ped_exec.nom_prog_ext AT 32 FORMAT "X(20)" SKIP.
END.

RETURN.
