/*****************************************************************************
** Programa..............: era_es_cons_estab_emp
** Descricao.............: Elimina De Para Estab X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071ga.p
** Criado por............: Hilton Borba
** Criado em.............: 04/10/2011
*****************************************************************************/
def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

/* buffer para tabelas iguais no ems2 e ems5 - ini */
def buffer empresa             for emsuni.empresa.
/* buffer para tabelas iguais no ems2 e ems5 - fim */

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i  era_es_cons_estab_emp FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "ERA_ES_CONS_ESTAB_EMP","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

def buffer b_es_cons_estab_emp
    for es_cons_estab_emp.

/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_rec_es_cons_estab_emp
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_log_answer
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_possui_val
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

/************************** Variable Definition End *************************/


/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
def new global shared var v_cod_arq
    as char
    format "x(60)":U
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format "x(08)":U
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ? then
    run pi_version_extract ('era_es_cons_estab_emp':U, 'esp/esfgl071ga.p':U, '1.00.00.000':U, 'pro':U).
/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado: prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado: prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado: prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado: prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'era_es_cons_estab_emp') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'era_es_cons_estab_emp')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'era_es_cons_estab_emp')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'era_es_cons_estab_emp' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'era_es_cons_estab_emp'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* ix_p00_era_es_cons_estab_emp */
assign v_rec_table  = v_rec_es_cons_estab_emp
       v_log_answer = no.

message substitute("Confirme eliminaá∆o de &1 ?" /*l_conf_erase*/ ,"De Para Estab X Empresa")
       view-as alert-box question buttons yes-no-cancel title substitute("&1", c-versao-prg) update v_log_answer.

if  v_log_answer = yes
then do:
    delete_block:
    do on error undo delete_block, leave delete_block:
        find es_cons_estab_emp where recid(es_cons_estab_emp) = v_rec_table exclusive-lock no-error.

        /* Begin_Include: ix_p10_era_es_cons_estab_emp */
        on delete of es_cons_estab override do:
        end.
        for each es_cons_estab exclusive-lock
           where es_cons_estab.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp:
            delete es_cons_estab.
        end.

        on delete of es_cons_cta_ctbl override do:
        end.
        for each es_cons_cta_ctbl exclusive-lock
           where es_cons_cta_ctbl.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp:
            delete es_cons_cta_ctbl.
        end.

        on delete of es_cons_ccusto override do:
        end.
        for each es_cons_ccusto exclusive-lock
           where es_cons_ccusto.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp:
            delete es_cons_ccusto.
        end.

        delete es_cons_estab_emp.
        /* ix_p20_era_es_cons_estab_emp */
        assign v_rec_es_cons_estab_emp = ?.
    end /* do delete_block */.
end.

/* ix_p30_era_es_cons_estab_emp */

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    end.
    release log_exec_prog_dtsul.
end.
/* End_Include: i_log_exec_prog_dtsul_fim */

/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_version
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(08)":U
        no-undo.

    /************************* Parameter Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) no-echo no-convert append.

        put stream s-arq unformatted
            p_cod_program            at 1
            p_cod_program_ext        at 43
            p_cod_version            at 69
            today                    at 84 format "99/99/99"
            string(time, 'HH:MM:SS') at 94 skip.

        output stream s-arq close.
    end.

END PROCEDURE. /* pi_version_extract */

/************************** Internal Procedure End **************************/

&endif

/*************************************  *************************************/
/*****************************************************************************
**  Procedure Interna: pi_messages
**  Descricao........: Mostra Mensagem com Ajuda
*****************************************************************************/
PROCEDURE pi_messages:

    def input param c_action    as char    no-undo.
    def input param i_msg       as integer no-undo.
    def input param c_param     as char    no-undo.

    def var c_prg_msg           as char    no-undo.

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/************************  End of era_es_cons_estab_emp ***********************/
