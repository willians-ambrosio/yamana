def buffer empresa     for emsbas.empresa.

def temp-table tt_param no-undo
    field c_empresa_orig_ini as char format "x(03)"
    field c_empresa_orig_fim as char format "x(03)"
    FIELD c_empresa_Dest     AS CHARA FORMAT "X(03)"
    field i_lote_orig_ini   as int  format ">>>>>>>>9"
    field i_lote_orig_fim   as int  format ">>>>>>>>9"
    field i_lote_dest_ini   as int  format ">>>>>>>>9"
    field i_lote_dest_fim   as int  format ">>>>>>>>9"
    field dt_lote_orig_ini  as date format 99/99/9999
    field dt_lote_orig_fim  as date format 99/99/9999
    field dt_lote_dest_ini  as date format 99/99/9999
    field dt_lote_dest_fim  as date format 99/99/9999
    field dt_lanca_orig_ini as date format 99/99/9999
    field dt_lanca_orig_fim as date format 99/99/9999
    field c_cenario_ini     as char format "x(09)"
    field c_cenario_fim     as char format "x(09)"
    field c_modulo_ini      as char format "x(03)"
    field c_modulo_fim      as char format "x(03)"
    field valida_origem     as log
    field class-1           as char format "x(15)"
    field class-2           as char format "x(15)"
    field class-3           as char format "x(15)"
    field tipo_detalhado    as log
    field rs_OUTPUT         as char
    field c_arquivo         as char format "x(30)".

def temp-table tt_concilia
    field empresa             like empresa.cod_empresa
    field num_lote_orig       like lote_ctbl.num_lote_ctbl
    FIELD cod_modul_orig      LIKE lancto_ctbl.cod_modul_dts
    FIELD cod_modul_dest      LIKE lancto_ctbl.cod_modul_dts
    field num_lote_dest       like lote_ctbl.num_lote_ctbl INITIAL 0
    FIELD num_lancto_orig     LIKE lancto_ctbl.num_lancto_ctbl
    FIELD num_lancto_dest     LIKE lancto_ctbl.num_lancto_ctbl INITIAL 0
    field des_lote_orig       like lote_ctbl.des_lote_ctbl
    field des_lote_dest       like lote_ctbl.des_lote_ctbl
    field dt_lancto_orig      like lancto_ctbl.dat_lancto_ctbl  initial ?
    field dt_lancto_dest      like lancto_ctbl.dat_lancto_ctbl  initial ?
    FIELD val_lancto_CR_orig  LIKE tot_lancto_ctbl.val_lancto_ctbl_cr
    FIELD val_lancto_CR_dest  LIKE tot_lancto_ctbl.val_lancto_ctbl_cr
    FIELD dados_conc          AS CHARACTER format "x(100)"
    index lote num_lote_orig.


def temp-table tt-es-lotes like es_cons_lotes
    field dados_conc as char format "x(100)".

def temp-table tt_lote_ctbl like lote_ctbl
    field rec_lote_ctbl as recid.

def temp-table tt_lancto_ctbl like lancto_ctbl
    field rec_lancto_ctbl as recid
    field rec_lote_ctbl   as recid.

def temp-table tt_item_lancto_ctbl like item_lancto_ctbl
    field rec_item_lancto_ctbl as recid
    field rec_lancto_ctbl      as recid.

def temp-table tt_aprop_lancto_ctbl like aprop_lancto_ctbl
    field cod_unid_negoc       as char
    field rec_item_lancto_ctbl as recid.

def temp-table tt_lote_ctbl_orig like tt_lote_ctbl.

def temp-table tt_lancto_ctbl_orig like tt_lancto_ctbl.

def temp-table tt_item_lancto_ctbl_orig like tt_item_lancto_ctbl.

def temp-table tt_aprop_lancto_ctbl_orig like tt_aprop_lancto_ctbl.

DEF VAR i AS INTEGER NO-UNDO.
DEF VAR v_num_mensagem AS INTEGER NO-UNDO.
DEFINE VARIABLE v_cod_msg          AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_des_ajuda_view   AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_des_message_view AS CHARACTER                                                         NO-UNDO.
DEFINE VARIABLE v_ind_tip_msg      AS CHARACTER                                                         NO-UNDO.

def input param table for tt_param.

find first tt_param NO-LOCK NO-ERROR.

DEF BUFFER bf_tot_lancto_ctbl FOR tot_lancto_ctbl.
DEF BUFFER bf_lote_ctbl     FOR lote_ctbl.
DEF BUFFER bf_lancto_ctbl   FOR lancto_ctbl.

DEFINE VARIABLE h-acomp  AS HANDLE  NO-UNDO.

def stream str-rp.

RUN utp/ut-acomp.p persistent set h-acomp.
RUN pi-inicializar in h-acomp (INPUT "Localizando Lotes...").

run pi-cria-tabelas.
RUN pi-verif-lote-ctbl-orig.
if tt_param.valida_origem then
    RUN pi-verif-lote-ctbl-dest.

run pi-imprime.

RUN pi-finalizar IN h-acomp.

procedure pi-cria-tabelas :

    DEF VAR v_tot LIKE tot_lancto_ctbl.val_lancto_ctbl_cr NO-UNDO.

    FOR EACH es_cons_lotes NO-LOCK WHERE es_cons_lotes.num_lote_orig     >= tt_param.i_lote_orig_ini
                                     AND es_cons_lotes.num_lote_orig     <= tt_param.i_lote_orig_fim
                                     AND es_cons_lotes.num_lote_dest     >= tt_param.i_lote_dest_ini
                                     AND es_cons_lotes.num_lote_dest     <= tt_param.i_lote_dest_fim
                                     AND es_cons_lotes.cod_empresa_orig  >= tt_param.c_empresa_orig_ini
                                     AND es_cons_lotes.cod_empresa_orig  <= tt_param.c_empresa_orig_fim
                                     AND (es_cons_lotes.cod_empresa_dest  = tt_param.c_empresa_dest
                                      OR  es_cons_lotes.cod_empresa_dest  = '')
                                     AND es_cons_lotes.dt_lote_orig      >= tt_param.dt_lote_orig_ini
                                     AND es_cons_lotes.dt_lote_orig      <= tt_param.dt_lote_orig_fim
                                     AND ((es_cons_lotes.dt_lote_dest    >= tt_param.dt_lote_dest_ini
                                     AND es_cons_lotes.dt_lote_dest      <= tt_param.dt_lote_dest_fim)
                                      OR es_cons_lotes.dt_lote_dest       = ?)
                                     AND ((es_cons_lotes.dt_lancto_orig  >= tt_param.dt_lanca_orig_ini
                                     AND es_cons_lotes.dt_lancto_orig    <= tt_param.dt_lanca_orig_fim)
                                      OR es_cons_lotes.dt_lancto_orig     = ? ) 
                                     AND es_cons_lotes.cod_cenario_orig  >= tt_param.c_cenario_ini
                                     AND es_cons_lotes.cod_cenario_orig  <= tt_param.c_cenario_fim
                                     AND es_cons_lotes.cod_mod_orig      >= tt_param.c_modulo_ini
                                     AND es_cons_lotes.cod_mod_orig      <= tt_param.c_modulo_fim:

        RUN pi-acompanhar in h-acomp ("Lote: " + STRING(es_cons_lotes.num_lote_orig)).
        PROCESS EVENTS.

        FIND FIRST lote_ctbl NO-LOCK WHERE lote_Ctbl.num_lote_ctbl = es_cons_lotes.num_lote_orig NO-ERROR.
        IF AVAIL lote_Ctbl THEN DO:
            FIND FIRST bf_lote_ctbl WHERE bf_lote_ctbl.num_lote_ctbl = es_cons_lotes.num_lote_dest NO-LOCK NO-ERROR.
            IF AVAIL bf_lote_ctbl THEN DO:
                IF bf_lote_ctbl.cod_empresa <> es_cons_lotes.cod_empresa_dest THEN DO:
                    CREATE tt_concilia.
                    ASSIGN tt_concilia.empresa         = lote_ctbl.cod_empresa
                           tt_concilia.num_lote_orig   = lote_ctbl.num_lote_ctbl
                           tt_concilia.num_lancto_orig = es_cons_lotes.num_lancto_orig
                           tt_concilia.des_lote_orig   = lote_ctbl.des_lote_ctbl
                           tt_concilia.dados_conc      = 'Empresa do Lote Destino diferente da Empresa '.
                    NEXT.
                END.
            END.
            ELSE DO:

                if not tt_param.valida_origem then
                    if not(can-find(first lancto_ctbl of lote_ctbl where lancto_ctbl.num_lancto_Ctbl = es_cons_lotes.num_lancto_orig )) then next.

                CREATE tt_concilia.
                ASSIGN tt_concilia.empresa         = lote_ctbl.cod_empresa
                       tt_concilia.num_lote_orig   = lote_ctbl.num_lote_ctbl
                       tt_concilia.num_lancto_orig = es_cons_lotes.num_lancto_orig
                       tt_concilia.cod_modul_orig  = es_cons_lotes.cod_mod_orig
                       tt_concilia.des_lote_orig   = lote_ctbl.des_lote_ctbl
                       tt_concilia.dados_conc      = es_cons_lotes.resultado.
    
                FIND FIRST lancto_ctbl OF lote_ctbl WHERE lancto_Ctbl.num_lancto_ctbl = es_cons_lotes.num_lancto_orig NO-LOCK NO-ERROR.
                IF AVAIL lancto_ctbl THEN DO:
                    
                    FIND FIRST tot_lancto_ctbl OF lancto_ctbl WHERE (tot_lancto_ctbl.val_lancto_ctbl_cr > 0 
                                                                 OR  tot_lancto_ctbl.val_lancto_ctbl_db > 0 ) NO-LOCK NO-ERROR.
                    ASSIGN tt_concilia.val_lancto_cr_orig = IF AVAIL tot_lancto_ctbl THEN tot_lancto_ctbl.val_lancto_ctbl_cr ELSE 0.
                    NEXT.                    
                end.
            END.

            FIND FIRST lancto_ctbl OF lote_ctbl WHERE lancto_Ctbl.num_lancto_ctbl = es_cons_lotes.num_lancto_orig NO-LOCK NO-ERROR.
            IF AVAIL lancto_ctbl THEN DO:

                RUN pi-acompanhar in h-acomp ("Lancto: " + STRING(lancto_ctbl.num_lancto_ctbl)).
                
                FIND FIRST tot_lancto_ctbl OF lancto_ctbl WHERE (tot_lancto_ctbl.val_lancto_ctbl_cr > 0 
                                                             OR  tot_lancto_ctbl.val_lancto_ctbl_db > 0 ) NO-LOCK NO-ERROR.

                FIND FIRST bf_lancto_ctbl OF bf_lote_ctbl WHERE bf_lancto_ctbl.num_lancto_Ctbl = lancto_ctbl.num_lancto_ctbl NO-LOCK NO-ERROR.
                IF AVAIL bf_lancto_ctbl THEN DO:

                    FIND FIRST bf_tot_lancto_ctbl OF bf_lancto_ctbl WHERE (bf_tot_lancto_ctbl.val_lancto_ctbl_cr > 0 
                                                                       OR  bf_tot_lancto_ctbl.val_lancto_ctbl_db > 0 ) NO-LOCK NO-ERROR.

                    CREATE tt_concilia.
                    ASSIGN tt_concilia.empresa             = lote_ctbl.cod_empresa
                           tt_concilia.num_lote_orig       = lote_ctbl.num_lote_ctbl
                           tt_concilia.num_lote_dest       = bf_lote_ctbl.num_lote_ctbl
                           tt_concilia.num_lancto_orig     = es_cons_lotes.num_lancto_orig
                           tt_concilia.num_lancto_Dest     = es_cons_lotes.num_lancto_dest
                           tt_concilia.cod_modul_orig      = es_cons_lotes.cod_mod_orig
                           tt_concilia.des_lote_orig       = lote_ctbl.des_lote_ctbl
                           tt_concilia.des_lote_dest       = bf_lote_ctbl.des_lote_ctbl
                           tt_concilia.dt_lancto_orig      = lancto_ctbl.dat_lancto_ctbl
                           tt_concilia.dt_lancto_dest      = bf_lancto_ctbl.dat_lancto_ctbl
                           tt_concilia.cod_modul_dest      = bf_lancto_ctbl.cod_modul_dts
                           tt_concilia.val_lancto_cr_orig  = IF AVAIL tot_lancto_ctbl THEN tot_lancto_ctbl.val_lancto_ctbl_cr ELSE 0
                           tt_concilia.val_lancto_cr_dest  = IF AVAIL bf_tot_lancto_ctbl THEN bf_tot_lancto_ctbl.val_lancto_ctbl_cr ELSE 0.                       
                END.
                ELSE DO:
                    CREATE tt_concilia.
                    ASSIGN tt_concilia.empresa             = lote_ctbl.cod_empresa
                           tt_concilia.num_lote_orig       = lote_ctbl.num_lote_ctbl
                           tt_concilia.num_lote_dest       = bf_lote_ctbl.num_lote_ctbl
                           tt_concilia.num_lancto_orig     = es_cons_lotes.num_lancto_orig
                           tt_concilia.num_lancto_Dest     = es_cons_lotes.num_lancto_dest
                           tt_concilia.cod_modul_orig      = es_cons_lotes.cod_mod_orig
                           tt_concilia.des_lote_orig       = lote_ctbl.des_lote_ctbl
                           tt_concilia.des_lote_dest       = bf_lote_ctbl.des_lote_ctbl
                           tt_concilia.dt_lancto_orig      = lancto_ctbl.dat_lancto_ctbl
                           tt_concilia.dt_lancto_dest      = ?
                           tt_concilia.val_lancto_cr_orig  = IF AVAIL tot_lancto_ctbl THEN tot_lancto_ctbl.val_lancto_ctbl_cr ELSE 0
                           tt_concilia.val_lancto_cr_dest  = 0
                           tt_concilia.dados_conc          = 'Lan‡amento Destino nÆo encontrado'. 
                END.
            END.
        END.            
    END.

end procedure.

PROCEDURE pi-verif-lote-ctbl-orig:
    
    FOR EACH lote_ctbl WHERE lote_ctbl.cod_empresa   >= tt_param.c_empresa_orig_ini
                         AND lote_ctbl.cod_empresa   <= tt_param.c_empresa_orig_fim 
                         AND lote_ctbl.num_lote_ctbl >= tt_param.i_lote_orig_ini 
                         AND lote_ctbl.num_lote_ctbl <= tt_param.i_lote_orig_fim 
                         and lote_ctbl.cod_empresa <> tt_param.c_empresa_dest NO-LOCK,
        EACH lancto_ctbl OF lote_ctbl NO-LOCK WHERE (NOT (CAN-FIND(FIRST es_cons_lotes WHERE es_cons_lotes.cod_empresa_orig = lancto_ctbl.cod_empresa
                                                                                         AND es_cons_lotes.num_lote_orig    = lancto_ctbl.num_lote_ctbl
                                                                                         AND es_cons_lotes.num_lancto_orig  = lancto_ctbl.num_lancto_ctbl)))
                                                AND lancto_ctbl.dat_lancto_ctbl >= tt_param.dt_lanca_orig_ini
                                                AND lancto_ctbl.dat_lancto_ctbl <= tt_param.dt_lanca_orig_fim
                                                AND lancto_ctbl.cod_cenar_ctbl  >= tt_param.c_cenario_ini
                                                AND lancto_ctbl.cod_cenar_ctbl  <= tt_param.c_cenario_fim
                                                AND lancto_ctbl.cod_modul_dts   >= tt_param.c_modulo_ini
                                                AND lancto_ctbl.cod_modul_dts   <= tt_param.c_modulo_fim:

        RUN pi-acompanhar in h-acomp ("Localizando Lotes Origem nÆo enviados: " + STRING(lancto_ctbl.num_lancto_ctbl)).

        FIND FIRST tot_lancto_ctbl OF lancto_ctbl WHERE (tot_lancto_ctbl.val_lancto_ctbl_cr > 0 
                                                     OR  tot_lancto_ctbl.val_lancto_ctbl_db > 0 ) NO-LOCK NO-ERROR.

        CREATE tt_concilia.
        ASSIGN tt_concilia.empresa             = lote_ctbl.cod_empresa
               tt_concilia.num_lote_orig       = lote_ctbl.num_lote_ctbl
               tt_concilia.num_lancto_orig     = lancto_ctbl.num_lancto_ctbl
               tt_concilia.cod_modul_orig      = lancto_ctbl.cod_modul_dts
               tt_concilia.des_lote_orig       = lote_ctbl.des_lote_ctbl
               tt_concilia.dt_lancto_orig      = lancto_ctbl.dat_lancto_ctbl
               tt_concilia.val_lancto_cr_orig  = IF AVAIL tot_lancto_ctbl THEN tot_lancto_ctbl.val_lancto_ctbl_cr ELSE 0
               tt_concilia.val_lancto_cr_dest  = 0
               tt_concilia.dados_conc          = 'Lote ou Lan‡amento nÆo enviado … empresa destino'.

    END.

END PROCEDURE.

PROCEDURE pi-verif-lote-ctbl-dest:

    FOR EACH lote_ctbl WHERE lote_ctbl.cod_empresa = tt_param.c_empresa_Dest
                         AND lote_ctbl.num_lote_ctbl >= tt_param.i_lote_dest_ini 
                         AND lote_ctbl.num_lote_ctbl <= tt_param.i_lote_dest_fim NO-LOCK,
        EACH lancto_ctbl OF lote_ctbl NO-LOCK WHERE (NOT (CAN-FIND(FIRST es_cons_lotes WHERE es_cons_lotes.cod_empresa_dest = lancto_ctbl.cod_empresa
                                                                                         AND es_cons_lotes.num_lote_dest    = lancto_ctbl.num_lote_ctbl
                                                                                         AND es_cons_lotes.num_lancto_dest  = lancto_ctbl.num_lancto_ctbl)))
                                                AND lancto_ctbl.dat_lancto_ctbl >= tt_param.dt_lanca_orig_ini
                                                AND lancto_ctbl.dat_lancto_ctbl <= tt_param.dt_lanca_orig_fim
                                                AND lancto_ctbl.cod_cenar_ctbl  >= tt_param.c_cenario_ini
                                                AND lancto_ctbl.cod_cenar_ctbl  <= tt_param.c_cenario_fim
                                                AND lancto_ctbl.cod_modul_dts   >= tt_param.c_modulo_ini
                                                AND lancto_ctbl.cod_modul_dts   <= tt_param.c_modulo_fim:

        RUN pi-acompanhar in h-acomp ("Localizando Lotes Destino nÆo enviados: " + STRING(lancto_ctbl.num_lancto_ctbl)).

        CREATE tt_concilia.
        ASSIGN tt_concilia.empresa         = ''
               tt_concilia.num_lote_dest   = lote_ctbl.num_lote_ctbl
               tt_concilia.num_lancto_dest = lancto_ctbl.num_lancto_ctbl
               tt_concilia.cod_modul_dest  = lancto_ctbl.cod_modul_dts
               tt_concilia.des_lote_dest   = lote_ctbl.des_lote_ctbl
               tt_concilia.dados_conc      = 'Lote ou Lan‡amento Origem nÆo encontrado'.

    END.

END PROCEDURE.

procedure pi-imprime :

    def var c-arquivo   as char format "x(30)" no-undo.
    def var c-key-value as char                no-undo.
    DEF VAR v_des_mensagem AS CHARACTER NO-UNDO.
    DEF VAR v_des_ajuda    AS CHARACTER NO-UNDO.

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "esfgl073.txt".

    OUTPUT stream str-rp to value(c-arquivo) NO-ECHO NO-CONVERT.

    PUT STREAM str-rp 'EMPRESA'                   AT 01 
                      'LOTE ORIGEM'               AT 10
                      'MODULO ORIGEM'             AT 23
                      'DESCRI€ÇO LOTE ORIGEM'     AT 38
                      'LANCTO ORIG'               AT 65
                      'DATA LANC ORIGEM'          AT 78
                      'VAL LANCTO DB/CR ORIGEM'   AT 98
                      'LOTE DESTINO'	          AT 123
                      'MODULO DESTINO'            AT 137
                      'DESCRI€ÇO LOTE DESTINO'    AT 155
                      'LANCTO DEST'               AT 183
                      'DATA LANC DESTINO'	      AT 195
                      'VAL LANCTO DB/CR DESTINO'  AT 215
                      'DIFEREN€A'                 AT 247
                      'OBSERVA€ÇO'                AT 262
                      FILL('-',280) FORMAT 'X(280)' AT 01 SKIP.
    
    FOR EACH tt_concilia NO-LOCK BY tt_concilia.empresa
                                 BY tt_concilia.num_lote_orig
                                 BY tt_concilia.num_lancto_orig
                                 BY tt_concilia.dt_lancto_orig:

        RUN pi-acompanhar in h-acomp ("Imprimindo Lote: " + STRING(tt_concilia.num_lote_orig)).

        PUT  stream str-rp
                    tt_concilia.empresa                         AT 01                  
                    tt_concilia.num_lote_orig                   AT 8
                    tt_concilia.cod_modul_orig                  AT 28
                    tt_concilia.des_lote_orig    FORMAT 'X(25)' AT 38
                    tt_concilia.num_lancto_orig                 AT 64
                    tt_concilia.dt_lancto_orig                  AT 80
                    tt_concilia.val_lancto_cr_orig              AT 100
                    tt_concilia.num_lote_dest                   AT 124
                    tt_concilia.cod_modul_dest                  AT 140
                    tt_concilia.des_lote_dest   FORMAT 'X(25)'  AT 155           
                    tt_concilia.num_lancto_dest                 AT 183
                    tt_concilia.dt_lancto_dest                  AT 198
                    tt_concilia.val_lancto_cr_dest              AT 217
                    ( tt_concilia.val_lancto_cr_dest -  tt_concilia.val_lancto_cr_orig) FORMAT '->>>,>>>,>>>,>>9.99' AT 235.

        IF NUM-ENTRIES(tt_concilia.dados_conc,'#') > 1 THEN DO:
            DO i = 1 TO NUM-ENTRIES(tt_concilia.dados_conc):
                RUN pi_msg_lote_ctbl_recebto_1 (INPUT INTEGER(ENTRY(i,tt_concilia.dados_conc,'#')),
                                                OUTPUT v_des_mensagem,
                                                OUTPUT v_des_ajuda).
                PUT STREAM str-rp UNFORMATTED ENTRY(i,tt_concilia.dados_conc,'#') + ' ' + v_des_mensagem + ' - ' +  v_des_ajuda AT 262.
                 
                IF i < NUM-ENTRIES(tt_concilia.dados_conc,'#') THEN PUT STREAM str-rp SKIP.                
            END.            
        END.
        ELSE PUT STREAM str-rp UNFORMATTED tt_concilia.dados_conc  AT 262     SKIP .
    END.
    OUTPUT stream str-rp CLOSE.

    ASSIGN c-key-value = "Notepad.exe":U.

    run winexec (input c-key-value + chr(32) + c-arquivo, input 1).
end procedure.

procedure WinExec external "kernel32.dll":U:
    def input param prg_name  as char.
    def input param prg_style as short.
end procedure.


PROCEDURE pi_msg_lote_ctbl_recebto_1:

    def Input param p_num_mensagem  as INTEGER   format ">>>>,>>9" no-undo.
    def output param p_des_mensagem as CHARACTER format "x(50)"    no-undo.
    def output param p_des_ajuda    as CHARACTER format "x(50)"    no-undo.

    ASSIGN v_num_mensagem = p_num_mensagem.
    RUN pi_view_message.
    ASSIGN p_des_mensagem = v_des_message_view
           p_des_ajuda    = v_des_ajuda_view. 

    
END PROCEDURE. /* pi_msg_lote_ctbl_recebto_1 */

PROCEDURE pi_messages: 

    def input param c_action    as char    no-undo. 
    def input param i_msg       as integer no-undo. 
    def input param c_param     as char    no-undo. 

    def var c_prg_msg           as char    no-undo. 

    assign c_prg_msg = "messages/" 
                     + string(trunc(i_msg / 1000,0),"99") 
                     + "/msg" 
                     + string(i_msg, "99999"). 

    if search(c_prg_msg + ".r") = ? and search(c_prg_msg + ".p") = ? then do: 
        message "Mensagem nr. " i_msg "!!!" skip 
                "Programa Mensagem" c_prg_msg "n’o encontrado." 
                view-as alert-box error. 
        return error. 
    end. 

    run value(c_prg_msg + ".p") (input c_action, input c_param). 
    return return-value. 
END PROCEDURE.

PROCEDURE pi_view_message: 

    assign v_cod_msg = 'messages/' 
                     + string(trunc(v_num_mensagem / 1000,0),'99') 
                     + '/msg' 
                     + string(v_num_mensagem, '99999'). 

    if  search(v_cod_msg + '.r') = ? and search(v_cod_msg + '.p') = ? 
    then do: 
        /* A mensagem &1 n’o estÿ dispon­vel em seu ambiente ! */ 
        run pi_messages (input "show", 
                         input 3530, 
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", 
                                           v_num_mensagem)) /*msg_3530*/. 
        assign v_des_ajuda_view = ''
               v_des_message_view = ''
               v_ind_tip_msg = ''.
        return 'nok'. 
    end /* if */. 

    run pi_messages (input "type", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_ind_tip_msg = return-value. 
    run pi_messages (input "msg", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_des_message_view = return-value. 
    run pi_messages (input "help", 
                     input v_num_mensagem, 
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")). 
    assign v_des_ajuda_view = return-value. 
END PROCEDURE.

