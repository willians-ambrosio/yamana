{esp/apb900zb.i new}

 DEF INPUT PARAM p_cod-matriz-trad-org-ext AS CHAR                       NO-UNDO.
 DEF INPUT PARAM p-docum-est               AS ROWID                      NO-UNDO.
 DEF INPUT PARAM p-embarque                LIKE embarque-imp.embarque    NO-UNDO.
 DEF INPUT PARAM p-est-embarque            LIKE embarque-imp.cod-estabel NO-UNDO.
 DEF INPUT PARAM p-tipo                    AS CHAR FORMAT "X(1)"         NO-UNDO.
/*
 MESSAGE 
     "p_cod-matriz-trad-org-ext " p_cod-matriz-trad-org-ext skip
     "p-docum-est               " STRING(p-docum-est)       skip
     "p-embarque                " p-embarque                skip
     "p-est-embarque            " p-est-embarque            skip
     "p-tipo                    " p-tipo                    skip
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
 DEF VAR c_nro-docto               LIKE dupli-apagar.nro-docto NO-UNDO.
 DEF VAR v_rec_lote                AS RECID    NO-UNDO. 
 DEF VAR v_rec_item                AS RECID    NO-UNDO.
 def var v_cod_matriz_trad_org_ext as char     NO-UNDO INITIAL "".
 DEF VAR v_hdl_aux                 AS HANDLE   NO-UNDO.
 DEFINE VARIABLE c-referencia AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE i_N AS INTEGER     NO-UNDO.
 DEF VAR c-comple   AS CHAR NO-UNDO.
 DEF VAR c-nro-tit LIKE tit_ap.cod_tit_ap NO-UNDO.  

 EMPTY TEMP-TABLE tt_log_erros_atualiz.
 EMPTY TEMP-TABLE tt_integr_apb_lote_impl.
 EMPTY TEMP-TABLE tt_integr_apb_item_lote_impl3v.
 EMPTY TEMP-TABLE tt_integr_apb_aprop_ctbl_pend.
 EMPTY TEMP-TABLE tt_integr_apb_impto_impl_pend.
 EMPTY TEMP-TABLE tt_integr_apb_abat_antecip_vouc . 
 EMPTY TEMP-TABLE tt_integr_apb_abat_prev_provis .  
 EMPTY TEMP-TABLE tt_integr_apb_aprop_relacto .     
 EMPTY TEMP-TABLE tt_integr_apb_item_lote_impl .    
 EMPTY TEMP-TABLE tt_integr_apb_relacto_pend .      
 EMPTY TEMP-TABLE tt_params_generic_api .           
 EMPTY TEMP-TABLE tt_integr_apb_relacto_pend_aux .  
 EMPTY TEMP-TABLE tt_integr_apb_aprop_relacto_1 . 

 assign i_N = i_N + 1.

 assign c-referencia = "F" + 
                                substring(string(year(today),"9999"),3,2) +                                             
                                entry(month(today),"A,B,C,D,E,F,G,H,I,J,K,L") +                                         
                                entry(day(today),  "0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U") +   
/*                                 string(time,"99999") + */
                                string(i_N,"99999").
assign 
  c-referencia = TRIM(STRING(TIME)).

/*=========================================================================================================*/
FIND FIRST docum-est
  NO-LOCK
  WHERE ROWID(docum-est) = p-docum-est
  NO-ERROR.
/*=========================================================================================================*/

RUN pi-nro-doc.

ASSIGN c_nro-docto = c-nro-tit.

 release tt_integr_apb_abat_antecip_vouc .
          release tt_integr_apb_abat_prev_provis .
          release tt_integr_apb_aprop_ctbl_pend .
          release tt_integr_apb_aprop_relacto .
          release tt_integr_apb_impto_impl_pend .
          release tt_integr_apb_item_lote_impl .
          release tt_integr_apb_lote_impl .
          release tt_integr_apb_relacto_pend .
          release tt_log_erros_atualiz .
          release tt_integr_apb_item_lote_impl3v .
          release tt_params_generic_api .
          release tt_integr_apb_relacto_pend_aux .
          release tt_integr_apb_aprop_relacto_1 .


          /*=========================================================================================================*/
          FIND FIRST docum-est
            NO-LOCK
            WHERE ROWID(docum-est) = p-docum-est
            NO-ERROR.
          /*=========================================================================================================*/
          FOR EACH dupli-apagar 
            OF docum-est
            NO-LOCK:

          CREATE tt_integr_apb_lote_impl.
          ASSIGN tt_integr_apb_lote_impl.tta_cod_estab                = "301"
                 tt_integr_apb_lote_impl.tta_cod_refer                = c-referencia
                 tt_integr_apb_lote_impl.tta_dat_transacao            = TODAY
                 tt_integr_apb_lote_impl.tta_ind_origin_tit_ap        = "APB"
                 tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap = dupli-apagar.vl-a-pagar /* 20000 */
                 tt_integr_apb_lote_impl.tta_cod_empresa              = "301"
                 tt_integr_apb_lote_impl.tta_cod_indic_econ           = "Real".

          CREATE tt_integr_apb_item_lote_impl3v.
          ASSIGN tt_integr_apb_item_lote_impl3v.ttv_rec_integr_apb_lote_impl      = RECID(tt_integr_apb_lote_impl)
                 tt_integr_apb_item_lote_impl3v.ttv_rec_integr_apb_item_lote      = recid(tt_integr_apb_item_lote_impl3v)
                 tt_integr_apb_item_lote_impl3v.tta_num_seq_refer                 = 10
                  tt_integr_apb_item_lote_impl3v.tta_cdn_fornecedor               = dupli-apagar.cod-emitente
                  tt_integr_apb_item_lote_impl3v.tta_cod_espec_docto              = dupli-apagar.cod-esp
                  tt_integr_apb_item_lote_impl3v.tta_cod_ser_docto                = dupli-apagar.serie-docto 
                  tt_integr_apb_item_lote_impl3v.tta_cod_tit_ap                   = c_nro-docto /* dupli-apagar.nro-docto */
                  tt_integr_apb_item_lote_impl3v.tta_cod_parcela                  = dupli-apagar.parcela
                  tt_integr_apb_item_lote_impl3v.tta_dat_emis_docto               = dupli-apagar.dt-emissao
                  tt_integr_apb_item_lote_impl3v.tta_dat_vencto_tit_ap            = dupli-apagar.dt-vencim
                  tt_integr_apb_item_lote_impl3v.tta_dat_prev_pagto               = dupli-apagar.dt-vencim
                  tt_integr_apb_item_lote_impl3v.tta_dat_desconto                 = ?
                  
                  tt_integr_apb_item_lote_impl3v.tta_val_tit_ap                   = dupli-apagar.vl-a-pagar
                  tt_integr_apb_item_lote_impl3v.tta_val_desconto                 = 0
                  tt_integr_apb_item_lote_impl3v.tta_val_perc_desc                = 0
                  tt_integr_apb_item_lote_impl3v.tta_num_dias_atraso              = 0
                  tt_integr_apb_item_lote_impl3v.tta_val_juros_dia_atraso         = 0
                  tt_integr_apb_item_lote_impl3v.tta_val_perc_juros_dia_atraso    = 0
                  tt_integr_apb_item_lote_impl3v.tta_val_perc_multa_atraso        = 0
    
                  tt_integr_apb_item_lote_impl3v.tta_cod_portador                 = "1"
                  tt_integr_apb_item_lote_impl3v.tta_cod_indic_econ               = "Real"
    
                  tt_integr_apb_item_lote_impl3v.tta_cod_finalid_econ_ext         = ""
                  tt_integr_apb_item_lote_impl3v.tta_cod_portad_ext               = ""
                
    
                  tt_integr_apb_item_lote_impl3v.tta_cod_apol_seguro              = ""
                  tt_integr_apb_item_lote_impl3v.tta_cod_seguradora               = ""
                  tt_integr_apb_item_lote_impl3v.tta_cod_arrendador               = ""
                  tt_integr_apb_item_lote_impl3v.tta_cod_contrat_leas             = ""
                  tt_integr_apb_item_lote_impl3v.tta_des_text_histor              = "Nota Fiscal: " + c_nro-docto
                  tt_integr_apb_item_lote_impl3v.tta_cod_cart_bcia                = "APB"
                  tt_integr_apb_item_lote_impl3v.tta_cod_forma_pagto              = "002"
                  tt_integr_apb_item_lote_impl3v.tta_val_cotac_indic_econ         = 1
                  tt_integr_apb_item_lote_impl3v.ttv_num_ord_invest               = 0.

          create tt_integr_apb_aprop_ctbl_pend.
          ASSIGN tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote     = recid(tt_integr_apb_item_lote_impl3v)
                 tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend         = ?
                 tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend    = ?
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_cta_ctbl           = "CONTSOC"
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl                 = "17202002"
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc               = "00"
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_ccusto             = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto                   = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ         = "220.11"
                 tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl               = dupli-apagar.vl-a-pagar
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_pais                     = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_federac             = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_imposto                  = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_classif_impto            = ""
                 tt_integr_apb_aprop_ctbl_pend.ttv_cod_tip_fluxo_financ_ext     = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl_ext             = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_sub_cta_ctbl_ext         = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto_ext               = ""
                 tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc_ext           = "".

    FOR EACH dupli-imp NO-LOCK OF dupli-apagar:

          CREATE tt_integr_apb_impto_impl_pend.
          /*
          MESSAGE dupli-imp.int-1
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          */    
          ASSIGN 
            tt_integr_apb_impto_impl_pend.ttv_rec_integr_apb_item_lote   = recid(tt_integr_apb_item_lote_impl3v)
            tt_integr_apb_impto_impl_pend.ttv_rec_antecip_pef_pend       = ?
            tt_integr_apb_impto_impl_pend.tta_cod_imposto                = STRING(dupli-imp.int-1)
            tt_integr_apb_impto_impl_pend.tta_cod_classif_impto          = STRING(dupli-imp.int-1)
            tt_integr_apb_impto_impl_pend.tta_cod_espec_docto            = dupli-imp.cod-esp
            tt_integr_apb_impto_impl_pend.tta_cod_ser_docto              = dupli-imp.serie-docto
            tt_integr_apb_impto_impl_pend.tta_cod_tit_ap                 = c_nro-docto /* dupli-apagar.nro-docto */
            tt_integr_apb_impto_impl_pend.tta_cod_parcela                = dupli-apagar.parcela
            tt_integr_apb_impto_impl_pend.tta_val_imposto                = dupli-imp.vl-imposto /*dupli-imp.rend-trib*/
            tt_integr_apb_impto_impl_pend.tta_val_base_liq_impto         = dupli-apagar.vl-a-pagar
            tt_integr_apb_impto_impl_pend.tta_val_rendto_tribut          = dupli-apagar.vl-a-pagar
            tt_integr_apb_impto_impl_pEND.tta_cod_indic_econ             = "Real"
            NO-ERROR.
          FIND emitente
            NO-LOCK
            WHERE emitente.cod-emitente = dupli-apagar.cod-emitente
            NO-ERROR.
        
          ASSIGN 
            tt_integr_apb_impto_impl_pend.tta_cod_pais         = "BRA"        
            tt_integr_apb_impto_impl_pend.tta_cod_unid_federac = "" /* emitente.estado */.

          FIND FIRST imposto 
            NO-LOCK
            WHERE imposto.cod_pais    = tt_integr_apb_impto_impl_pend.tta_cod_pais
            AND   imposto.cod_imposto = STRING(dupli-imp.int-1)
            NO-ERROR.

          IF AVAIL imposto THEN
            ASSIGN tt_integr_apb_impto_impl_pend.tta_cod_unid_federac = imposto.cod_unid_federac.

          FOR FIRST classif_impto FIELDS(val_aliq_impto) NO-LOCK
            WHERE classif_impto.cod_pais            = tt_integr_apb_impto_impl_pend.tta_cod_pais    
            AND   classif_impto.cod_unid_feder      = tt_integr_apb_impto_impl_pend.tta_cod_unid_federac                                           
            AND   classif_impto.cod_imposto         = tt_integr_apb_impto_impl_pend.tta_cod_imposto 
            AND   classif_impto.cod_classif_impto   = tt_integr_apb_impto_impl_pend.tta_cod_classif_impto:
              /*
              MESSAGE classif_impto.val_aliq_impto
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              */    
            ASSIGN tt_integr_apb_impto_impl_pend.tta_val_aliq_impto = classif_impto.val_aliq_impto.
          END.
      
          VALIDATE tt_integr_apb_impto_impl_pend.

        END. /* FOR EACH dupli-imp NO-LOCK OF dupli-apagar: */
  END.


  release tt_integr_apb_lote_impl.
  release tt_integr_apb_item_lote_impl3v.
  release tt_integr_apb_aprop_ctbl_pend.
  release tt_integr_apb_abat_prev_provis.


  RUN prgfin/apb/apb900zg.r PERSISTENT SET v_hdl_aux NO-ERROR.
  RUN pi_main_block_api_tit_ap_cria_4 IN v_hdl_aux (INPUT  5,
                                                    INPUT v_cod_matriz_trad_org_ext,
                                                    INPUT-OUTPUT TABLE tt_integr_apb_item_lote_impl3v).

  DELETE PROCEDURE v_hdl_aux.
/* OUTPUT TO d:\temp\bizna.txt.                                                       */
/* FOR EACH tt_log_erros_atualiz :                                                    */
/*   PUT                                                                              */
/*    "Estab:"   + STRING(tt_log_erros_atualiz.tta_cod_estab)                         */
/*       + ", ref.: " + STRING(tt_log_erros_atualiz.tta_cod_refer)                    */
/*       + " " + string(tt_log_erros_atualiz.tta_num_seq_refer)                       */
/*       + ",num erro: " + STRING(tt_log_erros_atualiz.ttv_num_mensagem)              */
/*       + ", erro: " + STRING(tt_log_erros_atualiz.ttv_des_msg_erro)                 */
/*       + ", "       + STRING(tt_log_erros_atualiz.ttv_des_msg_ajuda)                */
/*       + ", "       + STRING(tt_log_erros_atualiz.ttv_ind_tip_relacto)              */
/*       + ", "       + STRING(tt_log_erros_atualiz.ttv_num_relacto) FORMAT "X(500)". */
/*   PUT SKIP.                                                                        */
/* END.                                                                               */
/* OUTPUT CLOSE.                                                                      */
/* DOS SILENT START notepad.exe d:\temp\bizna.txt.                                    */
PROCEDURE pi-nro-doc:  
  ASSIGN c-nro-tit = "".
  FOR EACH tit_ap
    NO-LOCK
    WHERE tit_ap.cod_estab         = p-est-embarque
    AND   tit_ap.cdn_fornecedor    = docum-est.cod-emitente
    /*  AND   tit_ap.cod_espec_docto   = TRIM(STRING(docum-est.esp-docto)) */
    AND   tit_ap.cod_ser_docto     = docum-est.serie-docto
    AND   tit_ap.cod_tit_ap        BEGINS p-embarque + TRIM(c-comple)
    AND   tit_ap.cod_parcela       = "1":
      IF tit_ap.cod_tit_ap > c-nro-tit THEN
         ASSIGN c-nro-tit = tit_ap.cod_tit_ap.    
    END.
  IF c-nro-tit = "" THEN
    DO:
      ASSIGN c-nro-tit = p-embarque + TRIM(STRING(p-tipo,"X(1)")) + "1".
    END.
  ELSE
    DO:
      IF p-tipo = "S" THEN
        DO:
          ASSIGN 
            c-nro-tit = ENTRY(2,c-nro-tit,"S")
            c-nro-tit = p-embarque + TRIM(STRING(p-tipo,"X(1)")) + TRIM(STRING(INT(c-nro-tit) + 1)).
        END.
      IF p-tipo = "F" THEN
        DO:
          ASSIGN 
            c-nro-tit = ENTRY(2,c-nro-tit,"F")
            c-nro-tit = p-embarque + TRIM(STRING(p-tipo,"X(1)")) + TRIM(STRING(INT(c-nro-tit) + 1)).
        END.
    END.
END PROCEDURE.
