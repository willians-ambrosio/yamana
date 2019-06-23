/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: sea_contrat_apf
** Descricao.............: Pesquisa Pai contrat_apf
** Versao................:  1.00.00.005
** Procedimento..........: man_contrat_apf
** Nome Externo..........: prgint/utb/utb098ka.p
** Data Geracao..........: 10/01/2008 - 16:43:22
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: dalpra
** Alterado em...........: 04/01/1999 17:50:16
** Gerado por............: fut41420
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.005":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  sea_contrat_apf UTB}
&ENDIF

{include/i_fcldef.i}


/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "5.01" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "SEA_contrat_apf","~~EMSUNI", "~~{~&emsuni_version}", "~~5.01")) /*msg_5009*/.
&else

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)"
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)"
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def var v_cod_dat_type
    as character
    format "x(8)"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)"
    label "Usuÿrio"
    column-label "Usuÿrio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)"
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)"
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def var v_cod_final
    as character
    format "x(8)"
    initial ?
    label "Final"
    no-undo.
def var v_cod_format
    as character
    format "x(8)"
    label "Formato"
    column-label "Formato"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)"
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)"
    label "Grupo Usuÿrios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)"
    label "Idioma"
    column-label "Idioma"
    no-undo.
def var v_cod_initial
    as character
    format "x(8)"
    initial ?
    label "Inicial"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)"
    label "M½dulo Corrente"
    column-label "M½dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)"
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)"
    label "Pa­s Empresa Usuÿrio"
    column-label "Pa­s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)"
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)"
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg½cio"
    column-label "Unid Neg½cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)"
    label "Usuÿrio Corrente"
    column-label "Usuÿrio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)"
    no-undo.
def var v_nom_attrib
    as character
    format "x(30)"
    no-undo.
def var v_nom_prog_appc
    as character
    format "x(50)"
    label "Programa APPC"
    column-label "Programa APPC"
    no-undo.
def var v_nom_prog_dpc
    as character
    format "x(50)"
    label "Programa Dpc"
    column-label "Programa Dpc"
    no-undo.
def var v_nom_prog_upc
    as character
    format "X(50)"
    label "Programa UPC"
    column-label "Programa UPC"
    no-undo.
def var v_nom_table_epc
    as character
    format "x(30)"
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9"
    no-undo.
def new global shared var v_rec_contrat_apf
    as recid
    format ">>>>>>9"
    initial ?
    no-undo.
def new global shared var v_rec_layout_cheq
    as recid
    format ">>>>>>9"
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9"
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9"
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9"
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9"
    no-undo.


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conteœdo"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_sea_contrat_apf
    for contrat_apf
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_sea_contrat_apf query qr_sea_contrat_apf display 
    contrat_apf.cod_contrat_apf
    width-chars 30.00
    contrat_apf.des_contrat_apf
    width-chars 30.00
    
    with no-box separators single 
         size 71.00 by 07.00
         font 1
         bgcolor 15.


/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_cxcl
    size 1 by 1
    edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_add2
    label "Inclui"
    tooltip "Inclui"
    size 1 by 1.
def button bt_agenc_bcia
    label "Ag¼ncias"
    tooltip "Ag¼ncias Bancÿrias"
    size 1 by 1.
def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_det2
    label "Detalhe"
    tooltip "Detalhe"
    size 1 by 1.
def button bt_fil
    label "Filtro"
    tooltip "Filtro"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.

def button bt_mod2
    label "Modifica"
    tooltip "Modifica"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_ran
    label "Faixa"
    tooltip "Faixa"
    size 1 by 1.
def button bt_ran15
    label "Faixa"
    tooltip "Faixa"
    size 1 by 1.
/****************************** Function Button *****************************/


/*************************** Button Definition End **************************/

/************************ Radio-Set Definition Begin ************************/

def var rs_sea_contrat_apf
    as character
    initial "Por contrat_apf"
    view-as radio-set Horizontal
    radio-buttons "Por contrat_apf", "Por contrat_apf","Por Nome", "Por Nome"
     /*l_por_contrat_apf*/ /*l_por_contrat_apf*/ /*l_por_nome*/ /*l_por_nome*/
    bgcolor 15 
    no-undo.


/************************* Radio-Set Definition End *************************/

/************************** Frame Definition Begin **************************/

def frame f_sea_03_contrat_apf
    rt_cxcf
         at row 11.25 col 02.00 bgcolor 7 
    rt_cxcl
         at row 01.00 col 01.00 bgcolor 15 
    rs_sea_contrat_apf
         at row 01.21 col 03.00
         help "" no-label
    br_sea_contrat_apf
         at row 02.25 col 01.00
    bt_add2
         at row 09.75 col 02.00 font ?
         help "Inclui"
    bt_mod2
         at row 09.75 col 14.00 font ?
         help "Modifica"
    bt_det2
         at row 09.75 col 26.00 font ?
         help "Detalhe"
    bt_agenc_bcia
         at row 09.75 col 38.00 font ?
         help "Ag¼ncias Bancÿrias"
    
    bt_ran
         at row 09.75 col 50.00 font ?
         help "Faixa"
    bt_fil
         at row 09.75 col 62.00 font ?
         help "Filtro"
    bt_ran15
         at row 09.75 col 62.00 font ?
         help "Faixa"
    bt_ok
         at row 11.46 col 03.00 font ?
         help "OK"
    bt_can
         at row 11.46 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 11.46 col 60.14 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 72.57 by 13.08 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Pesquisa contrat_apfs".
    /* adjust size of objects in this frame */
    assign bt_add2:width-chars          in frame f_sea_03_contrat_apf = 10.00
           bt_add2:height-chars         in frame f_sea_03_contrat_apf = 01.00
           bt_agenc_bcia:width-chars    in frame f_sea_03_contrat_apf = 10.00
           bt_agenc_bcia:height-chars   in frame f_sea_03_contrat_apf = 01.00
           bt_can:width-chars           in frame f_sea_03_contrat_apf = 10.00
           bt_can:height-chars          in frame f_sea_03_contrat_apf = 01.00
           bt_det2:width-chars          in frame f_sea_03_contrat_apf = 10.00
           bt_det2:height-chars         in frame f_sea_03_contrat_apf = 01.00
           bt_fil:width-chars           in frame f_sea_03_contrat_apf = 10.00
           bt_fil:height-chars          in frame f_sea_03_contrat_apf = 01.00
           bt_hel2:width-chars          in frame f_sea_03_contrat_apf = 10.00
           bt_hel2:height-chars         in frame f_sea_03_contrat_apf = 01.00
           
           
           bt_mod2:width-chars          in frame f_sea_03_contrat_apf = 10.00
           bt_mod2:height-chars         in frame f_sea_03_contrat_apf = 01.00
           bt_ok:width-chars            in frame f_sea_03_contrat_apf = 10.00
           bt_ok:height-chars           in frame f_sea_03_contrat_apf = 01.00
           bt_ran:width-chars           in frame f_sea_03_contrat_apf = 10.00
           bt_ran:height-chars          in frame f_sea_03_contrat_apf = 01.00
           bt_ran15:width-chars         in frame f_sea_03_contrat_apf = 10.00
           bt_ran15:height-chars        in frame f_sea_03_contrat_apf = 01.00
           rt_cxcf:width-chars          in frame f_sea_03_contrat_apf = 69.14
           rt_cxcf:height-chars         in frame f_sea_03_contrat_apf = 01.42
           rt_cxcl:width-chars          in frame f_sea_03_contrat_apf = 71.00
           rt_cxcl:height-chars         in frame f_sea_03_contrat_apf = 01.25.
    /* set private-data for the help system */
    assign rs_sea_contrat_apf:private-data    in frame f_sea_03_contrat_apf = "HLP=000007914":U
           br_sea_contrat_apf:private-data    in frame f_sea_03_contrat_apf = "HLP=000007914":U
           bt_add2:private-data         in frame f_sea_03_contrat_apf = "HLP=000010825":U
           bt_mod2:private-data         in frame f_sea_03_contrat_apf = "HLP=000010827":U
           bt_det2:private-data         in frame f_sea_03_contrat_apf = "HLP=000010805":U
           bt_agenc_bcia:private-data   in frame f_sea_03_contrat_apf = "HLP=000009119":U
           
           bt_ran:private-data          in frame f_sea_03_contrat_apf = "HLP=000008967":U
           bt_fil:private-data          in frame f_sea_03_contrat_apf = "HLP=000008966":U
           bt_ran15:private-data        in frame f_sea_03_contrat_apf = "HLP=000007914":U
           bt_ok:private-data           in frame f_sea_03_contrat_apf = "HLP=000010721":U
           bt_can:private-data          in frame f_sea_03_contrat_apf = "HLP=000011050":U
           bt_hel2:private-data         in frame f_sea_03_contrat_apf = "HLP=000011326":U
           frame f_sea_03_contrat_apf:private-data                    = "HLP=000007914".



{include/i_fclfrm.i f_sea_03_contrat_apf }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON INS OF br_sea_contrat_apf IN FRAME f_sea_03_contrat_apf
DO:

    if  bt_add2:sensitive in frame f_sea_03_contrat_apf
    then do:
        apply "choose" to bt_add2 in frame f_sea_03_contrat_apf.
    end /* if */.


END. /* ON INS OF br_sea_contrat_apf IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_add2 IN FRAME f_sea_03_contrat_apf
DO:
  
END. /* ON CHOOSE OF bt_add2 IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_agenc_bcia IN FRAME f_sea_03_contrat_apf
DO:

    if  avail contrat_apf
    then do:
        assign v_rec_contrat_apf = recid(contrat_apf)
               v_rec_table    = recid(contrat_apf).
        if  search("prgint/utb/utb098ba.r") = ? and search("prgint/utb/utb098ba.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb098ba.p".
            else do:
                message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb098ba.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgint/utb/utb098ba.p /*prg_bas_agenc_bcia*/.
        if  v_rec_contrat_apf <> ?
        then do:
            assign v_rec_table = v_rec_contrat_apf.
        end /* if */.
        run pi_open_sea_contrat_apf /*pi_open_sea_contrat_apf*/.
        reposition qr_sea_contrat_apf
                   to recid v_rec_table no-error.
    end /* if */.

END. /* ON CHOOSE OF bt_agenc_bcia IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_can IN FRAME f_sea_03_contrat_apf
DO:

    apply "end-error" to self.
END. /* ON CHOOSE OF bt_can IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_det2 IN FRAME f_sea_03_contrat_apf
DO:

    if  avail contrat_apf
    then do:
        assign v_rec_contrat_apf = recid(contrat_apf).
        if  search("prgint/utb/utb098ia.r") = ? and search("prgint/utb/utb098ia.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb098ia.p".
            else do:
                message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb098ia.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgint/utb/utb098ia.p /*prg_det_contrat_apf*/.
        if  v_rec_contrat_apf <> ?
        then do:
            assign v_rec_table = v_rec_contrat_apf.
            reposition qr_sea_contrat_apf to recid v_rec_table no-error.
        end /* if */.
    end /* if */.
END. /* ON CHOOSE OF bt_det2 IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_fil IN FRAME f_sea_03_contrat_apf
DO:


END. /* ON CHOOSE OF bt_fil IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_hel2 IN FRAME f_sea_03_contrat_apf
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_sea_03_contrat_apf */



ON CHOOSE OF bt_mod2 IN FRAME f_sea_03_contrat_apf
DO:

   
END. /* ON CHOOSE OF bt_mod2 IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_ok IN FRAME f_sea_03_contrat_apf
DO:

    if  avail contrat_apf
    then do:
        assign v_rec_contrat_apf = recid(contrat_apf).
    end /* if */.
END. /* ON CHOOSE OF bt_ok IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_ran IN FRAME f_sea_03_contrat_apf
DO:

    



END. /* ON CHOOSE OF bt_ran IN FRAME f_sea_03_contrat_apf */

ON CHOOSE OF bt_ran15 IN FRAME f_sea_03_contrat_apf
DO:

   


END. /* ON CHOOSE OF bt_ran15 IN FRAME f_sea_03_contrat_apf */

ON VALUE-CHANGED OF rs_sea_contrat_apf IN FRAME f_sea_03_contrat_apf
DO:

    /* inifim: */
    case input frame f_sea_03_contrat_apf rs_sea_contrat_apf:
        when "Por contrat_apf" /*l_por_contrat_apf*/ then block_1:
         do:
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(8)":U
                   v_nom_attrib   = "contrat_apf"
                   v_cod_initial  = string("":U)
                   v_cod_final  = string("ZZZZZZZZ":U).
        end /* do block_1 */.
        when "Por Nome" /*l_por_nome*/ then block_1:
         do:
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(30)":U
                   v_nom_attrib   = "Nome contrat_apf"
                   v_cod_initial  = string("":U)
                   v_cod_final  = string("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ":U).
        end /* do block_1 */.
    end /* case inifim */.
    run pi_open_sea_contrat_apf /*pi_open_sea_contrat_apf*/.
END. /* ON VALUE-CHANGED OF rs_sea_contrat_apf IN FRAME f_sea_03_contrat_apf */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON ENDKEY OF FRAME f_sea_03_contrat_apf
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_upc) (input 'CANCEL',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_appc) (input 'CANCEL',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_dpc) (input 'CANCEL',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */

END. /* ON ENDKEY OF FRAME f_sea_03_contrat_apf */

ON END-ERROR OF FRAME f_sea_03_contrat_apf
DO:

    assign v_rec_contrat_apf = ?.
END. /* ON END-ERROR OF FRAME f_sea_03_contrat_apf */

ON ENTRY OF FRAME f_sea_03_contrat_apf
DO:

    apply "value-changed" to rs_sea_contrat_apf in frame f_sea_03_contrat_apf.
END. /* ON ENTRY OF FRAME f_sea_03_contrat_apf */

ON HELP OF FRAME f_sea_03_contrat_apf ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_sea_03_contrat_apf */

ON RIGHT-MOUSE-DOWN OF FRAME f_sea_03_contrat_apf ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9"
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end /* if */.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_sea_03_contrat_apf */

ON RIGHT-MOUSE-UP OF FRAME f_sea_03_contrat_apf ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9"
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end /* if */.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_sea_03_contrat_apf */

ON WINDOW-CLOSE OF FRAME f_sea_03_contrat_apf
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_sea_03_contrat_apf */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_sea_03_contrat_apf.





END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_release
        as character
        format "x(12)"
        no-undo.
    def var v_nom_prog
        as character
        format "x(8)"
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(8)"
        label "Nome Externo"
        no-undo.


    /************************** Variable Definition End *************************/


        assign v_nom_prog     = substring(frame f_sea_03_contrat_apf:title, 1, max(1, length(frame f_sea_03_contrat_apf:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "sea_contrat_apf":U.




    assign v_nom_prog_ext = "prgint/utb/utb098ka.p":U
           v_cod_release  = trim(" 1.00.00.005":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */

{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
def new global shared var v_cod_arq
    as char  
    format 'x(60)'
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format 'x(8)'
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('sea_contrat_apf':U, 'prgint/utb/utb098ka.p':U, '1.00.00.005':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb906za.py".
    else do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/men/men901za.py".
    else do:
        message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'sea_contrat_apf') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n’o ² um programa vÿlido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'sea_contrat_apf')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usuÿrio sem permiss’o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'sea_contrat_apf')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
log_exec:
do transaction:
    assign v_rec_log = ?.

    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = 'sea_contrat_apf'
        no-lock no-error.

    if  available prog_dtsul
    and  prog_dtsul.log_gera_log_exec = yes
    then do:
        create log_exec_prog_dtsul.
        assign log_exec_prog_dtsul.cod_prog_dtsul           = 'sea_contrat_apf'
               log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
               log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
        assign v_rec_log = recid(log_exec_prog_dtsul).
        release log_exec_prog_dtsul no-error.
    end /* if */.
end /* do log_exec */.


/* End_Include: i_log_exec_prog_dtsul_ini */


/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "sea_contrat_apf":U
    no-lock no-error.
if  avail prog_dtsul then do:
    if  prog_dtsul.nom_prog_upc <> ''
    and prog_dtsul.nom_prog_upc <> ? then
        assign v_nom_prog_upc = prog_dtsul.nom_prog_upc.
    if  prog_dtsul.nom_prog_appc <> ''
    and prog_dtsul.nom_prog_appc <> ? then
        assign v_nom_prog_appc = prog_dtsul.nom_prog_appc.
&if '{&emsbas_version}' > '5.00' &then
    if  prog_dtsul.nom_prog_dpc <> ''
    and prog_dtsul.nom_prog_dpc <> ? then
        assign v_nom_prog_dpc = prog_dtsul.nom_prog_dpc.
&endif
end.


assign v_wgh_frame_epc = frame f_sea_03_contrat_apf:handle.



assign v_nom_table_epc = 'contrat_apf'
       v_rec_table_epc = recid(contrat_apf).

&endif

/* End_Include: i_verify_program_epc */


/* ix_p00_sea_contrat_apf */


/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers’o */
assign frame f_sea_03_contrat_apf:title = frame f_sea_03_contrat_apf:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.005":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_sea_03_contrat_apf = menu m_help:handle.


/* End_Include: i_std_dialog_box */

{include/title5.i f_sea_03_contrat_apf FRAME}


assign br_sea_contrat_apf:num-locked-columns in frame f_sea_03_contrat_apf = 0.

/* ix_p05_sea_contrat_apf */

pause 0 before-hide.
view frame f_sea_03_contrat_apf.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(contrat_apf).
    run value(v_nom_prog_upc) (input 'INITIALIZE',
                               input 'viewer',
                               input this-procedure,
                               input v_wgh_frame_epc,
                               input v_nom_table_epc,
                               input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

if  v_nom_prog_appc <> '' then
do:
    assign v_rec_table_epc = recid(contrat_apf).
    run value(v_nom_prog_appc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

&if '{&emsbas_version}' > '5.00' &then
if  v_nom_prog_dpc <> '' then
do:
    assign v_rec_table_epc = recid(contrat_apf).
    run value(v_nom_prog_dpc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.
&endif
&endif
/* End_Include: i_exec_program_epc */


assign v_rec_table    = v_rec_contrat_apf.

main_block:
do on endkey undo main_block, leave main_block on error undo main_block, leave main_block.
    /* ix_p10_sea_contrat_apf */
    enable rs_sea_contrat_apf
           br_sea_contrat_apf
/*            bt_add2 */
/*            bt_mod2 */
/*            bt_det2 */
/*            bt_ran  */
/*            bt_fil  */
/*            bt_ok   */
/*            bt_can  */
/*            bt_hel2 */
           
           with frame f_sea_03_contrat_apf.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_upc) (input 'ENABLE',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_appc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(contrat_apf).
        run value(v_nom_prog_dpc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */

    assign bt_fil:sensitive in frame f_sea_03_contrat_apf = no.
    if  index(program-name(2), "prgint/utb/utb098ca.p") <> 0
    or   index(program-name(2), "prgint/utb/utb098ea.p") <> 0
    or   index(program-name(2), "prgint/utb/utb098ia.p") <> 0
    or   index(program-name(2), "prgint/utb/utb098ga.p") <> 0
    then do:
         assign bt_add2:sensitive in frame f_sea_03_contrat_apf = no
                bt_mod2:sensitive in frame f_sea_03_contrat_apf = no
                bt_det2:sensitive in frame f_sea_03_contrat_apf = no.
    end /* if */.

    /* Begin_Include: ix_p20_sea_contrat_apf */
    assign bt_ran:visible in frame f_sea_03_contrat_apf = no
           
           .
    /* End_Include: ix_p20_sea_contrat_apf */


/* **************
    @do(security_block) with frame @&(frame):
        @i(i_verify_security_button_sea &table=@&(table) &program_complement=@&(program_complement))
    @end_do(security_block).
***************/

    wait-for go of frame f_sea_03_contrat_apf
          or default-action of br_sea_contrat_apf
          or mouse-select-dblclick of br_sea_contrat_apf focus browse br_sea_contrat_apf.
    if  avail contrat_apf
    then do:
        assign v_rec_contrat_apf = recid(contrat_apf).
        /* ix_p30_sea_contrat_apf */
    end /* if */.
    else do:
        assign v_rec_contrat_apf = ?.
        /* ix_p35_sea_contrat_apf */
    end /* else */.
end /* do main_block */.

hide frame f_sea_03_contrat_apf.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
log_exec_block:
do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    end /* if */.
    release log_exec_prog_dtsul.
end /* do log_exec_block */.
/* End_Include: i_log_exec_prog_dtsul_fim */



/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_open_sea_contrat_apf
** Descricao.............: pi_open_sea_contrat_apf
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 27/04/1995 10:05:37
** Gerado por............: bre18805
*****************************************************************************/
PROCEDURE pi_open_sea_contrat_apf:

    /* case_block: */
    case input frame f_sea_03_contrat_apf rs_sea_contrat_apf:
        when "Por contrat_apf" /*l_por_contrat_apf*/ then
            code_block:
            do:
                open query qr_sea_contrat_apf for
                    each contrat_apf no-lock
                    where contrat_apf.cod_contrat_apf >= v_cod_initial
                      and contrat_apf.cod_contrat_apf <= v_cod_final /*cl_sea_codigo of contrat_apf*/
                    by contrat_apf.cod_contrat_apf.
            end /* do code_block */.
        when "Por Nome" /*l_por_nome*/ then
            name_block:
            do:
                open query qr_sea_contrat_apf for
                    each contrat_apf no-lock
                    where contrat_apf.des_contrat_apf >= v_cod_initial
                      and contrat_apf.des_contrat_apf <= v_cod_final /*cl_sea_nome of contrat_apf*/
                    by contrat_apf.des_contrat_apf.
            end /* do name_block */.
    end /* case case_block */.

END PROCEDURE. /* pi_open_sea_contrat_apf */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14020
** Alterado em...........: 12/06/2006 09:09:21
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_version
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then 
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            p_cod_program            at 1 
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 
            string(time, 'HH:MM:SS') at 94 skip.

        if  p_cod_program_type = 'pro' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find prog_dtsul 
                where prog_dtsul.cod_prog_dtsul = p_cod_program 
                no-lock no-error.
            if  avail prog_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  prog_dtsul.nom_prog_dpc <> '' then
                        put stream s-arq 'DPC : ' at 5 prog_dtsul.nom_prog_dpc  at 15 skip.
                &endif
                if  prog_dtsul.nom_prog_appc <> '' then
                    put stream s-arq 'APPC: ' at 5 prog_dtsul.nom_prog_appc at 15 skip.
                if  prog_dtsul.nom_prog_upc <> '' then
                    put stream s-arq 'UPC : ' at 5 prog_dtsul.nom_prog_upc  at 15 skip.
            end /* if */.
            &endif
        end.

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find tab_dic_dtsul 
                where tab_dic_dtsul.cod_tab_dic_dtsul = p_cod_program 
                no-lock no-error.
            if  avail tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' then
                        put stream s-arq 'DPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_delete <> '' then
                    put stream s-arq 'APPC-DELETE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_delete <> '' then
                    put stream s-arq 'UPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_write <> '' then
                        put stream s-arq 'DPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_write <> '' then
                    put stream s-arq 'APPC-WRITE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_write <> '' then
                    put stream s-arq 'UPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.
    end /* if */.

END PROCEDURE. /* pi_version_extract */


/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/
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

    assign c_prg_msg = "messages/"
                     + string(trunc(i_msg / 1000,0),"99")
                     + "/msg"
                     + string(i_msg, "99999").

    if search(c_prg_msg + ".r") = ? and search(c_prg_msg + ".p") = ? then do:
        message "Mensagem nr. " i_msg "!!!" skip
                "Programa Mensagem" c_prg_msg "nÊo encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p") (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/*****************************  End of sea_contrat_apf ****************************/
