/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_mapa_distrib_ccusto_copia
** Descricao.............: Fun»„es Mapa Distribui»’o Centro Custo
** Versao................:  1.00.00.004
** Procedimento..........: man_mapa_distrib_ccusto
** Nome Externo..........: prgint/utb/utb028za.p
** Data Geracao..........: 19/09/2005 - 11:26:09
** Criado por............: Emerson
** Criado em.............: 04/02/1998 09:14:25
** Alterado por..........: fut1180
** Alterado em...........: 16/09/2005 17:52:47
** Gerado por............: fut1180
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.004":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  fnc_mapa_distrib_ccusto_copia UTB}
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
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "FNC_MAPA_DISTRIB_CCUSTO_COPIA","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

def buffer b_estabelecimento
    for estabelecimento.
def buffer b_item_distrib_ccusto
    for item_distrib_ccusto.
def buffer b_item_lista_ccusto
    for item_lista_ccusto.
def buffer b_mapa_distrib_ccusto_dest
    for mapa_distrib_ccusto.


/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usuÿrio"
    column-label "Usuÿrio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_cod_estab
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estabelecimento"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usuÿrios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def var v_cod_mapa_distrib_ccusto
    as character
    format "x(8)":U
    label "Mapa Dist CCusto"
    column-label "Mapa Dist CCusto"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M½dulo Corrente"
    column-label "M½dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa­s Empresa Usuÿrio"
    column-label "Pa­s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg½cio"
    column-label "Unid Neg½cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usuÿrio Corrente"
    column-label "Usuÿrio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def var v_des_percent_complete
    as character
    format "x(06)":U
    no-undo.
def var v_des_percent_complete_fnd
    as character
    format "x(08)":U
    no-undo.
def var v_ind_mapa_distrib_ccusto
    as character
    format "X(08)":U
    initial "Ignorar" /*l_ignorar*/
    view-as radio-set Horizontal
    radio-buttons "Ignorar", "Ignorar","Atualizar", "Atualizar"
     /*l_ignorar*/ /*l_ignorar*/ /*l_atualizar*/ /*l_atualizar*/
    bgcolor 8 
    label "Caso Exista"
    column-label "Caso Exista"
    no-undo.
def var v_log_answer
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_method
    as logical
    format "Sim/N’o"
    initial yes
    no-undo.
def var v_log_repeat
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def new global shared var v_rec_estabelecimento
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_mapa_distrib_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conteœdo"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_002
    size 1 by 1
    edge-pixels 2.
def rectangle rt_004
    size 1 by 1
    edge-pixels 2.
def rectangle rt_005
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_can2
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
/****************************** Function Button *****************************/
def button bt_zoo_231809
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_231810
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.


/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_02_percent_update
    rt_001
         at row 01.29 col 02.00
    " Percentual Completo " view-as text
         at row 01.00 col 04.00
    v_des_percent_complete_fnd
         at row 02.04 col 03.00 no-label
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_des_percent_complete
         at row 02.04 col 03.00 no-label
         view-as fill-in
         size-chars 7.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_can2
         at row 03.50 col 20.00 font ?
         help "Cancela"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "".
    /* adjust size of objects in this frame */
    assign bt_can2:width-chars  in frame f_dlg_02_percent_update = 10.00
           bt_can2:height-chars in frame f_dlg_02_percent_update = 01.00
           rt_001:width-chars   in frame f_dlg_02_percent_update = 46.72
           rt_001:height-chars  in frame f_dlg_02_percent_update = 01.92.
    /* set private-data for the help system */
    assign v_des_percent_complete_fnd:private-data in frame f_dlg_02_percent_update = "HLP=000022169":U
           v_des_percent_complete:private-data     in frame f_dlg_02_percent_update = "HLP=000022167":U
           bt_can2:private-data                    in frame f_dlg_02_percent_update = "HLP=000011451":U
           frame f_dlg_02_percent_update:private-data                               = "HLP=000010975".

def frame f_dlg_04_mapa_distrib_ccusto_copia
    rt_001
         at row 01.21 col 02.00
    rt_004
         at row 05.00 col 03.29
    " Origem " view-as text
         at row 04.70 col 05.29 bgcolor 8 
    rt_002
         at row 01.63 col 03.29
    " Destino " view-as text
         at row 01.33 col 05.29 bgcolor 8 
    rt_005
         at row 08.50 col 03.29
    " Par³metros " view-as text
         at row 08.20 col 05.29
    rt_cxcf
         at row 13.17 col 02.00 bgcolor 7 
    b_mapa_distrib_ccusto_dest.cod_estab
         at row 02.25 col 24.72 colon-aligned label "Estab"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    b_estabelecimento.nom_pessoa
         at row 02.25 col 31.29 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
         at row 03.25 col 24.72 colon-aligned label "Mapa Dist CCusto"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    b_mapa_distrib_ccusto_dest.des_mapa_distrib_ccusto
         at row 03.25 col 36.43 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_estab
         at row 05.50 col 24.72 colon-aligned label "Estab"
         help "C½digo Estabelecimento"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_231809
         at row 05.50 col 30.86
    estabelecimento.nom_pessoa
         at row 05.50 col 35.29 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_mapa_distrib_ccusto
         at row 06.50 col 24.72 colon-aligned label "Mapa Dist CCusto"
         help "C½digo Mapa Distribui»’o Centro Custo"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_231810
         at row 06.50 col 35.86
    mapa_distrib_ccusto.des_mapa_distrib_ccusto
         at row 06.50 col 40.43 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_ind_mapa_distrib_ccusto
         at row 09.50 col 24.72 colon-aligned label "Caso Exista"
         view-as radio-set Horizontal
         radio-buttons "Ignorar", "Ignorar","Atualizar", "Atualizar"
          /*l_ignorar*/ /*l_ignorar*/ /*l_atualizar*/ /*l_atualizar*/
         bgcolor 8 
    bt_ok
         at row 13.38 col 03.00 font ?
         help "OK"
    bt_can
         at row 13.38 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 13.38 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 15.00 
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Mapa Distribui»’o Centro Custo".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 10.00
           bt_can:height-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 01.00
           bt_hel2:width-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 10.00
           bt_hel2:height-chars in frame f_dlg_04_mapa_distrib_ccusto_copia = 01.00
           bt_ok:width-chars    in frame f_dlg_04_mapa_distrib_ccusto_copia = 10.00
           bt_ok:height-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 01.00
           rt_001:width-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 86.57
           rt_001:height-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 11.13
           rt_002:width-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 84.00
           rt_002:height-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 02.88
           rt_004:width-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 84.00
           rt_004:height-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 03.00
           rt_005:width-chars   in frame f_dlg_04_mapa_distrib_ccusto_copia = 84.00
           rt_005:height-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 02.83
           rt_cxcf:width-chars  in frame f_dlg_04_mapa_distrib_ccusto_copia = 86.57
           rt_cxcf:height-chars in frame f_dlg_04_mapa_distrib_ccusto_copia = 01.42.
    /* set private-data for the help system */
    assign b_mapa_distrib_ccusto_dest.cod_estab:private-data               in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000020926":U
           b_estabelecimento.nom_pessoa:private-data                       in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000025116":U
           b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto:private-data in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000020927":U
           b_mapa_distrib_ccusto_dest.des_mapa_distrib_ccusto:private-data in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000006778":U
           bt_zoo_231809:private-data                                      in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000009431":U
           v_cod_estab:private-data                                        in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000020928":U
           estabelecimento.nom_pessoa:private-data                         in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000025116":U
           bt_zoo_231810:private-data                                      in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000009431":U
           v_cod_mapa_distrib_ccusto:private-data                          in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000020929":U
           mapa_distrib_ccusto.des_mapa_distrib_ccusto:private-data        in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000006778":U
           v_ind_mapa_distrib_ccusto:private-data                          in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000020930":U
           bt_ok:private-data                                              in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000010721":U
           bt_can:private-data                                             in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000011050":U
           bt_hel2:private-data                                            in frame f_dlg_04_mapa_distrib_ccusto_copia = "HLP=000011326":U
           frame f_dlg_04_mapa_distrib_ccusto_copia:private-data                                                       = "HLP=000020925".
    /* enable function buttons */
    assign bt_zoo_231809:sensitive in frame f_dlg_04_mapa_distrib_ccusto_copia = yes
           bt_zoo_231810:sensitive in frame f_dlg_04_mapa_distrib_ccusto_copia = yes.
    /* move buttons to top */
    bt_zoo_231809:move-to-top().
    bt_zoo_231810:move-to-top().



{include/i_fclfrm.i f_dlg_02_percent_update f_dlg_04_mapa_distrib_ccusto_copia }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_can2 IN FRAME f_dlg_02_percent_update
DO:

    hide frame f_dlg_02_percent_update.

    stop.
END. /* ON CHOOSE OF bt_can2 IN FRAME f_dlg_02_percent_update */

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON CHOOSE OF bt_ok IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:

    assign v_log_repeat = no.
END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON LEAVE OF v_cod_estab IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:

    find estabelecimento no-lock
         where estabelecimento.cod_estab = input frame f_dlg_04_mapa_distrib_ccusto_copia v_cod_estab
         use-index stblcmnt_id /* cl_frame of estabelecimento*/ no-error.
    display estabelecimento.nom_pessoa when avail estabelecimento
            "" when not avail estabelecimento @ estabelecimento.nom_pessoa
            with frame f_dlg_04_mapa_distrib_ccusto_copia.
END. /* ON LEAVE OF v_cod_estab IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON LEAVE OF v_cod_mapa_distrib_ccusto IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:

    find mapa_distrib_ccusto no-lock
         where mapa_distrib_ccusto.cod_mapa_distrib_ccusto = input frame f_dlg_04_mapa_distrib_ccusto_copia v_cod_mapa_distrib_ccusto
         no-error.
    display mapa_distrib_ccusto.des_mapa_distrib_ccusto when avail mapa_distrib_ccusto
            "" when not avail mapa_distrib_ccusto @ mapa_distrib_ccusto.des_mapa_distrib_ccusto
            with frame f_dlg_04_mapa_distrib_ccusto_copia.
END. /* ON LEAVE OF v_cod_mapa_distrib_ccusto IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON LEAVE OF b_mapa_distrib_ccusto_dest.cod_estab IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:

    find estabelecimento no-lock
         where estabelecimento.cod_estab = input frame f_dlg_04_mapa_distrib_ccusto_copia 
         b_mapa_distrib_ccusto_dest.cod_estab use-index stblcmnt_id  
         no-error.
    display estabelecimento.nom_pessoa when avail estabelecimento
            "" when not avail estabelecimento @ estabelecimento.nom_pessoa
            with frame f_dlg_04_mapa_distrib_ccusto_copia.

END. /* ON LEAVE OF b_mapa_distrib_ccusto_dest.cod_estab IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON LEAVE OF b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
DO:

    find mapa_distrib_ccusto no-lock
         where mapa_distrib_ccusto.cod_mapa_distrib_ccusto = input frame f_dlg_04_mapa_distrib_ccusto_copia 
         b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto   
         no-error.
    display mapa_distrib_ccusto.des_mapa_distrib_ccusto when avail mapa_distrib_ccusto
            "" when not avail mapa_distrib_ccusto @ mapa_distrib_ccusto.des_mapa_distrib_ccusto
            with frame f_dlg_04_mapa_distrib_ccusto_copia.

END. /* ON LEAVE OF b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_zoo_231809 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
OR F5 OF v_cod_estab IN FRAME f_dlg_04_mapa_distrib_ccusto_copia DO:

    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb071na.r") = ? and search("prgint/utb/utb071na.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb071na.p".
        else do:
            message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb071na.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb071na.p (Input b_mapa_distrib_ccusto_dest.cod_empresa) /*prg_see_estabelecimento_empresa*/.
    if  v_rec_estabelecimento <> ?
    then do:
        find estabelecimento where recid(estabelecimento) = v_rec_estabelecimento no-lock no-error.
        assign v_cod_estab:screen-value in frame f_dlg_04_mapa_distrib_ccusto_copia =
               string(estabelecimento.cod_estab).

        display estabelecimento.nom_pessoa
                with frame f_dlg_04_mapa_distrib_ccusto_copia.

        apply "entry" to v_cod_estab in frame f_dlg_04_mapa_distrib_ccusto_copia.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_231809 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON  CHOOSE OF bt_zoo_231810 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia
OR F5 OF v_cod_mapa_distrib_ccusto IN FRAME f_dlg_04_mapa_distrib_ccusto_copia DO:

    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb028nb.r") = ? and search("prgint/utb/utb028nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb028nb.p".
        else do:
            message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb028nb.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb028nb.p (Input input frame f_dlg_04_mapa_distrib_ccusto_copia v_cod_estab) /*prg_see_mapa_distrib_ccusto_estab*/.
    if  v_rec_mapa_distrib_ccusto <> ?
    then do:
        find mapa_distrib_ccusto where recid(mapa_distrib_ccusto) = v_rec_mapa_distrib_ccusto no-lock no-error.
        assign v_cod_mapa_distrib_ccusto:screen-value in frame f_dlg_04_mapa_distrib_ccusto_copia =
               string(mapa_distrib_ccusto.cod_mapa_distrib_ccusto).

        display mapa_distrib_ccusto.des_mapa_distrib_ccusto
                with frame f_dlg_04_mapa_distrib_ccusto_copia.

        apply "entry" to v_cod_mapa_distrib_ccusto in frame f_dlg_04_mapa_distrib_ccusto_copia.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_231810 IN FRAME f_dlg_04_mapa_distrib_ccusto_copia */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_dlg_02_percent_update ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_02_percent_update */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_02_percent_update ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_02_percent_update */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_02_percent_update ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_02_percent_update */

ON WINDOW-CLOSE OF FRAME f_dlg_02_percent_update
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_dlg_02_percent_update */

ON HELP OF FRAME f_dlg_04_mapa_distrib_ccusto_copia ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_mapa_distrib_ccusto_copia ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_mapa_distrib_ccusto_copia */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_mapa_distrib_ccusto_copia ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_mapa_distrib_ccusto_copia */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_dlg_04_mapa_distrib_ccusto_copia.





END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_release
        as character
        format "x(12)":U
        no-undo.
    def var v_nom_prog
        as character
        format "x(8)":U
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(8)":U
        label "Nome Externo"
        no-undo.


    /************************** Variable Definition End *************************/


        assign v_nom_prog     = substring(frame f_dlg_04_mapa_distrib_ccusto_copia:title, 1, max(1, length(frame f_dlg_04_mapa_distrib_ccusto_copia:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "fnc_mapa_distrib_ccusto_copia":U.




    assign v_nom_prog_ext = "prgint/utb/utb028za.p":U
           v_cod_release  = trim(" 1.00.00.004":U).
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
    run pi_version_extract ('fnc_mapa_distrib_ccusto_copia':U, 'prgint/utb/utb028za.p':U, '1.00.00.004':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'fnc_mapa_distrib_ccusto_copia') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n’o ² um programa vÿlido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_mapa_distrib_ccusto_copia')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usuÿrio sem permiss’o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_mapa_distrib_ccusto_copia')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'fnc_mapa_distrib_ccusto_copia' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'fnc_mapa_distrib_ccusto_copia'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */



/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers’o */
assign frame f_dlg_04_mapa_distrib_ccusto_copia:title = frame f_dlg_04_mapa_distrib_ccusto_copia:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.004":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_dlg_04_mapa_distrib_ccusto_copia = menu m_help:handle.


/* End_Include: i_std_dialog_box */

{include/title5.i f_dlg_04_mapa_distrib_ccusto_copia FRAME}


pause 0 before-hide.
view frame f_dlg_04_mapa_distrib_ccusto_copia.

find b_mapa_distrib_ccusto_dest no-lock
     where recid(b_mapa_distrib_ccusto_dest) = v_rec_mapa_distrib_ccusto no-error.

find b_estabelecimento no-lock
     where b_estabelecimento.cod_estab = b_mapa_distrib_ccusto_dest.cod_estab no-error.

assign v_rec_table  = v_rec_mapa_distrib_ccusto
       v_log_repeat = yes
       v_wgh_focus  = v_cod_estab:handle in frame f_dlg_04_mapa_distrib_ccusto_copia.


disable b_estabelecimento.nom_pessoa
        b_mapa_distrib_ccusto_dest.cod_estab
        b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
        b_mapa_distrib_ccusto_dest.des_mapa_distrib_ccusto
        estabelecimento.nom_pessoa
        mapa_distrib_ccusto.des_mapa_distrib_ccusto
        with frame f_dlg_04_mapa_distrib_ccusto_copia.

enable bt_can
       bt_hel2
       bt_ok
       v_cod_estab
       v_cod_mapa_distrib_ccusto
       v_ind_mapa_distrib_ccusto
       with frame f_dlg_04_mapa_distrib_ccusto_copia.                     

display b_mapa_distrib_ccusto_dest.cod_estab
        b_estabelecimento.nom_pessoa
        b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
        b_mapa_distrib_ccusto_dest.des_mapa_distrib_ccusto
        with frame f_dlg_04_mapa_distrib_ccusto_copia.

main_block:
repeat with frame f_dlg_04_mapa_distrib_ccusto_copia while v_log_repeat 
        on error undo main_block, retry main_block
        on endkey undo main_block, leave main_block:    

    wait-for go of frame f_dlg_04_mapa_distrib_ccusto_copia focus v_wgh_focus.

    assign v_cod_estab = input frame f_dlg_04_mapa_distrib_ccusto_copia v_cod_estab
           v_cod_mapa_distrib_ccusto = input frame f_dlg_04_mapa_distrib_ccusto_copia v_cod_mapa_distrib_ccusto
           v_ind_mapa_distrib_ccusto = input frame f_dlg_04_mapa_distrib_ccusto_copia v_ind_mapa_distrib_ccusto.

    /* --- Verifica se o mapa origem informado existe ---*/
    find mapa_distrib_ccusto no-lock
         where mapa_distrib_ccusto.cod_estab =  v_cod_estab 
         and   mapa_distrib_ccusto.cod_mapa_distrib_ccusto =  v_cod_mapa_distrib_ccusto 
         no-error.

    if  not avail mapa_distrib_ccusto
    then do:
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                            "Mapa Distribui»’o Centro Custo", "Mapas Distribui»’o CCusto")) /*msg_1284*/.
        undo main_block, retry main_block.
    end /* if */.


    /* --- Verifica se a empresa do mapa origem e destino s’o iguais ---*/
    if  mapa_distrib_ccusto.cod_empresa <> b_mapa_distrib_ccusto_dest.cod_empresa
    then do:
        /* Empresa de origem deve ser igual ¹ empresa de destino ! */
        run pi_messages (input "show",
                         input 5299,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_5299*/.
        undo main_block, retry main_block.
    end /* if */.

    /* --- Verifica se o tipo do mapa origem e destino s’o iguais ---*/
    if  mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto <> b_mapa_distrib_ccusto_dest.ind_tip_mapa_distrib_ccusto
    then do:
        /* Tipo de mapa distribui»’o origem deve ser igual ao destino ! */
        run pi_messages (input "show",
                         input 5301,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_5301*/.
        undo main_block, retry main_block.
    end /* if */.

    /* --- Verifica se o plano ccusto origem e destino s’o iguais ---*/
    if  mapa_distrib_ccusto.cod_plano_ccusto <> b_mapa_distrib_ccusto_dest.cod_plano_ccusto
    then do:
        /* Plano Centro Custo origem deve ser igual ao destino ! */
        run pi_messages (input "show",
                         input 11661,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_11661*/.
        undo main_block, retry main_block.
    end /* if */.

    /* --- Efetua a c½pia do Mapa de distribui»’o ccusto ---*/        
    assign v_log_method = session:set-wait-state('general').

    if  b_mapa_distrib_ccusto_dest.ind_tip_mapa_distrib_ccusto = "Lista" /*l_lista*/ 
    then do:
        item_block:
        for 
            each item_lista_ccusto no-lock
            where item_lista_ccusto.cod_estab = mapa_distrib_ccusto.cod_estab
              and item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
            :

            if can-find(restric_ccusto
                        where restric_ccusto.cod_empresa      = item_lista_ccusto.cod_empresa
                        and   restric_ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                        and   restric_ccusto.cod_ccusto       = item_lista_ccusto.cod_ccusto
                        and   restric_ccusto.cod_estab        = b_mapa_distrib_ccusto_dest.cod_estab) then next.

            find b_item_lista_ccusto no-lock
                 where b_item_lista_ccusto.cod_estab = b_mapa_distrib_ccusto_dest.cod_estab    
                   and b_item_lista_ccusto.cod_mapa_distrib_ccusto =  b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
                   and b_item_lista_ccusto.cod_empresa = item_lista_ccusto.cod_empresa
                   and b_item_lista_ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                   and b_item_lista_ccusto.cod_ccusto = item_lista_ccusto.cod_ccusto 
                   no-error.   

            if  not avail b_item_lista_ccusto
            then do:
                    create b_item_lista_ccusto.
                    assign b_item_lista_ccusto.cod_estab  =  b_mapa_distrib_ccusto_dest.cod_estab
                           b_item_lista_ccusto.cod_mapa_distrib_ccusto =  b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
                           b_item_lista_ccusto.cod_empresa = item_lista_ccusto.cod_empresa
                           b_item_lista_ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                           b_item_lista_ccusto.cod_ccusto = item_lista_ccusto.cod_ccusto.
            end.
        end /* for item_block */.
    end.
    if  b_mapa_distrib_ccusto_dest.ind_tip_mapa_distrib_ccusto = "Automÿtico" /*l_automatico*/ 
    then do:
        item_block:
        for 
            each item_distrib_ccusto no-lock
            where item_distrib_ccusto.cod_estab = mapa_distrib_ccusto.cod_estab
              and item_distrib_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
            :

            if can-find(restric_ccusto
                        where restric_ccusto.cod_empresa      = item_distrib_ccusto.cod_empresa
                        and   restric_ccusto.cod_plano_ccusto = item_distrib_ccusto.cod_plano_ccusto
                        and   restric_ccusto.cod_ccusto       = item_distrib_ccusto.cod_ccusto
                        and   restric_ccusto.cod_estab        = b_mapa_distrib_ccusto_dest.cod_estab) then next.

            find b_item_distrib_ccusto exclusive-lock
                 where b_item_distrib_ccusto.cod_estab =  b_mapa_distrib_ccusto_dest.cod_estab    
                   and b_item_distrib_ccusto.cod_mapa_distrib_ccusto =  b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
                   and b_item_distrib_ccusto.cod_unid_negoc = item_distrib_ccusto.cod_unid_negoc
                   and b_item_distrib_ccusto.cod_plano_ccusto = item_distrib_ccusto.cod_plano_ccusto 
                   and b_item_distrib_ccusto.cod_ccusto = item_distrib_ccusto.cod_ccusto 
                   no-error.   

            if  (not avail b_item_distrib_ccusto
            or  (avail b_item_distrib_ccusto
            and  v_ind_mapa_distrib_ccusto = "Atualizar" /*l_atualizar*/ ))
            then do:
                if  not avail b_item_distrib_ccusto
                then do:
                    create b_item_distrib_ccusto.
                    assign b_item_distrib_ccusto.cod_estab  =  b_mapa_distrib_ccusto_dest.cod_estab
                           b_item_distrib_ccusto.cod_mapa_distrib_ccusto =  b_mapa_distrib_ccusto_dest.cod_mapa_distrib_ccusto
                           b_item_distrib_ccusto.cod_unid_negoc = item_distrib_ccusto.cod_unid_negoc
                           b_item_distrib_ccusto.cod_plano_ccusto = item_distrib_ccusto.cod_plano_ccusto
                           b_item_distrib_ccusto.cod_ccusto = item_distrib_ccusto.cod_ccusto.
                end /* if */.
                assign b_item_distrib_ccusto.cod_empresa = item_distrib_ccusto.cod_empresa
                       b_item_distrib_ccusto.qtd_criter_distrib_ccusto = item_distrib_ccusto.qtd_criter_distrib_ccusto.
            end.
        end /* for item_block */.
    end. 
    assign v_log_method   = session:set-wait-state("").        
end /* repeat main_block */.

hide frame f_dlg_04_mapa_distrib_ccusto_copia.


/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    end /* if */.
    release log_exec_prog_dtsul.
end.

/* End_Include: i_log_exec_prog_dtsul_fim */

return.


/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_percent_update
** Descricao.............: pi_percent_update
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: vladimir
** Alterado em...........: 15/10/1996 09:46:31
*****************************************************************************/
PROCEDURE pi_percent_update:

    /************************ Parameter Definition Begin ************************/

    def Input param p_val_maximum
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_current_value
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_nom_frame_title
        as character
        format "x(32)"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  p_val_maximum = 0
    then do:
       assign p_val_maximum = 1.
    end /* if */.

    assign v_des_percent_complete     = string(integer(p_val_current_value * 100 / p_val_maximum))
                                      + chr(32) + chr(37)
           v_des_percent_complete_fnd = v_des_percent_complete.

    if  not v_cod_dwb_user begins 'es_'
    then do:
        if  p_val_current_value = 0
        then do:
            assign v_des_percent_complete:width-pixels     in frame f_dlg_02_percent_update = 1
                   v_des_percent_complete:bgcolor          in frame f_dlg_02_percent_update = 1
                   v_des_percent_complete:fgcolor          in frame f_dlg_02_percent_update = 15
                   v_des_percent_complete:font             in frame f_dlg_02_percent_update = 1
                   rt_001:bgcolor                          in frame f_dlg_02_percent_update = 8
                   v_des_percent_complete_fnd:width-pixels in frame f_dlg_02_percent_update = 315
                   v_des_percent_complete_fnd:font         in frame f_dlg_02_percent_update = 1.
            if  p_nom_frame_title <> ""
            then do:
                assign frame f_dlg_02_percent_update:title = p_nom_frame_title.
            end /* if */.
            else do:
                assign frame f_dlg_02_percent_update:title = "Aguarde, em processamento..." /*l_aguarde_em_processamento*/ .
            end /* else */.
            view frame f_dlg_02_percent_update.
        end /* if */.
        else do:
            assign v_des_percent_complete:width-pixels = max(((315 * p_val_current_value)
                                                       / p_val_maximum), 1).
        end /* else */.
        display v_des_percent_complete
                v_des_percent_complete_fnd
                with frame f_dlg_02_percent_update.
        enable all with frame f_dlg_02_percent_update.
        process events.
    end /* if */.
    else do:
        run prgtec/btb/btb908ze.py (Input 1,
                                    Input v_des_percent_complete) /*prg_api_atualizar_ult_obj*/.
    end /* else */.



END PROCEDURE. /* pi_percent_update */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14013
** Alterado em...........: 05/01/2005 19:27:44
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)"
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

    /************************* Variable Definition Begin ************************/

    def var v_cod_event_dic
        as character
        format "x(20)":U
        label "Evento"
        column-label "Evento"
        no-undo.
    def var v_cod_tabela
        as character
        format "x(28)":U
        label "Tabela"
        column-label "Tabela"
        no-undo.


    /************************** Variable Definition End *************************/

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
            assign v_cod_event_dic = ENTRY(1,p_cod_program ,'/':U)
                   v_cod_tabela    = ENTRY(2,p_cod_program ,'/':U). /* FO 1100.980 */
            find tab_dic_dtsul 
                where tab_dic_dtsul.cod_tab_dic_dtsul = v_cod_tabela 
                no-lock no-error.
            if  avail tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                        put stream s-arq 'DPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'APPC-DELETE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'UPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                        put stream s-arq 'DPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                    put stream s-arq 'APPC-WRITE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_write <> '' and v_cod_event_dic = 'Write':U  then
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

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n’o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/*******************  End of fnc_mapa_distrib_ccusto_copia ******************/
