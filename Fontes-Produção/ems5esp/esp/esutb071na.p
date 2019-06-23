/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: see_estabelecimento_empresa
** Descricao.............: Pesquisa Especial (Ver) Estabele
** Versao................:  1.00.00.006
** Procedimento..........: man_estabelecimento
** Nome Externo..........: prgint/utb/utb071na.p
** Data Geracao..........: 09/09/2009 - 17:32:40
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: fut1180
** Alterado em...........: 05/10/2004 11:23:16
** Gerado por............: fut41190_1
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.006":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i see_estabelecimento_empresa UTB}
&ENDIF

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
                                    "SEE_ESTABELECIMENTO_EMPRESA","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************ Parameter Definition Begin ************************/

def Input param p_cod_empresa
    as character
    format "x(3)"
    no-undo.


/************************* Parameter Definition End *************************/

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
def var v_cod_dat_type
    as character
    format "x(8)":U
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
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
def var v_cod_estab_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "at²"
    column-label "Estab Final"
    no-undo.
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
def var v_cod_estab_fim
    as Character
    format "x(5)":U
    initial "ZZZZZ"
    label "at²"
    column-label "Estab Final"
    no-undo.
&ENDIF
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
def var v_cod_estab_ini
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab Inicial"
    no-undo.
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
def var v_cod_estab_ini
    as Character
    format "x(5)":U
    label "Estabelecimento"
    column-label "Estab Inicial"
    no-undo.
&ENDIF
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def var v_cod_final
    as character
    format "x(8)":U
    initial ?
    label "Final"
    no-undo.
def var v_cod_format
    as character
    format "x(8)":U
    label "Formato"
    column-label "Formato"
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
def var v_cod_initial
    as character
    format "x(8)":U
    initial ?
    label "Inicial"
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
def var v_nom_abrev_fim
    as character
    format "x(15)":U
    initial "ZZZZZZZZZZZZZZZ"
    label "Nome Abrev Final"
    column-label "Nome Abreviado"
    no-undo.
def var v_nom_abrev_ini
    as character
    format "x(15)":U
    label "Nome Abrev Inicial"
    column-label "Nome Abreviado"
    no-undo.
def var v_nom_attrib
    as character
    format "x(30)":U
    no-undo.
def var v_nom_pessoa_fim
    as character
    format "x(40)":U
    initial "ZZZZZZZZZZZZZZZZZZZZZZZZZZ"
    label "Raz’o Social Final"
    column-label "Raz’o Social Final"
    no-undo.
def var v_nom_pessoa_ini
    as character
    format "x(40)":U
    label "Raz’o Social Inicial"
    column-label "Raz’o Social Inicial"
    no-undo.
def var v_nom_prog_appc
    as character
    format "x(50)":U
    label "Programa APPC"
    column-label "Programa APPC"
    no-undo.
def var v_nom_prog_dpc
    as character
    format "x(50)":U
    label "Programa Dpc"
    column-label "Programa Dpc"
    no-undo.
def var v_nom_prog_upc
    as character
    format "X(50)":U
    label "Programa UPC"
    column-label "Programa UPC"
    no-undo.
def var v_nom_table_epc
    as character
    format "x(30)":U
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
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
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

/************************** Query Definition Begin **************************/

def query qr_see_estabelecimento_empresa
    for estabelecimento,
        ems5.pais
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_see_estabelecimento_empresa query qr_see_estabelecimento_empresa display 
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
    estabelecimento.cod_estab
    width-chars 03.86
        column-label "Estab"
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
    estabelecimento.cod_estab
    width-chars 05.00
        column-label "Estab"
&ENDIF
    estabelecimento.nom_pessoa
    width-chars 40.00
        column-label "Nome"
    estabelecimento.nom_abrev
    width-chars 15.00
        column-label "Nome Abreviado"
    estabelecimento.num_pessoa_jurid
    width-chars 10.86
        column-label "Pessoa Jur­dica"
    estabelecimento.log_estab_princ
    width-chars 12.86
        column-label "Estabelec Principal"
    estabelecimento.cod_pais
    width-chars 03.14
        column-label "Pa­s"
    string(estabelecimento.cod_id_feder, pais.cod_format_id_feder_jurid) format "x(20)" column-label "ID Federal"
    string(estabelecimento.cod_id_previd_social, pais.cod_format_id_previd_jurid) format "x(20)" column-label "ID Previd Social"
    with no-box separators single 
         size 88.00 by 07.00
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
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_add2
    label "Inclui"
    tooltip "Inclui"
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
/****************************** Function Button *****************************/


/*************************** Button Definition End **************************/

/************************ Radio-Set Definition Begin ************************/

def var rs_see_estabelecimento_empresa
    as character
    initial "Por Estabelecimento"
    view-as radio-set Horizontal
    radio-buttons "Por Estabelecimento", "Por Estabelecimento","Por Raz’o Social", "Por Raz’o Social","Por Nome Abreviado", "Por Nome Abreviado"
     /*l_por_estabelecimento*/ /*l_por_estabelecimento*/ /*l_por_razao_social*/ /*l_por_razao_social*/ /*l_por_nome_abreviado*/ /*l_por_nome_abreviado*/
    bgcolor 15 
    no-undo.


/************************* Radio-Set Definition End *************************/

/************************** Frame Definition Begin **************************/

def frame f_ran_01_estabelecimento_codigo
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
    v_cod_estab_ini
         at row 01.38 col 20.00 colon-aligned label "Estabelecimento"
         help "C½digo Estabelecimento Inicial"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
    v_cod_estab_ini
         at row 01.38 col 20.00 colon-aligned label "Estabelecimento"
         help "C½digo Estabelecimento Inicial"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsuni_version}" >= "" AND "{&emsuni_version}" < "5.07A" &THEN
    v_cod_estab_fim
         at row 02.38 col 20.00 colon-aligned label "at²"
         help "C½digo Estabelecimento Final"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsuni_version}" >= "5.07A" AND "{&emsuni_version}" < "9.99" &THEN
    v_cod_estab_fim
         at row 02.38 col 20.00 colon-aligned label "at²"
         help "C½digo Estabelecimento Final"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 37.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Estabelecimento".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_estabelecimento_codigo = 10.00
           bt_can:height-chars  in frame f_ran_01_estabelecimento_codigo = 01.00
           bt_hel2:width-chars  in frame f_ran_01_estabelecimento_codigo = 10.00
           bt_hel2:height-chars in frame f_ran_01_estabelecimento_codigo = 01.00
           bt_ok:width-chars    in frame f_ran_01_estabelecimento_codigo = 10.00
           bt_ok:height-chars   in frame f_ran_01_estabelecimento_codigo = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_estabelecimento_codigo = 46.57
           rt_cxcf:height-chars in frame f_ran_01_estabelecimento_codigo = 01.42
           rt_mold:width-chars  in frame f_ran_01_estabelecimento_codigo = 46.57
           rt_mold:height-chars in frame f_ran_01_estabelecimento_codigo = 02.21.
    /* set private-data for the help system */
    assign v_cod_estab_ini:private-data in frame f_ran_01_estabelecimento_codigo = "HLP=000016633":U
           v_cod_estab_fim:private-data in frame f_ran_01_estabelecimento_codigo = "HLP=000016634":U
           bt_ok:private-data           in frame f_ran_01_estabelecimento_codigo = "HLP=000010721":U
           bt_can:private-data          in frame f_ran_01_estabelecimento_codigo = "HLP=000011050":U
           bt_hel2:private-data         in frame f_ran_01_estabelecimento_codigo = "HLP=000011326":U
           frame f_ran_01_estabelecimento_codigo:private-data                    = "HLP=000009891".

def frame f_ran_01_estabelecimento_nome_abrev
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
    v_nom_abrev_ini
         at row 01.38 col 18.00 colon-aligned label "Inicial"
         help "Nome Abreviado"
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_nom_abrev_fim
         at row 02.38 col 18.00 colon-aligned label "Final"
         help "Nome Abreviado"
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 37.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Nome Abreviado".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_estabelecimento_nome_abrev = 10.00
           bt_can:height-chars  in frame f_ran_01_estabelecimento_nome_abrev = 01.00
           bt_hel2:width-chars  in frame f_ran_01_estabelecimento_nome_abrev = 10.00
           bt_hel2:height-chars in frame f_ran_01_estabelecimento_nome_abrev = 01.00
           bt_ok:width-chars    in frame f_ran_01_estabelecimento_nome_abrev = 10.00
           bt_ok:height-chars   in frame f_ran_01_estabelecimento_nome_abrev = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_estabelecimento_nome_abrev = 46.57
           rt_cxcf:height-chars in frame f_ran_01_estabelecimento_nome_abrev = 01.42
           rt_mold:width-chars  in frame f_ran_01_estabelecimento_nome_abrev = 46.57
           rt_mold:height-chars in frame f_ran_01_estabelecimento_nome_abrev = 02.21.
    /* set private-data for the help system */
    assign v_nom_abrev_ini:private-data in frame f_ran_01_estabelecimento_nome_abrev = "HLP=000021979":U
           v_nom_abrev_fim:private-data in frame f_ran_01_estabelecimento_nome_abrev = "HLP=000021980":U
           bt_ok:private-data           in frame f_ran_01_estabelecimento_nome_abrev = "HLP=000010721":U
           bt_can:private-data          in frame f_ran_01_estabelecimento_nome_abrev = "HLP=000011050":U
           bt_hel2:private-data         in frame f_ran_01_estabelecimento_nome_abrev = "HLP=000011326":U
           frame f_ran_01_estabelecimento_nome_abrev:private-data                    = "HLP=000009891".

def frame f_ran_01_estabelecimento_nome_pessoa
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
    v_nom_pessoa_ini
         at row 01.38 col 10.00 colon-aligned label "Inicial"
         help "Nome Pessoa"
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_nom_pessoa_fim
         at row 02.38 col 10.00 colon-aligned label "Final"
         help "Nome Pessoa"
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 46.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 59.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Raz’o Social".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_estabelecimento_nome_pessoa = 10.00
           bt_can:height-chars  in frame f_ran_01_estabelecimento_nome_pessoa = 01.00
           bt_hel2:width-chars  in frame f_ran_01_estabelecimento_nome_pessoa = 10.00
           bt_hel2:height-chars in frame f_ran_01_estabelecimento_nome_pessoa = 01.00
           bt_ok:width-chars    in frame f_ran_01_estabelecimento_nome_pessoa = 10.00
           bt_ok:height-chars   in frame f_ran_01_estabelecimento_nome_pessoa = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_estabelecimento_nome_pessoa = 55.57
           rt_cxcf:height-chars in frame f_ran_01_estabelecimento_nome_pessoa = 01.42
           rt_mold:width-chars  in frame f_ran_01_estabelecimento_nome_pessoa = 55.57
           rt_mold:height-chars in frame f_ran_01_estabelecimento_nome_pessoa = 02.21.
    /* set private-data for the help system */
    assign v_nom_pessoa_ini:private-data in frame f_ran_01_estabelecimento_nome_pessoa = "HLP=000021935":U
           v_nom_pessoa_fim:private-data in frame f_ran_01_estabelecimento_nome_pessoa = "HLP=000021937":U
           bt_ok:private-data            in frame f_ran_01_estabelecimento_nome_pessoa = "HLP=000010721":U
           bt_can:private-data           in frame f_ran_01_estabelecimento_nome_pessoa = "HLP=000011050":U
           bt_hel2:private-data          in frame f_ran_01_estabelecimento_nome_pessoa = "HLP=000011326":U
           frame f_ran_01_estabelecimento_nome_pessoa:private-data                     = "HLP=000009891".

def frame f_see_01_estabelecimento_empresa
    rt_cxcf
         at row 11.25 col 02.00 bgcolor 7 
    rt_cxcl
         at row 01.00 col 01.00 bgcolor 15 
    rs_see_estabelecimento_empresa
         at row 01.21 col 02.00
         help "" no-label
    br_see_estabelecimento_empresa
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
    bt_ran
         at row 09.75 col 38.00 font ?
         help "Faixa"
    bt_fil
         at row 09.75 col 50.00 font ?
         help "Filtro"
    bt_ok
         at row 11.46 col 03.00 font ?
         help "OK"
    bt_can
         at row 11.46 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 11.46 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 13.08 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Pesquisa".
    /* adjust size of objects in this frame */
    assign bt_add2:width-chars  in frame f_see_01_estabelecimento_empresa = 10.00
           bt_add2:height-chars in frame f_see_01_estabelecimento_empresa = 01.00
           bt_can:width-chars   in frame f_see_01_estabelecimento_empresa = 10.00
           bt_can:height-chars  in frame f_see_01_estabelecimento_empresa = 01.00
           bt_det2:width-chars  in frame f_see_01_estabelecimento_empresa = 10.00
           bt_det2:height-chars in frame f_see_01_estabelecimento_empresa = 01.00
           bt_fil:width-chars   in frame f_see_01_estabelecimento_empresa = 10.00
           bt_fil:height-chars  in frame f_see_01_estabelecimento_empresa = 01.00
           bt_hel2:width-chars  in frame f_see_01_estabelecimento_empresa = 10.00
           bt_hel2:height-chars in frame f_see_01_estabelecimento_empresa = 01.00
           bt_mod2:width-chars  in frame f_see_01_estabelecimento_empresa = 10.00
           bt_mod2:height-chars in frame f_see_01_estabelecimento_empresa = 01.00
           bt_ok:width-chars    in frame f_see_01_estabelecimento_empresa = 10.00
           bt_ok:height-chars   in frame f_see_01_estabelecimento_empresa = 01.00
           bt_ran:width-chars   in frame f_see_01_estabelecimento_empresa = 10.00
           bt_ran:height-chars  in frame f_see_01_estabelecimento_empresa = 01.00
           rt_cxcf:width-chars  in frame f_see_01_estabelecimento_empresa = 86.57
           rt_cxcf:height-chars in frame f_see_01_estabelecimento_empresa = 01.42
           rt_cxcl:width-chars  in frame f_see_01_estabelecimento_empresa = 87.57
           rt_cxcl:height-chars in frame f_see_01_estabelecimento_empresa = 01.25.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_see_estabelecimento_empresa:ALLOW-COLUMN-SEARCHING in frame f_see_01_estabelecimento_empresa = no
       br_see_estabelecimento_empresa:COLUMN-MOVABLE in frame f_see_01_estabelecimento_empresa = no.
end.
&endif
    /* set private-data for the help system */
    assign rs_see_estabelecimento_empresa:private-data in frame f_see_01_estabelecimento_empresa = "HLP=000009891":U
           br_see_estabelecimento_empresa:private-data in frame f_see_01_estabelecimento_empresa = "HLP=000009891":U
           bt_add2:private-data                        in frame f_see_01_estabelecimento_empresa = "HLP=000010825":U
           bt_mod2:private-data                        in frame f_see_01_estabelecimento_empresa = "HLP=000010827":U
           bt_det2:private-data                        in frame f_see_01_estabelecimento_empresa = "HLP=000010805":U
           bt_ran:private-data                         in frame f_see_01_estabelecimento_empresa = "HLP=000008967":U
           bt_fil:private-data                         in frame f_see_01_estabelecimento_empresa = "HLP=000008966":U
           bt_ok:private-data                          in frame f_see_01_estabelecimento_empresa = "HLP=000010721":U
           bt_can:private-data                         in frame f_see_01_estabelecimento_empresa = "HLP=000011050":U
           bt_hel2:private-data                        in frame f_see_01_estabelecimento_empresa = "HLP=000011326":U
           frame f_see_01_estabelecimento_empresa:private-data                                   = "HLP=000009891".



{include/i_fclfrm.i f_ran_01_estabelecimento_codigo f_ran_01_estabelecimento_nome_abrev f_ran_01_estabelecimento_nome_pessoa f_see_01_estabelecimento_empresa }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_codigo
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_codigo */

ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_nome_abrev
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_nome_abrev */

ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_nome_pessoa
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_estabelecimento_nome_pessoa */

ON INS OF br_see_estabelecimento_empresa IN FRAME f_see_01_estabelecimento_empresa
DO:

    if  bt_add2:sensitive in frame f_see_01_estabelecimento_empresa
    then do:
        apply "choose" to bt_add2 in frame f_see_01_estabelecimento_empresa.
    end /* if */.


END. /* ON INS OF br_see_estabelecimento_empresa IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_add2 IN FRAME f_see_01_estabelecimento_empresa
DO:

    assign v_rec_estabelecimento = v_rec_table.
    if  search("prgint/utb/utb071ca.r") = ? and search("prgint/utb/utb071ca.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb071ca.p".
        else do:
            message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb071ca.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb071ca.p /*prg_add_estabelecimento*/.
    assign v_rec_table = v_rec_estabelecimento.
    run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
    reposition qr_see_estabelecimento_empresa to recid v_rec_table no-error.


END. /* ON CHOOSE OF bt_add2 IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_can IN FRAME f_see_01_estabelecimento_empresa
DO:

    apply "end-error" to self.
END. /* ON CHOOSE OF bt_can IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_det2 IN FRAME f_see_01_estabelecimento_empresa
DO:

    if  avail estabelecimento
    then do:
        assign v_rec_estabelecimento = recid(estabelecimento).
        if  search("prgint/utb/utb071ia.r") = ? and search("prgint/utb/utb071ia.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb071ia.p".
            else do:
                message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb071ia.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgint/utb/utb071ia.p /*prg_det_estabelecimento*/.
        if  v_rec_estabelecimento <> ?
        then do:
            assign v_rec_table = v_rec_estabelecimento.
            reposition qr_see_estabelecimento_empresa to recid v_rec_table no-error.
        end /* if */.
    end /* if */.
END. /* ON CHOOSE OF bt_det2 IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_hel2 IN FRAME f_see_01_estabelecimento_empresa
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_mod2 IN FRAME f_see_01_estabelecimento_empresa
DO:

    if  avail estabelecimento
    then do:
        assign v_rec_estabelecimento = recid(estabelecimento)
               v_rec_table    = recid(estabelecimento).
        if  search("prgint/utb/utb071ea.r") = ? and search("prgint/utb/utb071ea.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb071ea.p".
            else do:
                message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb071ea.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgint/utb/utb071ea.p /*prg_mod_estabelecimento*/.
        if  v_rec_estabelecimento <> ?
        then do:
            assign v_rec_table = v_rec_estabelecimento.
        end /* if */.
        run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
        reposition qr_see_estabelecimento_empresa to recid v_rec_table no-error.
    end /* if */.
END. /* ON CHOOSE OF bt_mod2 IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_ok IN FRAME f_see_01_estabelecimento_empresa
DO:

    if  avail estabelecimento
    then do:
        assign v_rec_estabelecimento = recid(estabelecimento).
    end /* if */.
END. /* ON CHOOSE OF bt_ok IN FRAME f_see_01_estabelecimento_empresa */

ON CHOOSE OF bt_ran IN FRAME f_see_01_estabelecimento_empresa
DO:

    /* case_block: */
    case input frame f_see_01_estabelecimento_empresa rs_see_estabelecimento_empresa:
        when "Por Estabelecimento" /*l_por_estabelecimento*/ then
            code_block:
            do:

              /* Begin_Include: i_see_range */
              view frame f_ran_01_estabelecimento_codigo.

              range_block:
              do on error undo range_block, retry range_block:
                  update v_cod_estab_ini
                         v_cod_estab_fim
                         bt_ok
                         bt_can
                         bt_hel2
                         with frame f_ran_01_estabelecimento_codigo.
                  run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
              end /* do range_block */.

              hide frame f_ran_01_estabelecimento_codigo.

              /* End_Include: i_see_range */

            end /* do code_block */.
        when "Por Raz’o Social" /*l_por_razao_social*/ then
            razao_social_block:
            do:

              /* Begin_Include: i_see_range */
              view frame f_ran_01_estabelecimento_nome_pessoa.

              range_block:
              do on error undo range_block, retry range_block:
                  update v_nom_pessoa_ini
                         v_nom_pessoa_fim
                         bt_ok
                         bt_can
                         bt_hel2
                         with frame f_ran_01_estabelecimento_nome_pessoa.
                  run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
              end /* do range_block */.

              hide frame f_ran_01_estabelecimento_nome_pessoa.

              /* End_Include: i_see_range */

            end /* do razao_social_block */.
        when "Por Nome Abreviado" /*l_por_nome_abreviado*/ then
            nome_abrev_block:
            do:

              /* Begin_Include: i_see_range */
              view frame f_ran_01_estabelecimento_nome_abrev.

              range_block:
              do on error undo range_block, retry range_block:
                  update v_nom_abrev_ini
                         v_nom_abrev_fim
                         bt_ok
                         bt_can
                         bt_hel2
                         with frame f_ran_01_estabelecimento_nome_abrev.
                  run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
              end /* do range_block */.

              hide frame f_ran_01_estabelecimento_nome_abrev.

              /* End_Include: i_see_range */

            end /* do nome_abrev_block */.
    end /* case case_block */.
END. /* ON CHOOSE OF bt_ran IN FRAME f_see_01_estabelecimento_empresa */

ON VALUE-CHANGED OF rs_see_estabelecimento_empresa IN FRAME f_see_01_estabelecimento_empresa
DO:

    run pi_open_see_estabelecimento_empresa /*pi_open_see_estabelecimento_empresa*/.
END. /* ON VALUE-CHANGED OF rs_see_estabelecimento_empresa IN FRAME f_see_01_estabelecimento_empresa */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_ran_01_estabelecimento_codigo ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_estabelecimento_codigo */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_codigo ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_codigo */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_codigo ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_codigo */

ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_codigo
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_codigo */

ON HELP OF FRAME f_ran_01_estabelecimento_nome_abrev ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_estabelecimento_nome_abrev */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_nome_abrev ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_nome_abrev */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_nome_abrev ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_nome_abrev */

ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_nome_abrev
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_nome_abrev */

ON HELP OF FRAME f_ran_01_estabelecimento_nome_pessoa ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_estabelecimento_nome_pessoa */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_nome_pessoa ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_estabelecimento_nome_pessoa */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_nome_pessoa ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_estabelecimento_nome_pessoa */

ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_nome_pessoa
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_estabelecimento_nome_pessoa */

ON ENDKEY OF FRAME f_see_01_estabelecimento_empresa
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(estabelecimento).    
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
        assign v_rec_table_epc = recid(estabelecimento).    
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
        assign v_rec_table_epc = recid(estabelecimento).    
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

END. /* ON ENDKEY OF FRAME f_see_01_estabelecimento_empresa */

ON END-ERROR OF FRAME f_see_01_estabelecimento_empresa
DO:

    assign v_rec_estabelecimento = ?.
END. /* ON END-ERROR OF FRAME f_see_01_estabelecimento_empresa */

ON ENTRY OF FRAME f_see_01_estabelecimento_empresa
DO:

    apply "value-changed" to rs_see_estabelecimento_empresa in frame f_see_01_estabelecimento_empresa.

END. /* ON ENTRY OF FRAME f_see_01_estabelecimento_empresa */

ON HELP OF FRAME f_see_01_estabelecimento_empresa ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_see_01_estabelecimento_empresa */

ON RIGHT-MOUSE-DOWN OF FRAME f_see_01_estabelecimento_empresa ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_see_01_estabelecimento_empresa */

ON RIGHT-MOUSE-UP OF FRAME f_see_01_estabelecimento_empresa ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_see_01_estabelecimento_empresa */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_see_01_estabelecimento_empresa.





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


        assign v_nom_prog     = substring(frame f_see_01_estabelecimento_empresa:title, 1, max(1, length(frame f_see_01_estabelecimento_empresa:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "see_estabelecimento_empresa":U.




    assign v_nom_prog_ext = "prgint/utb/utb071na.p":U
           v_cod_release  = trim(" 1.00.00.006":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */

{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
/*
{include/i-ctrlrp5.i see_estabelecimento_empresa}
*/


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
    run pi_version_extract ('see_estabelecimento_empresa':U, 'prgint/utb/utb071na.p':U, '1.00.00.006':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'see_estabelecimento_empresa') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n’o ² um programa vÿlido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'see_estabelecimento_empresa')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usuÿrio sem permiss’o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'see_estabelecimento_empresa')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'see_estabelecimento_empresa' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'see_estabelecimento_empresa'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */


/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "see_estabelecimento_empresa":U
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


assign v_wgh_frame_epc = frame f_see_01_estabelecimento_empresa:handle.



assign v_nom_table_epc = 'estabelecimento':U
       v_rec_table_epc = recid(estabelecimento).

&endif

/* End_Include: i_verify_program_epc */



/* Begin_Include: ix_p00_see_estabelecimento_empresa */
assign frame f_see_01_estabelecimento_empresa:title = frame f_see_01_estabelecimento_empresa:title
                            + chr(32)
                            + "dos Estabelecimentos da" /*l_dos_estab_da*/ 
                            + chr(32)
                            + p_cod_empresa.
/* End_Include: ix_p00_see_estabelecimento_empresa */


/* redefini»„es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers’o */
assign frame f_see_01_estabelecimento_empresa:title = frame f_see_01_estabelecimento_empresa:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.006":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_see_01_estabelecimento_empresa = menu m_help:handle.


/* End_Include: i_std_dialog_box */

{include/title5.i f_see_01_estabelecimento_empresa FRAME}


assign br_see_estabelecimento_empresa:num-locked-columns in frame f_see_01_estabelecimento_empresa = 1.

pause 0 before-hide.
view frame f_see_01_estabelecimento_empresa.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(estabelecimento).    
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
    assign v_rec_table_epc = recid(estabelecimento).    
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
    assign v_rec_table_epc = recid(estabelecimento).    
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


assign v_rec_table    = v_rec_estabelecimento.

main_block:
do on endkey undo main_block, leave main_block on error undo main_block, leave main_block.
    display rs_see_estabelecimento_empresa
            with frame f_see_01_estabelecimento_empresa.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(estabelecimento).    
        run value(v_nom_prog_upc) (input 'DISPLAY',
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
        assign v_rec_table_epc = recid(estabelecimento).    
        run value(v_nom_prog_appc) (input 'DISPLAY',
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
        assign v_rec_table_epc = recid(estabelecimento).    
        run value(v_nom_prog_dpc) (input 'DISPLAY',
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

    enable rs_see_estabelecimento_empresa
           br_see_estabelecimento_empresa
           bt_add2
           bt_mod2
           bt_det2
           bt_ran
           bt_fil
           bt_ok
           bt_can
           bt_hel2
           with frame f_see_01_estabelecimento_empresa.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(estabelecimento).    
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
        assign v_rec_table_epc = recid(estabelecimento).    
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
        assign v_rec_table_epc = recid(estabelecimento).    
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

    assign bt_fil:sensitive in frame f_see_01_estabelecimento_empresa = no.
    if  index(program-name(2), "add_estabelecimento_empresa") <> 0
    or   index(program-name(2), "mod_estabelecimento_empresa") <> 0
    or   index(program-name(2), "det_estabelecimento_empresa") <> 0
    or   index(program-name(2), "era_estabelecimento_empresa") <> 0
    then do:
         assign bt_add2:sensitive in frame f_see_01_estabelecimento_empresa = no
                bt_mod2:sensitive in frame f_see_01_estabelecimento_empresa = no
                bt_det2:sensitive in frame f_see_01_estabelecimento_empresa = no.
    end /* if */.
    /* ix_p10_see_estabelecimento_empresa */
    /* *************
    @do(security_block) with frame @&(frame):
        @i(i_verify_security_button_sea &table=@&(table) &program_complement=@&(program_complement))
    @end_do(security_block).
    ***************/
    wait-for go of frame f_see_01_estabelecimento_empresa
          or default-action of br_see_estabelecimento_empresa
          or mouse-select-dblclick of br_see_estabelecimento_empresa.
    if  avail estabelecimento
    then do:
        assign v_rec_estabelecimento = recid(estabelecimento).
    end /* if */.
    /* ix_p20_see_estabelecimento_empresa */
end /* do main_block */.

hide frame f_see_01_estabelecimento_empresa.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    end /* if */.
    release log_exec_prog_dtsul.
end.

/* End_Include: i_log_exec_prog_dtsul_fim */




/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_open_see_estabelecimento_empresa
** Descricao.............: pi_open_see_estabelecimento_empresa
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 30/06/1995 17:16:25
*****************************************************************************/
PROCEDURE pi_open_see_estabelecimento_empresa:

    /* case_block: */
    case input frame f_see_01_estabelecimento_empresa rs_see_estabelecimento_empresa:
        when "Por Estabelecimento" /*l_por_estabelecimento*/ then
            code_block:
            do:
               open query qr_see_estabelecimento_empresa for
                    each estabelecimento no-lock
                    where estabelecimento.cod_empresa = p_cod_empresa
                      and estabelecimento.cod_estab >= v_cod_estab_ini
                      and estabelecimento.cod_estab <= v_cod_estab_fim /*cl_see_codigo of estabelecimento*/,
                    each ems5.pais no-lock
                    where pais.cod_pais = estabelecimento.cod_pais

                    by estabelecimento.cod_estab.
            end /* do code_block */.
        when "Por Raz’o Social" /*l_por_razao_social*/ then
            name_block:
            do:
            open query qr_see_estabelecimento_empresa for
                    each estabelecimento no-lock
                    where estabelecimento.cod_empresa = p_cod_empresa
                      and estabelecimento.nom_pessoa >= v_nom_pessoa_ini
                      and estabelecimento.nom_pessoa <= v_nom_pessoa_fim /*cl_see_razao_social of estabelecimento*/,
                    each pais no-lock
                    where pais.cod_pais = estabelecimento.cod_pais

                    by estabelecimento.nom_pessoa.
            end /* do name_block */.
        when "Por Nome Abreviado" /*l_por_nome_abreviado*/ then
            nome_abrev_block:
            do:
            open query qr_see_estabelecimento_empresa for
                    each estabelecimento no-lock
                    where estabelecimento.cod_empresa = p_cod_empresa
                      and estabelecimento.nom_abrev >= v_nom_abrev_ini
                      and estabelecimento.nom_abrev <= v_nom_abrev_fim /*cl_see_nome_abrev of estabelecimento*/,
                    each pais no-lock
                    where pais.cod_pais = estabelecimento.cod_pais

                    by estabelecimento.nom_abrev.
            end /* do nome_abrev_block */.
    end /* case case_block */.

END PROCEDURE. /* pi_open_see_estabelecimento_empresa */
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
            today                    at 84 format "99/99/99"
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
/********************  End of see_estabelecimento_empresa *******************/
