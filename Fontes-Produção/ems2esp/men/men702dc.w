{include/i-prgvrs.i MEN702DC 2.00.00.003 } /*** 010003 ***/
{include/i_fcldef.i}
/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_executar_programa
** Descricao.............: Funá‰es
** Versao................:  1.00.000
** Procedimento..........: tar_seguranca_menu
** Nome Externo..........: MEN/men702dc.w
** Criado por............: Rodrigo
** Criado em.............: 23/04/1997 15:26:36
*****************************************************************************/
{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i-epc200.i fnc_executar_programa}
/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=8":U.
/*************************************  *************************************/

/********************** Temporary Table Definition Begin **********************/
def temp-table tt_window_session no-undo
    field ttv_wgh_window                   as widget-handle format ">>>>>>9"
    field ttv_log_sensitive                as logical format "Sim/N∆o" initial yes
    field ttv_log_visible                  as logical format "Sim/N∆o" initial yes.

/********************** Temporary Table Definition End **********************/

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
def new global shared var v_cod_dwb_user
    as character
    format "x(12)"
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)"
    label "Empresa"
    column-label "Empresa"
    no-undo.

{utp/ut-glob.i}    

def new global shared var v_cod_estab_usuar
    as character
    format "x(3)"
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)"
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)"
    label "Grupo Usu†rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)"
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)"
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)"
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)"
    label "Pa°s Empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)"
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def var v_cod_prog_dtsul
    LIKE prog_dtsul.cod_prog_dtsul
    label "Programa"
    column-label "Programa"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)"
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)"
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)"
    no-undo.
def var v_nom_filename
    as character
    format "x(80)"
    view-as editor max-chars 250 no-word-wrap
    size 40 by 1
    bgcolor 15 font 2
    label "Nome Arquivo"
    no-undo.
def var v_nom_name
    as character
    format "x(20)"
    extent 10
    no-undo.  
def var v_nom_title
    as character
    format "x(40)"
    no-undo.
def var v_des_filespec
    as character
    format "x(10)"
    extent 10
    no-undo.
def var v_log_answer
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9"
    no-undo.
define new global shared var l-autconlist as logical no-undo.    
def new global shared var v2_cod_empres_usuar
    as character
    format 'x(3)'
    label 'Empresa'
    column-label 'Empresa'
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9"
    no-undo.
def var v_log_ok 
    as logical 
    initial no.    
def var h_prog_handle
    as handle
    no-undo.
def var v_nom_programa
    like prog_dtsul.nom_prog_ext 
    no-undo.
def var v_wgh_var_glob
    as widget-handle
    format ">>>>>>9"
    no-undo.
def var v_wgh_window
    as widget-handle
    format ">>>>>>9"
    no-undo.    
def var h-teste as handle no-undo.

/*corp340762*/
def new global shared var v_cod_modul_dtsul_multi as CHARACTER format "x(3)":U  no-undo.
/*corp340762*/
/************************** Variable Definition End *************************/
/*************************** Menu Definition Begin **************************/
.
def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".
/************************ Rectangle Definition Begin ************************/
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
/************************* Rectangle Definition End *************************/
/************************** Button Definition Begin *************************/
def button bt_can
    label "Cancela"
    size 10 by 1
    auto-endkey.
def button bt_hel2
    label "Ajuda"
    size 10 by 1.
def button bt_ok
    label "OK"
    size 10 by 1
    auto-go.
def button bt_get_file
    label "Pesquisa Arquivo"
    image-up file "image/im-sea1"
    image-insensitive file "image/ii-sea1"
    size 4 by 1.1.
/****************************** Function Button *****************************/
FUNCTION changeFileExt RETURNS CHAR ( INPUT cFilename AS CHAR,
                                      INPUT cNewExtension AS CHAR) FORWARD.

/*************************** Button Definition End **************************/
/************************** Frame Definition Begin **************************/
def frame f_dlg_03_executar_programa
    rt_cxcf
         at row 3.92 col 02.00 bgcolor 7 
    v_cod_prog_dtsul
         at row 02.00 col 11.43 colon-aligned label "Programa"
         help "C¢digo do Programa"
         view-as EDITOR
         size-chars 51.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_get_file
         at row 01.90 col 64.70 font ?
         help "Pesquisa Arquivo"
    bt_ok
         at row 4.13 col 03.00 font ?
         help "OK"
    bt_can
         at row 4.13 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 4.13 col 59.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 72.00 by 5.82 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Executa Programa".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars        in frame f_dlg_03_executar_programa = 10.00
           bt_can:height-chars       in frame f_dlg_03_executar_programa = 01.00
           bt_hel2:width-chars       in frame f_dlg_03_executar_programa = 10.00
           bt_get_file:width-chars   in frame f_dlg_03_executar_programa = 04.00
           bt_get_file:height-chars  in frame f_dlg_03_executar_programa = 01.08
           bt_hel2:height-chars      in frame f_dlg_03_executar_programa = 01.00
           bt_ok:width-chars         in frame f_dlg_03_executar_programa = 10.00
           bt_ok:height-chars        in frame f_dlg_03_executar_programa = 01.00
           rt_cxcf:width-chars       in frame f_dlg_03_executar_programa = 69.57
           rt_cxcf:height-chars      in frame f_dlg_03_executar_programa = 01.42.
    /* set private-data for the help system */
    assign v_cod_prog_dtsul:private-data           in frame f_dlg_03_executar_programa = "HLP=000000000":U
           bt_ok:private-data                      in frame f_dlg_03_executar_programa = "HLP=000010721":U
           bt_can:private-data                     in frame f_dlg_03_executar_programa = "HLP=000011050":U
           bt_hel2:private-data                    in frame f_dlg_03_executar_programa = "HLP=000011326":U
           bt_get_file:private-data                in frame f_dlg_03_executar_programa = "HLP=000008782":U
           frame f_dlg_03_executar_programa:private-data                               = "HLP=000000000".
{include/i_fclfrm.i f_dlg_03_executar_programa }
/*************************** Frame Definition End ***************************/
/*********************** User Interface Trigger Begin ***********************/
ON "CTRL-ALT-X" OF FRAME f_dlg_03_executar_programa ANYWHERE DO:
    /* N∆o deixa executar 2 CTRL-ALT-X */
END.

ON CHOOSE OF bt_get_file IN FRAME f_dlg_03_executar_programa
DO:
    assign v_nom_name[1] = 'All Source(*.p,*.i,*.w,*.r)'
           v_des_filespec[1] = '*.p;*.i;*.w;*.r'
           v_nom_name[2] = 'Procedures(*.p)'
           v_des_filespec[2] = '*.p'
           v_nom_name[3] = 'Windows(*.w)'
           v_des_filespec[3] = '*.w'
           v_nom_name[4] = 'Includes(*.i)'
           v_des_filespec[4] = '*.i'
           v_nom_name[5] = 'Compiled Files(*.r)'
           v_des_filespec[5] = '*.r'.
    run pi_system_dialog_get_file /*pi_system_dialog_get_file*/.
    assign v_cod_prog_dtsul:screen-value in frame f_dlg_03_executar_programa = v_nom_filename.
END. /* ON CHOOSE OF bt_get_file IN FRAME f_dlg_03_executar_programa */
ON CHOOSE OF bt_ok IN FRAME f_dlg_03_executar_programa
DO:

    assign v_cod_prog_dtsul = input frame f_dlg_03_executar_programa v_cod_prog_dtsul.
    if  session:set-wait-state('general')
    then do:
    end /* if */.
    hide frame f_dlg_03_executar_programa.
    search_block:
    do:

    /*################## TRATAMENTO PARA N«O EXECUTAR .R ######################*/

      IF index(v_cod_grp_usuar_lst,"SU3") = 0 THEN DO:
            IF R-INDEX(v_cod_prog_dtsul,".") <> 0 THEN DO:
                MESSAGE "Vocà n∆o possui permiss∆o para executar este programa, entre em contato com o Administrador do Sistema"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    ASSIGN v_cod_prog_dtsul = "".
            END.
        END.

    /*################## TRATAMENTO PARA N«O EXECUTAR .R ######################*/

        /*tech868*/
        ASSIGN v_nom_programa = v_cod_prog_dtsul.
        IF INDEX(v_cod_prog_dtsul,".") <> 0  THEN DO:
            v_nom_programa = REPLACE(v_nom_programa,"~\","/").
            FIND FIRST prog_dtsul NO-LOCK where prog_dtsul.nom_prog_ext BEGINS SUBSTRING(v_nom_programa,1, INDEX(v_nom_programa,".")) NO-ERROR.
            IF AVAIL prog_dtsul THEN
                ASSIGN v_cod_prog_dtsul = prog_dtsul.cod_prog_dtsul.
       END.
       /*tech868*/
       /****************************************************************************************************/
       /****************************************************************************************************/
        {include/i-epc200.i2 &CodEvent       = '"execution_program_1x"':U
                             &CodParameter   = '"v_cod_prog_dtsul"':U
                             &ValueParameter = "v_cod_prog_dtsul"}
        {include/i-epc201.i "execution_program_1x"} /* este ponto UPC poder† ser utilizado para bloquear a execuá∆o de programas que n∆o encontram-se cadastrados no m¢dulo de Menu. O conteudo enviado refere-se ao valor digitado em tela pelo usuario */
        IF RETURN-VALUE = 'NOK':U THEN 
            LEAVE search_block.
       /****************************************************************************************************/
       /****************************************************************************************************/

        find prog_dtsul no-lock
            where prog_dtsul.cod_prog_dtsul = v_cod_prog_dtsul /*cl_key_var of prog_dtsul*/ no-error.

        if  not avail prog_dtsul
        then do:
            if  search (v_cod_prog_dtsul) = ? and changefileext(search(substring(v_cod_prog_dtsul,1,r-index(v_cod_prog_dtsul,".")) + "r"), '.r') = ?
            then do:
                run utp/ut-msgs.p (input "show",
                                   input 4,
                                   input v_cod_prog_dtsul).
                leave search_block.
            end.
            else do:
                run pi-seta-return-value(input "").
                for each tt-epc
                    where tt-epc.cod-event = "execution_program":U.
                    delete tt-epc.
                end.
                {include/i-epc200.i2 &CodEvent       = '"execution_program"':U
                                     &CodParameter   = '"v_cod_prog_dtsul"':U
                                     &ValueParameter = "v_cod_prog_dtsul"}
                {include/i-epc201.i "execution_program"} /* este ponto UPC poder† ser utilizado para bloquear a execuá∆o de programas que n∆o encontram-se cadastrados no m¢dulo de Menu. O conteudo enviado refere-se ao valor digitado em tela pelo usuario */
                IF RETURN-VALUE <> 'NOK':U THEN DO:

                    run value(v_cod_prog_dtsul) persistent set h_prog_handle.

                    if  valid-handle(h_prog_handle)
                    and h_prog_handle:type = "procedure":U
                    and h_prog_handle:file-name = v_cod_prog_dtsul then do:
                        IF RETURN-VALUE <> "ADM-ERROR" AND 
                           RETURN-VALUE <> "NOK" THEN
                            run dispatch in h_prog_handle ('initialize') no-error.
                    end.               
                        /* run value(v_cod_prog_dtsul). */                            
                END. /* if */
                assign v_log_ok = yes.
            end.
        end.
        else do:
            /*
            assign v_nom_programa = substring(prog_dtsul.nom_prog_ext,1,length(prog_dtsul.nom_prog_ext) - 1) + 'r'. 
            */

            /*corp340762*/
            /* buscar o m¢dulo */
            FOR FIRST procedimento
                WHERE procedimento.cod_proced = prog_dtsul.cod_proced NO-LOCK:
                ASSIGN v_cod_modul_dtsul_multi = procedimento.cod_modul_dtsul.
            END.
            /*corp340762*/
            assign v_nom_programa = changefileext(substring(prog_dtsul.nom_prog_ext,1,length(prog_dtsul.nom_prog_ext) - 1) + 'r', '.r'). 

            if search(prog_dtsul.nom_prog_ext) = ? and 
               search(v_nom_programa) = ? 
            then do:
                run utp/ut-msgs.p (input "show",
                                   input 4,
                                   input v_cod_prog_dtsul).
                leave search_block.
            end.
            else do:
                if prog_dtsul.idi_template = 20 then do:
                    run utp/ut-msgs.p (input "show",
                                       input 15538,
                                       input substitute
                                             ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")).
                    return .
                end.
                IF prog_dtsul.idi_interfac = 2 THEN  DO:
                    run utp/ut-msgs.p (input "show",
                                       input 52987,
                                       input "").
                    return .
                END.
/*                run value(prog_dtsul.nom_prog_ext) persistent set h_prog_handle. */


                if  prog_dtsul.log_outro_produt_dtsul
                then do:
                     def var wh_w_outro_produto
                         as widget-handle
                         no-undo.
                     create window wh_w_outro_produto
                         assign
                             row                  = 01.00
                             col                  = 01.00
                             height-chars         = 00.01
                             width-chars          = 00.01
                             min-width-chars      = 00.00
                             min-height-chars     = 00.00
                             max-width-chars      = 01.00
                             max-height-chars     = 01.00
                             virtual-width-chars  = 00.01
                             virtual-height-chars = 00.01
                             title                = "Conex∆o com EMS 5" 
                             resize               = yes
                             scroll-bars          = no
                             status-area          = no
                             status-area-font     = ?
                             message-area         = no
                             message-area-font    = ?
                             fgcolor              = ?
                             bgcolor              = ?.
                    {utp/ut-liter.i Conex∆o_com_EMS5 * C}
                    assign wh_w_outro_produto:title  = return-value.

                    assign v_wgh_window = session:first-child.
                    desativa_janela:
                    do while valid-handle(v_wgh_window):
                          create tt_window_session.
                          assign tt_window_session.ttv_wgh_window     = v_wgh_window
                                 tt_window_session.ttv_log_sensitive  = v_wgh_window:sensitive
                                 tt_window_session.ttv_log_visible    = v_wgh_window:visible
                                 v_wgh_window:visible                 = no.
                          assign v_wgh_window = v_wgh_window:next-sibling.       
                    end /* do desativa_janela */.           
                    run btb/btb925za.p persistent set v_wgh_var_glob /*prg_fnc_recuperar_variaveis_globais*/.
                    run pi_gravar_variaveis_globais_produto in v_wgh_var_glob /*pi_gravar_variaveis_globais_produto*/.
                    assign current-window = wh_w_outro_produto.
                    do on stop undo, leave:

                       run value(prog_dtsul.nom_prog_ext) /*prg_value(p_nom_prog_ext)*/.

                    end.   
                end /* if */.
                else do:

                /* FO 1842.608 - corp340521 - 02/09/2008 */
                    run value(prog_dtsul.nom_prog_ext) /*prg_value(p_nom_prog_ext)*/ persistent set h_prog_handle.
                /* FO 1842.608 - corp340521 - 02/09/2008 */

                end /* else */.

				if  avail prog_dtsul
				then do:
				   if  prog_dtsul.log_outro_produt_dtsul
				   then do:
					   run pi_recuperar_variaveis_globais_produto in v_wgh_var_glob /*pi_recuperar_variaveis_globais_produto*/.
					   delete procedure v_wgh_var_glob.
					   assign v_wgh_window = session:first-child.
					   ativa_janela:
					   do while valid-handle(v_wgh_window):
						  find tt_window_session where ttv_wgh_window = v_wgh_window no-error.
						  if  avail tt_window_session
						  then do:
							  assign v_wgh_window:visible                 = tt_window_session.ttv_log_visible.
							  delete tt_window_session.
						  end /* if */.
						  assign v_wgh_window = v_wgh_window:next-sibling.                
					   end /* do ativa_janela */.
					   if valid-handle(wh_w_outro_produto) then do:
						   delete widget wh_w_outro_produto.
					   end.

					   apply "entry" to current-window.
				   end /* if */.
				end /* if */.
				if  valid-handle(h_prog_handle)
				and h_prog_handle:type = "procedure":U
				and h_prog_handle:file-name = prog_dtsul.nom_prog_ext then do:
					IF RETURN-VALUE <> "ADM-ERROR" AND 
					   RETURN-VALUE <> "NOK" THEN
						run dispatch in h_prog_handle ('initialize') no-error.
				end.               
				/* run value(prog_dtsul.nom_prog_ext) */.
				assign v_log_ok = yes.
				/*corp340762*/
				/* Executa procedure de contagem de acessos */
				/*Dispon°vel a partir da branch 1.2.6.1*/
				RUN pi_conta_acesso (INPUT ROWID(prog_dtsul)).
				/*corp340762*/
            end.
        end.
    end /* search_block */.    
	
    usuar_rec:
    DO TRANSACTION:             
        IF v_log_ok THEN DO:
            FIND FIRST prog_dtsul WHERE prog_dtsul.cod_prog_dtsul = v_cod_prog_dtsul NO-ERROR.
            IF AVAIL prog_dtsul THEN DO:                                
                FIND FIRST usuar_rec WHERE usuar_rec.cod_usuar_produt = v_cod_usuar_corren
                AND  usuar_rec.cod_function = prog_dtsul.cod_prog_dtsul NO-ERROR.
                IF NOT AVAIL usuar_rec THEN DO:                            
                    CREATE usuar_rec.
                    ASSIGN usuar_rec.cod_usuar_produt = v_cod_usuar_corren
                           usuar_rec.cod_function     = prog_dtsul.cod_prog_dtsul
                           usuar_rec.dat_atualiz      = TODAY
                           usuar_rec.hra_atualiz      = REPLACE(STRING(TIME, "HH:MM:SS"),":","").                                                           
                END.                            
            END.                    
        END.
        RELEASE usuar_rec no-error.                 
    END /* do usuar_rec */.
	
    if  session:set-wait-state("")
    then do:
    end /* if */.

END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_03_executar_programa */
ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_executar_programa
DO:
    /* Begin_Include: i_context_help_frame */
    run men/men900za.p (input self:frame,
                               Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help_frame */
END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_executar_programa */
ON ENTER OF v_cod_prog_dtsul IN FRAME f_dlg_03_executar_programa 
OR F2 OF v_cod_prog_dtsul IN FRAME f_dlg_03_executar_programa 
DO:
    apply "choose" to bt_ok.
END. /* ON ENTER OF v_cod_prog_dtsul IN FRAME f_dlg_03_executar_programa */
/************************ User Interface Trigger End ************************/
/**************************** Frame Trigger Begin ***************************/
ON HELP OF FRAME f_dlg_03_executar_programa ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run men/men900za.p (input self:handle,
                               Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END. /* ON HELP OF FRAME f_dlg_03_executar_programa */
ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_executar_programa ANYWHERE
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
        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end /* if */.
    /* End_Include: i_right_mouse_down_dialog_box */
END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_executar_programa */
ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_executar_programa ANYWHERE
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
        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end /* if */.
    /* End_Include: i_right_mouse_up_dialog_box */
END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_executar_programa */
ON WINDOW-CLOSE OF FRAME f_dlg_03_executar_programa
DO:
    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_dlg_03_executar_programa */
/***************************** Frame Trigger End ****************************/
/**************************** Menu Trigger Begin ****************************/
ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
        apply "choose" to bt_hel2 in frame f_dlg_03_executar_programa.
END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */
ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:
    /************************* Variable Definition Begin ************************/
    def var v_cod_release
        as character
        format "x(9)"
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
        assign v_nom_prog     = substring(frame f_dlg_03_executar_programa:title, 1, max(1, length(frame f_dlg_03_executar_programa:title) - 10))
                              + chr(10)
                              + "fnc_executar_programa":U.
    assign v_nom_prog_ext = "MEN/men702dc.w":U
           v_cod_release  = c-prg-vrs.
/*    run btb/btb901zb.p (input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */

{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */
/***************************** Menu Trigger End *****************************/
/****************************** Main Code Begin *****************************/

{utp/ut-liter.i Programa * R}
ASSIGN v_cod_prog_dtsul:LABEL = TRIM(RETURN-VALUE).

if  search("btb/btb906za.r") = ? and search("btb/btb906za.p") = ? then do:
    run utp/ut-msgs.p (input "show",
                     input 4,
                     input "btb/btb906za.p").
    stop.
end.
else do:
    run btb/btb906za.p /*prg_fnc_verify_controls*/.
end.

  run men/men901za.p (input "fnc_executar_programa").

  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input "fnc_executar_programa").
      if "{&procedure-type}" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input "fnc_executar_programa").
      if "{&procedure-type}" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.

/* End_Include: i_verify_security */
/* Begin_Include: i_log_exec_prog_dtsul_ini */
log_exec:
do transaction:
    assign v_rec_log = ?.
    find prog_dtsul no-lock
         where prog_dtsul.cod_prog_dtsul = '{&program}' /*cl_program of prog_dtsul*/ no-error.
    if  available prog_dtsul
    and  prog_dtsul.log_gera_log_exec = yes
    then do:
        create log_exec_prog_dtsul.
        assign log_exec_prog_dtsul.cod_prog_dtsul           = 'fnc_executar_programa'
               log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
               log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
        assign v_rec_log = recid(log_exec_prog_dtsul).
        release log_exec_prog_dtsul no-error.
    end /* if */.
end /* do log_exec */.
/* End_Include: i_log_exec_prog_dtsul_ini */
/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_dlg_03_executar_programa:title = frame f_dlg_03_executar_programa:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_dlg_03_executar_programa = menu m_help:handle.
/* End_Include: i_std_dialog_box */

{include/title5.i f_dlg_03_executar_programa FRAME}
pause 0 before-hide.
run utp/ut-trdfr.p (input frame f_dlg_03_executar_programa:handle, input 'fnc_executar_programa', input c-prg-vrs).
view frame f_dlg_03_executar_programa.
main_block:
repeat while v_log_ok = no :
    update v_cod_prog_dtsul bt_ok bt_can bt_hel2 bt_get_file with frame f_dlg_03_executar_programa.
end /* do main_block */.
hide frame f_dlg_03_executar_programa.
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
return.
/******************************* Main Code End ******************************/
/*************************************  *************************************/
/*****************************************************************************
** Procedure Interna.....: pi_system_dialog_get_file
** Descricao.............: pi_system_dialog_get_file
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 17/04/1995 13:51:55
*****************************************************************************/
PROCEDURE pi_system_dialog_get_file:
    system-dialog get-file v_nom_filename
        title v_nom_title
        filters v_nom_name[1]  v_des_filespec[1] ,
                v_nom_name[2]  v_des_filespec[2] ,
                v_nom_name[3]  v_des_filespec[3] ,   
                v_nom_name[4]  v_des_filespec[4] ,
                v_nom_name[5]  v_des_filespec[5] ,
                v_nom_name[6]  v_des_filespec[6] ,
                v_nom_name[7]  v_des_filespec[7] ,
                v_nom_name[8]  v_des_filespec[8] ,   
                v_nom_name[9]  v_des_filespec[9] ,
                v_nom_name[10] v_des_filespec[10]
        must-exist
        initial-dir v_nom_filename
        use-filename
        update v_log_answer.

END PROCEDURE. /* pi_system_dialog_get_file */
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
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.
    run value(c_prg_msg + ".p") (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/********************  End of fnc_executar_programa *******************/

/*Procedures Contador*/
/*corp340762*/

{men/men906za.i}

FUNCTION changeFileExt RETURNS CHAR (   INPUT cFilename AS CHAR,
                                        INPUT cNewExtension AS CHAR):

    DEFINE VARIABLE cExtension AS CHARACTER   NO-UNDO.

    IF  NUM-ENTRIES(cFilename, ".") = 0 THEN
        RETURN cFilename + cNewExtension.

    cExtension = "." + ENTRY( NUM-ENTRIES(cFilename, ".") , cFilename, ".").

    RETURN REPLACE(cFilename, cExtension, cNewExtension).
END.
