&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

    Programa: ESUT0004RP

    Objetivo: Consolidacao Criterios Dist CCusto

    Autor...: Eugˆnio A. Marietti [EMA] - DSC

    Data....: Out/2010

    Versao..: 5.05.00.000

  ----------------------------------------------------------------------*/
/*          This .P file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

def buffer empresa for ems5.empresa.

{include/i-prgvrs.i ESUT0004RP 5.06.00.000}

/* ***************************  Definitions  ************************** */
def temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    Field cod_ccusto                Like item_distrib_ccusto.cod_ccusto                Extent 2
    Field cod_empresa               Like item_distrib_ccusto.cod_empresa               Extent 2
    Field cod_estab                 Like item_distrib_ccusto.cod_estab                 Extent 2 Label 'Estabelec'
    Field cod_mapa_distrib_ccusto   Like item_distrib_ccusto.cod_mapa_distrib_ccusto   Extent 2 Label 'Mapa Dist' 
    Field cod_plano_ccusto          Like item_distrib_ccusto.cod_plano_ccusto          Extent 2 Label 'Plano CCusto' 
    Field cod_unid_negoc            Like item_distrib_ccusto.cod_unid_negoc            Extent 2 
    Field dat_criter_distrib_ccusto As Date Format '99/99/9999'                        Extent 2 Label 'Data'
    Field consolidar                As Log
    Field gera_arq                  As Log
    Field dir_saida                 As Char
    Field rpw                       As Log.

Def Temp-table tt-digita No-undo
    Field cod_usuario      Like usuar_mestre.cod_usuario
    Field cod_e_mail_local Like usuar_mestre.cod_e_mail_local
    Index pk_tt_digita     Is Primary Unique
          cod_usuario.

def temp-table tt-raw-digita
    field raw-digita as raw.

{utp/ut-glob.i}
{utp/utapi009.i}

Def Stream s-imp.
Def Stream s-arq.

def new global shared var c-dir-spool-servid-exec as char no-undo.                     

Def Var h-acomp        As Handle No-undo.
Def Var c-status-acomp As Char   No-undo.

Run pi_cria_param.

Find First tt-param No-lock No-error.

{include/i-rpvar.i} 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no


/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.25
         WIDTH              = 30.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 

/* ***************************  Main Block  *************************** */
Assign c-programa     = "ESUT0004RP"
       c-versao       = "5.06"
       c-revisao      = ".00.000"
       c-sistema      = "Especifico"
       c-titulo-relat = "Consolida‡Æo Crit‚rios Dist CCusto".

For First empresa Fields(nom_razao_social) No-lock
    Where empresa.cod_empresa = v_cod_empres_usuar :
    Assign c-empresa = empresa.nom_razao_social.
End.

Run utp/ut-acomp.p Persistent Set h-acomp.
Run pi-inicializar In h-acomp ("Aguarde processando ...").

{include/i-rpout.i &stream="Stream s-imp"}

Run pi-executar.

{include/i-rpclo.i &stream="Stream s-imp"}

Run pi-finalizar In h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-consolidar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-consolidar Procedure 
PROCEDURE pi-consolidar :
/*----------------------------------------------------------------------------*/
    Def Var de_qtd_tot As Dec No-undo.

    For Each es_item_distrib_ccusto_dia No-lock
       Where es_item_distrib_ccusto_dia.cod_estab                 >= tt-param.cod_estab                [1]
         And es_item_distrib_ccusto_dia.cod_estab                 <= tt-param.cod_estab                [2]
         And es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   >= tt-param.cod_mapa_distrib_ccusto  [1]
         And es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   <= tt-param.cod_mapa_distrib_ccusto  [2]
         And es_item_distrib_ccusto_dia.cod_unid_negoc            >= tt-param.cod_unid_negoc           [1]
         And es_item_distrib_ccusto_dia.cod_unid_negoc            <= tt-param.cod_unid_negoc           [2]
         And es_item_distrib_ccusto_dia.cod_empresa               >= tt-param.cod_empresa              [1]
         And es_item_distrib_ccusto_dia.cod_empresa               <= tt-param.cod_empresa              [2]
         And es_item_distrib_ccusto_dia.cod_plano_ccusto          >= tt-param.cod_plano_ccusto         [1]
         And es_item_distrib_ccusto_dia.cod_plano_ccusto          <= tt-param.cod_plano_ccusto         [2]
         And es_item_distrib_ccusto_dia.cod_ccusto                >= tt-param.cod_ccusto               [1]
         And es_item_distrib_ccusto_dia.cod_ccusto                <= tt-param.cod_ccusto               [2]
         And date(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto) >= date(tt-param.dat_criter_distrib_ccusto[1])
         And date(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto) <= date(tt-param.dat_criter_distrib_ccusto[2])
       Break By es_item_distrib_ccusto_dia.cod_estab
             By es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto
             By es_item_distrib_ccusto_dia.cod_unid_negoc
             By es_item_distrib_ccusto_dia.cod_empresa
             By es_item_distrib_ccusto_dia.cod_plano_ccusto
             By es_item_distrib_ccusto_dia.cod_ccusto :
        Run pi-acompanhar In h-acomp(Input es_item_distrib_ccusto_dia.cod_plano_ccusto + ' - ' + String(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto)).

        Assign de_qtd_tot = de_qtd_tot + es_item_distrib_ccusto_dia.qtd_criter_distrib_ccusto.

        If Last-of(es_item_distrib_ccusto_dia.cod_ccusto) Then Do:
            Find First item_distrib_ccusto Exclusive-lock
                 Where item_distrib_ccusto.cod_estab                 = es_item_distrib_ccusto_dia.cod_estab
                   And item_distrib_ccusto.cod_mapa_distrib_ccusto   = es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto
                   And item_distrib_ccusto.cod_unid_negoc            = es_item_distrib_ccusto_dia.cod_unid_negoc
                   And item_distrib_ccusto.cod_empresa               = es_item_distrib_ccusto_dia.cod_empresa
                   And item_distrib_ccusto.cod_plano_ccusto          = es_item_distrib_ccusto_dia.cod_plano_ccusto
                   And item_distrib_ccusto.cod_ccusto                = es_item_distrib_ccusto_dia.cod_ccusto No-error.
            If Avail item_distrib_ccusto Then Do:
                Assign item_distrib_ccusto.qtd_criter_distrib_ccusto = de_qtd_tot.
            End.
            Assign de_qtd_tot = 0.
        End.
    End.

    Put Stream s-imp
        Unformatted
        Skip(2)
        'Consolida‡Æo Conclu¡da!'
        Skip(2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-enviar-email) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-enviar-email Procedure 
PROCEDURE pi-enviar-email :
/*----------------------------------------------------------------------------*/
    Def Var c_lst_emails As Char No-undo.
    Def Var c_arq_anexo  As Char No-undo.
    Def Var c_usuar_aux  As Char No-undo.

    {include/i-rpclo.i &stream="Stream s-imp"}

    For Each tt-digita :
        If c_lst_emails <> '' Then
            Assign c_lst_emails = c_lst_emails + ','.

        Assign c_lst_emails = c_lst_emails + tt-digita.cod_e_mail_local.
    End.

    Assign c_usuar_aux = v_cod_usuar_corren.

    if v_cod_usuar_corren begins 'es_' then do:
       Assign c_usuar_aux = entry(2,v_cod_usuar_corren,"_").
    End.

    Find First usuar_mestre No-lock
         Where usuar_mestre.cod_usuario = c_usuar_aux No-error.
    If Not Avail usuar_mestre Then Do:
        Put Stream s-imp Unformatted
            Skip(2)
            'Registro Usu rio Mestre NÆo Encontrado (' c_usuar_aux ')!'
            Skip.
        Return.
    End.

    If tt-param.rpw Then
        Assign c_arq_anexo = c-dir-spool-servid-exec + "~/" + tt-param.arquivo.
    Else
        Assign c_arq_anexo = tt-param.arquivo.

    If Search(c_arq_anexo) = ? Then
        Assign c_arq_anexo = Session:Temp-dir + tt-param.arquivo.

    Create tt-envio.
    Assign tt-envio.versao-integracao = 1
           tt-envio.destino           = c_lst_emails
           tt-envio.remetente         = usuar_mestre.cod_e_mail_local
           tt-envio.assunto           = If tt-param.consolidar
                                        Then 'Consolidacao Criterios Dist CCusto'
                                        Else 'Verificacao Criterios Dist CCusto'
           tt-envio.arq-anexo         = c_arq_anexo
           tt-envio.exchange          = no
           tt-envio.mensagem          = 'Segue anexo relat¢rio de ' + tt-envio.assunto + ' (ESUT0004).'.

    Run utp/utapi009.p(Input  Table tt-envio,
                       Output Table tt-erros).

    If Return-value = "NOK" Then Do:
        Output To Value(tt-param.arquivo) No-convert Append.
        Put Unformatted
            Skip(2)
            'Email nÆo foi enviado !!!'
            Skip(2).

        For Each tt-erros:
            Disp tt-erros.cod-erro  Column-label 'Erro'
                 tt-erros.desc-erro Column-label 'Descri‡Æo Erro'
                                    View-as Editor Size 50 By 2
                 With Stream-io Width 200.
        End.
        Output Close.
    End.
    Else Do:
        Output To Value(tt-param.arquivo) Append.
        Put Unformatted Skip(2)
            'Email enviado com Sucesso !!!'
            Skip(2).
        Output Close.
    End.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/******************************************************************************/
    {include/i-rpcab.i}

    View Stream s-imp Frame f-cabec.
    View Stream s-imp Frame f-rodape.

    If tt-param.rpw Then Do:
        Assign tt-param.dat_criter_distrib_ccusto[1] = Date(Month(Today), 1, Year(Today))
               tt-param.dat_criter_distrib_ccusto[2] = Today.
    End.

    If Not tt-param.consolidar Or Can-find(First tt-digita) Then Do:
        Run pi-inicializar In h-acomp(Input "Verificando...").
        Run pi-verificar.
    End.

    If tt-param.consolidar Then Do:
        Run pi-inicializar In h-acomp(Input "Consolidando...").
        Run pi-consolidar.
    End.

    If tt-param.gera_arq Then Do:
        Run pi-inicializar In h-acomp(Input "Exportando...").
        Run pi-exportar.
        Run pi-gerar-excel.
    End.

    Run pi-params.

    If Can-find(First tt-digita) Then Do:
        Run pi-inicializar In h-acomp(Input "Enviando Email...").
        Run pi-enviar-email.
    End.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-exportar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exportar Procedure 
PROCEDURE pi-exportar :
/******************************************************************************/
    Def Var txt_decimal_separator   As Char         No-undo. /* separador decimal do windows */
    Def Var txt_value_separator     As Char         No-undo. /* Separador do Arquivo CSV */

    Load "Control Panel\International".

    Use "Control Panel\International".

    Get-key-value Section "" Key "sDecimal" Value txt_decimal_separator.

    Unload "Control Panel\International".

    Assign txt_value_separator = If txt_decimal_separator = ',' then ';' else ','.

    Output Stream s-arq To Value(Session:Temp-dir + 'ESUT0004.csv') No-convert.
    Put Stream s-arq
        "Estab"               txt_value_separator
        "Mapa Dist CCusto"    txt_value_separator
        "Un Neg"              txt_value_separator
        "Empresa"             txt_value_separator
        "Plano Centros Custo" txt_value_separator
        "Centro Custo"        txt_value_separator
        "Dt"                  txt_value_separator
        "Quantidade"          txt_value_separator
        Skip.

    For Each es_item_distrib_ccusto_dia No-lock
       Where es_item_distrib_ccusto_dia.cod_estab                 >= tt-param.cod_estab                [1]
         And es_item_distrib_ccusto_dia.cod_estab                 <= tt-param.cod_estab                [2]
         And es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   >= tt-param.cod_mapa_distrib_ccusto  [1]
         And es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   <= tt-param.cod_mapa_distrib_ccusto  [2]
         And es_item_distrib_ccusto_dia.cod_unid_negoc            >= tt-param.cod_unid_negoc           [1]
         And es_item_distrib_ccusto_dia.cod_unid_negoc            <= tt-param.cod_unid_negoc           [2]
         And es_item_distrib_ccusto_dia.cod_empresa               >= tt-param.cod_empresa              [1]
         And es_item_distrib_ccusto_dia.cod_empresa               <= tt-param.cod_empresa              [2]
         And es_item_distrib_ccusto_dia.cod_plano_ccusto          >= tt-param.cod_plano_ccusto         [1]
         And es_item_distrib_ccusto_dia.cod_plano_ccusto          <= tt-param.cod_plano_ccusto         [2]
         And es_item_distrib_ccusto_dia.cod_ccusto                >= tt-param.cod_ccusto               [1]
         And es_item_distrib_ccusto_dia.cod_ccusto                <= tt-param.cod_ccusto               [2]
         And date(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto) >= date(tt-param.dat_criter_distrib_ccusto[1])
         And date(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto) <= date(tt-param.dat_criter_distrib_ccusto[2]):
        Run pi-acompanhar In h-acomp(Input String(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto)).

        If txt_decimal_separator = ',' Then Do:
            Export Stream s-arq
                   Delimiter ';'
                   es_item_distrib_ccusto_dia.cod_estab
                   es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto
                   es_item_distrib_ccusto_dia.cod_unid_negoc
                   es_item_distrib_ccusto_dia.cod_empresa
                   es_item_distrib_ccusto_dia.cod_plano_ccusto
                   es_item_distrib_ccusto_dia.cod_ccusto
                   es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto
                   es_item_distrib_ccusto_dia.qtd_criter_distrib_ccusto.
        End.
        Else Do:
            Export Stream s-arq
                   Delimiter ','
                   es_item_distrib_ccusto_dia.cod_estab
                   es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto
                   es_item_distrib_ccusto_dia.cod_unid_negoc
                   es_item_distrib_ccusto_dia.cod_empresa
                   es_item_distrib_ccusto_dia.cod_plano_ccusto
                   es_item_distrib_ccusto_dia.cod_ccusto
                   es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto
                   es_item_distrib_ccusto_dia.qtd_criter_distrib_ccusto.
        End.
    End.
    Output Stream s-arq Close.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gerar-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gerar-excel Procedure 
PROCEDURE pi-gerar-excel :
/******************************************************************************/
    Def Var chExcelApplication  As Com-handle No-undo.
    Def Var chWorkBook          As Com-handle No-undo.
    Def Var chWorkSheet         As Com-handle No-undo.

    Run pi-inicializar In h-acomp(Input "Gerando Planilha Excel ...").

    Create "Excel.Application" chExcelApplication.

    chWorkBook = chExcelapplication:Workbooks:Add(Session:Temp-dir + 'ESUT0004.csv').

    chWorkSheet = chExcelApplication:Sheets:Item(1).

    chWorkSheet:Range("A1:H1"):Font:Bold = True.

    /* chWorkSheet:Columns("C:C"):NumberFormat = "00000000000000". */
    /* chWorkSheet:Columns("O:O"):NumberFormat = "0,00".           */

    chWorkSheet:Columns("A:H"):EntireColumn:AutoFit.

    chExcelApplication:DisplayAlerts = False.
    
    chWorkBook:SaveAs(tt-param.dir_saida + 'ESUT0004_' + Replace(String(Time, 'hh:mm:ss'), ':', '') + '.xlsx',,,,,,).

   /*  chWorkbook:SaveAs(tt-param.dir_saida + 'ESUT0004_' + Replace(String(Time, 'hh:mm:ss'), ':', '') + '.xlsx',43,,,,,). */
/*    */
    chExcelApplication:DisplayAlerts = True.

    chExcelApplication:Visible = True.

    Release Object chWorksheet.
    Release Object chWorkbook.
    Release Object chExcelApplication.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-params) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-params Procedure 
PROCEDURE pi-params :
/*----------------------------------------------------------------------------*/
    Page Stream s-imp.

    Disp Stream s-imp
         Entry(tt-param.destino, 'Impressora,Arquivo,Terminal') Label 'Destino'         Format 'X(10)'
         tt-param.arquivo                                       Label 'Arquivo'         Format 'X(40)'
         tt-param.usuario                                       Label 'Usu rio'
         tt-param.data-exec                                     Label 'Dt Execu‡Æo'
         String(hora-exec, 'hh:mm:ss')                          Label 'Hr Execu‡Æo'
         tt-param.cod_ccusto
         tt-param.cod_empresa
         tt-param.cod_estab
         tt-param.cod_mapa_distrib_ccusto
         tt-param.cod_plano_ccusto
         tt-param.cod_unid_negoc
         tt-param.dat_criter_distrib_ccusto
         tt-param.consolidar                                    Label 'Consolidar'      Format 'Sim/NÆo'
         tt-param.gera_arq                                      Label 'Gerar Arquivo'   Format 'Sim/NÆo'
         tt-param.dir_saida                                     Label 'Dir Sa¡da'
         With Side-label Stream-io 1 Col Width 300 Column 30.

    For Each tt-digita :
        Disp Stream s-imp
             tt-digita
             With Stream-io Width 300 Column 30.
    End.

    Page Stream s-imp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-verificar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verificar Procedure 
PROCEDURE pi-verificar :
/*----------------------------------------------------------------------------*/
    Def Var dt_aux  As Date No-undo.
    Def Var dt_mes  As Date No-undo Extent 2.
    Def Var l_first As Log  No-undo.

    Assign dt_mes[1] = tt-param.dat_criter_distrib_ccusto[1]
           dt_mes[2] = tt-param.dat_criter_distrib_ccusto[2].

    For Each mapa_distrib_ccusto No-lock
       Where mapa_distrib_ccusto.dat_inic_valid              >= 10/01/2010
         And mapa_distrib_ccusto.dat_fim_valid               >= Today
         And mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto  = 'Autom tico'
         And mapa_distrib_ccusto.cod_estab                   >= tt-param.cod_estab              [1]
         And mapa_distrib_ccusto.cod_estab                   <= tt-param.cod_estab              [2]
         And mapa_distrib_ccusto.cod_mapa_distrib_ccusto     >= tt-param.cod_mapa_distrib_ccusto[1]
         And mapa_distrib_ccusto.cod_mapa_distrib_ccusto     <= tt-param.cod_mapa_distrib_ccusto[2]
         And mapa_distrib_ccusto.cod_empresa                 >= tt-param.cod_empresa            [1]
         And mapa_distrib_ccusto.cod_empresa                 <= tt-param.cod_empresa            [2]
         And mapa_distrib_ccusto.cod_plano_ccusto            >= tt-param.cod_plano_ccusto       [1]
         And mapa_distrib_ccusto.cod_plano_ccusto            <= tt-param.cod_plano_ccusto       [2]:
        Disp Stream s-imp
             mapa_distrib_ccusto.cod_estab
             mapa_distrib_ccusto.cod_mapa_distrib_ccusto
             mapa_distrib_ccusto.cod_empresa
             mapa_distrib_ccusto.cod_plano_ccusto
             With Stream-io Width 200.

        For Each item_distrib_ccusto No-lock
           Where item_distrib_ccusto.cod_estab                  = mapa_distrib_ccusto.cod_estab
             And item_distrib_ccusto.cod_mapa_distrib_ccusto    = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
             And item_distrib_ccusto.cod_empresa                = mapa_distrib_ccusto.cod_empresa
             And item_distrib_ccusto.cod_plano_ccusto           = mapa_distrib_ccusto.cod_plano_ccusto
             And item_distrib_ccusto.cod_unid_negoc            >= tt-param.cod_unid_negoc[1]
             And item_distrib_ccusto.cod_unid_negoc            <= tt-param.cod_unid_negoc[2]
             And item_distrib_ccusto.cod_ccusto                >= tt-param.cod_ccusto    [1]
             And item_distrib_ccusto.cod_ccusto                <= tt-param.cod_ccusto    [2]:
            Assign l_first = Yes.

            Do dt_aux = dt_mes[1] To dt_mes[2] :
                Run pi-acompanhar In h-acomp(Input String(dt_aux, '99/99/9999') + ' - ' + item_distrib_ccusto.cod_ccusto).

                If Can-find(First es_item_distrib_ccusto_dia
                            Where es_item_distrib_ccusto_dia.cod_estab                 = item_distrib_ccusto.cod_estab
                              And es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   = item_distrib_ccusto.cod_mapa_distrib_ccusto
                              And es_item_distrib_ccusto_dia.cod_unid_negoc            = item_distrib_ccusto.cod_unid_negoc
                              And es_item_distrib_ccusto_dia.cod_empresa               = item_distrib_ccusto.cod_empresa
                              And es_item_distrib_ccusto_dia.cod_plano_ccusto          = item_distrib_ccusto.cod_plano_ccusto
                              And es_item_distrib_ccusto_dia.cod_ccusto                = item_distrib_ccusto.cod_ccusto
                              And date(es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto) = date(dt_aux)) Then Next.

                If l_first Then Do:
                    Put Stream s-imp
                        Unformatted
                        Skip(1)
                        'Un Neg Centro Custo Dt Sem Lancto' Skip
                        '------ ------------ -------------'.
                End.

                Disp Stream s-imp
                     item_distrib_ccusto.cod_unid_negoc At  1 When l_first
                     item_distrib_ccusto.cod_ccusto     At  8 When l_first
                     dt_aux                             At 21
                     With No-labels Frame fItemDistrib Stream-io Width 200 Down.

                Down Stream s-imp
                     With Frame fItemDistrib.

                Assign l_first = No.
            End.
        End.
    End.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_cria_param) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_cria_param Procedure 
PROCEDURE pi_cria_param :
    Find First dwb_rpt_param No-lock
         Where dwb_rpt_param.cod_dwb_program = "esut0004rp"
           And dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
    If Avail dwb_rpt_param Then Do:
        Create tt-param.
        Assign tt-param.destino   = Lookup(dwb_rpt_param.cod_dwb_output, "Impressora,Arquivo,Terminal")
               tt-param.arquivo   = dwb_rpt_param.cod_dwb_file
               tt-param.usuario   = v_cod_usuar_corren
               tt-param.data-exec = Today
               tt-param.hora-exec = Time
               tt-param.rpw       = (dwb_rpt_param.ind_dwb_run_mode = "Batch").

        For Each dwb_rpt_select No-lock
           Where dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
             And dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user :

            Case dwb_rpt_select.cod_dwb_field :
                When 'cod_ccusto' Then Do:
                    Assign tt-param.cod_ccusto[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_ccusto[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'cod_empresa' Then Do:
                    Assign tt-param.cod_empresa[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_empresa[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'cod_estab' Then Do:
                    Assign tt-param.cod_estab[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_estab[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'cod_mapa_distrib_ccusto' Then Do:
                    Assign tt-param.cod_mapa_distrib_ccusto[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_mapa_distrib_ccusto[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'cod_plano_ccusto' Then Do:
                    Assign tt-param.cod_plano_ccusto[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_plano_ccusto[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'cod_unid_negoc' Then Do:
                    Assign tt-param.cod_unid_negoc[1] = dwb_rpt_select.cod_dwb_initial
                           tt-param.cod_unid_negoc[2] = dwb_rpt_select.cod_dwb_final.
                End.

                When 'dat_criter_distrib_ccusto' Then Do:
                    Assign tt-param.dat_criter_distrib_ccusto[1] = Date(dwb_rpt_select.cod_dwb_initial)
                           tt-param.dat_criter_distrib_ccusto[2] = Date(dwb_rpt_select.cod_dwb_final).
                End.

                When 'consolidar' Then Do:
                    Assign tt-param.consolidar = (dwb_rpt_select.cod_dwb_initial = "Yes").
                End.

                When 'gera_arq' Then Do:
                    Assign tt-param.gera_arq = (dwb_rpt_select.cod_dwb_initial = "Yes").
                End.

                When 'dir_saida' Then Do:
                    Assign tt-param.dir_saida = dwb_rpt_select.cod_dwb_initial.
                End.

                When 'cod_e_mail_local' Then Do:
                    Create tt-digita.
                    Assign tt-digita.cod_usuario      = String(dwb_rpt_select.num_dwb_order)
                           tt-digita.cod_e_mail_local = dwb_rpt_select.cod_dwb_initial.
                End.
            End Case.
        End.
    End.
End Procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
