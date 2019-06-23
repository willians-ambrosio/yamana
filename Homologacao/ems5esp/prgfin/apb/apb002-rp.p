/****************************************************************************************** 
**             Programa: apb002-rp.p
**            Autor: Vando Ribeiro
**       Fornecedor: DKP
**            Data: 20/11/2018
**  Change/Chamado: 
**        Objetivo: Relat¢rio de T°tulos e Movimentos manuais do Caixa e Bancos.
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 14/12/2018   Daniela Campos          DKP                        Inclus∆o no nome do usu†rio
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: raw-param e tt-raw-digita
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/* include de controle de vers∆o */
{include/i-prgvrs.i apb002-rp 12.00.00.001}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD tp-modulo        AS INTEGER
    FIELD tp-movto         AS INTEGER
    FIELD dt-movto-ini     AS DATE
    FIELD dt-movto-fim     AS DATE
    .

define temp-table tt-digita no-undo
    field it-codigo            as CHAR   format "x(16)"
    field descricao            as character format "x(36)".
def temp-table tt-raw-digita field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.


create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

assign
    c-programa 	= "SGTB0170RP"
    c-versao	= "12.00"
    c-revisao	= ".00.001"
    c-empresa	= "Yamana"
    c-sistema	= 'Financeiro'
    c-titulo-relat = "Relat¢rio APB/CMG".

    /*
    da-iniper-x = tt-param.dt-carteira
    da-fimper-x = tt-param.dt-carteira */.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}
/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i &STREAM="str-rp"}

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

DEFINE TEMP-TABLE tt_tit
    FIELD cod_estab                  like esp_hist_aprov_tit.cod_estab         
    FIELD cod_refer                  like esp_hist_aprov_tit.cod_refer         
    FIELD num_seq_refer              like esp_hist_aprov_tit.num_seq_refer     
    FIELD dt_aprovacao1              like esp_hist_aprov_tit.dt_aprovacao1     
    FIELD cod_usuario_aprov1         like esp_hist_aprov_tit.cod_usuario_aprov1
    FIELD dt_aprovacao2              like esp_hist_aprov_tit.dt_aprovacao2     
    FIELD cod_usuario_aprov2         like esp_hist_aprov_tit.cod_usuario_aprov2
    FIELD cod_usuario_reprov         like esp_hist_aprov_tit.cod_usuario_reprov
    FIELD cod_usuario                like esp_hist_aprov_tit.cod_usuario       
    FIELD dt_digitacao               like esp_hist_aprov_tit.dt_digitacao      
    FIELD cdn_fornecedor             like esp_hist_aprov_tit.cdn_fornecedor    
    FIELD cod_espec_docto            like esp_hist_aprov_tit.cod_espec_docto   
    FIELD cod_ser_docto              like esp_hist_aprov_tit.cod_ser_docto     
    FIELD cod_tit_ap                 like esp_hist_aprov_tit.cod_tit_ap        
    FIELD cod_parcela                like esp_hist_aprov_tit.cod_parcela       
    FIELD dat_emis_docto             like esp_hist_aprov_tit.dat_emis_docto    
    FIELD dat_vencto_tit_ap          like esp_hist_aprov_tit.dat_vencto_tit_ap 
    FIELD val_tit_ap                 like esp_hist_aprov_tit.val_tit_ap        
    FIELD des_text_histor            like esp_hist_aprov_tit.des_text_histor
    FIELD dt_reprovacao              like esp_hist_aprov_tit.dt_reprovacao     
    FIELD mot-reprov                 like esp_hist_aprov_tit.mot-reprov
    FIELD tp-movto                   AS CHAR.

DEFINE TEMP-TABLE tt_cc
    FIELD cod_cta_corren             like esp_hist_aprov_movto_cc.cod_cta_corren            
    FIELD dat_movto_cta_corren       like esp_hist_aprov_movto_cc.dat_movto_cta_corren      
    FIELD num_seq_movto_cta_corren   like esp_hist_aprov_movto_cc.num_seq_movto_cta_corren  
    FIELD dt_aprovacao1              like esp_hist_aprov_movto_cc.dt_aprovacao1             
    FIELD cod_usuario_aprov1         like esp_hist_aprov_movto_cc.cod_usuario_aprov1        
    FIELD dt_aprovacao2              like esp_hist_aprov_movto_cc.dt_aprovacao2             
    FIELD cod_usuario_aprov2         like esp_hist_aprov_movto_cc.cod_usuario_aprov2        
    FIELD cod_usuario_reprov         like esp_hist_aprov_movto_cc.cod_usuario_reprov        
    FIELD dt_reprovacao              like esp_hist_aprov_movto_cc.dt_reprovacao             
    FIELD mot-reprov                 like esp_hist_aprov_movto_cc.mot-reprov                
    FIELD cod_usuario                like esp_hist_aprov_movto_cc.cod_usuario               
    FIELD dt_digitacao               like esp_hist_aprov_movto_cc.dt_digitacao              
    FIELD dat_transacao              like esp_hist_aprov_movto_cc.dat_transacao             
    FIELD val_movto_cta_corren       like esp_hist_aprov_movto_cc.val_movto_cta_corren      
    FIELD des_text_histor            like esp_hist_aprov_movto_cc.des_text_histor           
    FIELD ind_fluxo_movto_cta_corren like esp_hist_aprov_movto_cc.ind_fluxo_movto_cta_corren
    FIELD tp-movto                   AS CHAR.

DEFINE VARIABLE ChExcelApplication  AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ChWorkBook          AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ChWorkSheet         AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE i-linha             AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-acomp             AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-texto-limpo       AS CHARACTER   NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Criando Planilha...").

IF tt-param.tp-modulo = 1 THEN
DO:
    IF tt-param.tp-movto = 1 OR tt-param.tp-movto = 4 THEN
    FOR EACH esp_pend_lote_ap NO-LOCK:
    
        /*FIND item_lote_impl_ap OF esp_pend_lote_ap NO-LOCK NO-ERROR.*/

        FIND FIRST item_lote_impl_ap NO-LOCK 
             WHERE item_lote_impl_ap.cod_estab     = esp_pend_lote_ap.cod_estab   
               and item_lote_impl_ap.cod_refer     = esp_pend_lote_ap.cod_refer
               and item_lote_impl_ap.num_seq_refer = esp_pend_lote_ap.num_seq_refer NO-ERROR.
        IF NOT AVAIL item_lote_impl_ap THEN NEXT.
    
        IF item_lote_impl_ap.dat_emis_docto < tt-param.dt-movto-ini OR
           item_lote_impl_ap.dat_emis_docto > tt-param.dt-movto-fim THEN NEXT.
    
        RUN pi-acompanhar IN h-acomp (INPUT "Lendo T°tulos Pendentes: " + STRING(item_lote_impl_ap.dat_emis_docto, "99/99/9999")).

        CREATE tt_tit.
        BUFFER-COPY esp_pend_lote_ap TO tt_tit NO-ERROR.
        
        ASSIGN 
               c-texto-limpo = item_lote_impl_ap.des_text_histor
               c-texto-limpo = REPLACE(c-texto-limpo, CHR(10), " ")
               c-texto-limpo = REPLACE(c-texto-limpo, CHR(13), " ")
               c-texto-limpo = REPLACE(c-texto-limpo, CHR(9),  " ")
               c-texto-limpo = TRIM(c-texto-limpo).
        ASSIGN
              tt_tit.cdn_fornecedor    = item_lote_impl_ap.cdn_fornecedor   
              tt_tit.cod_espec_docto   = item_lote_impl_ap.cod_espec_docto  
              tt_tit.cod_ser_docto     = item_lote_impl_ap.cod_ser_docto    
              tt_tit.cod_tit_ap        = item_lote_impl_ap.cod_tit_ap       
              tt_tit.cod_parcela       = item_lote_impl_ap.cod_parcela      
              tt_tit.dat_emis_docto    = item_lote_impl_ap.dat_emis_docto   
              tt_tit.dat_vencto_tit_ap = item_lote_impl_ap.dat_vencto_tit_ap
              tt_tit.val_tit_ap        = item_lote_impl_ap.val_tit_ap
              tt_tit.des_text_histor   = c-texto-limpo
              tt_tit.tp-movto          = "Pendente".

    END.
    
    CASE tt-param.tp-movto:
        WHEN 2 THEN
        FOR EACH esp_hist_aprov_tit NO-LOCK
           WHERE esp_hist_aprov_tit.dat_emis_docto >= tt-param.dt-movto-ini
             AND esp_hist_aprov_tit.dat_emis_docto <= tt-param.dt-movto-fim
             AND esp_hist_aprov_tit.dt_aprovacao1 <> ?
             AND esp_hist_aprov_tit.dt_aprovacao2 <> ?:
    
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo T°tulos Aprovados: " + STRING(esp_hist_aprov_tit.dat_emis_docto, "99/99/9999")).

            CREATE tt_tit.
            BUFFER-COPY esp_hist_aprov_tit TO tt_tit NO-ERROR.
            ASSIGN tt_tit.tp-movto = "Aprovado".
        END.
        WHEN 3 THEN
        FOR EACH esp_hist_aprov_tit NO-LOCK
           WHERE esp_hist_aprov_tit.dat_emis_docto >= tt-param.dt-movto-ini
             AND esp_hist_aprov_tit.dat_emis_docto <= tt-param.dt-movto-fim
             AND esp_hist_aprov_tit.dt_reprovacao <> ?:
        
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo T°tulos Reprovados: " + STRING(esp_hist_aprov_tit.dat_emis_docto, "99/99/9999")).

            CREATE tt_tit.
            BUFFER-COPY esp_hist_aprov_tit TO tt_tit NO-ERROR.
            ASSIGN tt_tit.tp-movto = "Reprovado".
        END.
        WHEN 4 THEN
        FOR EACH esp_hist_aprov_tit NO-LOCK
           WHERE esp_hist_aprov_tit.dat_emis_docto >= tt-param.dt-movto-ini
             AND esp_hist_aprov_tit.dat_emis_docto <= tt-param.dt-movto-fim:
    
            CREATE tt_tit.
            BUFFER-COPY esp_hist_aprov_tit TO tt_tit NO-ERROR.

            RUN pi-acompanhar IN h-acomp (INPUT "Lendo todos os T°tulos: " + STRING(esp_hist_aprov_tit.dat_emis_docto, "99/99/9999")).
    
            IF esp_hist_aprov_tit.dt_aprovacao1 <> ? AND
               esp_hist_aprov_tit.dt_aprovacao2 <> ? THEN
                ASSIGN tt_tit.tp-movto = "Aprovado".
            ELSE IF esp_hist_aprov_tit.dt_reprovacao <> ? THEN
                ASSIGN tt_tit.tp-movto = "Reprovado".
        END.
    
    END CASE.
END.

IF tt-param.tp-modulo = 2 THEN
DO:
    IF tt-param.tp-movto = 1 OR tt-param.tp-movto = 4 THEN
    FOR EACH esp_pend_movto_cta_corren NO-LOCK:
    
        FIND movto_cta_corren OF esp_pend_movto_cta_corren NO-LOCK NO-ERROR.
        IF NOT AVAIL movto_cta_corren THEN NEXT.
    
        IF movto_cta_corren.dat_transacao < tt-param.dt-movto-ini OR
           movto_cta_corren.dat_transacao > tt-param.dt-movto-fim THEN NEXT.
    
        RUN pi-acompanhar IN h-acomp (INPUT "Lendo Movtos Pendentes: " + STRING(movto_cta_corren.dat_transacao, "99/99/9999")).

        CREATE tt_cc.
        BUFFER-COPY esp_pend_movto_cta_corren TO tt_cc NO-ERROR.
    
        ASSIGN
            tt_cc.val_movto_cta_corren        = movto_cta_corren.val_movto_cta_corren          
            tt_cc.dat_transacao               = movto_cta_corren.dat_transacao                 
            tt_cc.des_text_histor             = movto_cta_corren.des_histor_movto_cta_corren   
            tt_cc.cod_usuario                 = movto_cta_corren.cod_usuar_ult_atualiz
            tt_cc.ind_fluxo_movto_cta_corren  = movto_cta_corren.ind_fluxo_movto_cta_corren
            tt_cc.tp-movto                    = "Pendente".
    END.
    
    CASE tt-param.tp-movto:
        WHEN 2 THEN
        FOR EACH esp_hist_aprov_movto_cc NO-LOCK
           WHERE esp_hist_aprov_movto_cc.dat_transacao >= tt-param.dt-movto-ini
             AND esp_hist_aprov_movto_cc.dat_transacao <= tt-param.dt-movto-fim
             AND esp_hist_aprov_movto_cc.dt_aprovacao1 <> ?
             AND esp_hist_aprov_movto_cc.dt_aprovacao2 <> ?:
    
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo Movtos Aprovados: " + STRING(movto_cta_corren.dat_transacao, "99/99/9999")).

            CREATE tt_cc.
            BUFFER-COPY esp_hist_aprov_movto_cc TO tt_cc NO-ERROR.
            ASSIGN tt_cc.tp-movto = "Aprovado".
        END.
        WHEN 3 THEN
        FOR EACH esp_hist_aprov_movto_cc NO-LOCK
           WHERE esp_hist_aprov_movto_cc.dat_transacao >= tt-param.dt-movto-ini
             AND esp_hist_aprov_movto_cc.dat_transacao <= tt-param.dt-movto-fim
             AND esp_hist_aprov_movto_cc.dt_reprovacao <> ?:
        
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo Movtos Reprovados: " + STRING(esp_hist_aprov_movto_cc.dat_transacao, "99/99/9999")).

            CREATE tt_cc.
            BUFFER-COPY esp_hist_aprov_movto_cc TO tt_cc NO-ERROR.
            ASSIGN tt_cc.tp-movto = "Reprovado".
        END.
        WHEN 4 THEN
        FOR EACH esp_hist_aprov_movto_cc NO-LOCK
           WHERE esp_hist_aprov_movto_cc.dat_transacao >= tt-param.dt-movto-ini
             AND esp_hist_aprov_movto_cc.dat_transacao <= tt-param.dt-movto-fim:
    
            CREATE tt_cc.
            BUFFER-COPY esp_hist_aprov_movto_cc TO tt_cc NO-ERROR.
    
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo todos os Movtos: " + STRING(esp_hist_aprov_movto_cc.dat_transacao, "99/99/9999")).

            IF esp_hist_aprov_movto_cc.dt_aprovacao1 <> ? AND
               esp_hist_aprov_movto_cc.dt_aprovacao2 <> ? THEN
                ASSIGN tt_cc.tp-movto = "Aprovado".
            ELSE IF esp_hist_aprov_movto_cc.dt_reprovacao <> ? THEN
                ASSIGN tt_cc.tp-movto = "Reprovado".
        END.
    END CASE.

END.

IF TEMP-TABLE tt_tit:HAS-RECORDS THEN
DO:
    c-titulo-relat = "Relat¢rio de T°tulos manuais Contas a Pagar".
    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:DefaultSaveFormat = 51. /* usa 1.048.576 linhas*/
    chWorkBook  = chExcelapplication:Workbooks:ADD().
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).
    
    chWorkSheet:range("A1:S1"):MergeCells = TRUE.
    chWorksheet:Range("A1"):VALUE = "Yamana - " + c-titulo-relat.
    chExcelApplication:range("A1"):FONT:SIZE = 14.
    chExcelApplication:range("A1:V3"):FONT:Bold = TRUE.
    chExcelApplication:range("A1:A1"):HorizontalAlignment = 2.
    chExcelApplication:range("A1:A1"):VerticalAlignment   = 2.

    chWorkSheet:range("T1:V1"):MergeCells = TRUE.
    chWorksheet:Range("V1"):VALUE = "Data: " + STRING(TODAY, "99/99/9999") + " -  Hora: " + STRING(TIME, "HH:MM:SS").

    chExcelApplication:range("T1:U1"):HorizontalAlignment = 4.
    chExcelApplication:range("T1:U1"):VerticalAlignment   = 2.

    chExcelApplication:COLUMNS("G"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("I"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("J"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("M"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("O"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("Q"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("S"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("K"):NumberFormat   = "####.###.##0,00".

    ASSIGN
        chWorksheet:Range("A3"):VALUE  = "Estab"            
        chWorksheet:Range("B3"):VALUE  = "Refer"            
        chWorksheet:Range("C3"):VALUE  = "Seq Refer"        
        chWorksheet:Range("D3"):VALUE  = "Fornecedor"       
        chWorksheet:Range("E3"):VALUE  = "EspÇcie Docto"    
        chWorksheet:Range("F3"):VALUE  = "SÇrie Docto"      
        chWorksheet:Range("G3"):VALUE  = "Nro Docto"        
        chWorksheet:Range("H3"):VALUE  = "Parcela"          
        chWorksheet:Range("I3"):VALUE  = "Dt Emiss∆o"       
        chWorksheet:Range("J3"):VALUE  = "Dt Vencto"        
        chWorksheet:Range("K3"):VALUE  = "Valor T°tulo"     
        chWorksheet:Range("L3"):VALUE  = "Hist¢rico"        
        chWorksheet:Range("M3"):VALUE  = "Data Aprov1"      
        chWorksheet:Range("N3"):VALUE  = "Usu†rio Aprov1"   
        chWorksheet:Range("O3"):VALUE  = "Data Aprov2"      
        chWorksheet:Range("P3"):VALUE  = "Usu†rio Aprov2"   
        chWorksheet:Range("Q3"):VALUE  = "Data Digitaá∆o"   
        chWorksheet:Range("R3"):VALUE  = "Usu†rio Digitaá∆o"
        chWorksheet:Range("S3"):VALUE  = "Data Reprov"      
        chWorksheet:Range("T3"):VALUE  = "Usu†rio Reprov"   
        chWorksheet:Range("U3"):VALUE  = "Motivo Reprov"    
        chWorksheet:Range("V3"):VALUE  = "Tipo Movto".

    ASSIGN i-linha = 3.

    FOR EACH tt_tit BY tt_tit.dat_emis_docto:

        RUN pi-acompanhar IN h-acomp (INPUT "Gerando Planilha: " + STRING(tt_tit.dat_emis_docto, "99/99/9999")).

        /* DPC - Busca nome dos usu†rios */
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_tit.cod_usuario_aprov1 NO-ERROR.
        ASSIGN tt_tit.cod_usuario_aprov1 = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_tit.cod_usuario_aprov2 NO-ERROR.
        ASSIGN tt_tit.cod_usuario_aprov2 = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.

        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_tit.cod_usuario NO-ERROR.
        ASSIGN tt_tit.cod_usuario = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.

        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_tit.cod_usuario_reprov NO-ERROR.
        ASSIGN tt_tit.cod_usuario_reprov = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
        /* Fim DPC */


        ASSIGN
            i-linha = i-linha + 1
            chWorksheet:Range("A"  + STRING(i-linha)):VALUE = tt_tit.cod_estab         
            chWorksheet:Range("B"  + STRING(i-linha)):VALUE = tt_tit.cod_refer         
            chWorksheet:Range("C"  + STRING(i-linha)):VALUE = tt_tit.num_seq_refer     
            chWorksheet:Range("D"  + STRING(i-linha)):VALUE = tt_tit.cdn_fornecedor    
            chWorksheet:Range("E"  + STRING(i-linha)):VALUE = tt_tit.cod_espec_docto   
            chWorksheet:Range("F"  + STRING(i-linha)):VALUE = tt_tit.cod_ser_docto     
            chWorksheet:Range("G"  + STRING(i-linha)):VALUE = tt_tit.cod_tit_ap        
            chWorksheet:Range("H"  + STRING(i-linha)):VALUE = tt_tit.cod_parcela       
            chWorksheet:Range("I"  + STRING(i-linha)):VALUE = STRING(tt_tit.dat_emis_docto, "99/99/9999")
            chWorksheet:Range("J"  + STRING(i-linha)):VALUE = STRING(tt_tit.dat_vencto_tit_ap, "99/99/9999")
            chWorksheet:Range("K"  + STRING(i-linha)):VALUE = tt_tit.val_tit_ap        
            chWorksheet:Range("L"  + STRING(i-linha)):VALUE = CAPS(tt_tit.des_text_histor)   
            chWorksheet:Range("M"  + STRING(i-linha)):VALUE = tt_tit.dt_aprovacao1     
            chWorksheet:Range("N"  + STRING(i-linha)):VALUE = tt_tit.cod_usuario_aprov1
            chWorksheet:Range("O"  + STRING(i-linha)):VALUE = tt_tit.dt_aprovacao2
            chWorksheet:Range("P"  + STRING(i-linha)):VALUE = tt_tit.cod_usuario_aprov2
            chWorksheet:Range("Q"  + STRING(i-linha)):VALUE = tt_tit.dt_digitacao
            chWorksheet:Range("R"  + STRING(i-linha)):VALUE = tt_tit.cod_usuario       
            chWorksheet:Range("S"  + STRING(i-linha)):VALUE = tt_tit.dt_reprovacao
            chWorksheet:Range("T"  + STRING(i-linha)):VALUE = tt_tit.cod_usuario_reprov
            chWorksheet:Range("U"  + STRING(i-linha)):VALUE = CAPS(tt_tit.mot-reprov)        
            chWorksheet:Range("V"  + STRING(i-linha)):VALUE = CAPS(tt_tit.tp-movto).

    END.

    chExcelApplication:range("A3:V3"):select.    /*seleciona as celuas*/
    chExcelApplication:selection:AutoFilter(,,,). /*aplica o filtro*/
    
    chWorkSheet:Range("A4"):Activate().
    chWorkSheet:APPLICATION:ActiveWindow:FreezePanes = TRUE. /*aplicar congelar painel*/
    chExcelApplication:COLUMNS("A:V"):autofit. /*ajustar colunas*/
    chExcelApplication:VISIBLE = TRUE.
    /*
    chExcelApplication:QUIT. /*fecha o excel*/
    */
    IF VALID-HANDLE(ChExcelApplication) THEN RELEASE OBJECT chExcelApplication.      
    IF VALID-HANDLE(chWorkbook)         THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorksheet)        THEN RELEASE OBJECT chWorksheet.
END.

IF TEMP-TABLE tt_cc:HAS-RECORDS THEN
DO:
    c-titulo-relat = "Relat¢rio de Movimentos manuais CCaixa e Bancos".
    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:DefaultSaveFormat = 51. /* usa 1.048.576 linhas*/
    chWorkBook  = chExcelapplication:Workbooks:ADD().
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).
    
    chWorkSheet:range("A1:N1"):MergeCells = TRUE.
    chWorksheet:Range("A1"):VALUE = "Yamana - " + c-titulo-relat.
    chExcelApplication:range("A1"):FONT:SIZE = 14.
    chExcelApplication:range("A1:V3"):FONT:Bold = TRUE.
    chExcelApplication:range("A1:A1"):HorizontalAlignment = 2.
    chExcelApplication:range("A1:A1"):VerticalAlignment   = 2.

    chWorkSheet:range("O1:V1"):MergeCells = TRUE.
    chWorksheet:Range("Q1"):VALUE = "Data: " + STRING(TODAY, "99/99/9999") + " -  Hora: " + STRING(TIME, "HH:MM:SS").

    chExcelApplication:range("Q1:U1"):HorizontalAlignment = 4.
    chExcelApplication:range("Q1:U1"):VerticalAlignment   = 2.

    chExcelApplication:COLUMNS("A"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("B"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("D"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("E"):NumberFormat   = "####.###.##0,00".
    chExcelApplication:COLUMNS("H"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("J"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("M"):NumberFormat   = "@".
    chExcelApplication:COLUMNS("N"):NumberFormat   = "@".

    ASSIGN
        chWorksheet:Range("A3"):VALUE  = "Conta Corrente"   
        chWorksheet:Range("B3"):VALUE  = "Data Movto"       
        chWorksheet:Range("C3"):VALUE  = "Seq Movto"        
        chWorksheet:Range("D3"):VALUE  = "Dt Transaá∆o"     
        chWorksheet:Range("E3"):VALUE  = "Valor Movto"      
        chWorksheet:Range("F3"):VALUE  = "Fluxo Movto"      
        chWorksheet:Range("G3"):VALUE  = "Hist¢rico"        
        chWorksheet:Range("H3"):VALUE  = "Data Aprov1"      
        chWorksheet:Range("I3"):VALUE  = "Usu†rio Aprov1"   
        chWorksheet:Range("J3"):VALUE  = "Data Aprov2"      
        chWorksheet:Range("K3"):VALUE  = "Usu†rio Aprov2"   
        chWorksheet:Range("L3"):VALUE  = "Usu†rio Digitaá∆o"
        chWorksheet:Range("M3"):VALUE  = "Data Digitaá∆o"   
        chWorksheet:Range("N3"):VALUE  = "Data Reprov"      
        chWorksheet:Range("O3"):VALUE  = "Usu†rio Reprov"   
        chWorksheet:Range("P3"):VALUE  = "Motivo Reprov"    
        chWorksheet:Range("Q3"):VALUE  = "Tipo Movto".

    ASSIGN i-linha = 3.

    FOR EACH tt_cc BY tt_cc.dat_transacao:

        RUN pi-acompanhar IN h-acomp (INPUT "Gerando Planilha: " + STRING(tt_cc.dat_transacao, "99/99/9999")).

        /* DPC - Busca nome dos usu†rios */
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_cc.cod_usuario_aprov1 NO-ERROR.
        ASSIGN tt_cc.cod_usuario_aprov1 = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
    
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_cc.cod_usuario_aprov2 NO-ERROR.
        ASSIGN tt_cc.cod_usuario_aprov2 = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
    
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_cc.cod_usuario NO-ERROR.
        ASSIGN tt_cc.cod_usuario = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
    
        FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = tt_cc.cod_usuario_reprov NO-ERROR.
        ASSIGN tt_cc.cod_usuario_reprov = CAPS(usuar_mestre.nom_usuario) WHEN AVAIL usuar_mestre.
        /* Fim DPC */

        ASSIGN
            i-linha = i-linha + 1
            chWorksheet:Range("A"  + STRING(i-linha)):VALUE = tt_cc.cod_cta_corren            
            chWorksheet:Range("B"  + STRING(i-linha)):VALUE = STRING(tt_cc.dat_movto_cta_corren,"99/99/9999")
            chWorksheet:Range("C"  + STRING(i-linha)):VALUE = tt_cc.num_seq_movto_cta_corren  
            chWorksheet:Range("D"  + STRING(i-linha)):VALUE = STRING(tt_cc.dat_transacao,"99/99/9999")             
            chWorksheet:Range("E"  + STRING(i-linha)):VALUE = tt_cc.val_movto_cta_corren      
            chWorksheet:Range("F"  + STRING(i-linha)):VALUE = tt_cc.ind_fluxo_movto_cta_corren
            chWorksheet:Range("G"  + STRING(i-linha)):VALUE = CAPS(tt_cc.des_text_histor)           
            chWorksheet:Range("H"  + STRING(i-linha)):VALUE = tt_cc.dt_aprovacao1
            chWorksheet:Range("I"  + STRING(i-linha)):VALUE = tt_cc.cod_usuario_aprov1        
            chWorksheet:Range("J"  + STRING(i-linha)):VALUE = tt_cc.dt_aprovacao2
            chWorksheet:Range("K"  + STRING(i-linha)):VALUE = tt_cc.cod_usuario_aprov2        
            chWorksheet:Range("L"  + STRING(i-linha)):VALUE = tt_cc.cod_usuario               
            chWorksheet:Range("M"  + STRING(i-linha)):VALUE = tt_cc.dt_digitacao              
            chWorksheet:Range("N"  + STRING(i-linha)):VALUE = tt_cc.dt_reprovacao             
            chWorksheet:Range("O"  + STRING(i-linha)):VALUE = tt_cc.cod_usuario_reprov        
            chWorksheet:Range("P"  + STRING(i-linha)):VALUE = CAPS(tt_cc.mot-reprov)
            chWorksheet:Range("Q"  + STRING(i-linha)):VALUE = CAPS(tt_cc.tp-movto).
    END.

    chExcelApplication:range("A3:Q3"):select.    /*seleciona as celuas*/
    chExcelApplication:selection:AutoFilter(,,,). /*aplica o filtro*/
    
    chWorkSheet:Range("A4"):Activate().
    chWorkSheet:APPLICATION:ActiveWindow:FreezePanes = TRUE. /*aplicar congelar painel*/
    chExcelApplication:COLUMNS("A:Q"):autofit. /*ajustar colunas*/
    chExcelApplication:VISIBLE = TRUE.
    /*
    chExcelApplication:QUIT. /*fecha o excel*/
    */
    IF VALID-HANDLE(ChExcelApplication) THEN RELEASE OBJECT chExcelApplication.      
    IF VALID-HANDLE(chWorkbook)         THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorksheet)        THEN RELEASE OBJECT chWorksheet.
END.


RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


