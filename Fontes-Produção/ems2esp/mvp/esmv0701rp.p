&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0701RP 2.06.00.000}  /*** 010004 ***/
{include/i_fnctrad.i}
/********************************************************************************
**
**       PROGRAMA: esmv0701rp.p
**
**       DATA....: Junho de 2008
**
**       AUTOR...: DATASUL S.A.
**
**       OBJETIVO: 
**       Versao..: 2.06.00.000
**
********************************************************************************/

/** Definicao das temp-tables para recebimento dos parametros **/
{mvp/esmv0701.i}  

    def temp-table tt-raw-digita
    field raw-digita as raw.

/* Recebimento de parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Include padroes para variaveis de relatorio */
{include/i-rpvar.i}

def var c-arquivo              as char init "Esmvv0701.dat" no-undo.


define variable chExcel    as com-handle  no-undo.
define variable chArquivo  as com-handle  no-undo.
define variable chPlanilha as com-handle  no-undo.
define variable hacomp     as handle      no-undo.

DEFINE VARIABLE     cData                             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE     cPrevisto                         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE     cRealizado                        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE     cEqpto                            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE     cStatus                           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE     c-coluna                          AS INTEGER    NO-UNDO.
DEFINE VARIABLE     difCalend                         AS INTEGER    NO-UNDO.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 10
         WIDTH              = 32.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

if connected("dthrpyc") then do:
    def var v_han_fpapi003 as handle no-undo.
    run prghur/fpp/fpapi003.p persistent set v_han_fpapi003 (input tt-param.usuario,
                                                             input tt-param.v_num_tip_aces_usuar).
END.

/** Parametriza padräes de cabe‡alho e rodap‚ a serem exibidos **/
run piInicial in this-procedure.
       
/** Imprime cabe‡alho e abre o output para arquivo **/
{include/i-rpcab.i}
{include/i-rpout.i}

/** Procedure para inicializar c lculos e impressÆo **/
run piPrincipal in this-procedure.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

return "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-carregaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaTT Procedure 
PROCEDURE carregaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dtInicio AS DATE      NO-UNDO.
DEFINE VARIABLE dtFim    AS DATE      NO-UNDO.
DEFINE VARIABLE aux      AS INT       NO-UNDO.
DEFINE VARIABLE cont     AS INTEGER   NO-UNDO.
    
    /*Carrega Temp-table das datas do cabe‡alho*/
    ASSIGN dtInicio = tt-param.dt-programacao-ini
           dtFim    = tt-param.dt-programacao-fim.
    
    ASSIGN cont = INT(dtFim) - INT(dtInicio).
    ASSIGN cont = (cont) / 7.
    
    IF cont = 0 THEN DO:
        CREATE tt-ano.
        ASSIGN tt-ano.inicial = tt-param.dt-programacao-ini 
               tt-ano.final   = tt-param.dt-programacao-fim.
    END.
    REPEAT aux = 1 TO cont:
        CREATE tt-ano.
        IF aux = cont THEN DO:
            ASSIGN tt-ano.inicial = dtInicio
                   tt-ano.final = dtFim
                   difCalend    = dtFim - (dtInicio + 7).
            IF difCalend = 0 THEN
                ASSIGN difCalend = 1.
            NEXT.
        END.
        ASSIGN tt-ano.inicial = dtInicio
               tt-ano.final = dtInicio + 6.
        ASSIGN dtInicio = dtInicio + 7.
    END.
    
    /*carrega temp-table dos equipamentos*/
    FOR EACH mmv-eqpto-calend-plano
        where mmv-eqpto-calend-plano.dat-movto    >=   tt-param.dt-programacao-ini 
        and   mmv-eqpto-calend-plano.dat-movto    <=   tt-param.dt-programacao-fim 
        and   mmv-eqpto-calend-plano.ep-codigo    >=   tt-param.i-empresa-ini      
        and   mmv-eqpto-calend-plano.ep-codigo    <=   tt-param.i-empresa-fim      
        and   mmv-eqpto-calend-plano.cod-eqpto    >=   tt-param.c-equipamento-ini  
        and   mmv-eqpto-calend-plano.cod-eqpto    <=   tt-param.c-equipamento-fim  
        and   mmv-eqpto-calend-plano.cod-ofici    >=   tt-param.c-oficina-ini      
        and   mmv-eqpto-calend-plano.cod-ofici    <=   tt-param.c-oficina-fim       
              break by mmv-eqpto-calend-plano.cod-eqpto:

                    CREATE tt-eqpto. 
                    FOR FIRST mab-eqpto
                        WHERE mab-eqpto.cod-eqpto = mmv-eqpto-calend-plano.cod-eqpto:
                        ASSIGN tt-eqpto.model = mab-eqpto.cod-model.
                    END.
                    ASSIGN tt-eqpto.cod-eqpto = mmv-eqpto-calend-plano.cod-eqpto
                           tt-eqpto.data      = mmv-eqpto-calend-plano.dat-movto.
                    
     

        FOR FIRST mmv-plano-prevent
            WHERE mmv-plano-prevent.cod-plano = mmv-eqpto-calend-plano.cod-plano:
            IF mmv-plano-prevent.val-km-padr <> 0 AND mmv-plano-prevent.val-km-aviso = 0 THEN DO:
                ASSIGN tt-eqpto.previsto = string(mmv-plano-prevent.val-km-padr).
            END.
            IF mmv-plano-prevent.val-km-aviso <> 0 AND mmv-plano-prevent.val-km-padr = 0 THEN DO:
                ASSIGN tt-eqpto.previsto = tt-eqpto.previsto + string(mmv-plano-prevent.val-km-aviso).
            END.
            IF mmv-plano-prevent.val-km-padr <> 0 AND mmv-plano-prevent.val-km-aviso <> 0 THEN DO:
                ASSIGN tt-eqpto.previsto = string(mmv-plano-prevent.val-km-padr) + "/" + string(mmv-plano-prevent.val-km-aviso).
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-ExportaExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ExportaExcel Procedure 
PROCEDURE pi-ExportaExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define variable l-linha  as integer no-undo.
    define variable l-coluna as integer no-undo.
   
    run utp/ut-acomp.p persistent set hacomp.
    {utp/ut-liter.i Exporta_Excel *}
    run pi-inicializar in hacomp (input return-value).
     
    run piCabecalho.
    RUN piColuna.

if valid-handle(hAcomp) then do:
   run pi-finalizar IN hAcomp.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-FimExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-FimExcel Procedure 
PROCEDURE pi-FimExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chPlanilha:Range("A1:IV65535"):SELECT.
    chExcel:SELECTION:EntireColumn:Autofit.

    chExcel:Range("A1"):select.
    chExcel:Cursor =-4143.
    chExcel:ScreenUpdating = true.
    chExcel:WindowState = -4137.

    /** Abre o excel com o arquivo **/
    chExcel:visible = true.

    /** Libera da mem¢ria e fecha a aplica‡Æo do excell **/
     if valid-handle(chPlanilha) then do:
         release object chPlanilha.
     end.

     if valid-handle(chArquivo) then do:
         release object chArquivo.
     end.

    /** Libera da mem¢ria e fecha a aplica‡Æo do excell **/
     if valid-handle(chExcel) then do:
         release object chExcel.
     end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-inicializa-parametros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inicializa-parametros Procedure 
PROCEDURE pi-inicializa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER alfabeto AS CHAR.
    DEFINE VARIABLE c-alfabeto-excel AS CHARACTER  NO-UNDO.

    DEF VAR i-alf         AS INTEGER NO-UNDO.
    DEF VAR i-alf2        AS INTEGER NO-UNDO.
    
     ASSIGN c-alfabeto-excel = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z":U.

    REPEAT i-alf = 1 TO 26:
        REPEAT i-alf2 = 1 TO 26:
            ASSIGN c-alfabeto-excel = c-alfabeto-excel + "," + ENTRY(i-alf,c-alfabeto-excel,",") + ENTRY(i-alf2,c-alfabeto-excel,",").
        END.
    END.
    ASSIGN alfabeto = c-alfabeto-excel.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-InicioExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-InicioExcel Procedure 
PROCEDURE pi-InicioExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   create "Excel.Application"  chExcel no-error.
    if  error-status:error then do:
        create "Excel.Application" chExcel.
    end.

    /* Inicia o excel com 1 planilha */
    chExcel:SheetsInNewWorkBook = 1.

    assign chArquivo  = chExcel:WorkBooks:ADD.

    /* Esconde as linhas de grade */
    chExcel:ActiveWindow:DisplayGridLines = TRUE.

    assign chPlanilha = chArquivo:Sheets:Item(1).
    chPlanilha:Activate().

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piCabecalho) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCabecalho Procedure 
PROCEDURE piCabecalho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i EQPTO * l}
    ASSIGN cEqpto  = RETURN-VALUE.
    {utp/ut-liter.i "STATUS" * l}
    ASSIGN cStatus  = RETURN-VALUE.

    chPlanilha:cells(3,1):value  = cEqpto.
    chPlanilha:cells(3,2):value  = cStatus.
    assign c-coluna = 2.

    FOR EACH tt-ano:
         assign c-coluna = c-coluna + 1.
        chPlanilha:cells(2,c-coluna):value  = STRING(tt-ano.inicial) + " " + "a" + " " + STRING (tt-ano.final).
        chPlanilha:cells(3,c-coluna):value  = c-coluna - 2.
        chPlanilha:Cells(3,c-coluna):HorizontalAlignment      = -4108.
        chPlanilha:Cells(3,c-coluna):VerticalAlignment        = -4108.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piColuna) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piColuna Procedure 
PROCEDURE piColuna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtInicio       AS DATE       NO-UNDO.
    DEFINE VARIABLE dtFim          AS DATE       NO-UNDO.
    DEFINE VARIABLE aux            AS INT        NO-UNDO.
    DEFINE VARIABLE cont           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE contColuna     AS INTEGER    NO-UNDO.
    define variable l-linha        as integer    no-undo.
    DEFINE VARIABLE l-linha-status AS INTEGER    NO-UNDO.
    DEFINE VARIABLE alfabeto       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i-linha        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE modelo         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-range        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-range2       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE teste          AS INTEGER    NO-UNDO.
    

    {utp/ut-liter.i Data * l}
    ASSIGN cData  = RETURN-VALUE.
    {utp/ut-liter.i Previsto * l}
    ASSIGN cPrevisto  = RETURN-VALUE.
    {utp/ut-liter.i Realizado * l}
    ASSIGN cRealizado  = RETURN-VALUE.


    assign l-linha = 0
           l-linha-status = 0.
    ASSIGN modelo = "".

    FOR EACH tt-eqpto 
        BREAK BY tt-eqpto.model: 
        IF modelo <> tt-eqpto.model AND modelo <> "" THEN DO:
            ASSIGN modelo = tt-eqpto.model
                   l-linha = l-linha + 4
                   l-linha-status = l-linha-status + 4.
            ASSIGN c-range = "A" + string(l-linha - 1) 
                   c-range2 = ENTRY(contColuna,alfabeto,",") + string(l-linha - 1).
            chPlanilha:range(c-range,c-range2):Interior:Colorindex = 1.
        END.
        ELSE DO:
            assign l-linha = l-linha + 4
                   l-linha-status = l-linha-status + 4
                   modelo = tt-eqpto.model.
        END.
        chPlanilha:cells(l-linha,1):value  = tt-eqpto.cod-eqpto.
        chPlanilha:Cells(l-linha,1):HorizontalAlignment      = -4108.
        chPlanilha:Cells(l-linha,1):VerticalAlignment        = -4108.
        chPlanilha:cells(l-linha-status,2):value  = cData.
        chPlanilha:cells(l-linha-status + 1,2):value  = cPrevisto.
        chPlanilha:cells(l-linha-status + 2,2):value  = cRealizado.
        chPlanilha:Range("A" + string(l-linha) + ":" + "A" + STRING(l-linha + 2)):MergeCells    = TRUE.
        
        ASSIGN dtInicio = tt-param.dt-programacao-ini
               dtFim    = tt-eqpto.data.
        
        ASSIGN cont = INT(dtFim) - INT(dtInicio).
        ASSIGN cont = (cont) / 7.
        ASSIGN contColuna = 3.
        
        REPEAT aux = 1 TO cont:
            IF dtInicio + 7 <= dtFim THEN DO:
                ASSIGN contColuna = contColuna + 1.
            END.
            ASSIGN dtInicio = dtInicio + 7.
        END.
        
        IF tt-eqpto.data <= tt-param.dt-programacao-fim AND difCalend > 0 
           AND tt-eqpto.data + (difCalend) >= tt-param.dt-programacao-fim THEN DO:
               ASSIGN contColuna = contColuna - 1.
        END.
        
        RUN pi-inicializa-parametros(OUTPUT alfabeto).
        chPlanilha:cells(l-linha,ENTRY(contColuna,alfabeto,",")):value      = tt-eqpto.data.
        chPlanilha:cells(l-linha + 1,ENTRY(contColuna,alfabeto,",")):value  = tt-eqpto.previsto.
        chPlanilha:Cells(l-linha,ENTRY(contColuna,alfabeto,",")):HorizontalAlignment      = -4108.
        chPlanilha:Cells(l-linha,ENTRY(contColuna,alfabeto,",")):VerticalAlignment        = -4108.
        chPlanilha:Cells(l-linha + 1,ENTRY(contColuna,alfabeto,",")):HorizontalAlignment  = -4108.
        chPlanilha:Cells(l-linha + 1,ENTRY(contColuna,alfabeto,",")):VerticalAlignment    = -4108.
        
        
        ASSIGN i-linha  = l-linha + 2.
               c-coluna = 2.
                
        FOR EACH tt-ano:
            assign c-coluna = c-coluna + 1.
        END.
        
    END.

    
        
    chPlanilha:Range("A1" + ":" + ENTRY(c-coluna,alfabeto,",") + "1"):MergeCells    = TRUE.
    {utp/ut-liter.i "CRONOGRAMA_DE_MANUTAN€åES_PREVENTIVAS"}
    chPlanilha:cells(1, "A"):value      = RETURN-VALUE.
    chPlanilha:cells(1, "A"):Font:ColorIndex = 54.
    chPlanilha:Cells(1, "A"):HorizontalAlignment  = -4108.
    chPlanilha:Cells(1, "A"):VerticalAlignment    = -4108.
    

    
    ASSIGN c-range  = "A3"
           c-range2 = "A" + string(l-linha + 2).
    chPlanilha:range(c-range,c-range2):Interior:Colorindex = 15.

    ASSIGN c-range = "A1"
           c-range2 = ENTRY(c-coluna,alfabeto,",") + string(l-linha + 2).
    chExcel:range(c-range,c-range2):Borders({&xlEdgeLeftTopBottomRight}):LineStyle = 1.
    
/**LEGENDA**/
    chPlanilha:Range("A" + string(i-linha + 1) + ":" + "B" + STRING(i-linha + 1)):MergeCells    = TRUE.
    {utp/ut-liter.i "Plano_Revisado"}
    chPlanilha:cells(i-linha + 1 , "A"):value      = RETURN-VALUE.
    
    chPlanilha:Range("A" + string(i-linha + 2) + ":" + "B" + STRING(i-linha + 2)):MergeCells    = TRUE.
    {utp/ut-liter.i "Real_executado"}
    chPlanilha:cells(i-linha + 2 , "A"):value      = RETURN-VALUE.
    chPlanilha:cells(i-linha + 2 , "A"):Interior:Colorindex = 4.
    
    chPlanilha:Range("A" + string(i-linha + 3) + ":" + "B" + STRING(i-linha + 3)):MergeCells    = TRUE.
    chPlanilha:cells(i-linha + 3 , "A"):value      = "".
    chPlanilha:cells(i-linha + 3 , "A"):Interior:Colorindex = 55.

    chPlanilha:Range("A" + string(i-linha + 4) + ":" + "B" + STRING(i-linha + 4)):MergeCells    = TRUE.
    {utp/ut-liter.i "%_de_conclusÆo"}
    chPlanilha:cells(i-linha + 4 , "A"):value      = RETURN-VALUE.
    
    {utp/ut-liter.i "Legenda"}
    chPlanilha:cells(i-linha + 7 , "A"):value      = RETURN-VALUE.
    {utp/ut-liter.i "P"}
    chPlanilha:cells(i-linha + 8 , "A"):value      = RETURN-VALUE.
    {utp/ut-liter.i "R"}
    chPlanilha:cells(i-linha + 9 , "A"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 8) + ":" + "C" + STRING(i-linha + 8)):MergeCells    = TRUE.
    {utp/ut-liter.i "Programado"}
    chPlanilha:cells(i-linha + 8 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 9) + ":" + "C" + STRING(i-linha + 9)):MergeCells    = TRUE.
    {utp/ut-liter.i "Realizado"}
    chPlanilha:cells(i-linha + 9 , "B"):value      = RETURN-VALUE.

    chPlanilha:cells(i-linha + 11 , "A"):Interior:Colorindex = 33.
    chPlanilha:cells(i-linha + 12 , "A"):Interior:Colorindex = 3.
    chPlanilha:cells(i-linha + 13 , "A"):Interior:Colorindex = 15.
    chPlanilha:cells(i-linha + 14 , "A"):Interior:Colorindex = 6.
    chPlanilha:cells(i-linha + 15 , "A"):Interior:Colorindex = 39.
    chPlanilha:cells(i-linha + 16 , "A"):Interior:Colorindex = 45.
    
    chPlanilha:Range("B" + string(i-linha + 11) + ":" + "E" + STRING(i-linha + 11)):MergeCells    = TRUE.
    {utp/ut-liter.i "Preventiva_Lubrifica‡Æo,_Mecanica_e_El‚trica"}
    chPlanilha:cells(i-linha + 11 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 12) + ":" + "E" + STRING(i-linha + 12)):MergeCells    = TRUE.
    {utp/ut-liter.i "Preventiva_Lubrifica‡Æo"}
    chPlanilha:cells(i-linha + 12 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 13) + ":" + "E" + STRING(i-linha + 13)):MergeCells    = TRUE.
    {utp/ut-liter.i "Reforma"}
    chPlanilha:cells(i-linha + 13 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 14) + ":" + "E" + STRING(i-linha + 14)):MergeCells    = TRUE.
    {utp/ut-liter.i "Em_andamento"}
    chPlanilha:cells(i-linha + 14 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 15) + ":" + "E" + STRING(i-linha + 15)):MergeCells    = TRUE.
    {utp/ut-liter.i "Opera‡Æo_nÆo_liberou"}
    chPlanilha:cells(i-linha + 15 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 16) + ":" + "E" + STRING(i-linha + 16)):MergeCells    = TRUE.
    {utp/ut-liter.i "Falta_de_mÆo-de-obra"}
    chPlanilha:cells(i-linha + 16 , "B"):value      = RETURN-VALUE.

    chPlanilha:cells(i-linha + 18 , "A"):value      = "1".
    chPlanilha:cells(i-linha + 18 , "A"):Interior:Colorindex = 4.
    chPlanilha:cells(i-linha + 19 , "A"):value      = "2".
    chPlanilha:cells(i-linha + 19 , "A"):Interior:Colorindex = 4.
    chPlanilha:cells(i-linha + 20 , "A"):value      = "3".
    chPlanilha:cells(i-linha + 20 , "A"):Interior:Colorindex = 4.
    chPlanilha:cells(i-linha + 21 , "A"):value      = "4".
    chPlanilha:cells(i-linha + 21 , "A"):Interior:Colorindex = 6.
    chPlanilha:cells(i-linha + 22 , "A"):value      = "5".
    chPlanilha:cells(i-linha + 22 , "A"):Interior:Colorindex = 6.
    chPlanilha:cells(i-linha + 23 , "A"):value      = "6".
    chPlanilha:cells(i-linha + 23 , "A"):Interior:Colorindex = 6.

    chPlanilha:Range("B" + string(i-linha + 18) + ":" + "E" + STRING(i-linha + 18)):MergeCells    = TRUE.
    {utp/ut-liter.i "Realizado_Prev._Total_(Mec,_El‚tr,_Ar_condicionado_e_Lubrifica‡Æo)"}
    chPlanilha:cells(i-linha + 18 , "B"):value      = RETURN-VALUE.

    chPlanilha:Range("B" + string(i-linha + 19) + ":" + "E" + STRING(i-linha + 19)):MergeCells    = TRUE.
    {utp/ut-liter.i "Realizado _rev._de_Lubrifica‡Æo"}
    chPlanilha:cells(i-linha + 19 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 20) + ":" + "E" + STRING(i-linha + 20)):MergeCells    = TRUE.
    {utp/ut-liter.i "Realizado_Prev._Mec,_El‚tr_e_Ar_condicionado"}
    chPlanilha:cells(i-linha + 20 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 21) + ":" + "E" + STRING(i-linha + 21)):MergeCells    = TRUE.
    {utp/ut-liter.i "Sem_necessidade_de execu‡Æo_-_horimetro_fora_da_periodicidade"}
    chPlanilha:cells(i-linha + 21 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 22) + ":" + "E" + STRING(i-linha + 22)):MergeCells    = TRUE.
    {utp/ut-liter.i "NÆo_executado,_mina_nÆo_liberou"}
    chPlanilha:cells(i-linha + 22 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:Range("B" + string(i-linha + 23) + ":" + "E" + STRING(i-linha + 23)):MergeCells    = TRUE.
    {utp/ut-liter.i "Nao_execudado,_a_manuten‡Æo_priorizou_outro_equipamento"}
    chPlanilha:cells(i-linha + 23 , "B"):value      = RETURN-VALUE.
    
    chPlanilha:cells(3,3):SELECT.
    chExcel:ActiveWindow:FreezePanes = True.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piInicial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piInicial Procedure 
PROCEDURE piInicial :
/*------------------------------------------------------------------------------
  Purpose:     piInicial
  Parameters:  <none>
  Notes:       Define os valores que serÆo mostrados o cabe‡alho e rodap‚
------------------------------------------------------------------------------*/
/** Busca os parƒmetros criados na interface gr fica **/
find first tt-param.


/* Guarda os parƒmetros nas vari veis do relat¢rio */

ASSIGN dt-programacao-ini  =  tt-param.dt-programacao-ini
       dt-programacao-fim  =  tt-param.dt-programacao-fim
       i-empresa-ini       =  tt-param.i-empresa-ini     
       i-empresa-fim       =  tt-param.i-empresa-fim     
       c-estabelec-ini     =  tt-param.c-estabelec-ini   
       c-estabelec-fim     =  tt-param.c-estabelec-fim   
       c-equipamento-ini   =  tt-param.c-equipamento-ini 
       c-equipamento-fim   =  tt-param.c-equipamento-fim 
       c-oficina-ini       =  tt-param.c-oficina-ini     
       c-oficina-fim       =  tt-param.c-oficina-fim.      
      
find first param-global no-lock no-error.

if available param-global THEN DO:
    FIND FIRST empresa WHERE ep-codigo = empresa-prin NO-LOCK NO-ERROR.
    if available empresa THEN
        ASSIGN c-empresa = empresa.razao-social.
END.

{utp/ut-liter.i Listagem_de_T‚cnicos_p/_Ordem_de_Manuten‡Æo * L}
assign c-programa     = "esmv0701"
       c-versao       = "2.00"
       c-revisao      = "000"
       c-titulo-relat = return-value
       c-sistema      = "".

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piPrincipal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPrincipal Procedure 
PROCEDURE piPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     piPrincipal
  Parameters:  <none>
  Notes:       Corpo princiapl da aplica‡Æo
------------------------------------------------------------------------------*/
/** Mostra frames de cabe‡alho e rodap‚ padräes da Datasul **/
view frame f-cabec.
view frame f-rodape.

RUN carregaTT.

run pi-InicioExcel.

run pi-ExportaExcel.
        
run pi-FimExcel.

RUN pi-ordem IN THIS-PROCEDURE.

/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/

/** L¢gica opcional do programador **/

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Mostra parƒmetros selecionados **/
run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

