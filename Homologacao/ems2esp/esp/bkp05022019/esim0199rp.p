&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/******************************************************************************
** Autor    : Jurandir Guedes 
** Criaá∆o  : 12/02/2013
** Alteraáoe:
**
** Data         Autor       Alteraá∆o
** -----------  ----------- ----------------------------------------------------
** 03/12/2013   Jurandir    Criaá∆o da Rotina
** 
** 
** 
*******************************************************************************/
Def Buffer pais For  ems2cadme.pais.

Def Buffer cotacao For ems2cadme.cotacao.

/* Def Buffer pais For  mgcad.pais.  */


/* definiá∆o das temp-tables para recebimento de parÉmetros */

define temp-table tt-param       no-undo
    field destino                as integer
    field arquivo                as char format "x(35)"
    field usuario                as char format "x(12)"
    field data-exec              as date
    field hora-exec              as integer
    Field dt-di-ini              As Date Format "99/99/9999"
    Field dt-di-fim              As Date Format "99/99/9999"
    Field cod-estabel-ini        As Char Format "x(03)"
    Field cod-estabel-fim        As Char Format "x(03)"
    Field embarque-ini           As Char Format "x(12)"
    Field embarque-fim           As Char Format "x(12)"
    Field idi-tipo-imp           As Integer
    Field impressora             As Char Format "x(256)"
    Field cod-emitente-ini       As Int                           
    Field cod-emitente-fim       As Int                           
    Field it-codigo-ini          As Char                          
    Field it-codigo-fim          As Char                          
    Field nr-nota-fis-ini        As Char                          
    Field nr-nota-fis-fim        As Char                          
    Field serie-ini              As Char                          
    Field serie-fim              As Char                          
    Field nr-di-ini              As Char                          
    Field nr-di-fim              As Char /*declaracao-import*/    
    Field situacao-ini           As Int                           
    Field situacao-fim           As Int

    .


define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
    field raw-digita        as raw.

Def Temp-table tt-mov                   No-undo                                                        
    Field cod-estabel                   Like embarque-imp.cod-estabel                
    Field embarque                      Like embarque-imp.embarque                   
    Field cod-emitente                  Like docum-est.cod-emitente              
    Field nome-emit                     Like emitente.nome-emit                  
    Field it-codigo                     Like Item.it-codigo                      
    Field desc-item                     Like Item.desc-item                      
    Field nr-drawback                   Like item-docto-estoq-nfe-imp.cod-livre-1
    Field cod-pais                      Like pais.cod-pais
    Field nome-pais                     Like emitente.pais
    Field cod-incoterm                  Like embarque-imp.cod-incoterm
    Field di                            Like embarque-imp.declaracao-import
    Field dt-di                         As Char
    Field via-transp                    As Char
    Field tx-di                         As Dec
    Field vl-mercadoria                 As Dec
    Field vl-desp-nf                    As Dec
    Field qt-peso                       As Dec
    Field qt-merc                       As Dec
    Field un                            As Char
    Field nro-docto                     As Char
    Field nat-operacao                  As Char
    Field dt-emiss                      As Date
    Field tp-docto                      As Char
    Field dt-origem                     As Date
    Field dt-destino                    As Date
    Field vl-ii                         As Dec
    Field vl-frete-inter                As Dec
    Field vl-seguro-inter               As Dec
    Field vl-ipi                        As Dec
    Field vl-cofins                     As Dec
    Field vl-pis                        As Dec
    Field vl-capatazia                  As Dec
    Field vl-siscomex                   As Dec
    Field vl-sda-antecipada             As Dec 
    Field vl-AFRMM                      As Dec
    Field vl-furmigacao                 As Dec
    Field vl-armazenagem                As Dec
    Field vl-liber-bl                   As Dec
    Field vl-correios                   As Dec
    Field vl-remoc-dta                  As Dec
    Field vl-multa-di                   As Dec
    Field vl-serv-despacho              As Dec
    Field vl-frete-rod-eadi             As Dec
    Field vl-frete-ferro-eadi           As Dec
    Field vl-frete-rodo-un              As Dec
    Field vl-icms                       As Dec
    Field vl-demurrage                  As Dec
    Field vl-sda                        As Dec
    Field vl-armazem-porto              As Dec
    Field vl-aluguel-container          As Dec
    Field vl-frete-rodo-santos          As Dec
    Field vl-compl-armazem-porto        As Dec
    Field vl-servico-entrega            As Dec
    Field vl-frete-ferro-santos         As Dec
    Field vl-lavagem-container          As Dec
    Field vl-tecnico-nf                 As Dec
    Field vl-retificacao-ii             As Dec
    Field vl-outras-despesas            As Dec
    Field vl-outras-despesas-imposto    As Dec
    Field vl-compl-frete-internacional  As Dec
    Field vl-tx-portuaria-origem        As Dec
    Field vl-inspec-carga-destino       As Dec
    Field vl-inspec-carga-embarque      As Dec
    Field vl-thc                        As Dec
    Field vl-coord-carga                As Dec
    Field vl-gestao-processo            As Dec
    Field vl-compl-capatazia            As Dec
    Field vl-ipi-filha                  As Dec
    Field vl-cofins-filha               As Dec
    Field vl-pis-filha                  As Dec
    Field vl-icms-filha                 As Dec
    Field vl-multa-di-icms              As Dec
    Field vl-ii-filha                   As Dec
    Field vl-imposto-filha              As Dec
    Field vl-sub-total-despesas         As Dec
    Field vl-ipi-a                      As Dec /*RE0701*/
    Field vl-icms-a                     As Dec /*RE0701*/
    Field vl-frete-rodoviario-a         As Dec /*RE0701(n∆o Encontrei)*/
    Field st-nota                       As Char /*IM0545H*/
    Field dt-recebimento                As Date /*Data Transacao NF RE0701*/ 
    Field nr-pedido                     As Int  /*it-docum-esp.num-pedido*/
    Field st-situacao                   As Char /*IM0545 Aba Embarque*/

    Field dt-cotacao                    As Date
    Field mo-codigo                     As Int

    Field serie-docto            Like item-doc-est.serie-docto 
    Field sequencia              Like item-doc-est.sequencia 
    Index idx-1 Is Primary
          serie-docto 
          nro-docto   
          cod-emitente
          nat-operacao
          sequencia   

    .
 

Def Input Param raw-param As  Raw No-undo.
Def Input Param Table     For tt-raw-digita.

Create tt-param.
Raw-transfer raw-param To tt-param.

Def Stream Saida.

Def            Var ExcelAPP             As Com-handle     No-undo.
Def            Var ExcelFile            As Com-handle     No-undo.
Def            Var ExcelPlan            As Com-handle     No-undo.
Def            Var chPicture            As Com-handle     No-undo.

Def            Var cModelo              As Char           No-undo.
Def            Var cRange               As Char           No-undo.
Def            Var iLinha               As Int            No-undo.
Def            Var iColuna              As Int            No-undo.
Def            Var hAcomp               As Handle         No-undo.
Def            Var cArquivo             As Char           No-undo.
Def            Var iCont                As Int            No-undo.
Def            Var i-cod-desp-ii        As Int            No-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnCodPais) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnCodPais Procedure 
FUNCTION fnCodPais RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnCotacao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnCotacao Procedure 
FUNCTION fnCotacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDataDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDataDI Procedure 
FUNCTION fnDataDI RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDrawback) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDrawback Procedure 
FUNCTION fnDrawback RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnEmbarqueDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnEmbarqueDestino Procedure 
FUNCTION fnEmbarqueDestino RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnEmbarqueOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnEmbarqueOrigem Procedure 
FUNCTION fnEmbarqueOrigem RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnNumDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnNumDI Procedure 
FUNCTION fnNumDI RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnSitNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnSitNota Procedure 
FUNCTION fnSitNota RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnTaxaDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnTaxaDI Procedure 
FUNCTION fnTaxaDI RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnTipoNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnTipoNota Procedure 
FUNCTION fnTipoNota RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValDespesa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValDespesa Procedure 
FUNCTION fnValDespesa RETURNS DECIMAL
  ( p-cod-desp As Int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnViaTransporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnViaTransporte Procedure 
FUNCTION fnViaTransporte RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


 
 Run utp/ut-acomp.p persistent set hAcomp.

 Run pi-inicializar in hAcomp (Input "Processando ...").
 
 Run rotProcessa.
 Run rotImprime.

 Run pi-finalizar In hAcomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-excelAbre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excelAbre Procedure 
PROCEDURE excelAbre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


        Create "Excel.Application"   excelAPP. 
        ExcelAPP:Visible = False.              

         /* Criar uma Pasta em Branco */
        ExcelFile = ExcelAPP:workbooks:Add(cModelo).

        /* Posicionar na Primeira Pasta */
        ExcelPlan = ExcelFile:Sheets:Item(1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-excelFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excelFecha Procedure 
PROCEDURE excelFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  Case tt-param.destino:
      When 1 Then Do:
          ExcelAPP:Visible = False.
          ExcelAPP:ActiveWorkbook:PrintOut().
          ExcelAPP:ActiveWindow:Close(No).
      End.
      When 3 Then Do:
          ExcelAPP:Range("A1"):Select.
          ExcelAPP:Visible = True.

      End.
      Otherwise Do:
          ExcelAPP:Visible = True.
      End.
  End Case.

  If  Valid-handle(ExcelAPP)      Then Release Object ExcelAPP.
  If  Valid-handle(ExcelFile)     Then Release Object ExcelFile.
  If  Valid-handle(ExcelPlan)     Then Release Object ExcelPlan.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rotImprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rotImprime Procedure 
PROCEDURE rotImprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    Assign cArquivo = Session:Temp-dir
                    + "esim0199rp.csv".

    Output Stream saida To Value(cArquivo) No-convert.

    Export Stream Saida Delimiter ";"
        "Estab"  
        "Embarque"  
        "Fornecedor"  
        "Raz∆o Social"  
        "Item"  
        "Descriá∆o do Item"  
        "Nro. Drawback"  
        "Pa°s Origem"  
        "Nome Pa°s"  
        "Incoterm"  
        "Nro.DI"  
        "Data DI"  
        "Via de Transporte"  
        "Taxa DI"  
        "Vl. Mercadoria"  
        "Vl. Desp NF"  
        "Peso"  
        "Qtde"  
        "Und"  
        "Nro. NF"  
        "Nat.Operaá∆o"  
        "Dt. Emiss∆o"  
        "Tipo Docto"  
        "Dt.Emb.Origem"  
        "Dt.Emb.Destino"  
        "Vl. II"  
        "Vl. Frete Internac."  
        "Vl. Seuro Internac."  
        "Vl. IPI"  
        "Vl. COFINS"  
        "Vl. PIS"  
        "Capatazia"  
        "SISCOMEX"  
        "SDA Antecipada"  
        "AFRMM"  
        "Furmigacao"  
        "Armazenagem"  
        "Liberaá∆o BL"  
        "Correios"  
        "Repoá∆o DTA"  
        "Multa DI"  
        "Despachante"  
        "vl-frete-rod-eadi "  
        "vl-frete-ferro-eadi         "  
        "vl-frete-rodo-un            "  
        "vl-icms                     "  
        "vl-demurrage                "  
        "vl-sda                      "  
        "vl-armazem-porto            "  
        "vl-aluguel-container        "  
        "vl-frete-rodo-santos        "  
        "vl-compl-armazem-porto      "  
        "vl-servico-entrega          "  
        "vl-frete-ferro-santos       "  
        "vl-lavagem-container        "  
        "vl-tecnico-nf               "  
        "vl-retificacao-ii           "  
        "vl-outras-despesas          "  
        "vl-outras-despesas-imposto  "  
        "vl-compl-frete-internacional"  
        "vl-tx-portuaria-origem      "  
        "vl-inspec-carga-destino     "  
        "vl-inspec-carga-embarque    "  
        "vl-thc                      "  
        "vl-coord-carga              "  
        "vl-gestao-processo          "  
        "vl-compl-capatazia          "  
        "vl-ipi-filha                "  
        "vl-cofins-filha             "  
        "vl-pis-filha                "  
        "vl-icms-filha               "  
        "vl-multa-di-icms            "  
        "vl-ii-filha                 "  
        "vl-imposto-filha            "  
        "vl-sub-total-despesas       "  
        "vl-ipi-a                    "  
        "vl-icms-a                   "  
        "vl-frete-rodoviario-a       "  
        "st-nota                     "  
        "dt-recebimento              "  
        "nr-pedido                   "  
        "st-situacao                 "
        "Dt. Cotaá∆o"
        "Moeda"
        .

    For Each tt-mov:
        Export Stream Saida Delimiter ";"
               tt-mov.cod-estabel                   
               tt-mov.embarque                      
               tt-mov.cod-emitente                  
               tt-mov.nome-emit                     
               tt-mov.it-codigo                     
               tt-mov.desc-item                     
               tt-mov.nr-drawback                   
               tt-mov.cod-pais                      
               tt-mov.nome-pais                     
               tt-mov.cod-incoterm                  
               tt-mov.di                            
               tt-mov.dt-di                         
               tt-mov.via-transp                    
               tt-mov.tx-di                         
               tt-mov.vl-mercadoria                 
               tt-mov.vl-desp-nf                    
               tt-mov.qt-peso                       
               tt-mov.qt-merc                       
               tt-mov.un                            
               tt-mov.nro-docto                     
               tt-mov.nat-operacao                  
               tt-mov.dt-emiss                      
               tt-mov.tp-docto                      
               tt-mov.dt-origem                     
               tt-mov.dt-destino                    
               tt-mov.vl-ii                         
               tt-mov.vl-frete-inter                
               tt-mov.vl-seguro-inter               
               tt-mov.vl-ipi                        
               tt-mov.vl-cofins                     
               tt-mov.vl-pis                        
               tt-mov.vl-capatazia                  
               tt-mov.vl-siscomex                   
               tt-mov.vl-sda-antecipada             
               tt-mov.vl-AFRMM                      
               tt-mov.vl-furmigacao                 
               tt-mov.vl-armazenagem                
               tt-mov.vl-liber-bl                   
               tt-mov.vl-correios                   
               tt-mov.vl-remoc-dta                  
               tt-mov.vl-multa-di                   
               tt-mov.vl-serv-despacho              
               tt-mov.vl-frete-rod-eadi             
               tt-mov.vl-frete-ferro-eadi           
               tt-mov.vl-frete-rodo-un              
               tt-mov.vl-icms                       
               tt-mov.vl-demurrage                  
               tt-mov.vl-sda                        
               tt-mov.vl-armazem-porto              
               tt-mov.vl-aluguel-container          
               tt-mov.vl-frete-rodo-santos          
               tt-mov.vl-compl-armazem-porto        
               tt-mov.vl-servico-entrega            
               tt-mov.vl-frete-ferro-santos         
               tt-mov.vl-lavagem-container          
               tt-mov.vl-tecnico-nf                 
               tt-mov.vl-retificacao-ii             
               tt-mov.vl-outras-despesas            
               tt-mov.vl-outras-despesas-imposto    
               tt-mov.vl-compl-frete-internacional  
               tt-mov.vl-tx-portuaria-origem        
               tt-mov.vl-inspec-carga-destino       
               tt-mov.vl-inspec-carga-embarque      
               tt-mov.vl-thc                        
               tt-mov.vl-coord-carga                
               tt-mov.vl-gestao-processo            
               tt-mov.vl-compl-capatazia            
               tt-mov.vl-ipi-filha                  
               tt-mov.vl-cofins-filha               
               tt-mov.vl-pis-filha                  
               tt-mov.vl-icms-filha                 
               tt-mov.vl-multa-di-icms              
               tt-mov.vl-ii-filha                   
               tt-mov.vl-imposto-filha              
               tt-mov.vl-sub-total-despesas         
               tt-mov.vl-ipi-a                      
               tt-mov.vl-icms-a                     
               tt-mov.vl-frete-rodoviario-a         
               tt-mov.st-nota                       
               tt-mov.dt-recebimento                
               tt-mov.nr-pedido                     
               tt-mov.st-situacao
               tt-mov.dt-cotacao
               tt-mov.mo-codigo
            .                   


    End.

    Output Stream Saida Close.


    Dos Silent Start Value(cArquivo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rotProcessa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rotProcessa Procedure 
PROCEDURE rotProcessa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/




For Each embarque-imp No-lock 
   Where embarque-imp.cod-estabel             >= tt-param.cod-estabel-ini
     And embarque-imp.cod-estabel             <= tt-param.cod-estabel-fim
     And embarque-imp.declaracao-import       >= tt-param.nr-di-ini
     And embarque-imp.declaracao-import       <= tt-param.nr-di-fim
     And (embarque-imp.embarque               >= tt-param.embarque-ini
     And embarque-imp.embarque                <= tt-param.embarque-fim Or 
         Substring(embarque-imp.char-1,21,12) >= tt-param.embarque-ini
     And Substring(embarque-imp.char-1,21,12) <= tt-param.embarque-fim)
     And embarque-imp.data-di                 >= tt-param.dt-di-ini
     And embarque-imp.data-di                 <= tt-param.dt-di-fim,
   Each ordens-embarque No-lock 
  Where ordens-embarque.cod-estabel   = embarque-imp.cod-estabel
    And ordens-embarque.embarque      = embarque-imp.embarque,
   Each processo-imp No-lock Of ordens-embarque,
   Each ordem-compra No-lock 
  Where ordem-compra.numero-ordem     = ordens-embarque.numero-ordem
    And ordem-compra.cod-emitente    >= tt-param.cod-emitente-ini
    And ordem-compra.cod-emitente    <= tt-param.cod-emitente-fim
    And ordem-compra.it-codigo       >= tt-param.it-codigo-ini
    And ordem-compra.it-codigo       <= tt-param.it-codigo-fim
    And ordem-compra.situacao        >= tt-param.situacao-ini
    And ordem-compra.situacao        <= tt-param.situacao-fim,
   Each emitente No-lock 
  Where emitente.cod-emitente         = ordem-compra.cod-emitente,
   Each Item No-lock 
  Where Item.it-codigo                = ordem-compra.it-codigo,
   Each cotacao-item No-lock 
  Where cotacao-item.numero-ordem     = ordem-compra.numero-ordem
    And cotacao-item.cod-emitente     = ordem-compra.cod-emitente
    And cotacao-item.data-cotacao     = ordem-compra.data-cotacao
    And cotacao-item.cot-aprovada     = Yes:


    Run pi-acompanhar in hAcomp (input "Tratando Embarque: " + embarque-imp.embarque).


    For First param-imp fields(cod-estabel char-1 ) No-lock
        Where param-imp.cod-estabel = embarque-imp.cod-estabel:
    end.


    If  Not Available param-imp Then  Next.

    Assign i-cod-desp-ii  = Int(substring(param-imp.char-1,21,20)). 


    For Each docum-est No-lock
       Where docum-est.cod-estabel       = embarque-imp.cod-estabel
         And docum-est.embarque          = embarque-imp.embarque
         And docum-est.nro-docto        >= tt-param.nr-nota-fis-ini
         And docum-est.nro-docto        <= tt-param.nr-nota-fis-fim
         And docum-est.serie-docto      >= tt-param.serie-ini      
         And docum-est.serie-docto      <= tt-param.serie-fim,      
        Each item-doc-est No-lock                                      
       Where item-doc-est.serie-docto    = docum-est.serie-docto       
         And item-doc-est.nro-docto      = docum-est.nro-docto         
         And item-doc-est.cod-emitente   = docum-est.cod-emitente      
         And item-doc-est.nat-operacao   = docum-est.nat-operacao
         And item-doc-est.numero-ordem   = ordem-compra.numero-ordem
         And item-doc-est.it-codigo      = Item.it-codigo:


        Find First tt-mov
             Where tt-mov.serie-docto  = item-doc-est.serie-docto 
               And tt-mov.nro-docto    = item-doc-est.nro-docto   
               And tt-mov.cod-emitente = item-doc-est.cod-emitente
               And tt-mov.nat-operacao = item-doc-est.nat-operacao
               And tt-mov.sequencia    = item-doc-est.sequencia   
            No-error.

        If  Available tt-mov Then Next.


        Create tt-mov.

        Assign tt-mov.serie-docto                  = item-doc-est.serie-docto     
               tt-mov.nro-docto                    = item-doc-est.nro-docto       
               tt-mov.cod-emitente                 = item-doc-est.cod-emitente    
               tt-mov.nat-operacao                 = item-doc-est.nat-operacao    
               tt-mov.sequencia                    = item-doc-est.sequencia .

        Assign tt-mov.cod-estabel                  = embarque-imp.cod-estabel
               tt-mov.embarque                     = embarque-imp.embarque
               tt-mov.cod-emitente                 = emitente.cod-emitente
               tt-mov.nome-emit                    = emitente.nome-emit
               tt-mov.it-codigo                    = Item.it-codigo
               tt-mov.desc-item                    = Item.desc-item
               tt-mov.cod-pais                     = fnCodPais()
               tt-mov.nome-pais                    = emitente.pais
               tt-mov.cod-incoterm                 = embarque-imp.cod-incoterm
               tt-mov.di                           = fnNumDI()          
               tt-mov.dt-di                        = fnDataDI()         
               tt-mov.via-transp                   = fnViaTransporte()  
               tt-mov.tx-di                        = fnTaxaDI()
               tt-mov.nr-drawback                  = fnDrawback()
               tt-mov.vl-mercadoria                = Round((cotacao-item.preco-unit * item-doc-est.quantidade) / tt-mov.tx-di,2)
               tt-mov.qt-peso                      = item-doc-est.peso-liquido
               tt-mov.qt-merc                      = item-doc-est.quantidade
               tt-mov.un                           = item-doc-est.un
               tt-mov.nat-operacao                 = docum-est.nat-operacao
               tt-mov.nro-docto                    = docum-est.nro-docto
               tt-mov.dt-emis                      = docum-est.dt-emis
               tt-mov.tp-docto                     = fnTipoNota()
               tt-mov.dt-origem                    = fnEmbarqueOrigem()
               tt-mov.dt-destino                   = fnEmbarqueDestino()
               
               tt-mov.vl-ii                        = fnValDespesa(01)
               tt-mov.vl-frete-inter               = fnValDespesa(02) 
               tt-mov.vl-seguro-inter              = fnValDespesa(03)
               tt-mov.vl-ipi                       = fnValDespesa(04) 
               tt-mov.vl-cofins                    = fnValDespesa(05) 
               tt-mov.vl-pis                       = fnValDespesa(06) 
               tt-mov.vl-capatazia                 = fnValDespesa(07) 
               tt-mov.vl-siscomex                  = fnValDespesa(08)
               tt-mov.vl-sda-antecipada            = fnValDespesa(09)
               tt-mov.vl-AFRMM                     = fnValDespesa(10)
               tt-mov.vl-furmigacao                = fnValDespesa(11)
               tt-mov.vl-armazenagem               = fnValDespesa(12)
               tt-mov.vl-liber-bl                  = fnValDespesa(13)
               tt-mov.vl-correios                  = fnValDespesa(14)
               tt-mov.vl-remoc-dta                 = fnValDespesa(15)
               tt-mov.vl-multa-di                  = fnValDespesa(16)
               tt-mov.vl-serv-despacho             = fnValDespesa(17)
               tt-mov.vl-frete-rod-eadi            = fnValDespesa(18)
               tt-mov.vl-frete-ferro-eadi          = fnValDespesa(19)
               tt-mov.vl-frete-rodo-un             = fnValDespesa(20)
               tt-mov.vl-icms                      = fnValDespesa(21)
               tt-mov.vl-demurrage                 = fnValDespesa(23)
               tt-mov.vl-sda                       = fnValDespesa(24)
               tt-mov.vl-armazem-porto             = fnValDespesa(25)
               tt-mov.vl-aluguel-container         = fnValDespesa(26)
               tt-mov.vl-frete-rodo-santos         = fnValDespesa(00)  /*N∆o Encontrado*/
               tt-mov.vl-compl-armazem-porto       = fnValDespesa(00)  /*N∆o Encontrado*/
               tt-mov.vl-servico-entrega           = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-frete-ferro-santos        = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-lavagem-container         = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-tecnico-nf                = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-retificacao-ii            = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-outras-despesas           = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-outras-despesas-imposto   = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-compl-frete-internacional = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-tx-portuaria-origem       = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-inspec-carga-destino      = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-inspec-carga-embarque     = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-thc                       = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-coord-carga               = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-gestao-processo           = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-compl-capatazia           = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-ipi-filha                 = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-cofins-filha              = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-pis-filha                 = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-icms-filha                = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-multa-di-icms             = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-ii-filha                  = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-imposto-filha             = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-sub-total-despesas        = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.vl-ipi-a                     = docum-est.ipi-deb-cre  
               tt-mov.vl-icms-a                    = docum-est.icm-deb-cre  
               tt-mov.vl-frete-rodoviario-a        = fnValDespesa(00)  /*N∆o Encontrada*/
               tt-mov.st-nota                      = fnSitNota()
               tt-mov.dt-recebimento               = docum-est.dt-trans
               tt-mov.nr-pedido                    = ordem-compra.num-pedido
               tt-mov.st-situacao                  = {cxinc/i01cx220.i 04 embarque-imp.situacao}
            .   

        For Each item-doc-est-cex No-lock
           Where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
             And item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
             And item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
             And item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
             And item-doc-est-cex.sequencia    = item-doc-est.sequencia 
             /*And item-doc-est-cex.cod-desp     = i-cod-desp-ii */ :

            If  item-doc-est-cex.cod-desp = i-cod-desp-ii Then Next. /*II*/
            If  item-doc-est-cex.cod-desp = 3             Then Next. /*Seguro Internacional*/

            Assign tt-mov.vl-desp-nf = tt-mov.vl-desp-nf + (item-doc-est-cex.val-desp / tt-mov.tx-di).
        End.

    End.


End.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnCodPais) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnCodPais Procedure 
FUNCTION fnCodPais RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    For First pais No-lock
        Where pais.nome-pais = emitente.pais:
        Return pais.cod-pais.
    End.


   Return  0.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnCotacao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnCotacao Procedure 
FUNCTION fnCotacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDataDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDataDI Procedure 
FUNCTION fnDataDI RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Return String(embarque-imp.data-di,"99/99/9999").
      

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDrawback) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDrawback Procedure 
FUNCTION fnDrawback RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/


    For First item-docto-estoq-nfe-imp No-lock
        Where item-docto-estoq-nfe-imp.cod-ser-docto    = docum-est.serie  
          And item-docto-estoq-nfe-imp.cod-docto        = docum-est.nro-docto    
          And item-docto-estoq-nfe-imp.cdn-emitente     = docum-est.cod-emitente 
/*           And item-docto-estoq-nfe-imp.cod-natur-operac = docum-est.nat-operacao  */
/*           And item-docto-estoq-nfe-imp.num-seq          = item-doc-est.num-seq    */
        
        :

        Return item-docto-estoq-nfe-imp.cod-livre-1.

    End.

    Return "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnEmbarqueDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnEmbarqueDestino Procedure 
FUNCTION fnEmbarqueDestino RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    
    For Each historico-embarque No-lock
       Where historico-embarque.cod-estabel = embarque-imp.cod-estabel
         And historico-embarque.embarque    = embarque-imp.embarque:
        Return historico-embarque.dt-efetiva.
    End.

    Return ?.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnEmbarqueOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnEmbarqueOrigem Procedure 
FUNCTION fnEmbarqueOrigem RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    For Each historico-embarque No-lock
       Where historico-embarque.cod-estabel = embarque-imp.cod-estabel
         And historico-embarque.embarque    = embarque-imp.embarque:
        Return historico-embarque.dt-previsao.
    End.



    Return ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnNumDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnNumDI Procedure 
FUNCTION fnNumDI RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

      
    For First docto-estoq-nfe-imp No-lock
        Where docto-estoq-nfe-imp.cod-ser-docto    = docum-est.serie  
          And docto-estoq-nfe-imp.cod-docto        = docum-est.nro-docto    
          And docto-estoq-nfe-imp.cdn-emitente     = docum-est.cod-emitente 
          And docto-estoq-nfe-imp.cod-natur-operac = docum-est.nat-operacao: 

        Return docto-estoq-nfe-imp.des-decla-import.

    End.

    Return "".   


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnSitNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnSitNota Procedure 
FUNCTION fnSitNota RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    /* 1 N∆o Confirmada */
    /* 2 Confirmada     */
    /* 3 Cotada         */
    /* 4 Eliminada      */
    /* 5 Em Cotaá∆o     */
    /* 6 Recebida       */

    Case ordem-compra.situacao:
        When 1 Then Return "N∆o Confirmada". 
        When 2 Then Return "Confirmada    ". 
        When 3 Then Return "Cotada        ". 
        When 4 Then Return "Eliminada     ". 
        When 5 Then Return "Em Cotaá∆o    ". 
        When 6 Then Return "Recebida      ". 
    End Case.




    Return  "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnTaxaDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnTaxaDI Procedure 
FUNCTION fnTaxaDI RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Def Var dataDI  As Date No-undo.
    Def Var cont    As Int  No-undo.
    Def Var Taxa    As Dec  No-undo.

    Assign dataDI = Date(fnDataDI())
           cont   = 0.

    Repeat:
        dataDI = dataDI - 1.

        If  Weekday(dataDI) <> 1 And Weekday(dataDI) <> 7
        Then cont = cont + 1.

        If  cont = 3 Then Leave.

    End.

    Find First cotacao No-lock
         Where cotacao.mo-codigo    = ordem-compra.mo-codigo
           And cotacao.ano-periodo  = String(Year (dataDi),"9999")
                                    + String(Month(dataDi),"99")
        No-error.


    If  cotacao.cotacao[Day(dataDI)] = 0 
    Then taxa = 1.
    Else taxa = cotacao.cotacao[Day(dataDI)].

    Assign tt-mov.dt-cotacao = dataDI
           tt-mov.mo-codigo  = ordem-compra.mo-codigo.


    Return taxa.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnTipoNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnTipoNota Procedure 
FUNCTION fnTipoNota RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    If   Substring(embarque-imp.char-1,21,12) = "" 
    Then Return "Nota M∆e".
    Else If  embarque-imp.embarque = Trim(Substring(embarque-imp.char-1,21,12))   
         Then Return "Nota M∆e".
         Else Return "Nota Filha" .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnValDespesa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValDespesa Procedure 
FUNCTION fnValDespesa RETURNS DECIMAL
  ( p-cod-desp As Int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    
        Def Var valorDespesa As Dec No-undo.

        Assign valorDespesa = 0.

        For Each item-doc-est-cex No-lock
           Where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
             And item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
             And item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
             And item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
             And item-doc-est-cex.sequencia    = item-doc-est.sequencia 
             And item-doc-est-cex.cod-desp     = p-cod-desp  :

            Assign valorDespesa = valorDespesa + (item-doc-est-cex.val-desp / tt-mov.tx-di).
        End.


        Return valorDespesa.   
    



END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnViaTransporte) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnViaTransporte Procedure 
FUNCTION fnViaTransporte RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    Def Var viaTransp   As Char No-undo.
    Def Var indTransp   As Int  No-undo.


    indtransp = Int(Substring(docto-estoq-nfe-imp.cod-livre-1,1,2)).

/* Conforme RE0701h(page1)            */
/* 1  = Mar°tima,1,                    */
/* 2  = Fluvial,2,                     */
/* 3  = Lacustre,3,                    */
/* 4  = AÇrea,4,                       */
/* 5  = Postal,5,                      */
/* 6  = Ferrovi†ria,6,                 */
/* 7  = Rodovi†ria,7,                  */
/* 8  = Conduto / Rede Transmiss∆o,8,  */
/* 9  = Meios Pr¢prios,9,              */
/* 10 = Entrada / Sa°da ficta,10,     */
/* 11 = Courier,11,                   */
/* 12 = Handcarry,12                  */


    Assign viaTransp = "Mar°tima,                  " 
                     + "Fluvial,                   " 
                     + "Lacustre,                  " 
                     + "AÇrea,                     " 
                     + "Postal,                    " 
                     + "Ferrovi†ria,               " 
                     + "Rodovi†ria,7,              "   
                     + "Conduto / Rede Transmiss∆o," 
                     + "Meios Pr¢prios,            " 
                     + "Entrada / Sa°da ficta,     "
                     + "Courier,                   "
                     + "Handcarry,                 ". 




    For  First docto-estoq-nfe-imp No-lock
         Where docto-estoq-nfe-imp.cod-estab          = docum-est.cod-estab
           And docto-estoq-nfe-imp.cod-ser-docto      = docum-est.serie-docto
           And docto-estoq-nfe-imp.cod-docto          = docum-est.nro-docto
           And docto-estoq-nfe-imp.cdn-emitente       = docum-est.cod-emitente:

        indtransp = Int(Substring(docto-estoq-nfe-imp.cod-livre-1,1,2)).

        If  indTransp > 0 And indTransp < 13
        Then Return Entry(indTransp,viaTransp).
        Else Return "".

           
    End.

    Return "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

