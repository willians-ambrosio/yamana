&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/******************************************************************************
** Autor    : Jurandir Guedes 
** Criação  : 12/02/2013
** Altera‡oe:
**
** Data         Autor          Alteração
** -----------  -------------- ----------------------------------------------------
** 03/12/2013   Jurandir       Criação da Rotina
**    02/2019   Flávia Acioli  Ajustes do Programa
** 
** 
*******************************************************************************/
Def Buffer pais For  ems2cadme.pais.

Def Buffer cotacao For ems2cadme.cotacao.

Def Buffer b-docum-est    For docum-est.
Def Buffer b-item-doc-est For item-doc-est.

/* definição das temp-tables para recebimento de parâmetros */

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
    Field situacao-fim           As Int.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

Def Temp-table tt-mov                   No-undo                                                        
    Field cod-estabel                   Like embarque-imp.cod-estabel
    Field nr-pedido                     As Int
    Field nome-estab                    Like estabelec.nome
    Field embarque                      Like embarque-imp.embarque
    Field cod-emitente                  Like docum-est.cod-emitente /* RE0701 */
    Field nome-emit                     Like emitente.nome-emit
    Field it-codigo                     Like Item.it-codigo
    Field desc-item                     Like Item.desc-item
    Field nr-drawback                   Like item-docto-estoq-nfe-imp.cod-livre-1
    Field cod-pais                      Like pais.cod-pais
    Field nome-pais                     Like emitente.pais
    Field cod-incoterm                  Like embarque-imp.cod-incoterm
    Field desc-moeda                    As Char
    Field data-pedido                   Like ordem-compra.data-pedido
    Field di                            Like embarque-imp.declaracao-import
    Field dt-di                         As Char
    Field via-transp                    As Char
    Field tx-di                         As Dec
    Field vl-mercadoria                 As Dec
    Field vl-desp-nf                    As Dec
    Field qt-peso                       As Dec
    Field qt-merc                       As Dec /* Só mostra se for Nota Mãe */
    Field un                            As Char
    Field nro-docto                     As Char
    Field serie-docto                   Like docum-est.serie-docto 
/*     Field sequencia                     Like item-doc-est.sequencia */
    Field nat-operacao                  As Char
    Field tp-docto                      As Char
    Field dt-emis                       As Date
    Field dt-solicitacao-nf             As Char /* Se Nota Mãe ou Filha, mostra a dt-efetiva do ponto de controle 7 (IM0055); Se Nota Complementar, mostra a dt-efetiva do ponto de controle 8; Se Nota Complementar com natureza 2352LK (frete), mostra em branco */
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
    Field vl-fumigacao                  As Dec
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
    Field vl-coord-descarga             As Dec
    Field vl-gestao-processo            As Dec
    Field vl-compl-capatazia            As Dec
    Field vl-ipi-filha                  As Dec
    Field vl-cofins-filha               As Dec
    Field vl-pis-filha                  As Dec
    Field vl-icms-filha                 As Dec
    Field vl-multa-di-icms              As Dec
    Field vl-ii-filha                   As Dec
    Field vl-imposto-filha              As Dec
    Field vl-total-despesas             As Dec
    Field vl-ipi-a                      As Dec /*RE0701*/
    Field vl-icms-a                     As Dec /*RE0701*/
    Field vl-frete-rodoviario-a         As Dec /* RE0701 - Valor Total da aba Totais II se a natureza de operação da nota for 2352LK - Somente para Notas Complementares */
    Field st-nota                       As Char /*IM0545H*/
    Field dt-recebimento                As Date /*Data Transacao NF RE0701*/
    Field status-proc                   As Char /*IM0545 Aba Embarque*/
    Field dt-cotacao                    As Date
    Index idx-1 Is Primary
          serie-docto
          nro-docto
          cod-emitente
          nat-operacao
          /*it-codigo
          sequencia*/.

Def Input Param raw-param As  Raw No-undo.
Def Input Param Table     For tt-raw-digita.

Create tt-param.
Raw-transfer raw-param To tt-param.

Def Stream Saida.

Def Var ExcelAPP      As Com-handle No-undo.
Def Var ExcelFile     As Com-handle No-undo.
Def Var ExcelPlan     As Com-handle No-undo.
Def Var chPicture     As Com-handle No-undo.
Def Var cModelo       As Char       No-undo.
Def Var cRange        As Char       No-undo.
Def Var iLinha        As Int        No-undo.
Def Var iColuna       As Int        No-undo.
Def Var hAcomp        As Handle     No-undo.
Def Var cArquivo      As Char       No-undo.
Def Var iCont         As Int        No-undo.
Def Var i-cod-desp-ii As Int        No-undo.
Def Var c-nome-estab  As Char       No-undo.

Def Buffer b-tt-mov For tt-mov.

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

&IF DEFINED(EXCLUDE-fnDataPedidoNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDataPedidoNota Procedure 
FUNCTION fnDataPedidoNota RETURNS Character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDataPedidoNotaCompl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDataPedidoNotaCompl Procedure 
FUNCTION fnDataPedidoNotaCompl RETURNS Character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDesembarqueDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDesembarqueDestino Procedure 
FUNCTION fnDesembarqueDestino RETURNS DATE
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

&IF DEFINED(EXCLUDE-fnvalDespesaComplementar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnvalDespesaComplementar Procedure 
FUNCTION fnvalDespesaComplementar RETURNS DECIMAL
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
    Define Variable c-data              As Char                            No-undo.
    Define Variable c-hora              As Char                            No-undo.

    Assign c-data   = String(Year(Today),"9999") + String(Month(Today),"99") + String(Day(Today),"99")
           c-hora   = Substring(String(Time,"hh:mm:ss"),1,2)
           c-hora   = c-hora + Substring(String(Time,"hh:mm:ss"),4,2).
           cArquivo = Session:Temp-dir
                    + "esim0199rp" + "_" + c-data + "_" + c-hora + ".csv".

    Output Stream saida To Value(cArquivo) No-convert.

    Export Stream Saida Delimiter ";"
        "Unidade"
        "Nr. Pedido"
        "Embarque"
        "Código Fornecedor"
        "Nome Fornecedor"
        "Código Item"
        "Descrição do Item"
        "Nr. Drawback"
        "País Origem"
        "Nome País"
        "Incoterm"
        "Moeda do pedido"
        "Data do pedido"
        "Nr. DI"
        "Data DI"
        "Via de Transporte"
        "Taxa DI"
        "Dt. Cotação DI"
        "Vl. Mercadoria R$"
        "Vl. Desp NF c/imp s/ IPI exceto NF filhas"
        "Peso líquido (kg)"
        "Quantidade"
        "Unidade Medida"
        "Nr. NF"
        "Série"
/*         "Sequência" */
        "Nat. Operação"
        "Tipo Docto"
        "Dt. Solicitação NF"
        "Dt. Emissão da NF"
        "Dt.Emb.Origem"
        "Dt.Chegada Destino"
        "Dt. Receb. Planta"
        "Frete Intern." /* Frete Internacional */
        "Seguro Intern." /* Seguro Internacional */
        "Capatazia"
        "Tx Siscomex"
        "AFRMM"
        "Serviços despachante"
        "Demurrage"
        "S D A"
        "Armazenagem porto"
        "II" /* Imposto de Importação */
        "IPI"
        "Cofins"
        "PIS"
        "S D A Antecipada"
        "Fumigação"
        "Armazenagem EADI"
        "Liberação BL"
        "Correios"
        "Remoção de DTA"
        "Multas DI"
        "Frete rodo até EADI" /* Frete rodoviário até EADI */
        "Frete ferro até EADI" /* Frete ferroviário até EADI */
        "Frete rodo até unid." /* Frete rodoviário até unidade */
        "ICMS"
        "Aluguel de ctn" /* Aluguel de Container */
        "Frete ferro até Santos" /* Frete ferroviário até Santos */
        "Compl. Armazenagem Porto"
        "Serviço de entrega"
        "Frete ferro até Santos"
        "Lavagem ctn." /* Lavagem de Container */
        "Técnico NF"
        "Retificação II"
        "Outras despesas"
        "Outras despesas impostos"
        "Compl. Frete Intern."
        "Tx Portuárias na origem"
        "Inspeção da carga no destino"
        "Inspeção da carga no embarque"
        "THC"
        "Coordenação de descarga"
        "Gestão do processo"
        "Compl. Capatazia"
        "IPI - Filha"
        "Cofins - Filha"
        "PIS - Filha"
        "ICMS - Filha"
        "Multas DI - ICMS"
        "II - Filha"
        "Imposto Filha"
        "Total despesas"
        "IPI"
        "ICMS"
        "Frete rodo c/ imp."
        "Situação chegada na planta"
        "Status processo".

    For Each tt-mov
        Break By tt-mov.embarque
              By tt-mov.serie-docto  
              by tt-mov.nro-docto    
              by tt-mov.cod-emitente 
              by tt-mov.nat-operacao 
              by tt-mov.it-codigo    
              /*by tt-mov.sequencia*/:

        If tt-mov.dt-solicitacao-nf = ? Then
            Assign tt-mov.dt-solicitacao-nf = "".

        If  First-of(tt-mov.it-codigo) Then Do:

            Export Stream Saida Delimiter ";"
                   tt-mov.nome-estab
                   tt-mov.nr-pedido
                   tt-mov.embarque                      
                   tt-mov.cod-emitente                  
                   tt-mov.nome-emit                     
                   tt-mov.it-codigo                     
                   tt-mov.desc-item                     
                   tt-mov.nr-drawback                   
                   tt-mov.cod-pais                      
                   tt-mov.nome-pais                     
                   tt-mov.cod-incoterm
                   tt-mov.desc-moeda
                   tt-mov.data-pedido
                   tt-mov.di                            
                   tt-mov.dt-di                         
                   tt-mov.via-transp                    
                   tt-mov.tx-di
                   tt-mov.dt-cotacao
                   tt-mov.vl-mercadoria                 
                   tt-mov.vl-desp-nf                    
                   tt-mov.qt-peso                       
                   tt-mov.qt-merc                       
                   tt-mov.un                            
                   tt-mov.nro-docto
                   tt-mov.serie
/*                    tt-mov.sequencia */
                   tt-mov.nat-operacao
                   tt-mov.tp-docto
                   tt-mov.dt-solicitacao-nf
                   tt-mov.dt-emis 
                   tt-mov.dt-origem                     
                   tt-mov.dt-destino                       
                   tt-mov.dt-recebimento
                   tt-mov.vl-frete-inter                
                   tt-mov.vl-seguro-inter               
                   tt-mov.vl-capatazia                  
                   tt-mov.vl-siscomex                   
                   tt-mov.vl-AFRMM                      
                   tt-mov.vl-serv-despacho              
                   tt-mov.vl-demurrage                  
                   tt-mov.vl-sda                        
                   tt-mov.vl-armazem-porto
                   tt-mov.vl-ii
                   tt-mov.vl-ipi                        
                   tt-mov.vl-cofins                     
                   tt-mov.vl-pis                        
                   tt-mov.vl-sda-antecipada             
                   tt-mov.vl-fumigacao                 
                   tt-mov.vl-armazenagem                
                   tt-mov.vl-liber-bl                   
                   tt-mov.vl-correios                   
                   tt-mov.vl-remoc-dta                  
                   tt-mov.vl-multa-di                   
                   tt-mov.vl-frete-rod-eadi             
                   tt-mov.vl-frete-ferro-eadi           
                   tt-mov.vl-frete-rodo-un              
                   tt-mov.vl-icms                       
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
                   tt-mov.vl-coord-descarga                
                   tt-mov.vl-gestao-processo            
                   tt-mov.vl-compl-capatazia            
                   tt-mov.vl-ipi-filha                  
                   tt-mov.vl-cofins-filha               
                   tt-mov.vl-pis-filha                  
                   tt-mov.vl-icms-filha                 
                   tt-mov.vl-multa-di-icms              
                   tt-mov.vl-ii-filha                   
                   tt-mov.vl-imposto-filha              
                   tt-mov.vl-total-despesas         
                   tt-mov.vl-ipi-a                      
                   tt-mov.vl-icms-a                     
                   tt-mov.vl-frete-rodoviario-a         
                   tt-mov.st-nota
                   tt-mov.status-proc.
        End.


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
/*    Each emitente No-lock                                           */
/*   Where emitente.cod-emitente         = ordem-compra.cod-emitente, */
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

    If Not Available param-imp Then Next.

    Find First estabelec Where estabelec.cod-estabel = embarque-imp.cod-estabel No-lock No-error.
    If Not Avail estabelec Then Next.

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
/*          And item-doc-est.numero-ordem   = ordem-compra.numero-ordem */
         And item-doc-est.it-codigo      = Item.it-codigo:

        Find First tt-mov
             Where tt-mov.serie-docto  = item-doc-est.serie-docto 
               And tt-mov.nro-docto    = item-doc-est.nro-docto   
               And tt-mov.cod-emitente = item-doc-est.cod-emitente
               And tt-mov.nat-operacao = item-doc-est.nat-operacao
/*                And tt-mov.sequencia    = item-doc-est.sequencia */
            No-error.

        If  Available tt-mov Then Next.

        Find First emitente Where emitente.cod-emitente = item-doc-est.cod-emitente No-lock No-error.

        If estabelec.nome = "MINERACAO MARACA IND. E COMERCIO S.A." Then
            Assign c-nome-estab = "MMIC".
        Else
            If estabelec.nome = "JACOBINA MINERACAO E COMERCIO LTDA" Then
                Assign c-nome-estab = "JMC".

        Create tt-mov.

        Assign tt-mov.serie-docto                  = item-doc-est.serie-docto     
               tt-mov.nro-docto                    = item-doc-est.nro-docto       
               tt-mov.cod-emitente                 = item-doc-est.cod-emitente    
               tt-mov.nat-operacao                 = item-doc-est.nat-operacao    
/*                tt-mov.sequencia                    = item-doc-est.sequencia */

               tt-mov.cod-estabel                  = embarque-imp.cod-estabel
               tt-mov.nome-estab                   = c-nome-estab
               tt-mov.nr-pedido                    = ordem-compra.num-pedido
               tt-mov.embarque                     = embarque-imp.embarque
               tt-mov.cod-emitente                 = If Avail emitente  Then emitente.cod-emitente Else item-doc-est.cod-emitente
               tt-mov.nome-emit                    = If Avail emitente  Then emitente.nome-emit    Else ""
               tt-mov.it-codigo                    = Item.it-codigo
               tt-mov.desc-item                    = Item.desc-item
               tt-mov.nome-pais                    = If Avail emitente  Then emitente.pais         Else ""
               tt-mov.cod-pais                     = fnCodPais()
               tt-mov.cod-incoterm                 = embarque-imp.cod-incoterm
               tt-mov.data-pedido                  = ordem-compra.data-pedido
               tt-mov.di                           = fnNumDI()          
               tt-mov.dt-di                        = fnDataDI()
               tt-mov.via-transp                   = fnViaTransporte()  
               tt-mov.tx-di                        = fnTaxaDI()
               tt-mov.nr-drawback                  = fnDrawback()
               tt-mov.vl-mercadoria                = /*Round(cotacao-item.preco-unit * item-doc-est.quantidade,2) */ docum-est.valor-mercad
               tt-mov.qt-peso                      = /*item-doc-est.peso-liquido*/ docum-est.tot-peso
               tt-mov.tp-docto                     = fnTipoNota()
               tt-mov.qt-merc                      = /* item-doc-est.quantidade */ If tt-mov.tp-docto = "Nota Mãe" Then ordem-compra.qt-solic Else 0 /* Só deve mostrar quantidade para a Nota Mãe */
               tt-mov.un                           = /* item-doc-est.un         */ cotacao-item.un
               tt-mov.nro-docto                    = docum-est.nro-docto
               tt-mov.nat-operacao                 = docum-est.nat-operacao
               tt-mov.dt-solicitacao-nf            = fnDataPedidoNota()
               tt-mov.dt-emis                      = docum-est.dt-emis
               tt-mov.dt-origem                    = fnEmbarqueOrigem()
               tt-mov.dt-destino                   = fnDesembarqueDestino()
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
               tt-mov.vl-fumigacao                 = fnValDespesa(11)
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
               tt-mov.vl-frete-rodo-santos         = fnValDespesa(27)
               tt-mov.vl-compl-armazem-porto       = fnValDespesa(28)
               tt-mov.vl-servico-entrega           = fnValDespesa(29)
               tt-mov.vl-frete-ferro-santos        = fnValDespesa(30)
               tt-mov.vl-lavagem-container         = fnValDespesa(31)
               tt-mov.vl-tecnico-nf                = fnValDespesa(32)
               tt-mov.vl-retificacao-ii            = fnValDespesa(33)
               tt-mov.vl-outras-despesas           = fnValDespesa(34)
               tt-mov.vl-outras-despesas-imposto   = fnValDespesa(35)
               tt-mov.vl-compl-frete-internacional = fnValDespesa(36)
               tt-mov.vl-tx-portuaria-origem       = fnValDespesa(37)
               tt-mov.vl-inspec-carga-destino      = fnValDespesa(38)
               tt-mov.vl-inspec-carga-embarque     = fnValDespesa(39)
               tt-mov.vl-thc                       = fnValDespesa(40)
               tt-mov.vl-coord-descarga            = fnValDespesa(41)
               tt-mov.vl-gestao-processo           = fnValDespesa(42)
               tt-mov.vl-compl-capatazia           = fnValDespesa(43)
               tt-mov.vl-ipi-filha                 = fnValDespesa(44)
               tt-mov.vl-cofins-filha              = fnValDespesa(45)
               tt-mov.vl-pis-filha                 = fnValDespesa(46)
               tt-mov.vl-icms-filha                = fnValDespesa(47)
               tt-mov.vl-multa-di-icms             = fnValDespesa(48)
               tt-mov.vl-ii-filha                  = fnValDespesa(49)
               tt-mov.vl-imposto-filha             = fnValDespesa(50)
               tt-mov.vl-ipi-a                     = docum-est.ipi-deb-cre
               tt-mov.vl-icms-a                    = docum-est.icm-deb-cre
/*                tt-mov.vl-frete-rodoviario-a        = fnValDespesa(00) */ /* Somente para Notas Complementares */
               tt-mov.st-nota                      = fnSitNota()
               tt-mov.dt-recebimento               = docum-est.dt-trans
               tt-mov.status-proc                  = {cxinc/i01cx220.i 04 embarque-imp.situacao}
               tt-mov.vl-desp-nf                   = docum-est.despesa-nota
               tt-mov.vl-total-despesas            = tt-mov.vl-ii                        +
                                                     tt-mov.vl-frete-inter               +
                                                     tt-mov.vl-seguro-inter              +
                                                     tt-mov.vl-ipi                       +
                                                     tt-mov.vl-cofins                    +
                                                     tt-mov.vl-pis                       +
                                                     tt-mov.vl-capatazia                 +
                                                     tt-mov.vl-siscomex                  +
                                                     tt-mov.vl-sda-antecipada            +
                                                     tt-mov.vl-AFRMM                     +
                                                     tt-mov.vl-fumigacao                 +
                                                     tt-mov.vl-armazenagem               +
                                                     tt-mov.vl-liber-bl                  +
                                                     tt-mov.vl-correios                  +
                                                     tt-mov.vl-remoc-dta                 +
                                                     tt-mov.vl-multa-di                  +
                                                     tt-mov.vl-serv-despacho             +
                                                     tt-mov.vl-frete-rod-eadi            +
                                                     tt-mov.vl-frete-ferro-eadi          +
                                                     tt-mov.vl-frete-rodo-un             +
                                                     tt-mov.vl-icms                      +
                                                     tt-mov.vl-demurrage                 +
                                                     tt-mov.vl-sda                       +
                                                     tt-mov.vl-armazem-porto             +
                                                     tt-mov.vl-aluguel-container         +
                                                     tt-mov.vl-frete-rodo-santos         +
                                                     tt-mov.vl-compl-armazem-porto       +
                                                     tt-mov.vl-servico-entrega           +
                                                     tt-mov.vl-frete-ferro-santos        +
                                                     tt-mov.vl-lavagem-container         +
                                                     tt-mov.vl-tecnico-nf                +
                                                     tt-mov.vl-retificacao-ii            +
                                                     tt-mov.vl-outras-despesas           +
                                                     tt-mov.vl-outras-despesas-imposto   +
                                                     tt-mov.vl-compl-frete-internacional +
                                                     tt-mov.vl-tx-portuaria-origem       +
                                                     tt-mov.vl-inspec-carga-destino      +
                                                     tt-mov.vl-inspec-carga-embarque     +
                                                     tt-mov.vl-thc                       +
                                                     tt-mov.vl-coord-descarga            +
                                                     tt-mov.vl-gestao-processo           +
                                                     tt-mov.vl-compl-capatazia           +
                                                     tt-mov.vl-ipi-filha                 +
                                                     tt-mov.vl-cofins-filha              +
                                                     tt-mov.vl-pis-filha                 +
                                                     tt-mov.vl-icms-filha                +
                                                     tt-mov.vl-multa-di-icms             +
                                                     tt-mov.vl-ii-filha                  +
                                                     tt-mov.vl-imposto-filha.

/*         For Each item-doc-est-cex No-lock                                                              */
/*            Where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto                              */
/*              And item-doc-est-cex.nro-docto    = item-doc-est.nro-docto                                */
/*              And item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente                             */
/*              And item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao                             */
/*              And item-doc-est-cex.sequencia    = item-doc-est.sequencia                                */
/*              /*And item-doc-est-cex.cod-desp     = i-cod-desp-ii */ :                                  */
/*                                                                                                        */
/*             If  item-doc-est-cex.cod-desp = i-cod-desp-ii Then Next. /*II*/                            */
/*             If  item-doc-est-cex.cod-desp = 3             Then Next. /*Seguro Internacional*/          */
/*                                                                                                        */
/*             Assign tt-mov.vl-desp-nf = tt-mov.vl-desp-nf + (item-doc-est-cex.val-desp / tt-mov.tx-di). */
/*         End.                                                                                           */

         Run trataNotasComplementares.

    End. /* For Each docum-est No-lock */

End. /* For Each embarque-imp No-lock */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-trataNotasComplementares) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trataNotasComplementares Procedure 
PROCEDURE trataNotasComplementares :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

        /*impostos das nfs complementares*/
        For Each rat-docum NO-LOCK
           Where rat-docum.nf-serie       = docum-est.serie-docto
             And rat-docum.nf-nro         = docum-est.nro-docto
             And rat-docum.nf-emitente    = docum-est.cod-emitente
             And rat-docum.nf-nat-oper    = docum-est.nat-operacao,
           First b-docum-est NO-LOCK
           Where b-docum-est.serie-docto  = rat-docum.serie-docto 
             And b-docum-est.nro-docto    = rat-docum.nro-docto   
             And b-docum-est.cod-emitente = rat-docum.cod-emitente
             And b-docum-est.nat-operacao = rat-docum.nat-operacao:

            For First b-item-doc-est No-lock
               Where b-item-doc-est.serie-docto  = b-docum-est.serie-docto
                 And b-item-doc-est.nro-docto    = b-docum-est.nro-docto
                 And b-item-doc-est.cod-emitente = b-docum-est.cod-emitente
                 And b-item-doc-est.nat-operacao = b-docum-est.nat-operacao
                 And b-item-doc-est.it-codigo    = item.it-codigo:

                Find First tt-mov
                     Where tt-mov.serie-docto  = b-item-doc-est.serie-docto 
                       And tt-mov.nro-docto    = b-item-doc-est.nro-docto   
                       And tt-mov.cod-emitente = b-item-doc-est.cod-emitente
                       And tt-mov.nat-operacao = b-item-doc-est.nat-operacao
                       And b-item-doc-est.it-codigo    = item.it-codigo
/*                        And tt-mov.sequencia    = b-item-doc-est.sequencia */
                    No-error.

                If  Available tt-mov Then Next.

                Find First emitente Where emitente.cod-emitente = b-item-doc-est.cod-emitente No-lock No-error.

                Create tt-mov.

                Assign tt-mov.serie-docto                  = b-item-doc-est.serie-docto     
                       tt-mov.nro-docto                    = b-item-doc-est.nro-docto       
                       tt-mov.cod-emitente                 = b-item-doc-est.cod-emitente    
                       tt-mov.nat-operacao                 = b-item-doc-est.nat-operacao    
/*                        tt-mov.sequencia                    = b-docum-est.sequencia */

                       tt-mov.cod-estabel                  = embarque-imp.cod-estabel
                       tt-mov.nome-estab                   = c-nome-estab
                       tt-mov.nr-pedido                    = ordem-compra.num-pedido
                       tt-mov.embarque                     = embarque-imp.embarque
                       tt-mov.cod-emitente                 = If Avail emitente  Then emitente.cod-emitente Else b-item-doc-est.cod-emitente
                       tt-mov.nome-emit                    = If Avail emitente  Then emitente.nome-emit    Else ""
                       tt-mov.it-codigo                    = Item.it-codigo
                       tt-mov.desc-item                    = Item.desc-item
                       tt-mov.nome-pais                    = If Avail emitente  Then emitente.pais         Else ""
                       tt-mov.cod-pais                     = fnCodPais()
                       tt-mov.cod-incoterm                 = embarque-imp.cod-incoterm
                       tt-mov.data-pedido                  = ordem-compra.data-pedido
                       tt-mov.di                           = fnNumDI()          
                       tt-mov.dt-di                        = fnDataDI()
                       tt-mov.via-transp                   = fnViaTransporte()  
                       tt-mov.tx-di                        = fnTaxaDI()
                       tt-mov.nr-drawback                  = fnDrawback()
                       tt-mov.vl-mercadoria                = /*Round(b-item-doc-est.preco-unit[1] * b-item-doc-est.quantidade,2) */ b-docum-est.valor-mercad
                       tt-mov.qt-peso                      = /*b-item-doc-est.peso-liquido*/ b-docum-est.tot-peso
                       tt-mov.qt-merc                      = /* b-item-doc-est.quantidade */ /* ordem-compra.qt-solic */ 0 /* Em branco para Nota Complementar */
                       tt-mov.un                           = /* b-item-doc-est.un */ /* cotacao-item.un */ "" /* Em branco para Nota Complementar */
                       tt-mov.nro-docto                    = b-docum-est.nro-docto
                       tt-mov.nat-operacao                 = /*docum-est.nat-operacao*/ rat-docum.nat-oper
                       tt-mov.tp-docto                     = "Nota Complementar"
                       tt-mov.dt-solicitacao-nf            = fnDataPedidoNotaCompl()
                       tt-mov.dt-emis                      = b-docum-est.dt-emis
                       tt-mov.dt-origem                    = fnEmbarqueOrigem()
                       tt-mov.dt-destino                   = fnDesembarqueDestino()
                       tt-mov.vl-ii                        = fnvalDespesaComplementar(01)
                       tt-mov.vl-frete-inter               = fnvalDespesaComplementar(02) 
                       tt-mov.vl-seguro-inter              = fnvalDespesaComplementar(03)
                       tt-mov.vl-ipi                       = fnvalDespesaComplementar(04) 
                       tt-mov.vl-cofins                    = fnvalDespesaComplementar(05) 
                       tt-mov.vl-pis                       = fnvalDespesaComplementar(06) 
                       tt-mov.vl-capatazia                 = fnvalDespesaComplementar(07) 
                       tt-mov.vl-siscomex                  = fnvalDespesaComplementar(08)
                       tt-mov.vl-sda-antecipada            = fnvalDespesaComplementar(09)
                       tt-mov.vl-AFRMM                     = fnvalDespesaComplementar(10)
                       tt-mov.vl-fumigacao                 = fnvalDespesaComplementar(11)
                       tt-mov.vl-armazenagem               = fnvalDespesaComplementar(12)
                       tt-mov.vl-liber-bl                  = fnvalDespesaComplementar(13)
                       tt-mov.vl-correios                  = fnvalDespesaComplementar(14)
                       tt-mov.vl-remoc-dta                 = fnvalDespesaComplementar(15)
                       tt-mov.vl-multa-di                  = fnvalDespesaComplementar(16)
                       tt-mov.vl-serv-despacho             = fnvalDespesaComplementar(17)
                       tt-mov.vl-frete-rod-eadi            = fnvalDespesaComplementar(18)
                       tt-mov.vl-frete-ferro-eadi          = fnvalDespesaComplementar(19)
                       tt-mov.vl-frete-rodo-un             = fnvalDespesaComplementar(20)
                       tt-mov.vl-icms                      = fnvalDespesaComplementar(21)
                       tt-mov.vl-demurrage                 = fnvalDespesaComplementar(23)
                       tt-mov.vl-sda                       = fnvalDespesaComplementar(24)
                       tt-mov.vl-armazem-porto             = fnvalDespesaComplementar(25)
                       tt-mov.vl-aluguel-container         = fnvalDespesaComplementar(26)
                       tt-mov.vl-frete-rodo-santos         = fnvalDespesaComplementar(27)
                       tt-mov.vl-compl-armazem-porto       = fnvalDespesaComplementar(28)
                       tt-mov.vl-servico-entrega           = fnvalDespesaComplementar(29)
                       tt-mov.vl-frete-ferro-santos        = fnvalDespesaComplementar(30)
                       tt-mov.vl-lavagem-container         = fnvalDespesaComplementar(31)
                       tt-mov.vl-tecnico-nf                = fnvalDespesaComplementar(32)
                       tt-mov.vl-retificacao-ii            = fnvalDespesaComplementar(33)
                       tt-mov.vl-outras-despesas           = fnvalDespesaComplementar(34)
                       tt-mov.vl-outras-despesas-imposto   = fnvalDespesaComplementar(35)
                       tt-mov.vl-compl-frete-internacional = fnvalDespesaComplementar(36)
                       tt-mov.vl-tx-portuaria-origem       = fnvalDespesaComplementar(37)
                       tt-mov.vl-inspec-carga-destino      = fnvalDespesaComplementar(38)
                       tt-mov.vl-inspec-carga-embarque     = fnvalDespesaComplementar(39)
                       tt-mov.vl-thc                       = fnvalDespesaComplementar(40)
                       tt-mov.vl-coord-descarga            = fnvalDespesaComplementar(41)
                       tt-mov.vl-gestao-processo           = fnvalDespesaComplementar(42)
                       tt-mov.vl-compl-capatazia           = fnvalDespesaComplementar(43)
                       tt-mov.vl-ipi-filha                 = fnvalDespesaComplementar(44)
                       tt-mov.vl-cofins-filha              = fnvalDespesaComplementar(45)
                       tt-mov.vl-pis-filha                 = fnvalDespesaComplementar(46)
                       tt-mov.vl-icms-filha                = fnvalDespesaComplementar(47)
                       tt-mov.vl-multa-di-icms             = fnvalDespesaComplementar(48)
                       tt-mov.vl-ii-filha                  = fnvalDespesaComplementar(49)
                       tt-mov.vl-imposto-filha             = fnvalDespesaComplementar(50)
                       tt-mov.vl-ipi-a                     = b-docum-est.ipi-deb-cre
                       tt-mov.vl-icms-a                    = b-docum-est.icm-deb-cre
                       tt-mov.st-nota                      = fnSitNota()
                       tt-mov.dt-recebimento               = b-docum-est.dt-trans
                       tt-mov.status-proc                  = {cxinc/i01cx220.i 04 embarque-imp.situacao}
                       tt-mov.vl-desp-nf                   = b-docum-est.despesa-nota
                       tt-mov.vl-total-despesas            = tt-mov.vl-ii                        +
                                                             tt-mov.vl-frete-inter               +
                                                             tt-mov.vl-seguro-inter              +
                                                             tt-mov.vl-ipi                       +
                                                             tt-mov.vl-cofins                    +
                                                             tt-mov.vl-pis                       +
                                                             tt-mov.vl-capatazia                 +
                                                             tt-mov.vl-siscomex                  +
                                                             tt-mov.vl-sda-antecipada            +
                                                             tt-mov.vl-AFRMM                     +
                                                             tt-mov.vl-fumigacao                 +
                                                             tt-mov.vl-armazenagem               +
                                                             tt-mov.vl-liber-bl                  +
                                                             tt-mov.vl-correios                  +
                                                             tt-mov.vl-remoc-dta                 +
                                                             tt-mov.vl-multa-di                  +
                                                             tt-mov.vl-serv-despacho             +
                                                             tt-mov.vl-frete-rod-eadi            +
                                                             tt-mov.vl-frete-ferro-eadi          +
                                                             tt-mov.vl-frete-rodo-un             +
                                                             tt-mov.vl-icms                      +
                                                             tt-mov.vl-demurrage                 +
                                                             tt-mov.vl-sda                       +
                                                             tt-mov.vl-armazem-porto             +
                                                             tt-mov.vl-aluguel-container         +
                                                             tt-mov.vl-frete-rodo-santos         +
                                                             tt-mov.vl-compl-armazem-porto       +
                                                             tt-mov.vl-servico-entrega           +
                                                             tt-mov.vl-frete-ferro-santos        +
                                                             tt-mov.vl-lavagem-container         +
                                                             tt-mov.vl-tecnico-nf                +
                                                             tt-mov.vl-retificacao-ii            +
                                                             tt-mov.vl-outras-despesas           +
                                                             tt-mov.vl-outras-despesas-imposto   +
                                                             tt-mov.vl-compl-frete-internacional +
                                                             tt-mov.vl-tx-portuaria-origem       +
                                                             tt-mov.vl-inspec-carga-destino      +
                                                             tt-mov.vl-inspec-carga-embarque     +
                                                             tt-mov.vl-thc                       +
                                                             tt-mov.vl-coord-descarga            +
                                                             tt-mov.vl-gestao-processo           +
                                                             tt-mov.vl-compl-capatazia           +
                                                             tt-mov.vl-ipi-filha                 +
                                                             tt-mov.vl-cofins-filha              +
                                                             tt-mov.vl-pis-filha                 +
                                                             tt-mov.vl-icms-filha                +
                                                             tt-mov.vl-multa-di-icms             +
                                                             tt-mov.vl-ii-filha                  +
                                                             tt-mov.vl-imposto-filha.

                /* Se for uma nota complementar com natureza de operação 2352LK, indica que é uma nota de frete e
                   não terá nenhuma despesa (aba Doc Imp do RE0701 estará em branco) */
                If tt-mov.nat-operacao = "2352LK" Then
                    Assign tt-mov.vl-frete-rodoviario-a = b-docum-est.tot-valor /* Valor Total da aba Totais II do RE0701 */
                           tt-mov.dt-solicitacao-nf     = "".

/*                 For Each item-doc-est-cex No-lock                                                              */
/*                    Where item-doc-est-cex.serie-docto  = b-item-doc-est.serie-docto                            */
/*                      And item-doc-est-cex.nro-docto    = b-item-doc-est.nro-docto                              */
/*                      And item-doc-est-cex.cod-emitente = b-item-doc-est.cod-emitente                           */
/*                      And item-doc-est-cex.nat-operacao = b-item-doc-est.nat-operacao                           */
/*                      And item-doc-est-cex.sequencia    = b-item-doc-est.sequencia                              */
/*                      /*And item-doc-est-cex.cod-desp     = i-cod-desp-ii */ :                                  */
/*                                                                                                                */
/*                     If  item-doc-est-cex.cod-desp = i-cod-desp-ii Then Next. /*II*/                            */
/*                     If  item-doc-est-cex.cod-desp = 3             Then Next. /*Seguro Internacional*/          */
/*                                                                                                                */
/*                     Assign tt-mov.vl-desp-nf = tt-mov.vl-desp-nf + (item-doc-est-cex.val-desp / tt-mov.tx-di). */
/*                 End.                                                                                           */

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
        Where pais.nome-pais = tt-mov.nome-pais:
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

&IF DEFINED(EXCLUDE-fnDataPedidoNota) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDataPedidoNota Procedure 
FUNCTION fnDataPedidoNota RETURNS Character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    If c-nome-estab = "MMIC" Then Do:
        For Each historico-embarque No-lock
           Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
             And historico-embarque.embarque      = embarque-imp.embarque
             And historico-embarque.cod-pto-contr = 7:
            Return String(historico-embarque.dt-efetiva,"99/99/9999").
        End.
    End.
    Else
        If c-nome-estab = "JMC" Then Do:
            For Each historico-embarque No-lock
               Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
                 And historico-embarque.embarque      = embarque-imp.embarque
                 And historico-embarque.cod-pto-contr = 5:
                Return String(historico-embarque.dt-efetiva,"99/99/9999").
            End.
        End.

    Return "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDataPedidoNotaCompl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDataPedidoNotaCompl Procedure 
FUNCTION fnDataPedidoNotaCompl RETURNS Character
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    If c-nome-estab = "MMIC" Then Do:
        For Each historico-embarque No-lock
           Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
             And historico-embarque.embarque      = embarque-imp.embarque
             And historico-embarque.cod-pto-contr = 8:
            Return String(historico-embarque.dt-efetiva,"99/99/9999").
        End.
    End.
    Else
        If c-nome-estab = "JMC" Then Do:
            For Each historico-embarque No-lock
               Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
                 And historico-embarque.embarque      = embarque-imp.embarque
                 And historico-embarque.cod-pto-contr = 6:
                Return String(historico-embarque.dt-efetiva,"99/99/9999").
            End.
        End.


    Return "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnDesembarqueDestino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDesembarqueDestino Procedure 
FUNCTION fnDesembarqueDestino RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    
    For Each historico-embarque No-lock
       Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
         And historico-embarque.embarque      = embarque-imp.embarque
         And historico-embarque.cod-pto-contr = 3:
        Return historico-embarque.dt-efetiva.
    End.

    Return ?.   /* Function return value. */


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

&IF DEFINED(EXCLUDE-fnEmbarqueOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnEmbarqueOrigem Procedure 
FUNCTION fnEmbarqueOrigem RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    For Each historico-embarque No-lock
       Where historico-embarque.cod-estabel   = embarque-imp.cod-estabel
         And historico-embarque.embarque      = embarque-imp.embarque
         And historico-embarque.cod-pto-contr = 2:
        Return historico-embarque.dt-efetiva.
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
    /* 1 Não Confirmada */
    /* 2 Confirmada     */
    /* 3 Cotada         */
    /* 4 Eliminada      */
    /* 5 Em Cotação     */
    /* 6 Recebida       */

    Case ordem-compra.situacao:
        When 1 Then Return "Não Confirmada". 
        When 2 Then Return "Confirmada    ". 
        When 3 Then Return "Cotada        ". 
        When 4 Then Return "Eliminada     ". 
        When 5 Then Return "Em Cotação    ". 
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

/*     Repeat:                                               */
/*         dataDI = dataDI - 1.                              */
/*                                                           */
/*         If  Weekday(dataDI) <> 1 And Weekday(dataDI) <> 7 */
/*         Then cont = cont + 1.                             */
/*                                                           */
/*         If  cont = 2 Then Leave.                          */
/*                                                           */
/*     End.                                                  */

    Repeat:
        dataDI = dataDI - 1.

        If  Weekday(dataDI) <> 1 And Weekday(dataDI) <> 7 Then Do:
            /* Verifica se é dia útil */
            Find First dia_calend_glob Where dia_calend_glob.cod_calend   = "Fiscal"
                                       And   dia_calend_glob.dat_calend   = dataDI
                                       And   dia_calend_glob.log_dia_util = Yes No-lock No-error.
            If Avail dia_calend_glob Then
                Assign cont = cont + 1.
        End.

        If cont = 2 Then Leave.

    End.

    Find First cotacao No-lock
         Where cotacao.mo-codigo    = ordem-compra.mo-codigo
           And cotacao.ano-periodo  = String(Year (dataDi),"9999")
                                    + String(Month(dataDi),"99")
        No-error.


    If  cotacao.cotacao[Day(dataDI)] = 0 
    Then taxa = 1.
    Else taxa = cotacao.cotacao[Day(dataDI)].

    Find First ems2cadme.moeda Where moeda.mo-codigo = ordem-compra.mo-codigo No-lock No-error.

    Assign tt-mov.dt-cotacao = dataDI
           tt-mov.desc-moeda = If Avail moeda Then moeda.descricao Else "" /*ordem-compra.mo-codigo*/.


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

    If docum-est.idi-nf-simples-remes = 1 Then
        Return "Nota Mãe".
    Else Do:
        If docum-est.idi-nf-simples-remes = 2 Then
            Return "Nota Filha".
        Else
            Return "Nota Normal".
    End.


/*     If  item-doc-est.numero-ordem                  = 0                                   */
/*     And (Substring(embarque-imp.char-1,21,12)      = ""                                  */
/*     Or  Trim(Substring(embarque-imp.char-1,21,12)) = "") Then                            */
/*         Return "Nota Mãe".                                                               */
/*     Else If  embarque-imp.embarque = Substring(embarque-imp.char-1,21,12)                */
/*          Or  embarque-imp.embarque = Trim(Substring(embarque-imp.char-1,21,12))          */
/*          Then Return "Nota Mãe".                                                         */
/*          Else Do:                                                                        */
/*              Find First b-tt-mov Where b-tt-mov.nr-pedido = tt-mov.nr-pedido             */
/*                                  And   b-tt-mov.tp-docto  = "Nota Mãe" No-lock No-error. */
/*              If Avail b-tt-mov Then Do:                                                  */
/*                  If Substring(embarque-imp.char-1,21,12)       = ""                      */
/*                  Or Trim(Substring(embarque-imp.char-1,21,12)) = "" Then                 */
/*                     Return "Nota Filha".                                                 */
/*                  Else                                                                    */
/*                      Return "Nota Normal".                                               */
/*              End.                                                                        */
/*              Else                                                                        */
/*                  Return "Nota Normal".                                                   */
/*          End.                                                                            */

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

/*         For Each item-doc-est-cex No-lock                                            */
/*            Where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto            */
/*              And item-doc-est-cex.nro-docto    = item-doc-est.nro-docto              */
/*              And item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente           */
/*              And item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao           */
/*              And item-doc-est-cex.sequencia    = item-doc-est.sequencia              */
/*              And item-doc-est-cex.cod-desp     = p-cod-desp  :                       */
/*                                                                                      */
/*             Assign valorDespesa = round(valorDespesa + item-doc-est-cex.val-desp,2). */
/*         End.                                                                         */

        For Each docum-est-cex Of docum-est
            Where docum-est-cex.cod-desp = p-cod-desp:

            Assign valorDespesa = docum-est-cex.val-desp.
        End.


        Return valorDespesa.   
    



END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnvalDespesaComplementar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnvalDespesaComplementar Procedure 
FUNCTION fnvalDespesaComplementar RETURNS DECIMAL
  ( p-cod-desp As Int ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/


        Def Var valorDespesa As Dec No-undo.

        Assign valorDespesa = 0.

/*         For Each item-doc-est-cex No-lock                                             */
/*            Where item-doc-est-cex.serie-docto  = b-item-doc-est.serie-docto           */
/*              And item-doc-est-cex.nro-docto    = b-item-doc-est.nro-docto             */
/*              And item-doc-est-cex.cod-emitente = b-item-doc-est.cod-emitente          */
/*              And item-doc-est-cex.nat-operacao = b-item-doc-est.nat-operacao          */
/*              And item-doc-est-cex.sequencia    = b-item-doc-est.sequencia             */
/*              And item-doc-est-cex.cod-desp     = p-cod-desp  :                        */
/*                                                                                       */
/*             Assign valorDespesa = Round (valorDespesa + item-doc-est-cex.val-desp,2). */
/*         End.                                                                          */

        For Each docum-est-cex Of b-docum-est
            Where docum-est-cex.cod-desp = p-cod-desp:

            Assign valorDespesa = docum-est-cex.val-desp.
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


/*     Assign indtransp = Int(Substring(docto-estoq-nfe-imp.cod-livre-1,1,2)). */

/* Conforme RE0701h(page1)            */
/* 1  = Marítima,1,                    */
/* 2  = Fluvial,2,                     */
/* 3  = Lacustre,3,                    */
/* 4  = A‚rea,4,                       */
/* 5  = Postal,5,                      */
/* 6  = Ferrovi ria,6,                 */
/* 7  = Rodovi ria,7,                  */
/* 8  = Conduto / Rede Transmissão,8,  */
/* 9  = Meios Pr¢prios,9,              */
/* 10 = Entrada / Saída ficta,10,     */
/* 11 = Courier,11,                   */
/* 12 = Handcarry,12                  */


    Assign viaTransp = "Marítima,                  " 
                     + "Fluvial,                   " 
                     + "Lacustre,                  " 
                     + "Aérea,                     " 
                     + "Postal,                    " 
                     + "Ferroviária,               " 
                     + "Rodoviária,7,              "   
                     + "Conduto / Rede Transmissão," 
                     + "Meios Próprios,            " 
                     + "Entrada / Saída ficta,     "
                     + "Courier,                   "
                     + "Handcarry,                 ". 




    For  First docto-estoq-nfe-imp No-lock
         Where docto-estoq-nfe-imp.cod-estab          = docum-est.cod-estab
           And docto-estoq-nfe-imp.cod-ser-docto      = docum-est.serie-docto
           And docto-estoq-nfe-imp.cod-docto          = docum-est.nro-docto
           And docto-estoq-nfe-imp.cdn-emitente       = docum-est.cod-emitente:

        Assign indtransp = Int(Substring(docto-estoq-nfe-imp.cod-livre-1,1,2)).

        If  indTransp > 0 And indTransp < 13
        Then Return Entry(indTransp,viaTransp).
        Else Return "".

           
    End.

    Return "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


