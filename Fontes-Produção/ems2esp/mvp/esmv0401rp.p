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

{include/i-prgvrs.i ESMV0401RP 2.00.00.000}  /*** 010004 ***/
{include/i_fnctrad.i}
/********************************************************************************
**
**       PROGRAMA: esmv0401rp.p
**
**       DATA....: Dezembro de 2002
**
**       AUTOR...: DATASUL S.A.
**
**       OBJETIVO: 
**       Versao..: 2.00.00.000
**
********************************************************************************/

/** Definicao das temp-tables para recebimento dos parametros **/
{mvp/esmv0401.i}  

    def temp-table tt-raw-digita
    field raw-digita as raw.

/* Recebimento de parametros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Include padroes para variaveis de relatorio */
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
         HEIGHT             = 6.63
         WIDTH              = 32.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{mvp/esmv0401.i2}   /* pi-ordem       */
{mvp/esmv0401.i3}   /* pi-empresa     */
{mvp/esmv0401.i4}   /* pi-dt-abertura */
{mvp/esmv0401.i5}   /* pi-oficina     */



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

&IF DEFINED(EXCLUDE-displayParametros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayParametros Procedure 
PROCEDURE displayParametros :
/*------------------------------------------------------------------------------
  Purpose:     displayParametros
  Parameters:  <none>
  Notes:       Mostra os parƒmetros selecionados
------------------------------------------------------------------------------*/
if  tt-param.l-param then do:
    page.
    PUT SKIP (1).
    {utp/ut-liter.i PAR¶METROS * C}
    PUT unformatted skip(1) return-value format "x(30)" AT 23 skip.
    FORM l-motorizados      colon 33     
         l-n-motorizados    colon 33   
         l-ativos           colon 33   
         l-Inativos         colon 33   
         l-proprios         colon 33   
         l-terceiros        colon 33   
         l-combust          colon 33   
         l-caract           colon 33
         l-comptes          colon 33   
         l-hist             colon 33   
         l-eventos          colon 33   
         l-planos           colon 33   
         l-config           colon 33   
         l-param            colon 33   
         l-Insere           colon 33
        with stream-io   overlay side-labels row 014 frame f-imp-par.

    {utp/ut-liter.i Motorizados  * L}
    assign l-motorizados:label in frame f-imp-par = return-value.

    {utp/ut-liter.i NÆo_Motorizados  * L}
    assign  l-n-motorizados:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Ativos  * L}
    assign  l-ativos:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Inativos  * L}
    assign    l-Inativos:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Pr¢prios  * L}
    assign l-proprios:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Terceiros  * L}
    assign l-terceiros:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Combust¡veis  * L}
    assign l-combust:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Caracter¡stica  * L}
    assign l-caract:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Compartimentos/Filtros  * L}
    assign l-comptes:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Hist¢rico_Transferˆncia  * L}
    assign l-hist:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Eventos_Programados  * L}
    assign l-eventos:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Planos_Manuten‡Æo_Preventiva  * L}
    assign l-planos:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Configura‡Æo_dos_Eixos  * L}
    assign l-config:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Imprimir_P g_de_Parƒmetros  * L}
    assign l-param:label in frame f-imp-par = return-value.
    {utp/ut-liter.i Insere_Quebra_de_P gina  * L}
    assign l-Insere:label in frame f-imp-par = return-value.

    {utp/ut-liter.i Sim/NÆo}
    assign l-motorizados:FORMAT   in frame f-imp-par = return-value
           l-n-motorizados:FORMAT in frame f-imp-par = return-value
           l-ativos:FORMAT        in frame f-imp-par = return-value       
           l-Inativos:FORMAT      in frame f-imp-par = return-value     
           l-proprios:FORMAT      in frame f-imp-par = return-value     
           l-terceiros:FORMAT     in frame f-imp-par = return-value    
           l-combust:FORMAT       in frame f-imp-par = return-value      
           l-caract:FORMAT        in frame f-imp-par = return-value      
           l-comptes:FORMAT       in frame f-imp-par = return-value      
           l-hist:FORMAT          in frame f-imp-par = return-value         
           l-eventos:FORMAT       in frame f-imp-par = return-value      
           l-planos:FORMAT        in frame f-imp-par = return-value       
           l-config:FORMAT        in frame f-imp-par = return-value       
           l-param:FORMAT         in frame f-imp-par = return-value       
           l-Insere:FORMAT        in frame f-imp-par = return-value.       

    disp l-motorizados 
         l-n-motorizados 
         l-ativos       
         l-Inativos     
         l-proprios     
         l-terceiros    
         l-combust      
         l-caract
         l-comptes      
         l-hist         
         l-eventos      
         l-planos       
         l-config       
         l-param        
         l-Insere
        with frame f-imp-par.

    {utp/ut-liter.i SELE€ÇO * C}
    PUT  unformatted skip(1) return-value format "x(30)" AT 26 skip.
    disp i-ordem-ini          colon 33  "|< >|"  at 57 i-ordem-fim          no-label
         i-empresa-ini        colon 33  "|< >|"  at 57 i-empresa-fim        no-label
         c-equipamento-ini    colon 33  "|< >|"  at 57 c-equipamento-fim    no-label
         dt-abertura-ini      colon 33  "|< >|"  at 57  dt-abertura-fim     no-label
         dt-entrada-ini       colon 33  "|< >|"  at 57 dt-entrada-fim       no-label   
         i-tipo-manut-ini     colon 33  "|< >|"  at 57 i-tipo-manut-fim     no-label
         i-planejador-ini     colon 33  "|< >|"  at 57 i-planejador-fim     no-label   
         c-oficina-ini        colon 33  "|< >|"  at 57 c-oficina-fim        no-label
         dt-prev-termino-ini  colon 33  "|< >|"  at 57  dt-prev-termino-ini no-label
         dt-termino-ini       colon 33  "|< >|"  at 57 dt-termino-ini       no-label
         i-ano-fab-ini        COLON 33  "|< >|"  at 57 i-ano-fab-fim        no-label
    with stream-io side-labels overlay row 030 frame f-imp-sel.


   
  

    {utp/ut-liter.i CLASSIFICA€ÇO * R}
    PUT  unformatted skip(1) return-value format "x(30)" AT 21 skip.
    disp desc-classifica  AT 35 NO-LABEL
        with stream-io side-labels overlay row 030 frame f-imp-cla.

    {utp/ut-liter.i IMPRESSÇO * C}
    PUT  unformatted skip(1) return-value format "x(30)" AT 25 skip.
    /*{utp/ut-liter.i Destino: * L}
    put unformatted skip "    " return-value format "x(10)" AT 23 v-cod-destino-impres " - " tt-param.arquivo format "x(40)".*/
    {utp/ut-liter.i Execu‡Æo: * L}
    put  unformatted skip "    " return-value format "x(12)" AT 22 if i-num-ped-exec-rpw = 0 then " On-Line" else " Batch".
    {utp/ut-liter.i Usu rio: * L}
    put  unformatted skip "    " return-value format "x(10)" AT 23 tt-param.usuario.

END.

return "OK":U.

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

ASSIGN i-ordem-ini         =  tt-param.i-ordem-ini      
       i-ordem-fim         =  tt-param.i-ordem-fim      
       i-empresa-ini       =  tt-param.i-empresa-ini    
       i-empresa-fim       =  tt-param.i-empresa-fim    
       c-equipamento-ini   =  tt-param.c-equipamento-ini
       c-equipamento-fim   =  tt-param.c-equipamento-fim
       dt-abertura-ini     =  tt-param.dt-abertura-ini  
       dt-abertura-fim     =  tt-param.dt-abertura-fim  
       dt-entrada-ini      =  tt-param.dt-entrada-ini   
       dt-entrada-fim      =  tt-param.dt-entrada-fim   
       i-tipo-manut-ini    =  tt-param.i-tipo-manut-ini 
       i-tipo-manut-fim    =  tt-param.i-tipo-manut-fim 
       i-planejador-ini    =  tt-param.i-planejador-ini 
       i-planejador-fim    =  tt-param.i-planejador-fim 
       c-oficina-ini       =  tt-param.c-oficina-ini    
       c-oficina-fim       =  tt-param.c-oficina-fim    
       dt-prev-termino-ini =  tt-param.dt-termino-ini   
       dt-prev-termino-fim =  tt-param.dt-termino-fim   
       dt-termino-ini      =  tt-param.dt-termino-ini   
       dt-termino-fim      =  tt-param.dt-termino-fim   
       i-ano-fab-ini       =  tt-param.i-ano-fab-ini    
       i-ano-fab-fim       =  tt-param.i-ano-fab-fim.    
                                 
                                 


find first param-global no-lock no-error.

if available param-global THEN DO:
    FIND FIRST empresa WHERE ep-codigo = empresa-prin NO-LOCK NO-ERROR.
    if available empresa THEN
        ASSIGN c-empresa = empresa.razao-social.
END.

{utp/ut-liter.i Listagem_de_T‚cnicos_p/_Ordem_de_Manuten‡Æo * L}
assign c-programa     = "esmv0401"
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

CASE tt-param.classifica:
    WHEN 1 THEN DO:
        RUN pi-ordem IN THIS-PROCEDURE.
    END.
    WHEN 2 THEN DO:
        RUN pi-empresa IN THIS-PROCEDURE.
    END.
    WHEN 3 THEN DO:
       RUN pi-dt-abertura IN THIS-PROCEDURE.
    END.
    WHEN 4 THEN DO:
       RUN pi-oficina IN THIS-PROCEDURE.
    END.

END CASE.


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

