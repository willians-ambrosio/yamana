&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ymof0122RP 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    FIELD arquivo          AS CHAR
    field data-exec        as date
    field hora-exec        as integer.
    
define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
     tt-param.arquivo    no-label     
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

/* find ems2cadme.empresa                           */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = "Carga do informa‡oes para o F100".
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


DEF VAR X-cod-empres              LIKE dwf-outros-docto-operac.cod-empres.              /*   Empresa           */
DEF VAR X-cod-estab               LIKE dwf-outros-docto-operac.cod-estab.               /*   Estabelec         */
DEF VAR X-dat-trans               AS CHAR FORMAT "x(10)" /* LIKE dwf-outros-docto-operac.dat-trans */.               /*   Data Transa»’o    */
DEF VAR X-num-seq-ident-reg       LIKE dwf-outros-docto-operac.num-seq-ident-reg.       /*   Seq Indent Reg    */
DEF VAR X-cod-indic-operac        LIKE dwf-outros-docto-operac.cod-indic-operac.        /*   Tipo Operac       */
DEF VAR X-cod-participan          LIKE dwf-outros-docto-operac.cod-participan.          /*   Participante      */
DEF VAR X-cod-item                LIKE dwf-outros-docto-operac.cod-item.                /*   Item              */
DEF VAR X-dat-operac              AS CHAR FORMAT "x(10)" /*LIKE dwf-outros-docto-operac.dat-operac */.              /*   Data Opera»’o     */
DEF VAR X-val-operac              AS CHAR FORMAT "x(20)" /* LIKE dwf-outros-docto-operac.val-operac */.              /*   Valor Opera»’o    */
DEF VAR X-cod-sit-tributar-pis    LIKE dwf-outros-docto-operac.cod-sit-tributar-pis.    /*   Sit Trib PIS      */
DEF VAR X-val-base-calc-pis       AS CHAR FORMAT "x(20)" /*LIKE dwf-outros-docto-operac.val-base-calc-pis */.       /*   Base PIS          */
DEF VAR X-val-aliq-pis            AS CHAR FORMAT "x(20)" /*LIKE dwf-outros-docto-operac.val-aliq-pis*/.            /*   % PIS             */
DEF VAR X-val-pis                 AS CHAR FORMAT "x(20)" /* LIKE dwf-outros-docto-operac.val-pis*/.                 /*   Vl PIS            */
DEF VAR X-cod-sit-tributar-cofins LIKE dwf-outros-docto-operac.cod-sit-tributar-cofins. /*   Sit Trib COFINS   */
DEF VAR X-val-base-calc-cofins    AS CHAR FORMAT "x(20)" /*LIKE dwf-outros-docto-operac.val-base-calc-cofins*/.    /*   Base Calc Cofins  */
DEF VAR X-val-aliq-cofins         AS CHAR FORMAT "x(20)" /*LIKE dwf-outros-docto-operac.val-aliq-cofins */.         /*   % COFINS          */
DEF VAR X-val-cofins              AS CHAR FORMAT "x(20)" /* LIKE dwf-outros-docto-operac.val-cofins*/.              /*   COFINS            */
DEF VAR X-cod-nat-base-calc-cr    LIKE dwf-outros-docto-operac.cod-nat-base-calc-cr.    /*   Natur Base Calc CR*/
DEF VAR X-ind-orig                LIKE dwf-outros-docto-operac.ind-orig.                /*   Org               */
DEF VAR X-cod-cta-ctbl            LIKE dwf-outros-docto-operac.cod-cta-ctbl.            /*   Conta Contÿbil    */
DEF VAR X-cod-ccusto              LIKE dwf-outros-docto-operac.cod-ccusto.              /*   Centro Custo      */
DEF VAR X-des-operac              LIKE dwf-outros-docto-operac.des-operac.              /*   Descri»’o         */

DEF STREAM str-rp.

{method/dbotterr.i}

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
   Type: Procedure Template
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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:
   {include/i-rpout.i  }
    view frame f-cabec.
    view frame f-rodape.
    run utp/ut-acomp.p persistent set h-acomp.  
    
    {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}
    
    run pi-inicializar in h-acomp (input "Processando":U). 



    RUN pi-processa.



    
    run pi-finalizar in h-acomp.
    {include/i-rpclo.i }
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-carrega-dados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados Procedure 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-registro AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-reg AS INTEGER     NO-UNDO.

FOR EACH dwf-outros-docto-operac BY dwf-outros-docto-operac.num-seq-ident-reg DESC:   
    ASSIGN i-reg = i-reg + 1.
    IF i-reg = 1 THEN DO:
        ASSIGN i-registro = dwf-outros-docto-operac.num-seq-ident-reg + 1.
    END.
END.                                                                                     
IF i-registro = 0 THEN i-registro = i-registro + 1.








input from  VALUE(tt-param.arq-entrada).
repeat:
   import delimiter ";"
       X-cod-empres                  
       X-cod-estab                  
       X-dat-trans                  
       X-cod-indic-operac           
       X-cod-participan             
       X-cod-item                   
       X-dat-operac                 
       X-val-operac                 
       X-cod-sit-tributar-pis       
       X-val-base-calc-pis          
       X-val-aliq-pis               
       X-val-pis                    
       X-cod-sit-tributar-cofins
       X-val-base-calc-cofins       
       X-val-aliq-cofins            
       X-val-cofins                 
       X-cod-nat-base-calc-cr       
       X-ind-orig                   
       X-cod-cta-ctbl               
       X-cod-ccusto                 
       X-des-operac.  

       IF X-cod-empres = "Empresa" THEN  NEXT.
       IF X-cod-empres = ""        THEN  NEXT.



       find dwf-outros-docto-operac WHERE dwf-outros-docto-operac.cod-empres         = X-cod-empres 
                                      and dwf-outros-docto-operac.cod-estab          = X-cod-estab        
                                      and dwf-outros-docto-operac.dat-trans          = date(X-dat-trans)         
                                      AND dwf-outros-docto-operac.num-seq-ident-reg  = i-registro    no-lock no-error. 
       
       if avail dwf-outros-docto-operac THEN DO:
           i-registro = i-registro + 1.    
           next.
       END.
       ELSE  DO:

           IF X-cod-participan <> "" THEN DO:
               FIND emitente WHERE emitente.cod-emitente = INT(X-cod-participan) NO-LOCK  NO-ERROR.
               IF NOT AVAIL emitente THEN DO:
                   PUT UNFORMAT "Emitente => "  + X-cod-participan + "nÆo cadastrado!!!"  SKIP.
                   NEXT.
               END.
           END.


           
          
           
           create dwf-outros-docto-operac.
           assign dwf-outros-docto-operac.cod-empres              = X-cod-empres             
                  dwf-outros-docto-operac.cod-estab               = X-cod-estab              
                  dwf-outros-docto-operac.dat-trans               = date(X-dat-trans)              
                  dwf-outros-docto-operac.num-seq-ident-reg       = i-registro      
                  dwf-outros-docto-operac.cod-indic-operac        = X-cod-indic-operac       
                  dwf-outros-docto-operac.cod-participan          = X-cod-participan         
                  dwf-outros-docto-operac.cod-item                = X-cod-item               
                  dwf-outros-docto-operac.dat-operac              = date(X-dat-operac)           
                  dwf-outros-docto-operac.val-operac              = dec(X-val-operac)             
                  dwf-outros-docto-operac.cod-sit-tributar-pis    = X-cod-sit-tributar-pis   
                  dwf-outros-docto-operac.val-base-calc-pis       = dec(X-val-base-calc-pis)      
                  dwf-outros-docto-operac.val-aliq-pis            = dec(X-val-aliq-pis)           
                  dwf-outros-docto-operac.val-pis                 = dec(X-val-pis)                
                  dwf-outros-docto-operac.cod-sit-tributar-cofins = X-cod-sit-tributar-cofins
                  dwf-outros-docto-operac.val-base-calc-cofins    = dec(X-val-base-calc-cofins)   
                  dwf-outros-docto-operac.val-aliq-cofins         = dec(X-val-aliq-cofins)        
                  dwf-outros-docto-operac.val-cofins              = dec(X-val-cofins)             
                  dwf-outros-docto-operac.cod-nat-base-calc-cr    = X-cod-nat-base-calc-cr   
                  dwf-outros-docto-operac.ind-orig                = X-ind-orig               
                  dwf-outros-docto-operac.cod-cta-ctbl            = X-cod-cta-ctbl           
                  dwf-outros-docto-operac.cod-ccusto              = X-cod-ccusto             
                  dwf-outros-docto-operac.des-operac              = X-des-operac  .
       END.
       i-registro = i-registro + 1.       
END.
INPUT CLOSE.

PUT UNFORMAT "Importa‡Æo Finalizada!!!" SKIP.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-emitente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-emitente Procedure 
PROCEDURE pi-cria-emitente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* dwf-outros-docto-operac.cod-participan*/

    def VAR p-emitente  as char no-undo.
    /*def input param p-cdn-ender as int  no-undo.
    def input param p-endereco  as char no-undo.*/

    def var h-lfapi072 as handle no-undo.
    def var h-bofi073  as handle no-undo.
    def var c-rua      as char   no-undo.
    def var c-nro      as char   no-undo.
    def var c-comp     as char   no-undo.
    def var c-bairro   as char   no-undo.
    def var c-cidade   as char   no-undo.
    def var c-uf       as char   no-undo.
    def var c-pais     as char   no-undo.
    def var c-cep      as char   no-undo.

    if  not valid-handle(h-lfapi072) then do:
        run lfp/lfapi072.p persistent set h-lfapi072.
        run pi-inicializa-dbos in h-lfapi072.
    end.

    if  not valid-handle(h-bofi073) then do:
        run fibo/bofi073.p  persistent set h-bofi073.
        run openQueryStatic in h-bofi073(input "Default":U).
    end.

 DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

FOR EACH dwf-outros-docto-operac NO-LOCK:

    FIND FIRST emitente WHERE emitente.cod-emit = int(dwf-outros-docto-operac.cod-participan) NO-ERROR.
    IF AVAIL emitente THEN DO:
        find first dwf-emit no-lock
             where dwf-emit.cod-emitente  = STRING(emitente.cod-emit)
               and dwf-emit.dat-fim-valid = ? no-error.
        IF NOT AVAIL dwf-emit THEN DO:
    
            i-cont = i-cont + 1.
         
    
    
            ASSIGN p-emitente = string(dwf-outros-docto-operac.cod-participan).
    
             run pi-emitente in h-lfapi072(input p-emitente, output table RowErrors). /*** api emitente ***/
    
            for each RowErrors
               where RowErrors.ErrorType <> "INTERNAL":U:
                /*MESSAGE RowErrors.errorDescription VIEW-AS ALERT-BOX INFO BUTTONS OK.
                run piInsereErros(input 0, input RowErrors.errorDescription).*/
                PUT RowErrors.errorDescription SKIP.
            end. 
    
        END.
    END.
    ELSE DO:

        PUT UNFORMAT "Emitente n’o Encontrado - " dwf-outros-docto-operac.cod-participan SKIP.

    END.

END.



    if  valid-handle (h-bofi073) then do:
        delete procedure h-bofi073.
        assign h-bofi073 = ?.
    end.

    run pi-finaliza-dbos in h-lfapi072.
    if  valid-handle(h-lfapi072) then do:
        delete procedure h-lfapi072.
        assign h-lfapi072 = ?.
    end.





























END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa Procedure 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-carrega-dados.
RUN pi-cria-emitente.











END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

