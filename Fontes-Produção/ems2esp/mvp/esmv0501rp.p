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
{include/i-prgvrs.i ESMV0501RP 1.00.00.000}  /*** 010004 ***/
{include/i_fnctrad.i}
/*****************************************************************************
**
**       PROGRAMA:  ESMV0501RP.p
**
**       DATA....: 22/11/2003
**
**       AUTOR...: Leonardo Correia Santos deOliveira - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Programa de RP para impressÆo de Fichas de Ordem de Mnauten‡Æo de Frotas
**
*****************************************************************************/
                                             /** Defini‡Æo das temp-tables **/
{mvp/esmv0501rp.i}

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita as raw.

DEFINE BUFFER bfTarefa FOR mmv-tar-ord-manut.
def buffer empresa for ems2cadme.empresa.
/****************************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    /** Transferˆncia de parƒmetros para temp-table padrÆo **/
create tt-param.
raw-transfer raw-param to tt-param.
/** Este c¢digo deve existir somente se relat¢rio contiver digita‡Æo **/
for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.
/****************************************************************************/
                                                /** Defini‡Æo de Vari veis **/
DEFINE VARIABLE h-acomp     AS HANDLE  NO-UNDO.
{include/i-rpvar.i}
/****************************************************************************/
                                                     /** Cabe‡alho e Forms **/

DEFINE VARIABLE i-cont-linhas AS INTEGER    NO-UNDO.
DEFINE VARIABLE l-aprovado  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE l-reprovado AS LOGICAL    NO-UNDO.

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
         HEIGHT             = 8.83
         WIDTH              = 29.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{mvp/esmv0501.i2}  /** piBuscaOrdem **/
/*{include/tt-edit.i} /** Procedure que cria a temp- do editor para ser impressa **/*/
{include/pi-edit.i}


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
/** Cria a p gina dos parƒmetros **/
page.

/******************************************************************
  Espa‡o reservado para buscar informa‡äes a serem impressas nos 
  parƒmetros (Opcional)
******************************************************************/

/** L¢gica opcional do programador **/

/******************************************************************
  Final do espa‡o reservado
******************************************************************/
IF l-param  THEN DO:
  PAGE.
DISPLAY /*SELE€ÇO*/ 
        c-liter-sel
        tt-param.c-ord-ini
        tt-param.c-ord-fim
        tt-param.c-oficina-ini
        tt-param.c-oficina-fim
        tt-param.c-data-ini
        tt-param.c-data-fim
        tt-param.c-empresa-ini
        tt-param.c-empresa-fim
        tt-param.c-eqpto-ini
        tt-param.c-eqpto-fim
        tt-param.c-componente-ini
        tt-param.c-componente-fim
        tt-param.tag-ini
        tt-param.tag-fim
        tt-param.cc-ini
        tt-param.cc-fim
        /*PAR¶METRO*/
        c-liter-par          
        tt-param.l-tarefas       

        /*IMPRESSÇO*/
        c-liter-imp          
        c-destino            
        tt-param.arquivo
        tt-param.usuario 
        tt-param.l-param
        tt-param.l-QuebraOM
       WITH FRAME f-param-definidos.

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
assign c-programa = "MV/0501"
       c-versao   = "2.00"
       c-revisao  = "000".
       /** Define o destino do arquivo a ser gerado **/

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 integer(tt-param.destino)}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN c-destino  = RETURN-VALUE.
&else
    ASSIGN c-destino  = {varinc/var00002.i 04 integer(tt-param.destino)}.
&endif

/** Busca os parƒmetros criados na interface gr fica **/
find first tt-param.

/** Busca empresa padrÆo **/
for first param-global fields(empresa-prin) no-lock:
    for FIRST empresa 
        WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
        ASSIGN c-empresa = empresa.razao-social
               cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
    end.
end.


/** Guarda valores para imprimir t¡tulos **/
{utp/ut-liter.i "Ficha da Ordem de Manuten‡Æo" *}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i "Manuten‡Æo Mecƒnica" *}
assign c-sistema = trim(return-value).

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
/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/

assign lTarefas        =  tt-param.l-tarefas       
       lMateriais      =  tt-param.l-materiais     
       lEpi            =  tt-param.l-epi           
       lFerramentas    =  tt-param.l-ferramentas   
       lFicha          =  tt-param.l-fichas        
       lCompartimento  =  tt-param.l-compartimentos
       lEventos        =  tt-param.l-eventos       
       lAnexos         =  tt-param.l-anexos        
       lUltima         =  tt-param.l-ultima        
       lObs            =  tt-param.l-obs           
       i-num-ficha     =  tt-param.i-num-ficha     
       cSusSist        =  tt-param.c-sub-sist 
       l-param         =  tt-param.l-param
       l-planovenc     =  tt-param.l-planovenc
       l-durabilidade  =  tt-param.l-durabilidade
       l-aprovado      =  tt-param.l-Aprovado
       l-reprovado     =  tt-param.l-Reprovado.
run piBuscaOrdem in this-procedure.

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Mostra parƒmetros selecionados **/
run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

