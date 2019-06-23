/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i MV0613RP 2.00.00.005}  /*** 010005 ***/
{include/i_fnctrad.i}
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*****************************************************************************
**
**       PROGRAMA: MV0613RP.p
**
**       DATA....: Agosto de 2003
**
**       AUTOR...: Marcio Willwock - Manufatura - DATASUL S.A.
**
**       OBJETIVO: ImpressÆo Consulta MV0613 - Eventos Vencidos e a Vencer
**
*****************************************************************************/
                                             /** Defini‡Æo das temp-tables **/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field i-seq            as integer
    field c-cod            as character format "x(300)"
    field i-visao          as integer
    field iTipo            as integer
    field cTipo            as character
    field lParam           as logical.

{mvp/esmv0613.i3 tt-digita shared} /** Defini‡Æo da temp-table de dados **/

def temp-table tt-raw-digita
   field raw-digita as raw.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

/****************************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    /** Transferˆncia de parƒmetros para temp-table padrÆo **/
create tt-param.
raw-transfer raw-param to tt-param.
/****************************************************************************/
                                                /** Defini‡Æo de Vari veis **/
DEFINE VARIABLE h-acomp     AS HANDLE                   NO-UNDO.
DEFINE VARIABLE c-origem    AS character format "x(13)" NO-UNDO.
define buffer bfDigita  for tt-digita.
define buffer bfDigita2 for tt-digita.
{include/i-rpvar.i}
/****************************************************************************/
                                                     /** Cabe‡alho e Forms **/
{mvp/esmv0613.i1}
{include/pi-edit.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fnOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnOrigem Procedure 
FUNCTION fnOrigem RETURNS CHARACTER
  ( i-orig as integer )  FORWARD.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.63
         WIDTH              = 28.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

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

DISPLAY /*PAR¶METROS*/
        c-liter-par
        tt-param.iTipo
        tt-param.cTipo
        /*IMPRESSÇO*/
        c-liter-imp          
        c-destino            
        tt-param.arquivo
        tt-param.usuario     
    WITH FRAME f-param-definidos.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-imprimeDados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimeDados Procedure 
PROCEDURE imprimeDados :
/*------------------------------------------------------------------------------
  Purpose:     imprimeDados
  Parameters:  <none>
  Notes:       Busca n¡veis abaixo, coforme sele‡Æo do n¡vel
------------------------------------------------------------------------------*/
define input parameter cDimensao as character no-undo.

define variable cPai as character no-undo.

assign cPai = "".

for each  bfDigita use-index vencto
    where bfDigita.cod-dimens-pai = cDimensao no-lock:
    /** Verifica se existem n¡veis abaixo  **/
    if bfDigita.sequencia <= tt-param.i-visao then do:
        /** Chama recursivamente at‚ encontrar visÆo pai das tarefas **/
        run imprimeDados (bfDigita.cod-dimensao).
    end.
    else do:
        if cPai <> bfDigita.cod-dimens-pai then do:
            /** Imprime os cabe‡alhos **/
            run imprimeVisao (input rowid(bfDigita),         
                              input bfDigita.cod-dimens-pai).
            put skip(2).
            assign cPai = bfDigita.cod-dimens-pai.
        end.

        {mvp/esmv0613.i2 bfDigita.p-image}
        assign bfDigita.cod-oficial:label in frame f-dados  = trim(return-value)
               c-origem                                     = fnOrigem(bfDigita.i-origem).
        
        display bfDigita.cod-oficial
                bfDigita.desc-dimensao
                bfDigita.uso-real
                bfDigita.uso-padrao
                bfDigita.diferenca
                bfDigita.un
                bfDigita.dat-vencto
                bfDigita.dat-atualiz
                c-origem
            with frame f-dados.
        down with frame f-dados.
    end.
end.

/** Pula linhas para dividir relat¢rio apenas 
    se imprimiu tarefas **/
if cPai <> "":U then
    put skip(2).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-imprimeVisao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimeVisao Procedure 
PROCEDURE imprimeVisao :
/*------------------------------------------------------------------------------
  Purpose:     imprimeVisao
  Parameters:  entrada rRow    = Rowid da temp-table                
               entrada cCodPai = C¢digo da dimensÆo do registro pai 
  Notes:       Imprime os cabe‡alhos, buscando os n¡veis acima do selecionado
------------------------------------------------------------------------------*/
define input parameter rRow    as rowid     no-undo.
define input parameter cCodPai as character no-undo.

/** Verifica se existem visäes acima **/
if can-find(first bfDigita2
            where bfDigita2.cod-dimensao = cCodPai no-lock) then do:
    /** Se existir, chama recursivamente enquanto existir **/
    for first bfDigita2
        where bfDigita2.cod-dimensao = cCodPai no-lock:
        /** Chamada recursiva **/
        run imprimeVisao (input rowid(bfDigita2),
                          input bfDigita2.cod-dimens-pai).
    end.
end.
/** Imprime a visÆo, ignorando se for uma tarefa **/
for first bfDigita2
    where rowid(bfDigita2) = rRow 
    and   bfDigita2.p-image <> 30 no-lock:
    /** Busca label do c¢digo **/
    {mvp/esmv0613.i2 bfDigita2.p-image}
    assign tt-digita.cod-oficial:label in frame f-visoes = trim(return-value).
    /** Imprime visÆo **/
    display bfDigita2.cod-oficial   @ tt-digita.cod-oficial   
            bfDigita2.desc-dimensao @ tt-digita.desc-dimensao
        with frame f-visoes.
    down with frame f-visoes.
end.

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
assign c-programa = "MV/0613"
       c-versao   = "2.00"
       c-revisao  = "000"
       /** Define o destino do arquivo a ser gerado **/
.
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
find first param-global no-lock no-error.
if  available param-global then
    assign c-empresa = param-global.grupo.

/** Guarda valores para imprimir t¡tulos **/
{utp/ut-liter.i "Gerencidor da Manuten‡Æo Mecƒnica" *}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i "MANUTEN€ÇO MEC¶NICA" *}
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
/** Mostra frames de cabe‡alho e rodap‚ padräes da Datasul **/
view frame f-cabec.
view frame f-rodape. 

/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/

/** Imprime Somente visÆo selecionada **/
if tt-param.iTipo = 1 then do:
  /** Atrav‚s da visÆo selecionada, chama a impressÆo **/
  for first tt-digita
      where tt-digita.sequencia    = tt-param.i-seq
      and   tt-digita.cod-dimensao = tt-param.c-cod no-lock:
      /** Busca visäes a serem impressas **/
      run imprimeDados (input tt-digita.cod-dimensao).
  end.
end.
/** Imprime Completo **/
else do:
    /** Busca todas as visäes de primeiro n¡vel **/
    for each  tt-digita
        where tt-digita.sequencia = 1 no-lock:
        /** Busca visäes a serem impressas **/
        run imprimeDados (input tt-digita.cod-dimensao).
    end.
end.

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Mostra parƒmetros selecionados **/
if tt-param.lParam then
    run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnOrigem Procedure 
FUNCTION fnOrigem RETURNS CHARACTER
  ( i-orig as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnOrigem
    Notes:  Descri‡Æo da Origem da Tarefa
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

case i-orig:
    when 1 then do:
        {utp/ut-liter.i "Lubrifica‡äes"}
    end.
    when 2 then do:
        {utp/ut-liter.i "Eventos"}
    end.
    when 3 then do:
        {utp/ut-liter.i "Planos Manuten‡Æo"}
    end.
    when 4 then do:
        {utp/ut-liter.i "Calend rio Manuten‡Æo"}
    end.
    when 5 then do:
        {utp/ut-liter.i "Servi‡o Pneus"}
    end.
    when 6 then do:
        {utp/ut-liter.i "Ordens Manuten‡Æo"}
    end.
    when 7 then do:
        {utp/ut-liter.i "Planos Componentes"}
    end.
    when 8 then do:
        {utp/ut-liter.i "Componentes Vencidos"}
    end.
    when 9 then do:
        {utp/ut-liter.i "Durab Sub-Sistema"}
    end.
    when 10 then do:
        {utp/ut-liter.i "Garantia Sub-Sistema"}
    end.
end case.

assign cRetorno = trim(return-value).


  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

