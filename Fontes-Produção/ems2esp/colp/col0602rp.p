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
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i COL0602RP 2.03.00.005}  /*** 010002 ***/
{include/i_fnctrad.i}
/*****************************************************************************
**
**       PROGRAMA: COL0602RP.p
**
**       DATA....: Janeiro de 2005
**
**       AUTOR...: Herc°lio Werneck de Capistrano - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Impress∆o Consulta COL0602
**
*****************************************************************************/
                                             /** Definiá∆o das temp-tables **/
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
    field lParam           as LOGICAL
    field l-lDetalharPorData as logical.

{colp/col0602.i3 tt-digita shared} /** Definiá∆o da temp-table de dados **/

def temp-table tt-raw-digita
   field raw-digita as raw.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

/****************************************************************************/
                                               /** Definiá∆o de ParÉmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    /** Transferància de parÉmetros para temp-table padr∆o **/
create tt-param.
raw-transfer raw-param to tt-param.
/****************************************************************************/
                                                /** Definiá∆o de Vari†veis **/
DEFINE VARIABLE h-acomp     AS HANDLE                   NO-UNDO.
DEFINE VARIABLE c-origem    AS character format "x(13)" NO-UNDO.
DEFINE VARIABLE cest AS CHARACTER format "x(10)" NO-UNDO.
define buffer bfDigita  for tt-digita.
define buffer bfDigita2 for tt-digita.
{include/i-rpvar.i}
/****************************************************************************/
                                                     /** Cabeáalho e Forms **/
{colp/col0602.i1}
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

&IF DEFINED(EXCLUDE-fnEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnEstado Procedure 
FUNCTION fnEstado RETURNS CHARACTER
  ( iEstado as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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

/** Parametriza padr‰es de cabeáalho e rodapÇ a serem exibidos **/
run piInicial in this-procedure.
       
/** Imprime cabeáalho e abre o output para arquivo **/
{include/i-rpcab.i}    
{include/i-rpout.i}

/** Procedure para inicializar c†lculos e impress∆o **/
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
  Notes:       Mostra os parÉmetros selecionados
------------------------------------------------------------------------------*/
/** Cria a p†gina dos parÉmetros **/
page.

/******************************************************************
  Espaáo reservado para buscar informaá‰es a serem impressas nos 
  parÉmetros (Opcional)
******************************************************************/

/** L¢gica opcional do programador **/

/******************************************************************
  Final do espaáo reservado
******************************************************************/

DISPLAY /*PAR∂METROS*/
        c-liter-par
        tt-param.iTipo
        tt-param.cTipo
        /*IMPRESS«O*/
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
  Notes:       Busca n°veis abaixo, coforme seleá∆o do n°vel
------------------------------------------------------------------------------*/
define input parameter cDimensao as character no-undo.

define variable cPai as character no-undo.

assign cPai = "".

for each  bfDigita use-index id
    where bfDigita.cod-dimens-pai = cDimensao no-lock:
    /** Verifica se existem n°veis abaixo  **/
    if bfDigita.sequencia <= tt-param.i-visao then do:
        /** Chama recursivamente atÇ encontrar vis∆o pai das tarefas **/
        run imprimeDados (bfDigita.cod-dimensao).
    end.
    else do:
        if cPai <> bfDigita.cod-dimens-pai then do:
            /** Imprime os cabeáalhos **/
            run imprimeVisao (input rowid(bfDigita),
                              input bfDigita.cod-dimens-pai).
            put skip(2).
            assign cPai = bfDigita.cod-dimens-pai.
        end.

        {colp/col0602.i2 bfDigita.p-image}
        assign c-origem = fnOrigem(bfDigita.i-origem).

        display bfDigita.desc-dimensao
                bfDigita.uso-real
                bfDigita.val-uso-padr
                bfDigita.un
                bfDigita.garantia
                bfDigita.ung
                bfDigita.atualizacao
                bfDigita.ordem
                fnEstado(bfDigita.i-estado) @ cest
            with frame f-dados.
        down with frame f-dados.
    end.
end.

put skip(1).

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
               entrada cCodPai = C¢digo da dimens∆o do registro pai 
  Notes:       Imprime os cabeáalhos, buscando os n°veis acima do selecionado
------------------------------------------------------------------------------*/
define input parameter rRow    as rowid     no-undo.
define input parameter cCodPai as character no-undo.

DEFINE VARIABLE i-cod-image AS INTEGER    NO-UNDO.

/** Verifica se existem vis‰es acima **/
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
/** Imprime a vis∆o, ignorando se for uma tarefa **/
ASSIGN i-cod-image = 30.
IF   tt-param.l-lDetalharPorData = YES THEN
     ASSIGN i-cod-image = 4.
     
for first bfDigita2
    where rowid(bfDigita2) = rRow 
    and   bfDigita2.p-image <> i-cod-image no-lock:
    /** Busca label do c¢digo **/
    {colp/col0602.i2 bfDigita2.p-image}
    assign tt-digita.cod-oficial:label in frame f-visoes = trim(return-value).
    /** Imprime vis∆o **/
    display trim(bfDigita2.c-des-eqpto) @ tt-digita.c-des-eqpto
            bfDigita2.cod-oficial       @ tt-digita.cod-oficial   
            bfDigita2.desc-dimensao     @ tt-digita.desc-dimensao
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
  Notes:       Define os valores que ser∆o mostrados o cabeáalho e rodapÇ
------------------------------------------------------------------------------*/
assign c-programa = "COL/0602"
       c-versao   = "1.00"
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

/** Busca os parÉmetros criados na interface gr†fica **/
find first tt-param.

/** Busca empresa padr∆o **/
find first param-global no-lock no-error.

if available param-global then do:
    find first empresa where ep-codigo = empresa-prin no-lock no-error.
    if available empresa then
        assign c-empresa = empresa.razao-social.
end.

/** Guarda valores para imprimir t°tulos **/
{utp/ut-liter.i "Consulta Garantias do Equipamento" *}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i "FROTAS" *}
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
  Notes:       Corpo princiapl da aplicaá∆o
------------------------------------------------------------------------------*/
/** Mostra frames de cabeáalho e rodapÇ padr‰es da Datasul **/
view frame f-cabec.
view frame f-rodape. 

/** Inicializa programa de acompanhamento padr∆o Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espaáo deve-se colocar a l¢gica para busca as informaá‰es e 
   tambÇm para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espaáo.
*********************************************************************/

/** Imprime Somente vis∆o selecionada **/
if tt-param.iTipo = 1 then do:
  /** AtravÇs da vis∆o selecionada, chama a impress∆o **/
  for first tt-digita
      where tt-digita.sequencia    = tt-param.i-seq
      and   tt-digita.cod-dimensao = tt-param.c-cod no-lock:
      /** Busca vis‰es a serem impressas **/
      run imprimeDados (input tt-digita.cod-dimensao).
  end.
end.
/** Imprime Completo **/
else do:
    /** Busca todas as vis‰es de primeiro n°vel **/
    for each  tt-digita
        where tt-digita.sequencia = 1 no-lock:
        /** Busca vis‰es a serem impressas **/
        run imprimeDados (input tt-digita.cod-dimensao).
    end.
end.

/*********************************************************************
   Fim do espaáo para l¢gica de c†lculo e display das informaá‰es
*********************************************************************/

/** Mostra parÉmetros selecionados **/
if tt-param.lParam then
    run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padr∆o Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fnEstado) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnEstado Procedure 
FUNCTION fnEstado RETURNS CHARACTER
  ( iEstado as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define variable cRetorno as character no-undo.

case iEstado:
    when 1 then do:
        {utp/ut-liter.i "Sem Garantia"}
    end.
    when 2 then do:
        {utp/ut-liter.i "Em Garantia"}
    end.
    when 3 then do:
        {utp/ut-liter.i "Vencida Garantia"}
    end.
end case.

assign cRetorno = return-value.

RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fnOrigem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnOrigem Procedure 
FUNCTION fnOrigem RETURNS CHARACTER
  ( i-orig as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnOrigem
    Notes:  Descriá∆o da Origem da Tarefa
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

case i-orig:
    when 1 then do:
        {utp/ut-liter.i "Lubrificaá‰es"}
    end.
    when 2 then do:
        {utp/ut-liter.i "Eventos"}
    end.
    when 3 then do:
        {utp/ut-liter.i "Planos Manutená∆o"}
    end.
    when 4 then do:
        {utp/ut-liter.i "Calend†rio Manutená∆o"}
    end.
    when 5 then do:
        {utp/ut-liter.i "Serviáo Pneus"}
    end.
    when 6 then do:
        {utp/ut-liter.i "Ordens Manutená∆o"}
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

