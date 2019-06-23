/*******************************************************
** Autor...: Leonardo Correia Santos de Oliveira
** Empresa.: YAMANA
** Programa: MI0408-UPC
** UPC cadastrada para programa: MI0408
** Objetivo: Inserir campos em tela e sugerir valores
*******************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alterado para possuir a definiá∆o dos prÇprocessadores logo no in°cio do programa*/
/**** Alteraá∆o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR */
/*Esta include est† sendo liberada vazia para o EMS 2
 para n∆o ocorrer erros de compilaá∆o*/
 


/* Fim */


/* Fim */
    

/* Fim */
    
.    
/* Fim */

 
/*Fim alteraá∆o 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[1.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "1.00.00.000"
       c-prg-obj = "mi0408-upc".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */
/***
{include/i-ctrlrp.i {1}}
***/

/*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/

/*Fim alteraá∆o 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "mi0408-upc"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Alteraá∆o - 08/09/2006 - tech1007 - Alteraá∆o para exibir o nome do programa que executou o programa que ser† exibido no extrato de vers∆o
                                      Solicitaá∆o realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "mi0408-upc" AT 1 "1.00.00.000" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim alteraá∆o 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.

/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                        
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 

/* fim da alateraá∆o */

/* Alteraá∆o realizada por tech38629 - 19/07/2006 - Definiá∆o do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de criaá∆o: 19/07/2006                                  */
/* Descriá∆o: Define o prÇ-processador que indica a utilizaá∆o  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da alteraá∆o */

 
/** ParÉmetros **/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

def new global shared var wh-button  as widget-handle no-undo.
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var tx-label   as widget-handle no-undo.
def new global shared var h_campo    as widget-handle no-undo.

def var c-objeto as char no-undo.
def var h_frame as widget-handle no-undo. 

DEFINE VARIABLE h-f-pg-par AS HANDLE NO-UNDO.

DEFINE VARIABLE h-i-cod-tipo          AS HANDLE NO-UNDO.
DEFINE VARIABLE h-c-descricao-nivel   AS HANDLE NO-UNDO.
DEFINE VARIABLE h-rs-cons-parada      AS HANDLE NO-UNDO.
DEFINE VARIABLE h-l-narrativa         AS HANDLE NO-UNDO.
DEFINE VARIABLE h-l-quebra-pag        AS HANDLE NO-UNDO.
DEFINE VARIABLE h-l-imp-param         AS HANDLE NO-UNDO.
DEFINE VARIABLE h-i-tipo-data         AS HANDLE NO-UNDO.
DEFINE VARIABLE h-c-texto             AS HANDLE NO-UNDO.
DEFINE VARIABLE h-RECT-41             AS HANDLE NO-UNDO.
DEFINE VARIABLE h-RECT-40             AS HANDLE NO-UNDO.
DEFINE VARIABLE h-c-cd-parada         AS HANDLE NO-UNDO.
DEFINE VARIABLE h-c-descricao-parada  AS HANDLE NO-UNDO.
DEFINE VARIABLE h-da-ini-parada       AS HANDLE NO-UNDO.
DEFINE VARIABLE h-i-sequencia         AS HANDLE NO-UNDO.

if p-ind-event = "YAMANA":U then do:

RETURN "OK":U.

END.

/*** Busca a variavel handle do folder***/
RUN findWidget (INPUT  "f-pg-par",
                INPUT  "FRAME",
                INPUT  p-wgh-frame,
                OUTPUT h-f-pg-par).

RUN findWidget (INPUT  "i-cod-tipo",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-i-cod-tipo).

RUN findWidget (INPUT  "c-descricao-nivel",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-c-descricao-nivel).

RUN findWidget (INPUT  "rs-cons-parada",
                INPUT  "RADIO-SET",
                INPUT  h-f-pg-par,
                OUTPUT h-rs-cons-parada).

RUN findWidget (INPUT  "l-narrativa",
                INPUT  "TOGGLE-BOX",
                INPUT  h-f-pg-par,
                OUTPUT h-l-narrativa).

RUN findWidget (INPUT  "l-quebra-pag",
                INPUT  "TOGGLE-BOX",
                INPUT  h-f-pg-par,
                OUTPUT h-l-quebra-pag).

RUN findWidget (INPUT  "l-imp-param",
                INPUT  "TOGGLE-BOX",
                INPUT  h-f-pg-par,
                OUTPUT h-l-imp-param).

RUN findWidget (INPUT  "i-tipo-data",
                INPUT  "RADIO-SET",
                INPUT  h-f-pg-par,
                OUTPUT h-i-tipo-data).

RUN findWidget (INPUT  "c-texto",
                INPUT  "TEXT",
                INPUT  h-f-pg-par,
                OUTPUT h-c-texto).

RUN findWidget (INPUT  "RECT-41",
                INPUT  "RECTANGLE",
                INPUT  h-f-pg-par,
                OUTPUT h-RECT-41).

RUN findWidget (INPUT  "RECT-40",
                INPUT  "RECTANGLE",
                INPUT  h-f-pg-par,
                OUTPUT h-RECT-40).

RUN findWidget (INPUT  "c-cd-parada",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-c-cd-parada).

RUN findWidget (INPUT  "c-descricao-parada",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-c-descricao-parada).

RUN findWidget (INPUT  "da-ini-parada",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-da-ini-parada).

RUN findWidget (INPUT  "i-sequencia",
                INPUT  "FILL-IN",
                INPUT  h-f-pg-par,
                OUTPUT h-i-sequencia).

ASSIGN h-i-cod-tipo         :VISIBLE = NO
       h-c-descricao-nivel  :VISIBLE = NO
       h-rs-cons-parada     :VISIBLE = NO
       h-l-narrativa        :VISIBLE = NO
/*        h-l-quebra-pag       :VISIBLE = YES */
       h-l-quebra-pag       :SENSITIVE = YES
       h-l-quebra-pag       :CHECKED = YES 
       h-l-imp-param        :VISIBLE = NO
       h-i-tipo-data        :VISIBLE = NO
       h-c-texto            :VISIBLE = NO
       h-RECT-41            :VISIBLE = NO
       h-RECT-40            :VISIBLE = NO
       h-c-cd-parada        :VISIBLE = NO
       h-c-descricao-parada :VISIBLE = NO
       h-da-ini-parada      :VISIBLE = NO
       h-i-sequencia        :VISIBLE = NO.

PROCEDURE findWidget:
    
    DEFINE INPUT  PARAMETER c-widget-name  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER c-widget-type  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        if h-start-widget:NAME = c-widget-name AND
           h-start-widget:TYPE = c-widget-type THEN DO:
            ASSIGN h-widget = h-start-widget:HANDLE.
            LEAVE.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN findWidget (INPUT  c-widget-name,
                            INPUT  c-widget-type,
                            INPUT  h-start-widget:FIRST-CHILD,
                            OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.
        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.
