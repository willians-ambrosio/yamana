/*****************************************************************************************
** Programa: 
** Autor...: Log°stica (log339640)
** Data....: 07/2008
** OBS.....: 
** Objetivo: 
*****************************************************************************************/

/*{include/i-prgvrs.i CNAPI003-UPC 3.00.00.001 } /*** 010001 ***/*/

  /***************************************************************
**
** I-EPC200.I - Include de definiªío da temp-table padrío usada
**              para EPCs em Pontos Estrat≤gicos de programas
**          
** Par≥metros : 
** {1}  - nome do programa a ser localizado na tabela prog_dtsul     
***************************************************************/

/* definicao da temp-table */
/***************************************************************
**
** I-EPC200.I1 - Padroniza a temp-table usada para os epcs
**
***************************************************************/ 

/* begin_temp_table_definition */

define temp-table tt-epc no-undo
   field cod-event     as char format "x(12)"
   field cod-parameter as char format "x(32)"
   field val-parameter as char format "x(54)"
   index  id is primary cod-parameter cod-event ascending.    
   
/* end_temp_table_definition */   
    

  def var c-nom-prog-dpc-mg97  as char init "" no-undo.   
  def var c-nom-prog-appc-mg97 as char init "" no-undo.
  def var c-nom-prog-upc-mg97  as char init "" no-undo.
  def var raw-rowObject        as raw          no-undo.

   
find prog_dtsul where prog_dtsul.cod_prog_dtsul = "" no-lock no-error.
if  avail prog_dtsul then do:
    assign c-nom-prog-dpc-mg97  = prog_dtsul.nom_prog_dpc
           c-nom-prog-appc-mg97 = prog_dtsul.nom_prog_appc
           c-nom-prog-upc-mg97  = prog_dtsul.nom_prog_upc.
end.          
/* i-epc200.i */
 

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE r-contrato-for AS ROWID      NO-UNDO.
DEFINE VARIABLE r-item-contrat AS ROWID      NO-UNDO.
DEFINE VARIABLE de-val-receb   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE h-esapi001     AS HANDLE     NO-UNDO.

DEFINE VARIABLE cod-empresa  like param-global.empresa-prin no-undo.
/*{cdp\cd0669.i}*/
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descriá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).

if p-ind-event = "BeforeUpdate" then do:

    for each  tt-epc
        where tt-epc.cod-event = p-ind-event:

        if tt-epc.cod-parameter = "rowid-contrato-for" then
            assign r-contrato-for = to-rowid(tt-epc.val-parameter).
        if tt-epc.cod-parameter = "rowid-item-contrat" then
            assign r-item-contrat = to-rowid(tt-epc.val-parameter).
        if tt-epc.cod-parameter = "de-val"       then
            assign de-val-receb   = decimal(tt-epc.val-parameter).
    end.


    if de-val-receb <> 0 then do:
        run esp\esapi001.p persistent set h-esapi001.
        find first param-global no-lock no-error.
        
        find first item-contrat no-lock
             where rowid(item-contrat) = r-item-contrat no-error.
    
        if avail item-contrat then do:
    
            find first contrato-for no-lock
                 where contrato-for.nr-contrato = item-contrat.nr-contrato no-error.
            if avail contrato-for then do:
                find first estabelec no-lock
                     where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
                if avail estabelec then
                    assign cod-empresa = estabelec.ep-codigo.
            end.
            else do:
                find first param-global no-lock no-error.
                assign cod-empresa = param-global.empresa-prin.
            end.

            find first param-inv no-lock 
                 where param-inv.ep-codigo = cod-empresa no-error.
    
            find first item-contrat-ext EXCLUSIVE-LOCK
                 where item-contrat-ext.nr-contrato       = item-contrat.nr-contrato
                 and   item-contrat-ext.num-seq-item      = item-contrat.num-seq-item no-error.
    
            if avail item-contrat-ext then do:

                find first controle-inv-esp exclusive-lock 
                     where controle-inv-esp.tipo-doc     = "Item Contrato"
                     and   controle-inv-esp.nr-contrato  = item-contrat.nr-contrato
                     and   controle-inv-esp.num-seq-item = item-contrat.num-seq-item  no-error.
    
                if avail controle-inv-esp then do:

                    /*assign controle-inv-esp.sai-comp = item-contrat.sld-val-receb
                           controle-inv-esp.ent-real = item-contrat.sld-val-receb
                           controle-inv-esp.sai-real = 0.*/
                    
                    run pi-atualiza-verba in h-esapi001 (input 2,
                                                         input controle-inv-esp.ep-codigo,
                                                         input controle-inv-esp.num-ord-inv,
                                                         input controle-inv-esp.dt-trans,
                                                         input param-inv.moeda-inv,
                                                         input de-val-receb * -1,
                                                         input 0, /*de-val-receb,*/
                                                         output table tt-erro).
                end.
                if can-find (first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    if valid-handle(h-esapi001) then do:
                        delete procedure h-esapi001.
                        assign h-esapi001 = ?.
                    end.
                    return "NOK":U.
                end.
            end.
        end.
    
        find first contrato-for no-lock
             where rowid(contrato-for) = r-contrato-for no-error.
    
        if avail contrato-for then do:
    
            find first estabelec no-lock
                 where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
            if avail estabelec then
                assign cod-empresa = estabelec.ep-codigo.

            find first param-inv no-lock 
                 where param-inv.ep-codigo = cod-empresa no-error.
    
            find first contrato-for-ext EXCLUSIVE-LOCK
                 where contrato-for-ext.nr-contrato       = item-contrat.nr-contrato no-error.
    
            if avail contrato-for-ext then do:

                find first controle-inv-esp exclusive-lock 
                     where controle-inv-esp.tipo-doc     = "Contrato"
                     and   controle-inv-esp.nr-contrato  = contrato-for.nr-contrato no-error.
    
                if avail controle-inv-esp then do:

                    
                    /*assign controle-inv-esp.sai-comp = contrato-for.sld-val-receb
                           controle-inv-esp.ent-real = contrato-for.sld-val-receb
                           controle-inv-esp.sai-real = 0.*/
        
                    run pi-atualiza-verba in h-esapi001 (input 2,
                                                         input controle-inv-esp.ep-codigo,
                                                         input controle-inv-esp.num-ord-inv,
                                                         input controle-inv-esp.dt-trans,
                                                         input param-inv.moeda-inv,
                                                         input de-val-receb * -1,
                                                         input 0, /*de-val-receb,*/
                                                         output table tt-erro).
                    
                end.

                if can-find (first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    if valid-handle(h-esapi001) then do:
                        delete procedure h-esapi001.
                        assign h-esapi001 = ?.
                    end.
                    return "NOK":U.
                end.
            end.
        end.
    end.
end.
