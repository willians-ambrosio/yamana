/*------------------------------------------------------------------------
    File        : epc-im0091.p
    Description : Epc para definir novo fornecedor do Imposto de Importacao 
    Author(s)   : Joao B. C. Bisneto - DSC - PRAXIS
------------------------------------------------------------------------*/
{include/i-epc200.i} /* Upc  */
/*------------------------------------------------------------------------*/
/*
define temp-table tt-epc no-undo
   field cod-event     as char format "x(12)"
   field cod-parameter as char format "x(32)"
   field val-parameter as char format "x(54)"
   index  id is primary cod-parameter cod-event ascending.    
*/
/*------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.
/*------------------------------------------------------------------------*/
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
/*------------------------------------------------------------------------*/
DEF VAR c-cod-estabel  LIKE es-emb-imp.cod-estabel NO-UNDO.
DEF VAR c-embarque     LIKE es-emb-imp.embarque    NO-UNDO.
/*------------------------------------------------------------------------*/
def    new global shared var v_cdn_empres_usuar            like mguni.empresa.ep-codigo no-undo.
/*------------------------------------------------------------------------*/
DEF BUFFER b-dupli-apagar FOR dupli-apagar.
DEF BUFFER bf-rat-ordem FOR rat-ordem.
/*------------------------------------------------------------------------*/
/* MESSAGE "1*epc-bocx220a.p*"            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*------------------------------------------------------------------------*/
/* FOR EACH tt-epc:                                       */
/*   MESSAGE                                              */
/*     "tt-epc.cod-event      " tt-epc.cod-event     skip */
/*     "tt-epc.cod-parameter  " tt-epc.cod-parameter skip */
/*     "tt-epc.val-parameter  " tt-epc.val-parameter skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/* END.                                                   */
/* im0091.i1 */
/*------------------------------------------------------------------------*/
FIND FIRST tt-epc 
  NO-LOCK 
  WHERE tt-epc.cod-event     = "FornecIINacionalizacao"
  AND   tt-epc.cod-parameter = "cod-estabel"
  NO-ERROR.
IF AVAIL tt-epc THEN
  ASSIGN c-cod-estabel = tt-epc.val-parameter.
/*------------------------------------------------------------------------*/ 
FIND FIRST tt-epc 
  NO-LOCK 
  WHERE tt-epc.cod-event     = "FornecIINacionalizacao"
  AND   tt-epc.cod-parameter = "embarque"
  NO-ERROR.
IF AVAIL tt-epc THEN
  ASSIGN c-embarque = tt-epc.val-parameter.
/*------------------------------------------------------------------------*/
/* IF c-cod-estabel <> "" THEN               */
/*   MESSAGE "c-cod-estabel: " c-cod-estabel */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*                                           */
/* IF c-embarque <> "" THEN                  */
/*   MESSAGE "c-embarque: " c-embarque       */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/*------------------------------------------------------------------------*/
FIND es-emb-imp
  NO-LOCK
  WHERE es-emb-imp.cod-estabel = c-cod-estabel
  AND   es-emb-imp.embarque    = c-embarque
  AND   es-emb-imp.ep-codigo   = v_cdn_empres_usuar
  NO-ERROR.
IF AVAIL es-emb-imp THEN
  DO:
    FIND FIRST tt-epc 
      NO-LOCK 
      WHERE tt-epc.cod-event     = "RetornaFornecIINacionalizacao"
      AND   tt-epc.cod-parameter = "Fornecedor-ii"
      NO-ERROR.
    IF NOT AVAIL tt-epc THEN
      DO:
        CREATE tt-epc. 
        ASSIGN 
          tt-epc.cod-event     = "RetornaFornecIINacionalizacao"
          tt-epc.cod-parameter = "Fornecedor-ii".
      END.
    ASSIGN tt-epc.val-parameter = STRING(es-emb-imp.cod-fornec-ii). 
  END.
/*------------------------------------------------------------------------*/

