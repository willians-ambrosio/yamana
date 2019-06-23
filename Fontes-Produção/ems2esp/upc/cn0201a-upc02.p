/****************************************************************************************** 
** 	   Programa: cn0201a-upc02.P 
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: Cria aba Garantia de contratos - chamada pelo programa upc\cn0201-upc00.p
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: cn0201a
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include/i-prgvrs.i cn0201a-upc02.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.


/* MESSAGE p-ind-event   skip             */
/*         p-ind-object  skip             */
/*         p-wgh-object  skip             */
/*         p-wgh-frame   skip             */
/*         p-cod-table   skip             */
/*         string(p-row-table)            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


/* Global Variable Definitions **********************************************/
define new global shared var adm-broker-hdl as handle no-undo.
define new global shared var h-folderRet    as handle no-undo.
define new global shared var h-viewer       as handle no-undo.

/* Variable Definitions *****************************************************/
define var c-folder       as character no-undo.
define var c-objects      as character no-undo.
define var h-object       as handle    no-undo.
define var i-objects      as integer   no-undo.
define var l-record       as logical   no-undo initial no.
define var l-group-assign as logical   no-undo initial no.

define new global shared VARIABLE views-folder AS HANDLE EXTENT 3 NO-UNDO. /*pega as viewers dos folders*/

define new global shared var h-bt-ok AS HANDLE.
define new global shared var falso-bt-ok AS HANDLE.
DEFINE VARIABLE evento AS CHARACTER INITIAL "Salvar_aux"  NO-UNDO.

{include/i_fclpreproc.i} /* Include que define o processador do Facelift ativado ou n∆o. */ 

&IF "{&aplica_facelift}" = "YES" &THEN
	{include/i_fcldef.i}
&endif

define variable c-handle-obj        as character no-undo.
define variable c-objeto            as character no-undo. 

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                  p-wgh-object:FILE-NAME,'/').



/* Main Block ***************************************************************/

/* OUTPUT TO C:\TEMP\POINT2.TXT APPEND. */
/*                                      */
/*     PUT UNFORMATTED                  */
/*         p-ind-event   SPACE(02)      */
/*         p-ind-object  SPACE(02)      */
/*         c-objeto      space(02)      */
/*         p-wgh-object:TYPE SPACE(02)  */
/*         p-wgh-object  SPACE(02)      */
/*         p-wgh-frame   SPACE(02)      */
/*         p-cod-table   SPACE(02)      */
/*         string(p-row-table)  SKIP.   */
/*                                      */
/* OUTPUT CLOSE.                        */

IF p-ind-event = "INITIALIZE" AND p-ind-object = "CONTAINER" AND NOT VALID-HANDLE(h-folderRet) then do:

     RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                           INPUT "PAGE-SOURCE":U,
                                           OUTPUT  c-folder).
   
    assign h-folderRet = widget-handle(c-folder) no-error.
       
    if valid-handle(h-folderRet) then do:

       RUN create-folder-page IN h-folderRet (INPUT 4, 
                                              INPUT "Garantia":U).

       &IF "{&aplica_facelift}" = "YES" &THEN
            {include/i_fcldin.i h-folderRet}
       &endif

       RUN create-folder-label IN h-folderRet (INPUT 4, INPUT "Garantia":U). 

       RUN select-Page IN p-wgh-object (INPUT 4).

       RUN init-object IN p-wgh-object (INPUT "cnp\escn0201a-v01.w":U, /* Nome do Objeto Viewer */
                                        INPUT p-wgh-frame,
                                        INPUT "Layout = ":U,
                                        OUTPUT h-viewer).

       RUN set-position IN h-viewer ( 4.10, 5.00).

       RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object, INPUT "CONTAINER-TARGET":U, OUTPUT c-objects).

       do i-objects = 1 to num-entries(c-objects):
          assign h-object = widget-handle(entry(i-objects, c-objects)).

          if index(h-object:private-data, "qry") <> 0 and  /* Vocà deve verificar se e a query principal */
             not l-record then do:
             assign l-record = yes.

             RUN add-link IN adm-broker-hdl (INPUT h-object,
                                             INPUT "Record":U,
                                             INPUT h-viewer).
          end.

          if index(h-object:private-data, "vwr") <> 0 and /* Voce deve verificar se e a viewer principal */
             not l-group-assign then do:
             assign l-group-assign = yes.

             RUN add-link IN adm-broker-hdl (INPUT h-object,
                                             INPUT "Group-Assign":U,
                                             INPUT h-viewer).
          end.
       END.

       RUN dispatch IN h-viewer ("initialize":U).
        
       RUN select-page IN p-wgh-object (INPUT 1).
    END.
end.   

/* IF p-ind-event  = "AFTER-VALIDATE" AND       */
/*    p-ind-object = "VIEWER"         AND       */
/*    c-objeto     = "cnp\escn0201a-v01.w"      */
/*    THEN RUN local-assign-record IN h-viewer. */

IF p-ind-event  = "DISPLAY" AND 
   p-ind-object = "VIEWER"  AND 
   c-objeto    = "cnp\escn0201a-v01.w"
   THEN RUN pi-display IN h-viewer (INPUT p-row-table).

IF p-ind-event  = "ADD" AND
    p-ind-object = "VIEWER"  AND
   c-objeto    = "cnp\escn0201a-v01.w"
    THEN RUN local-add-record IN h-viewer.

/* IF p-ind-event  = "ENABLE" AND               */
/*    p-ind-object = "VIEWER"  AND              */
/*    c-objeto    = "cnp\escn0201a-v01.w"       */
/*    THEN RUN local-enable-fields IN h-viewer. */

IF p-ind-event  = "AFTER-END-UPDATE" AND
   p-ind-object = "VIEWER"         AND
   c-objeto     = "cn0201a-v03.w"
   THEN RUN pi-assign IN h-viewer (INPUT p-row-table).

