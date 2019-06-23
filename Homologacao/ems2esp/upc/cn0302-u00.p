/********************************************************************************* 
** Programa...: upc_bota.p 
** Author.....: foreach 
** Descricao..: Criar um campo em uma frame do Datasul EMS.
** Data.......: 22/02/2002 
** Mail.......: foreahc@foreach.com.br 
** 
** Visite www.foreach.com.br 
** 
*********************************************************************************/
/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* variaveis locais *********************************************************/
define variable              c-objeto           as char           no-undo.
define variable              h_frame            as widget-handle  no-undo.
define new global shared var wh-objeto          as Widget-handle  No-undo.
define new global shared var adm-broker-hdl     as handle         no-undo.
define new global shared var wh-excel             as Widget-handle  no-undo.
define new global shared var w-rowid-ord-comp   AS Rowid          No-undo.
                                                                  
assign c-objeto   = entry(num-entries(p-wgh-object:file-name, "~/"), p-wgh-object:file-name, "~/").

if  p-ind-event = "INITIALIZE" and
    p-ind-object = "CONTAINER" then
do:
    
    create button wh-excel
    assign row       = 1.10
           column    = 62
           width     = 4.5
           height    = 1.25
           frame     = p-wgh-frame
           sensitive = YES
           visible   = yes
           label     = "Detalhe"
           TOOLTIP   = "Importa rateio de medi‡äes".
    
    wh-excel:LOAD-IMAGE("image/toolbar/mip-csv.bmp").

    on choose of wh-excel persistent run upc/cn0302-u01.w. 
end. 
IF p-ind-event = "display"     AND
   P-IND-OBJECT = "VIEWER"  THEN DO:
      ASSIGN w-rowid-ord-comp = ?
             w-rowid-ord-comp = p-row-table.
END.
