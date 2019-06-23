/*****************************************************************************
**  Programa : prghur\upc\fp1440aymup.p                                     **          
**  Descricao: cria campo Adicional de turno na viewer v06py202             **
**  Data     : setembro/09                                                  **          
**  Autor    : Datasul HCM - Toni Rodrigo de Souza                          **
******************************************************************************/                             
    
/*parametros padrao*/
def input param p-ind-event     as char          no-undo.
def input param p-ind-object    as char          no-undo.
def input param p-wgh-object    as handle        no-undo.
def input param p-wgh-frame     as widget-handle no-undo.
def input param p-cod-table     as char          no-undo.
def input param p-row-table     as rowid         no-undo.
     
/*variaveis locais*/
def var                   c-objeto          as char             no-undo.
/*def var                   wgh-child         as widget-handle    no-undo.
def var                   wgh-grupo         as widget-handle    no-undo.*/

def new global shared VAR wh-tg-adc-turno        as widget-handle no-undo.  /* toni */ 

{utp\ut-glob.i}
/*l¢gica*/
assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

/*IF c-objeto = "v05py202.w" then
    
message "Evento...........:" string(p-ind-event)      skip
        "Objeto...........:" string(p-ind-object)     skip
        "Handle do Objeto.:" string(p-wgh-object)     skip
        "Handle da Frame..:" p-wgh-frame:name         skip
        "Tabela...........:" p-cod-table              skip
        "Rowid............:" string(p-row-table)      skip
        "Programa.........:" p-wgh-object:FILE-NAME   SKIP
        "Objeto...........:" c-objeto.

*/

if  p-ind-object = 'viewer'       and  
   (p-ind-event  = 'after-enable' or
    p-ind-event  = 'add')         and
    c-objeto     = 'v05py202.w' then do:
    
    

    /*run pi-busca-campos. */

    create toggle-box wh-tg-adc-turno
           assign frame           = p-wgh-frame
               label              = "Adicional de Turno"
               row                = 1.5
               col                = 55.00
               width              = 16.72
               height             = 0.83 
               name               = "tg-adc-turno-esp"
               visible            = yes
               sensitive          = yes.


    find first turno_trab no-lock
         where rowid(turno_trab) = p-row-table no-error.
    if avail turno_trab then do:
       find first turno_trab_ext of turno_trab no-lock no-error.
       if avail turno_trab_ext then
           assign wh-tg-adc-turno:checked = turno_trab_ext.log_adc_turno.
    end.

end.


if p-ind-event  = "END-UPDATE" and
   p-ind-object = "VIEWER"     and 
   c-objeto     = "v05py202.w" then do:
   if valid-handle(wh-tg-adc-turno) then do:

      find first turno_trab no-lock
           where rowid(turno_trab) = p-row-table no-error.
      if avail turno_trab then do:
         if not can-find(first turno_trab_ext of turno_trab) then do:
            create turno_trab_ext.
            assign turno_trab_ext.cdn_turno_trab = turno_trab.cdn_turno_trab.
         end.
         else
            find first turno_trab_ext of turno_trab exclusive-lock no-error.
         assign turno_trab_ext.log_adc_turno = logical(wh-tg-adc-turno:screen-value). 
      end.
   end.
   
end.



