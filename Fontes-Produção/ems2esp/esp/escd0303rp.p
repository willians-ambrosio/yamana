/************************************************************************
* Programa..: ESCD0303RP.P                                                *
* Descri¯Êo.: 
* AUTOR.....: DSC
* DATA......: 18/11/2014                                                *
              
************************************************************************/
{include/i-prgvrs.i ESCD0303RP 2.06.00.000}

/* ------------------------ Defini‡äes ------------------------- */
define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field arq-import       as character
    field idi-tip-import   as integer.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

define temp-table tt-it-doc-fisc no-undo
    field row-it-doc-fisc  as rowid.

define temp-table tt-report no-undo
    field cod-erro         as integer
    field des-erro         as character
    field num-linha        as integer
    field cdn-tribut       like sit-tribut-relacto.cdn-tribut
    field cdn-sit-tribut   like sit-tribut-relacto.cdn-sit-tribut
    field idi-tip-docto    like sit-tribut-relacto.idi-tip-docto
    field dat-valid-inic   like sit-tribut-relacto.dat-valid-inic
    field cod-estab        like sit-tribut-relacto.cod-estab
    field cod-natur-operac like sit-tribut-relacto.cod-natur-operac
    field cod-ncm          like sit-tribut-relacto.cod-ncm
    field cod-item         like sit-tribut-relacto.cod-item
    field cdn-grp-emit     like sit-tribut-relacto.cdn-grp-emit
    field cdn-emitente     like sit-tribut-relacto.cdn-emitente.


define buffer b-tt-digita for tt-digita.

define temp-table tt-raw-digita no-undo
    field raw-digita	   as raw.

define variable v_han_acomp             as handle                                no-undo.

define variable chExcel                 as component-handle                      no-undo.
define variable chWBook                 as component-handle                      no-undo.
define variable chWSheet                as component-handle                      no-undo.

define variable v_num_linha             as integer                               no-undo.

define variable v-cdn-tribut            like sit-tribut-relacto.cdn-tribut       no-undo.
define variable v-cdn-sit-tribut        like sit-tribut-relacto.cdn-sit-tribut   no-undo.
define variable v-idi-tip-docto         like sit-tribut-relacto.idi-tip-docto    no-undo.
define variable v-dat-valid-inic        like sit-tribut-relacto.dat-valid-inic   no-undo.
define variable v-cod-estab             like sit-tribut-relacto.cod-estab        no-undo.
define variable v-cod-natur-operac      like sit-tribut-relacto.cod-natur-operac no-undo.
define variable v-cod-ncm               like sit-tribut-relacto.cod-ncm          no-undo.
define variable v-cod-item              like sit-tribut-relacto.cod-item         no-undo.
define variable v-cdn-grp-emit          like sit-tribut-relacto.cdn-grp-emit     no-undo.
define variable v-cdn-emitente          like sit-tribut-relacto.cdn-emitente     no-undo.

define buffer b-sit-tribut-relacto for sit-tribut-relacto.

/* --------------------------- Inicializa‡Æo -------------------------- */
define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

run utp/ut-acomp.p persistent set v_han_acomp.
run pi-inicializar in v_han_acomp('Processando').

create 'Excel.application' chExcel.

chExcel:WorkBooks:open(tt-param.arq-import).
chWBook  = chExcel:ActiveWorkBook.
chWSheet = chWBook:ActiveSheet.


/* --------------------------- Processamento --------------------------- */
block1:
do v_num_linha = 2 to 65535:

    run pi-acompanhar in v_han_acomp('Linha: ' + string(v_num_linha)).

    assign v-cdn-tribut = chWSheet:Range('A' + string(v_num_linha)):value.

    if v-cdn-tribut = ? or
       v-cdn-tribut = 0 then
        leave block1.

    assign v-cdn-sit-tribut   = chWSheet:Range('B' + string(v_num_linha)):value
           v-idi-tip-docto    = if chWSheet:Range('C' + string(v_num_linha)):value = 'Entrada' then 1 else 2
           v-dat-valid-inic   = chWSheet:Range('D' + string(v_num_linha)):value
           v-cod-estab        = entry(1,chWSheet:Range('E' + string(v_num_linha)):value,",")
           v-cod-natur-operac = entry(1,chWSheet:Range('F' + string(v_num_linha)):value,",")
           v-cod-ncm          = entry(1,chWSheet:Range('G' + string(v_num_linha)):value,",")
           v-cod-item         = entry(1,chWSheet:Range('H' + string(v_num_linha)):value,",")
           v-cdn-grp-emit     = chWSheet:Range('I' + string(v_num_linha)):value
           v-cdn-emitente     = chWSheet:Range('J' + string(v_num_linha)):value.

    if tt-param.idi-tip-import = 1 then do:

        find last sit-tribut-relacto exclusive-lock where
                  sit-tribut-relacto.cdn-tribut       = v-cdn-tribut       and
                  /*sit-tribut-relacto.cdn-sit-tribut   = v-cdn-sit-tribut   and*/
                  sit-tribut-relacto.idi-tip-docto    = v-idi-tip-docto    and
                  /*sit-tribut-relacto.dat-valid-inic   = v-dat-valid-inic   and*/
                  sit-tribut-relacto.cod-estab        = v-cod-estab        and
                  sit-tribut-relacto.cod-natur-operac = v-cod-natur-operac and
                  sit-tribut-relacto.cod-ncm          = v-cod-ncm          and
                  sit-tribut-relacto.cod-item         = v-cod-item         and
                  sit-tribut-relacto.cdn-grp-emit     = v-cdn-grp-emit     and
                  sit-tribut-relacto.cdn-emitente     = v-cdn-emitente     no-error.
        if avail sit-tribut-relacto then do:

            assign sit-tribut-relacto.dat-valid-inic   = v-dat-valid-inic
                   sit-tribut-relacto.cdn-sit-tribut   = v-cdn-sit-tribut.

            create tt-report.
            assign tt-report.cod-erro         = 2
                   tt-report.des-erro         = 'Registro atualizado com sucesso'
                   tt-report.num-linha        = v_num_linha
                   tt-report.cdn-tribut       = v-cdn-tribut
                   tt-report.cdn-sit-tribut   = v-cdn-sit-tribut
                   tt-report.idi-tip-docto    = v-idi-tip-docto
                   tt-report.dat-valid-inic   = v-dat-valid-inic
                   tt-report.cod-estab        = v-cod-estab
                   tt-report.cod-natur-operac = v-cod-natur-operac
                   tt-report.cod-ncm          = v-cod-ncm
                   tt-report.cod-item         = v-cod-item
                   tt-report.cdn-grp-emit     = v-cdn-grp-emit
                   tt-report.cdn-emitente     = v-cdn-emitente.

        end.
        ELSE DO:
            create sit-tribut-relacto.                                       
            assign sit-tribut-relacto.cdn-tribut       = v-cdn-tribut        
                   sit-tribut-relacto.cdn-sit-tribut   = v-cdn-sit-tribut    
                   sit-tribut-relacto.idi-tip-docto    = v-idi-tip-docto     
                   sit-tribut-relacto.dat-valid-inic   = v-dat-valid-inic    
                   sit-tribut-relacto.cod-estab        = v-cod-estab         
                   sit-tribut-relacto.cod-natur-operac = v-cod-natur-operac  
                   sit-tribut-relacto.cod-ncm          = v-cod-ncm           
                   sit-tribut-relacto.cod-item         = v-cod-item          
                   sit-tribut-relacto.cdn-grp-emit     = v-cdn-grp-emit      
                   sit-tribut-relacto.cdn-emitente     = v-cdn-emitente.

            create tt-report.                                                                   
            assign tt-report.cod-erro         = 2                                              
                   tt-report.des-erro         = 'Registro importado com sucesso'                
                   tt-report.num-linha        = v_num_linha                                     
                   tt-report.cdn-tribut       = v-cdn-tribut                                    
                   tt-report.cdn-sit-tribut   = v-cdn-sit-tribut                                
                   tt-report.idi-tip-docto    = v-idi-tip-docto                                 
                   tt-report.dat-valid-inic   = v-dat-valid-inic                                
                   tt-report.cod-estab        = v-cod-estab                                     
                   tt-report.cod-natur-operac = v-cod-natur-operac                              
                   tt-report.cod-ncm          = v-cod-ncm                                       
                   tt-report.cod-item         = v-cod-item                                      
                   tt-report.cdn-grp-emit     = v-cdn-grp-emit                                  
                   tt-report.cdn-emitente     = v-cdn-emitente. 
        END.


    end.
    else do:

        find last sit-tribut-relacto no-lock where
                  sit-tribut-relacto.cdn-tribut       = v-cdn-tribut       and
                  /*sit-tribut-relacto.cdn-sit-tribut   = v-cdn-sit-tribut   and*/
                  sit-tribut-relacto.idi-tip-docto    = v-idi-tip-docto    and
                  sit-tribut-relacto.dat-valid-inic   = v-dat-valid-inic   and
                  sit-tribut-relacto.cod-estab        = v-cod-estab        and
                  sit-tribut-relacto.cod-natur-operac = v-cod-natur-operac and
                  sit-tribut-relacto.cod-ncm          = v-cod-ncm          and
                  sit-tribut-relacto.cod-item         = v-cod-item         and
                  sit-tribut-relacto.cdn-grp-emit     = v-cdn-grp-emit     and
                  sit-tribut-relacto.cdn-emitente     = v-cdn-emitente     no-error.

        if not avail sit-tribut-relacto then do:
    
            create sit-tribut-relacto.
            assign sit-tribut-relacto.cdn-tribut       = v-cdn-tribut
                   sit-tribut-relacto.cdn-sit-tribut   = v-cdn-sit-tribut
                   sit-tribut-relacto.idi-tip-docto    = v-idi-tip-docto
                   sit-tribut-relacto.dat-valid-inic   = v-dat-valid-inic
                   sit-tribut-relacto.cod-estab        = v-cod-estab
                   sit-tribut-relacto.cod-natur-operac = v-cod-natur-operac
                   sit-tribut-relacto.cod-ncm          = v-cod-ncm
                   sit-tribut-relacto.cod-item         = v-cod-item
                   sit-tribut-relacto.cdn-grp-emit     = v-cdn-grp-emit
                   sit-tribut-relacto.cdn-emitente     = v-cdn-emitente.
    
            create tt-report.
            assign tt-report.cod-erro         = 1
                   tt-report.des-erro         = 'Registro importado com sucesso'
                   tt-report.num-linha        = v_num_linha
                   tt-report.cdn-tribut       = v-cdn-tribut
                   tt-report.cdn-sit-tribut   = v-cdn-sit-tribut
                   tt-report.idi-tip-docto    = v-idi-tip-docto
                   tt-report.dat-valid-inic   = v-dat-valid-inic
                   tt-report.cod-estab        = v-cod-estab
                   tt-report.cod-natur-operac = v-cod-natur-operac
                   tt-report.cod-ncm          = v-cod-ncm
                   tt-report.cod-item         = v-cod-item
                   tt-report.cdn-grp-emit     = v-cdn-grp-emit
                   tt-report.cdn-emitente     = v-cdn-emitente.
    
        end.
        else do:

            create tt-report.
            assign tt-report.cod-erro         = 3
                   tt-report.des-erro         = 'Registro j  existente'
                   tt-report.num-linha        = v_num_linha
                   tt-report.cdn-tribut       = v-cdn-tribut
                   tt-report.cdn-sit-tribut   = v-cdn-sit-tribut
                   tt-report.idi-tip-docto    = v-idi-tip-docto
                   tt-report.dat-valid-inic   = v-dat-valid-inic
                   tt-report.cod-estab        = v-cod-estab
                   tt-report.cod-natur-operac = v-cod-natur-operac
                   tt-report.cod-ncm          = v-cod-ncm
                   tt-report.cod-item         = v-cod-item
                   tt-report.cdn-grp-emit     = v-cdn-grp-emit
                   tt-report.cdn-emitente     = v-cdn-emitente.

        end.

    end.

end.

release object chWSheet.

chWBook:close(no).

release object chWBook.

/* ------------------------------------------- Impressao de Relatorio -------------------------------- */
chExcel:WorkBooks:add().
chWBook  = chExcel:ActiveWorkBook.
chWSheet = chWBook:ActiveSheet.

run pi-seta-titulo in v_han_acomp('Imprimindo Relat¢rio').

chWSheet:Range('A1:M3'):MergeCells = true.
chWSheet:Range('A1'):value = 'Relat¢rio de Atualiza‡Æo Cadastral CST de PIS e COFINS'.
/*chWSheet:Range('A1'):HorizontalAlignment = -4103.
chWSheet:Range('A1'):VerticalAlignment   = -4103.*/

assign v_num_linha = 5.

chWSheet:Range('A' + string(v_num_linha)):value = 'C¢d. Erro'.
chWSheet:Range('B' + string(v_num_linha)):value = 'Desc. Erro'.
chWSheet:Range('C' + string(v_num_linha)):value = 'Linha Arquivo'.
chWSheet:Range('D' + string(v_num_linha)):value = 'C¢d. Tributo'.
chWSheet:Range('E' + string(v_num_linha)):value = 'Sit. Tribut ria'.
chWSheet:Range('F' + string(v_num_linha)):value = 'E/S'.
chWSheet:Range('G' + string(v_num_linha)):value = 'In¡cio Valid'.
chWSheet:Range('H' + string(v_num_linha)):value = 'Estab'.
chWSheet:Range('I' + string(v_num_linha)):value = 'Nat. Opera‡Æo'.
chWSheet:Range('J' + string(v_num_linha)):value = 'NCM'.
chWSheet:Range('K' + string(v_num_linha)):value = 'Item'.
chWSheet:Range('L' + string(v_num_linha)):value = 'Gr Emit'.
chWSheet:Range('M' + string(v_num_linha)):value = 'Emitente'.

assign v_num_linha = v_num_linha + 1.

for each tt-report no-lock:

    chWSheet:Range('A' + string(v_num_linha)):value = tt-report.cod-erro.
    chWSheet:Range('B' + string(v_num_linha)):value = tt-report.des-erro.
    chWSheet:Range('C' + string(v_num_linha)):value = tt-report.num-linha.
    chWSheet:Range('D' + string(v_num_linha)):value = tt-report.cdn-tribut.
    chWSheet:Range('E' + string(v_num_linha)):value = tt-report.cdn-sit-tribut.
    chWSheet:Range('F' + string(v_num_linha)):value = if tt-report.idi-tip-docto = 1 then "E" else "S".
    chWSheet:Range('G' + string(v_num_linha)):value = tt-report.dat-valid-inic.
    chWSheet:Range('H' + string(v_num_linha)):value = tt-report.cod-estab.
    chWSheet:Range('I' + string(v_num_linha)):value = tt-report.cod-natur-operac.
    chWSheet:Range('J' + string(v_num_linha)):value = tt-report.cod-ncm.
    chWSheet:Range('K' + string(v_num_linha)):value = tt-report.cod-item.
    chWSheet:Range('L' + string(v_num_linha)):value = tt-report.cdn-grp-emit.
    chWSheet:Range('M' + string(v_num_linha)):value = tt-report.cdn-emitente.

    assign v_num_linha = v_num_linha + 1.

end.

chWSheet:Range('A1:IV65535'):EntireColumn:Autofit.

release object chWSheet.

release object chWBook.

/* ------------------------------------------ Finalizacao ------------------------------------ */
chExcel:visible = true.

release object chExcel.

run pi-finalizar in v_han_acomp.

return 'OK':U.
