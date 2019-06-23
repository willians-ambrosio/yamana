/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i UPC-OF0770RP 2.00.00.001}  /*** 010001 ***/
/**************************************************************************
**    Programa....: upc-of0770rp.p
**    Objetivo....: Demonstrativo de PIS/COFINS (Customizado)
**    Data........: Maio/2009
**    Responsavel.: Diogo Cezar Amaral
**************************************************************************/
{cdp/cdcfgdis.i} 

define input  parameter pperiodo-ini as date       no-undo.
define input  parameter pperiodo-fim as date       no-undo.
define input  parameter pcod-estabel as character  no-undo.

define variable h-acomp        as handle                             no-undo.
define variable de-vl-contab   as decimal format '->,>>>,>>>,>>9.99' no-undo.
define variable de-base-pis    as decimal format '->,>>>,>>>,>>9.99' no-undo.
define variable de-base-cofins as decimal format '->,>>>,>>>,>>9.99' no-undo.
define variable de-vl-pis      as decimal format '->,>>>,>>>,>>9.99' no-undo.
define variable de-vl-cofins   as decimal format '->,>>>,>>>,>>9.99' no-undo.
define variable c-arquivo      as character                          no-undo.
define variable c-formato      as character                          no-undo.
define variable c-linha        as character                          no-undo.
define variable c-delimitador  as character initial "|"              no-undo.
define variable c-descricao-db like it-doc-fisc.descricao-db         no-undo.

define variable chExcel        as component-handle                   no-undo.
define variable chWBook        as component-handle                   no-undo.
define variable chWSheet       as component-handle                   no-undo.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "OF0770 - Demonstrativo Auxiliar de PIS/COFINS").

assign c-arquivo = session:temp-directory + "of0770rp" + ".tmp":U.

output to value(c-arquivo) convert target 'ISO8859-1'.
    assign c-linha   = "Dt Docto|Docto Fiscal|S‚rie|Fornec|Emitente|Nat Oper|Denomina‡Æo|Descri‡Æo DD|Valor Cont bil|Base de C lc. PIS|" +
                       "Base de C lc. COFINS|Valor PIS/PASEP (ENT)|Valor COFINS (ENT)|Valor PIS/PASEP (SAI)|Valor COFINS (SAI)":u
           c-formato = "2,1,2,2,1,1,1,1,2,2,2,2,2,2,2":U. /*1 - Texto , 2 - Outros*/.

    put c-linha format "x(" + string(length(c-linha)) + ")" skip.

    run pi-gera-docto (1). /*T: Entradas */
    run pi-gera-docto (3). /*T: Saidas   */
output close.


if c-arquivo <> "" then do:

    create 'Excel.application' chExcel.
    chExcel:WorkBooks:OpenText(c-arquivo,2,1,1,-4142,false,false,false,false,false,true,c-delimitador).

    chExcel:visible = true.

    release object chExcel.

end.

run pi-finalizar   in h-acomp.
if valid-handle(h-acomp) then
    delete procedure h-acomp.

procedure pi-gera-docto :
    define input  parameter p-tipo-nat as integer    no-undo.

    for each doc-fiscal
        where doc-fiscal.dt-docto    >= pperiodo-ini
        and   doc-fiscal.dt-docto    <= pperiodo-fim
        and   doc-fiscal.cod-estabel  = pcod-estabel
        and   doc-fiscal.ind-sit-doc <> 2 no-lock, /* cancelado */
        first natur-oper fields (nat-operacao denominacao tipo)
        where natur-oper.nat-operacao = doc-fiscal.nat-operacao no-lock,
        each  it-doc-fisc no-lock
        where it-doc-fisc.cod-estabel = doc-fiscal.cod-estabel
        and   it-doc-fisc.serie       = doc-fiscal.serie
        and   it-doc-fisc.nr-doc-fis  = doc-fiscal.nr-doc-fis
        and   it-doc-fisc.cod-emitente = doc-fiscal.cod-emitente
        and   it-doc-fisc.nat-operacao = natur-oper.nat-operacao 
        and ( &if '{&bf_dis_versao_ems}' >= '2.06' &then 
              it-doc-fisc.val-pis <> 0
              &else 
              (substr(it-doc-fisc.char-1,116,14) <> '0' and substr(it-doc-fisc.char-1,116,14) <> '')
              &endif or
              &if '{&bf_dis_versao_ems}' >= '2.06' &then 
              it-doc-fisc.val-cofins <> 0
              &else 
              (substr(it-doc-fisc.char-1,144,14) <> '0' and substr(it-doc-fisc.char-1,144,14) <> '')
              &endif )
        break by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                     doc-fiscal.cod-cfop
                 &ELSE
                     substr(doc-fiscal.char-1,1,10)
                 &ENDIF
              by doc-fiscal.nat-operacao
              by doc-fiscal.dt-emis-doc
              by doc-fiscal.nr-doc-fis:

        /***** Verifica documentos de entrada/servi‡o  ***/
        if  p-tipo-nat = 1 then do: /* entrada */
            if (doc-fiscal.tipo-nat <> 1 and doc-fiscal.tipo-nat <> 3) then next. 

            if (doc-fiscal.tipo-nat  = 3 and natur-oper.tipo     <> 1) then next.
        end.
        else do :  /*** Verifica documentos de Sa¡da ***/
            if (doc-fiscal.tipo-nat <> 2 and doc-fiscal.tipo-nat <> 3) then next. 

            if doc-fiscal.tipo-nat = 3 then do:
                if (natur-oper.tipo <> 2 and natur-oper.tipo <> 3) then next.
            end.
        end.
        
        run pi-acompanhar in h-acomp (input "Natureza: " + doc-fiscal.nat-operacao). 

        accumulate (truncate(it-doc-fisc.vl-tot-item,2))
            (total  by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                           doc-fiscal.cod-cfop
                       &ELSE
                           substr(doc-fiscal.char-1,1,10)
                       &ENDIF
                    by doc-fiscal.nat-operacao
                    by doc-fiscal.dt-emis-doc
                    by doc-fiscal.nr-doc-fis).  

        accumulate (truncate(&if '{&bf_dis_versao_ems}' >= '2.06' &then 
                                 it-doc-fisc.val-base-calc-pis 
                             &else 
                                 decimal(substr(it-doc-fisc.char-1,102,14)) 
                             &endif,2))  /* base de c lculo do pis    */ 
            (total  by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                           doc-fiscal.cod-cfop
                       &ELSE
                           substr(doc-fiscal.char-1,1,10)
                       &ENDIF
                    by doc-fiscal.nat-operacao
                    by doc-fiscal.dt-emis-doc
                    by doc-fiscal.nr-doc-fis).                                                        
        
        accumulate (truncate(&if '{&bf_dis_versao_ems}' >= '2.06' &then
                                 it-doc-fisc.val-base-calc-cofins 
                             &else 
                                 decimal(substr(it-doc-fisc.char-1,130,14)) 
                             &endif,2))  /* base de c lculo do cofins */ 
            (total  by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                           doc-fiscal.cod-cfop
                       &ELSE
                           substr(doc-fiscal.char-1,1,10)
                       &ENDIF
                    by doc-fiscal.nat-operacao
                    by doc-fiscal.dt-emis-doc
                    by doc-fiscal.nr-doc-fis).                                                        

        accumulate (truncate(&if '{&bf_dis_versao_ems}' >= '2.06' &then 
                                 it-doc-fisc.val-pis 
                             &else 
                                 decimal(substr(it-doc-fisc.char-1,116,14)) 
                             &endif,2))  /*  valor do pis normal      */ 
            (total  by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                           doc-fiscal.cod-cfop
                       &ELSE
                           substr(doc-fiscal.char-1,1,10)
                       &ENDIF
                    by doc-fiscal.nat-operacao
                    by doc-fiscal.dt-emis-doc
                    by doc-fiscal.nr-doc-fis).                                                        

        accumulate (truncate(&if '{&bf_dis_versao_ems}' >= '2.06' &then
                                 it-doc-fisc.val-cofins
                             &else
                                 decimal(substr(it-doc-fisc.char-1,144,14))
                             &endif,2))  /*  valor do cofins normal   */ 
            (total  by &IF "{&bf_dis_versao_ems}" >= "2.05" &THEN
                           doc-fiscal.cod-cfop
                       &ELSE
                           substr(doc-fiscal.char-1,1,10)
                       &ENDIF
                    by doc-fiscal.nat-operacao
                    by doc-fiscal.dt-emis-doc
                    by doc-fiscal.nr-doc-fis).                                                        
        
        /******************** ACUMULA PELA NATUREZA DE OPERA€ÇO ********************/
        if last-of(doc-fiscal.nr-doc-fis) then do :

            assign c-descricao-db = replace(it-doc-fisc.descricao-db,chr(10)," ").


            if  p-tipo-nat = 1 then
                assign c-linha = string(doc-fiscal.dt-docto,"99/99/9999")                                                                                   + c-delimitador + 
                                 string(doc-fiscal.nr-doc-fis)                                                                                              + c-delimitador + 
                                 string(doc-fiscal.serie)                                                                                                   + c-delimitador + 
                                 string(doc-fiscal.cod-emitente)                                                                                            + c-delimitador + 
                                 string(doc-fiscal.nome-ab-emi)                                                                                             + c-delimitador + 
                                 string(doc-fiscal.nat-operacao)                                                                                            + c-delimitador +
                                 string(natur-oper.denominacao)                                                                                             + c-delimitador +
                                 string(replace(c-descricao-db,c-delimitador,' '),"x(60)")                                                                  + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.vl-tot-item,2)),"->>>,>>>,>>9.99")                       + c-delimitador + 
                                 &if '{&bf_dis_versao_ems}' >= '2.06' &then
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-base-calc-pis   ,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-base-calc-cofins,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-pis             ,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-cofins          ,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 &else
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,102,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,130,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,116,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,144,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 &endif
                                 string(0,"->>>,>>>,>>9.99")                                                                                                + c-delimitador +
                                 string(0,"->>>,>>>,>>9.99"). 
            else
                assign c-linha = string(doc-fiscal.dt-docto,"99/99/9999")                                                                                   + c-delimitador + 
                                 string(doc-fiscal.nr-doc-fis)                                                                                              + c-delimitador + 
                                 string(doc-fiscal.serie)                                                                                                   + c-delimitador + 
                                 string(doc-fiscal.cod-emitente)                                                                                            + c-delimitador + 
                                 string(doc-fiscal.nome-ab-emi)                                                                                             + c-delimitador + 
                                 string(doc-fiscal.nat-operacao)                                                                                            + c-delimitador +
                                 string(natur-oper.denominacao)                                                                                             + c-delimitador +
                                 string(replace(c-descricao-db,c-delimitador,' '),"x(60)")                                                                  + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.vl-tot-item,2)),"->>>,>>>,>>9.99")                       + c-delimitador + 
                                 &if '{&bf_dis_versao_ems}' >= '2.06' &then
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-base-calc-pis   ,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-base-calc-cofins,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(0,"->>>,>>>,>>9.99")                                                                                                + c-delimitador +
                                 string(0,"->>>,>>>,>>9.99")                                                                                                + c-delimitador +
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-pis             ,2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(it-doc-fisc.val-cofins          ,2)),"->>>,>>>,>>9.99") + c-delimitador
                                 &else
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,102,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,130,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(0,"->>>,>>>,>>9.99")                                                                                                + c-delimitador +
                                 string(0,"->>>,>>>,>>9.99")                                                                                                + c-delimitador +
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,116,14)),2)),"->>>,>>>,>>9.99") + c-delimitador + 
                                 string(accum total by doc-fiscal.nr-doc-fis (truncate(decimal(substr(it-doc-fisc.char-1,144,14)),2)),"->>>,>>>,>>9.99") + c-delimitador 
                                 &endif
                                 .

            put c-linha format "x(" + string(length(c-linha)) + ")" skip.

        end.
    end. /*** for each doc-fiscal ***/      

    return "ok":u.
end procedure.  

