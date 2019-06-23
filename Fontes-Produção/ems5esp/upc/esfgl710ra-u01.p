/******************************************************************
** Programa: esfgl710ra-u01
** Objetivo: Upc para o Programa fgl710ra
**     Data: Outubro/2007
**   Versao: 5.06.00.000 - Desenvolvimento Inicial
******************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

def new global shared var wh-tb-cenario as handle no-undo.

DEF NEW GLOBAL SHARED var v_cod_usuar_corren as CHARACTER format "x(12)":U label "Usu rio Corrente" column-label "Usu rio Corrente" no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

/* ===> Main Block <=== */

if  p-ind-event = "INITIALIZE" then do:
    create toggle-box wh-tb-cenario
    assign frame        = p-wgh-frame
           row          = 7
           column       = 58
           visible      = true
/*            sensitive    = true */
           screen-value = "yes"
           label        = "Replicar para todos os cen rios".
end.

if  p-ind-event = "Atualiza Lote" THEN DO:
    FIND FIRST es-param-cenario NO-LOCK NO-ERROR.

    IF wh-tb-cenario:SCREEN-VALUE = "yes" then do:
        IF INDEX (es-param-cenario.empresas,v_cod_empres_usuar) > 0 THEN DO:

            find first lote_ctbl no-lock
                 where lote_ctbl.num_lote_ctbl = int(entry(2, p-cod-table, ";")) no-error.
            if  avail lote_ctbl AND INDEX (es-param-cenario.modulos,lote_ctbl.cod_modul_dtsul) > 0 THEN DO:
                FOR EACH lancto_ctbl OF lote_ctbl:
                    FOR EACH item_lancto_ctbl OF lancto_ctbl:
                        ASSIGN item_lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
                    END.
                    ASSIGN lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
                END.
            END.
        end.
    end.
END.


