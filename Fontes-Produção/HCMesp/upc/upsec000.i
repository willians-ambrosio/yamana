/********************************************************************************
** PROGRAMA : upc/upsec000.I
** DESCRICAO: include utilizada nas upc upsec001zb.p, upsec001zf.p e nas triggers td-usuar-grp-usuar.p e td_prog_dtsul_grp.p
** HISTORICO: Daniela Campso - 11/05/2018
********************************************************************************/

PROCEDURE pi-retorno-origem:

    DEFINE OUTPUT PARAMETER opc-origem AS CHAR NO-UNDO.
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO. 
    
    REPEAT ix = 1 TO NUM-DBS:  
        
        IF LDBNAME(ix) = "EMS5" THEN
            ASSIGN l-ok = YES.
    END.

    ASSIGN opc-origem = (IF l-ok THEN "ERP" ELSE "HCM").
    
    RETURN opc-origem.

END.
