/*******************************************************************************
** Defini��o das temp-tables de comunica��o para integra��o de contratos
*******************************************************************************/

&IF '{&bf_mat_versao_ems}' >= '2.04' &THEN

    {cnp/cnapi020.i3}
    DEF VAR hshowmsg AS HANDLE NO-UNDO.
    {method/dbotterr.i}

&ENDIF







