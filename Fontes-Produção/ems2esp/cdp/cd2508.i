/***********************************************************************
              Verifica fun‡Æo "EMS2-COMEX-MEXICO"
***********************************************************************/

DEF VAR l-EMS2-IMP-MEXICO AS LOG NO-UNDO.
DEF VAR l-EMS2-EXP-MEXICO AS LOG NO-UNDO.

FIND FIRST funcao 
     WHERE funcao.cd-funcao = "EMS2-COMEX-MEXICO" NO-LOCK NO-ERROR.
FIND FIRST param-global NO-LOCK NO-ERROR.
IF AVAIL funcao AND funcao.ativo = YES THEN DO:
    IF param-global.modulo-07 THEN
        ASSIGN l-EMS2-IMP-MEXICO = YES.
    IF param-global.modulo-ex THEN
        ASSIGN l-EMS2-EXP-MEXICO = YES. 
END.

