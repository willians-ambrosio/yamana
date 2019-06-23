/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*{include/i-prgvrs.i TD-DIST-EMITENTE-01 2.00.00.001} /*** 010001 ***/*/
/*******************************************************************************
**  Programa: <name>
**  Objetivo: <comment>
**  Autor...: Datasul Logistica - <who>
**  Data....: <date>
*******************************************************************************/
DEF PARAM BUFFER b-dist-emitente FOR mgdis.dist-emitente.

DISABLE TRIGGERS FOR LOAD OF table-alias.dist-emitente.

{utp/ut-glob.i}

FOR FIRST table-alias.dist-emitente exclusive-lock
    WHERE table-alias.dist-emitente.cod-emitente = b-dist-emitente.cod-emitente:
    delete table-alias.dist-emitente.
END.

RELEASE table-alias.dist-emitente NO-ERROR.

RETURN "OK".

