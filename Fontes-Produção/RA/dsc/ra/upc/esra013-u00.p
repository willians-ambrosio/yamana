/******************************************************************
**
** Programa: ESRA013-U00
**
** Objetivo: UPC chamadora da ESRA013
**
**    Autor: Willians Ambrosio / Grupo DKP
**
**     Data: JUN/2018
**
******************************************************************/
{include/i-prgvrs.i ESRA013-U00 12.01.19.000}

{include/i-epc200.i ESRA013}

Def Input Param p-ind-event As Char No-undo.
Def Input-output Param Table For tt-epc.

Run dsc/ra/upc/ESRA013-U01.p(Input p-ind-event,
                      Input-output Table tt-epc).

If Return-value = "NOK":U Then
    Return "NOK":U.
