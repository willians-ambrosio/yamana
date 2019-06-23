/****************************************************************************
**
**  I-RPCAB.I - Form do Cabe‡alho PadrÆo e Rodap‚, (ex-CD9500.F)
**                              
** {&STREAM} - indica o nome da stream (opcional)
****************************************************************************/

Define Variable vPagina  As Char No-undo.
Define Variable vPeriodo As Char No-undo.
Define Variable vTo As Char format "x(2)" No-undo.

{utp/ut-liter.i P gina: *}
Assign vPagina = Return-value.

{utp/ut-liter.i Periodo: *}
Assign vPeriodo = Return-value.

{utp/ut-liter.i a RPCAB}
Assign vTo = Return-value.

&IF  "{&STREAM}":U = "":U &THEN
    form header
        fill("-":U, 132) format "x(132)":U skip
        c-empresa c-titulo-relat at 50
        vPagina format "x(07)" at 121 page-number  at 128 format ">>>>9":U skip
        fill("-":U, 112) format "x(110)":U today format "99/99/9999":U
        "-":U string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabec.
    
    form header
        fill("-":U, 132) format "x(132)":U skip
        c-empresa c-titulo-relat at 50
        vPagina  format "x(07)" at 121 page-number  at 128 format ">>>>9":U skip
        vPeriodo i-numper-x at 09 "-":U
        da-iniper-x at 14 vTo da-fimper-x
        fill("-":U, 74) format "x(72)":U today format "99/99/9999":U
        "-" string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabper.
&else
   form header
        fill("-":U, 132) format "x(132)":U skip
        c-empresa c-titulo-relat at 50
        vPagina format "x(07)"  at 121 page-number({&STREAM})  at 128 format ">>>>9":U skip
        fill("-":U, 112) format "x(110)":U today format "99/99/9999":U
        "-" string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabec.
    
    form header
        fill("-":U, 132) format "x(132)":U skip
        c-empresa c-titulo-relat at 50
        vPagina format "x(07)"  at 121 page-number({&STREAM})  at 128 format ">>>>9":U skip
        vPeriodo i-numper-x at 10 "-":U
        da-iniper-x at 14 vTo da-fimper-x
        fill("-":U, 74) format "x(72)":U today format "99/99/9999":U
        "-" string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 132 no-labels no-box page-top frame f-cabper.
&endif

&IF "{&STREAM}":U <> "":U &THEN
&GLOBAL-DEFINE STREAM_ONLY {&STREAM}
&ENDIF
    
c-rodape = "DATASUL - ":U + c-sistema + " - ":U + c-prg-obj + " - V:":U + c-prg-vrs.
c-rodape = fill("-":U, 132 - length(c-rodape)) + c-rodape.

form header
    c-rodape format "x(132)":U
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.

/* I-RPCAB.I */

