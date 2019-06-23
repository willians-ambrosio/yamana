/****************************************************************************
** Programa: MV0301-ZOOM
** Autor...: Juliana K. Oliveira
** Data....: 12/06/2009
****************************************************************************/
{include/i-prgvrs.i MV0301-ZOOM  1.00.00.000} 
{utp/ut-glob.i}

/** PARAMETROS **/
DEFINE INPUT PARAMETER p-wgh-object           AS HANDLE             NO-UNDO.
DEFINE INPUT PARAMETER p-empresa              AS INTEGER            NO-UNDO.

/** VARIAVEIS **/
DEFINE                   VARIABLE wh-pesquisa          AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl       AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage1            AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-mv0301-ordem-inv  AS WIDGET-HANDLE NO-UNDO.


{include/zoomvar.i &prog-zoom="ivzoom/z05iv043.w"
                   &campohandle=wh-mv0301-ordem-inv
                   &campozoom="num-ord-magnus"
                   &framehandle=wh-fpage1
                   &proghandle=p-wgh-object
                   &parametros="run pi-seta-inicial in wh-pesquisa
                             (input p-empresa)."}

