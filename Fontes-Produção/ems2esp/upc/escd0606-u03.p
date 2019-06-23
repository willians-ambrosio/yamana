DEFINE NEW GLOBAL SHARED VARIABLE wh-cst-icms   AS HANDLE        NO-UNDO.
def new global shared var adm-broker-hdl        as handle        no-undo.
def new global shared var wh-window             as handle        no-undo.

DEF NEW GLOBAL SHARED VAR wh-pesquisa AS WIDGET-HANDLE NO-UNDO.

{utp/ut-glob.i}

{include/zoomvar.i &prog-zoom=esp\esnfe004-z01.w
                   &proghandle=wh-window 
                   &campohandle=wh-cst-icms
                   &campozoom=codigo}  
