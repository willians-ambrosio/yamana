/**********************************************************************************
Chamada de zoom SMART via UPC - z05iv043.w
Utilizado pelas upcs que fazem uso desse zoom - YAMANA
*********************************************************************************/
{utp/ut-glob.i}

define variable wh-pesquisa    as widget-handle no-undo.
define NEW GLOBAL SHARED var adm-broker-hdl                 AS HANDLE        NO-UNDO.

define input param ordem-invest as widget-handle no-undo.
define input param p-wgh-object as handle        no-undo.

assign l-implanta = no.
{include/zoomvar.i &prog-zoom   = ivzoom/z05iv043.w
                   &campohandle = ordem-invest
                   &campozoom   = num-ord-magnus
                   &proghandle  = p-wgh-object}
