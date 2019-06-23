/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: CD0738-UPCA.P
** Objetivo: - Abrir zoom do campo Tecnico.
*******************************************************/

def var wh-pesquisa as widget-handle.
def new global shared var l-implanta            as logical init no.
define new global shared var wh-fi-tecnico-0738 as widget-handle no-undo.
def new global shared var wh-window             as handle        no-undo.
def new global shared var adm-broker-hdl        as handle        no-undo.

  assign l-implanta = no.
  {include/zoomvar.i &prog-zoom="inzoom/z01in428.w"
                     &proghandle=wh-window
                     &campohandle=wh-fi-tecnico-0738
                     &campozoom=cd-tecnico}
                   
return "OK":U.



