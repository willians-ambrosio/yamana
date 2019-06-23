/****************************************************************************
**  Programa.: Trigger Delete Tabela turno_trab_ext
**  Autor....:  
**  Descricao: UPC de Trigger de delete 
************************************************************************/
{utp/ut-glob.i}

def parameter buffer b-turno_trab for turno_trab.

    find first turno_trab_ext exclusive-lock
         where turno_trab_ext.cdn_turno_trab = b-turno_trab.cdn_turno_trab no-error.
    if  avail  turno_trab_ext then delete turno_trab_ext.

RETURN "OK".

/* FIM */

