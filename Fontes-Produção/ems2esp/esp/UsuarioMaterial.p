/*******************************************************************************
Extracao de Usuario Material
Kraft Consulting
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}


DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH usuar-mater NO-LOCK:

    FIND FIRST es_usuar_mater
        WHERE es_usuar_mater.Codigo = usuar-mater.cod-usuario  NO-ERROR.

    IF NOT AVAIL es_usuar_mater THEN DO:
        Create es_usuar_mater.
        Assign es_usuar_mater.Codigo            = usuar-mater.cod-usuario.
    END.

    ASSIGN es_usuar_mater.nome              = usuar-mater.nome-usuar
           es_usuar_mater.requisitante      = IF usuar-mater.usuar-requis         = YES THEN 1 ELSE 0
           es_usuar_mater.solicitante       = IF usuar-mater.usuar-solic          = YES THEN 1 ELSE 0
           es_usuar_mater.requisitanteManut = IF usuar-mater.usuar-requis-manut   = YES THEN 1 ELSE 0
           es_usuar_mater.comprador         = IF usuar-mater.usuar-comprador      = YES THEN 1 ELSE 0
           es_usuar_mater.aprovador         = IF usuar-mater.usuar-aprovador      = YES THEN 1 ELSE 0.

END.



/*RUN D:\DadosBI\Extratores\disconnect.p.*/



