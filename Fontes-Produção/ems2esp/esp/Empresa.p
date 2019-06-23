/*******************************************************************************
EXTRACAO DE EMPRESAS
DANIEL LIMA
28/12/2010
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

FOR EACH ES_EMPRESA:
    DELETE ES_EMPRESA.
END.

FOR EACH EMPRESA NO-LOCK:
    CREATE ES_EMPRESA.
    ASSIGN ES_EMPRESA.EP_CODIGO    = EMPRESA.EP-CODIGO
           ES_EMPRESA.NOME         = EMPRESA.NOME
           ES_EMPRESA.RAZAO_SOCIAL = EMPRESA.RAZAO-SOCIAL
           ES_EMPRESA.UF           = EMPRESA.UF.

    VALIDATE ES_EMPRESA.
END.
