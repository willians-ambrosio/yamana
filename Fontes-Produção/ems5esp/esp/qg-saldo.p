

    FOR EACH cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_plano_cta   = "CONTSOC"
        AND   cta_ctbl.cod_cta_ctbl    = '11601014':
        for LAST sdo_ctbl OF cta_ctbl NO-LOCK
            WHERE sdo_ctbl.cod_empresa      = "CGO"
            AND   sdo_ctbl.cod_finalid_econ = "Corrente":
            DISP sdo_ctbl WITH WIDTH 333 1 COL.
        END.
    END.

