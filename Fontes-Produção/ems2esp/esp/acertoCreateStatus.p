/*
FOR EACH mmv-ord-manut:
    CREATE mmv-ord-status.
    ASSIGN mmv-ord-status.nr-ord-produ = mmv-ord-manut.nr-ord-produ
           mmv-ord-status.idi-status-ord = 1
           mmv-ord-status.data-sit       = TODAY 
           mmv-ord-status.hora-sit       = REPLACE(STRING(TIME,"HH:MM:SS"),":","").
           
END.
*/
/*
FOR EACH mmv-ord-status:
    DISP mmv-ord-status.nr-ord-produ SKIP
         mmv-ord-status.idi-status-ord SKIP
         mmv-ord-status.data-sit.
END.
*/
