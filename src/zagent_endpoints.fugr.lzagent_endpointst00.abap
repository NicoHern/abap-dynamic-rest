*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAGENT_ENDPOINTS................................*
DATA:  BEGIN OF STATUS_ZAGENT_ENDPOINTS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAGENT_ENDPOINTS              .
CONTROLS: TCTRL_ZAGENT_ENDPOINTS
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZAGENT_ENDPOINTS              .
TABLES: ZAGENT_ENDPOINTS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
