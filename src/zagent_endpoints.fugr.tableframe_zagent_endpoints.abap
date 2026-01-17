*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZAGENT_ENDPOINTS
*   generation date: 17.01.2026 at 21:27:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZAGENT_ENDPOINTS   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
