*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTRM_LIQ_SERV
*   generation date: 07.09.2018 at 13:29:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTRM_LIQ_SERV      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
