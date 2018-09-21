*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.09.2018 at 13:21:51
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTRM_LIQ_BENS...................................*
DATA:  BEGIN OF STATUS_ZTRM_LIQ_BENS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_LIQ_BENS                 .
CONTROLS: TCTRL_ZTRM_LIQ_BENS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTRM_LIQ_BENS                 .
TABLES: ZTRM_LIQ_BENS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
