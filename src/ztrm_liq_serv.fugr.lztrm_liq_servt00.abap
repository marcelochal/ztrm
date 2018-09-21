*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.09.2018 at 13:29:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTRM_LIQ_SERV...................................*
DATA:  BEGIN OF STATUS_ZTRM_LIQ_SERV                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_LIQ_SERV                 .
CONTROLS: TCTRL_ZTRM_LIQ_SERV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTRM_LIQ_SERV                 .
TABLES: ZTRM_LIQ_SERV                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
