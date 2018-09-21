*&---------------------------------------------------------------------*
*&      Form  create_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ICON_RED_LIGHT  text
*      -->P_TEXT_003  text
*      -->P_LC_STATUS_RED  text
*      <--P_L_STATEMENT_PROC_STAT  text
*----------------------------------------------------------------------*
FORM create_icon  USING    u_TEXT
                           u_LC_STATUS
                  CHANGING c_L_STATEMENT_PROC_STAT.

 DATA: l_info(50) type c.                "n2262558
 DATA: l_ADD_STDINF type ICON-INTERNAL.  "n2262558

* suppress quick info for spool list
  if sy-batch ne 'X'.            "n2262558
   l_info = u_TEXT.              "n2262558
   l_ADD_STDINF = 'X'.           "n2262558
  endif.                         "n2262558

  case u_LC_STATUS.
    when 'RED'.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = icon_red_light
          text                  = u_TEXT
          info                  = l_info               "n2262558
          ADD_STDINF            = l_ADD_STDINF         "n2262558
        IMPORTING
          RESULT                = c_L_STATEMENT_PROC_STAT
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3.
        IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
    when 'YELLOW'.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = icon_yellow_light
          text                  = u_TEXT
          info                  = l_info               "n2262558
          ADD_STDINF            = l_ADD_STDINF         "n2262558
        IMPORTING
          RESULT                = c_L_STATEMENT_PROC_STAT
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3.
       IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
    when 'GREEN'.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = icon_green_light
          text                  = u_TEXT
          info                  = l_info               "n2262558
          ADD_STDINF            = l_ADD_STDINF         "n2262558
        IMPORTING
          RESULT                = c_L_STATEMENT_PROC_STAT
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3.
       IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
  endcase.

ENDFORM.                    " create_icon
*&---------------------------------------------------------------------*
*&      Form  FILL_BR_ABSND
*&---------------------------------------------------------------------*
*       Generates ABSND for Brazil
*----------------------------------------------------------------------*

FORM FILL_BR_ABSND  USING    P_L_T012_BANKL
                             P_L_T012K_BANKN
                    CHANGING P_L_ABSND_BR.

  DATA: CONVERT_BANK_1(6) TYPE C VALUE '. - / ',
        L_BANKL           TYPE T012-BANKL.

  CLEAR P_L_ABSND_BR.
  CHECK P_L_T012_BANKL  <> SPACE
    AND P_L_T012K_BANKN <> SPACE.

  L_BANKL = P_L_T012_BANKL.

*     Only numbers and blanks
  TRANSLATE L_BANKL USING CONVERT_BANK_1.
  CONDENSE  L_BANKL NO-GAPS.
*     converts bank to agency
  L_BANKL = L_BANKL+4.
  SHIFT L_BANKL LEFT DELETING LEADING '0'.

  P_L_ABSND_BR(3) = P_L_T012_BANKL(3).
  P_L_ABSND_BR+3  = L_BANKL.
  P_L_ABSND_BR+15 = P_L_T012K_BANKN.

ENDFORM.                    " FILL_BR_ABSND
*&---------------------------------------------------------------------*
*&      Form  REMOVE_ZERO_BALANCES
*&---------------------------------------------------------------------*
FORM REMOVE_ZERO_BALANCES  CHANGING C_LT_RESULT type fagl_t_glt0.

 DATA: lt_result_copy type fagl_t_glt0,
       ls_result TYPE glt0,
       l_tsl like glt0-TSLVT,
       l_numberofcur type i,                                   "n1762262
       l_exit(1) type c.

 loop at c_lt_result into ls_result.
   clear ls_result-DRCRK.
   collect ls_result into lt_result_copy.
 endloop.

* if there is only one currency involved, no need to remove second currency balances
 describe table lt_result_copy lines l_numberofcur.            "n1762262
 if l_numberofcur = 1.                                         "n1762262
   exit.                                                       "n1762262
 endif.                                                        "n1762262

 loop at lt_result_copy into ls_result.
   clear l_exit.
   if ls_result-TSLVT is not initial.                                   "n1882689
       CONTINUE.
   endif.

   do 16 times varying l_tsl FROM ls_result-TSL01 NEXT ls_result-TSL02. "n1882689
     if l_tsl is not initial.                                           "n1882689
       l_exit = 'X'.
       exit.
     endif.
   enddo.
   if l_exit ne 'X'.
     delete c_lt_result where
       RLDNR = ls_result-rldnr and
       RRCTY = ls_result-RRCTY and
       RVERS = ls_result-RVERS and
       BUKRS = ls_result-BUKRS and
       RYEAR = ls_result-RYEAR and
       RACCT = ls_result-RACCT and
       RBUSA = ls_result-RBUSA and
       RTCUR = ls_result-RTCUR and
*      DRCRK = ls_result-DRCRK and
       RPMAX = ls_result-RPMAX.
   endif.
 endloop.

ENDFORM.                    " REMOVE_ZERO_BALANCES
