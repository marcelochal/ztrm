*----------------------------------------------------------------------*
***INCLUDE LFTE_BSMGLB .
*----------------------------------------------------------------------*

TYPES:gusl_t_fields LIKE dfies-fieldname OCCURS 0.
*&---------------------------------------------------------------------*
*&      Form  add_credit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_BUPER  text
*      -->P_<L_RESULT>  text
*      <--P_L_TOTAL_CREDIT  text
*----------------------------------------------------------------------*
FORM add_credit  USING    u_l_buper
                          u_l_result
                 CHANGING c_l_total_credit.

  DATA l_count TYPE i VALUE 0.
  DATA l_total_credit TYPE esbtr_eb VALUE 0.
  DATA lt_result TYPE TABLE OF faglflext.
  DATA ls_result LIKE LINE OF lt_result.
* FIELD-SYMBOLS  <l_result> TYPE faglflext.
* assign  u_l_result to <l_result>.
  ls_result = u_l_result.

  WHILE l_count LT u_l_buper.

    CASE l_count.
      WHEN 0.
        l_total_credit = l_total_credit + ls_result-tslvt.
        l_count = l_count + 1.
      WHEN 1.
        l_total_credit = l_total_credit + ls_result-tsl01.
        l_count = l_count + 1.
      WHEN 2.
        l_total_credit = l_total_credit + ls_result-tsl02.
        l_count = l_count + 1.
      WHEN 3.
        l_total_credit = l_total_credit + ls_result-tsl03.
        l_count = l_count + 1.
      WHEN 4.
        l_total_credit = l_total_credit + ls_result-tsl04.
        l_count = l_count + 1.
      WHEN 5.
        l_total_credit = l_total_credit + ls_result-tsl05.
        l_count = l_count + 1.
      WHEN 6.
        l_total_credit = l_total_credit + ls_result-tsl06.
        l_count = l_count + 1.
      WHEN 7.
        l_total_credit = l_total_credit + ls_result-tsl07.
        l_count = l_count + 1.
      WHEN 8.
        l_total_credit = l_total_credit + ls_result-tsl08.
        l_count = l_count + 1.
      WHEN 9.
        l_total_credit = l_total_credit + ls_result-tsl09.
        l_count = l_count + 1.
      WHEN 10.
        l_total_credit = l_total_credit + ls_result-tsl10.
        l_count = l_count + 1.
      WHEN 11.
        l_total_credit = l_total_credit + ls_result-tsl11.
        l_count = l_count + 1.
      WHEN 12.
        l_total_credit = l_total_credit + ls_result-tsl12.
        l_count = l_count + 1.
      WHEN 13.
        l_total_credit = l_total_credit + ls_result-tsl13.
        l_count = l_count + 1.
      WHEN 14.
        l_total_credit = l_total_credit + ls_result-tsl14.
        l_count = l_count + 1.
      WHEN 15.
        l_total_credit = l_total_credit + ls_result-tsl15.
        l_count = l_count + 1.
      WHEN 16.
        l_total_credit = l_total_credit + ls_result-tsl16.
        l_count = l_count + 1.
    ENDCASE.
  ENDWHILE.

  c_l_total_credit = l_total_credit.

ENDFORM.                    " add_credit
*&---------------------------------------------------------------------*
*&      Form  fill_fieldlist
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDLIST  text
*----------------------------------------------------------------------*
FORM fill_fieldlist  CHANGING ct_fieldlist TYPE gusl_t_fields .

  DATA ld_fieldlist TYPE dfies-fieldname.
  DATA ls_fagl_ab_c TYPE fagl_ab_c.

  ld_fieldlist = 'RACCT'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'RBUKRS'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'RTCUR'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'DRCRK'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'RYEAR'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'RLDNR'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'TSL'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'HSL'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'KSL'.
  APPEND ld_fieldlist TO ct_fieldlist.
  ld_fieldlist = 'OSL'.
  APPEND ld_fieldlist TO ct_fieldlist.

ENDFORM.                    " fill_fieldlist
*&---------------------------------------------------------------------*
*&      Form  sum_credit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_BUPER  text
*      -->P_LS_RESULT  text
*      <--P_L_TOTAL_CREDIT  text
*----------------------------------------------------------------------*
FORM sum_credit  USING    u_l_buper TYPE poper
                          u_ls_result TYPE glt0
                 CHANGING c_l_total_credit TYPE esbtr_eb.

  DATA l_count TYPE i VALUE 0.
  DATA l_total_credit TYPE esbtr_eb VALUE 0.
  DATA ls_result TYPE glt0.
* FIELD-SYMBOLS  <l_result> TYPE faglflext.
* assign  u_l_result to <l_result>.
  ls_result = u_ls_result.

  WHILE l_count LT u_l_buper.

    CASE l_count.
      WHEN 0.
        l_total_credit = l_total_credit + ls_result-tslvt.
        l_count = l_count + 1.
      WHEN 1.
        l_total_credit = l_total_credit + ls_result-tsl01.
        l_count = l_count + 1.
      WHEN 2.
        l_total_credit = l_total_credit + ls_result-tsl02.
        l_count = l_count + 1.
      WHEN 3.
        l_total_credit = l_total_credit + ls_result-tsl03.
        l_count = l_count + 1.
      WHEN 4.
        l_total_credit = l_total_credit + ls_result-tsl04.
        l_count = l_count + 1.
      WHEN 5.
        l_total_credit = l_total_credit + ls_result-tsl05.
        l_count = l_count + 1.
      WHEN 6.
        l_total_credit = l_total_credit + ls_result-tsl06.
        l_count = l_count + 1.
      WHEN 7.
        l_total_credit = l_total_credit + ls_result-tsl07.
        l_count = l_count + 1.
      WHEN 8.
        l_total_credit = l_total_credit + ls_result-tsl08.
        l_count = l_count + 1.
      WHEN 9.
        l_total_credit = l_total_credit + ls_result-tsl09.
        l_count = l_count + 1.
      WHEN 10.
        l_total_credit = l_total_credit + ls_result-tsl10.
        l_count = l_count + 1.
      WHEN 11.
        l_total_credit = l_total_credit + ls_result-tsl11.
        l_count = l_count + 1.
      WHEN 12.
        l_total_credit = l_total_credit + ls_result-tsl12.
        l_count = l_count + 1.
      WHEN 13.
        l_total_credit = l_total_credit + ls_result-tsl13.
        l_count = l_count + 1.
      WHEN 14.
        l_total_credit = l_total_credit + ls_result-tsl14.
        l_count = l_count + 1.
      WHEN 15.
        l_total_credit = l_total_credit + ls_result-tsl15.
        l_count = l_count + 1.
      WHEN 16.
        l_total_credit = l_total_credit + ls_result-tsl16.
        l_count = l_count + 1.
    ENDCASE.
  ENDWHILE.

  c_l_total_credit = c_l_total_credit + l_total_credit.

ENDFORM.                    " sum_credit
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_FISCAL_GL_BALANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_HKONT  text
*      -->P_I_BUKRS  text
*      -->P_I_KEY_DATE  text
*      <--P_LT_RESULT  text
*----------------------------------------------------------------------*
form GET_CURRENT_FISCAL_GL_BALANCE  using    u_hkont type HKONT
                                             u_bukrs type bukrs
                                             u_key_date type budat
                                 changing c_lt_result type fagl_t_glt0
                                          c_buper  type poper
                                          c_gjahr  type bdatj
                                          c_first_date type sydatum.

data: ls_org_data TYPE glx_org_info.
data: ld_glflex_active TYPE c.                       "n1698595

* check if NewGL is active                           "n1698595
  CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'           "n1698595
    IMPORTING                                        "n1698595
       E_GLFLEX_ACTIVE           = ld_glflex_active  "n1698595
     EXCEPTIONS                                      "n1698595
       ERROR_IN_SETUP            = 1                 "n1698595
       OTHERS                    = 2.                "n1698595
  IF SY-SUBRC <> 0.                                  "n1698595
   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO "n1698595
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.   "n1698595
  ENDIF.                                             "n1698595

  IF ld_glflex_active IS NOT INITIAL.                "n1698595
*   get leading ledger
  CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
    EXPORTING
      i_client      = sy-mandt
    IMPORTING
      e_rldnr       = gv_relevant_ledger
    EXCEPTIONS
      not_found     = 1
      more_than_one = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  ELSE.                                              "n1698595
    gv_relevant_ledger = '00'.                       "n1698595
  ENDIF.                                             "n1698595

* get organizational data
  CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
    EXPORTING
      i_rldnr             = gv_relevant_ledger
      i_orgunit           = u_bukrs
    IMPORTING
      organizational_info = ls_org_data
    EXCEPTIONS
      no_info_found       = 1
      error_in_setup      = 2
      error_in_depld      = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Find to which period the input date belongs
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = u_key_date
      i_periv        = ls_org_data-periv
    IMPORTING
      e_buper        = c_buper
      e_gjahr        = c_gjahr
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* retrieve first date of period
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = c_gjahr
      i_periv = ls_org_data-periv
      i_poper = c_buper
    IMPORTING
      E_DATE  = c_first_date
    EXCEPTIONS
      OTHERS = 4.
  IF sy-subrc <> 0.
    c_first_date      = u_key_date.
    c_first_date+6(2) = '01'.
  ENDIF.

* get GL account balance summary for the input Fiscal year
  CALL FUNCTION 'FAGL_GET_GLT0'
    EXPORTING
      i_rldnr           = gv_relevant_ledger
      i_rrcty           = '0'                 "n2293226
      i_rvers           = '001'               "n2293226
      i_bukrs           = u_bukrs
      i_ryear           = c_gjahr
      i_racct           = u_hkont
    IMPORTING
      et_glt0           = c_lt_result
    EXCEPTIONS
      invalid_selection = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
endform.                    " GET_CURRENT_FISCAL_GL_BALANCE
