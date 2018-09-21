FUNCTION ZFTE_BSM_GL_BALANCE_DETERMINE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_HKONT) TYPE  HKONT
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"     VALUE(I_KEY_DATE) TYPE  BUDAT
*"  EXPORTING
*"     VALUE(E_GL_BALANCE) TYPE  ESBTR_EB
*"     VALUE(E_TRANS_CURRENCY) TYPE  RTCUR
*"     VALUE(E_NO_GL_BALANCE) TYPE  BOOLE_D
*"  EXCEPTIONS
*"      NO_BALANCE_AVAILABLE
*"--------------------------------------------------------------------
*******************************************************************
*data declarations for getting previous posting balances that
*have happened before the key date
*****************************************************************

  TYPES: gusl_s_range LIKE rsdsselopt,
         gusl_t_range TYPE gusl_s_range OCCURS 10.
  TYPES: BEGIN OF gusl_s_selection,
         fieldname LIKE dfies-fieldname,
         t_range   TYPE gusl_t_range,
         END   OF gusl_s_selection,
         gusl_t_selection TYPE gusl_s_selection OCCURS 20.
  DATA gt_selection_full     TYPE  gusl_t_selection.
  DATA  l_gusl_s_selection_wa TYPE gusl_s_selection.
  DATA  lt_range TYPE gusl_t_range.
  DATA l_t_range_wa TYPE gusl_s_range.
  DATA l_period_total TYPE esbtr_eb.
  DATA: it_sel TYPE faglpose_t.
* field symbol for the relevant sum table.
  FIELD-SYMBOLS <tot_table> TYPE STANDARD TABLE.
  DATA gs_fagl_tabnames TYPE fagl_tabnames.
  DATA ls_org_data TYPE glx_org_info.
  DATA l_buper LIKE  t009b-poper."period to which the date belongs
  DATA l_gjahr LIKE  t009b-bdatj."fiscal year
  DATA l_gl_balance_not_found type BOOLE_D value ''.

  CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
    EXPORTING
      I_CLIENT      = SY-MANDT
    IMPORTING
      E_RLDNR       = gv_relevant_ledger
    EXCEPTIONS
      NOT_FOUND     = 1
      MORE_THAN_ONE = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA'
    EXPORTING
      i_rldnr             = gv_relevant_ledger
      i_orgunit           = i_bukrs
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


  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = i_key_date
      i_periv        = ls_org_data-periv
    IMPORTING
      e_buper        = l_buper
      e_gjahr        = l_gjahr
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*build data to fetch all gl postings for the input year,
*company code,gl account number and the ledger.

  l_gusl_s_selection_wa-fieldname = 'RBUKRS'."company code
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_bukrs.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RACCT'."gl account
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_hkont.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RLDNR'."ledger account
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = gv_relevant_ledger.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RYEAR'."fiscal year
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = l_gjahr.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

*...call FAGL_GET_TABLENAMES in order to fill
*...gs_fagl_tablename
*...gs_fagl_tablename is an import parameter of function modul
*...G_TABLE_SELECT_WITH_CURSOR
  CALL FUNCTION 'FAGL_GET_TABLENAMES'
    EXPORTING
      i_ledger            = gv_relevant_ledger
    IMPORTING
      es_tabnames         = gs_fagl_tabnames
    EXCEPTIONS
      not_found           = 1
      configuration_error = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*...fill gt_fieldlist with fields, which have to be ignored by
*...function modul G_TABLE_SELECT_WITH_CURSOR during aggregation.
  TYPES:gusl_t_fields LIKE dfies-fieldname OCCURS 0.
  DATA gt_fieldlist TYPE gusl_t_fields.
  DATA ld_fieldlist TYPE dfies-fieldname.
  DATA gr_tot_table TYPE REF TO data.

  PERFORM fill_fieldlist
                 CHANGING gt_fieldlist.

  CREATE DATA gr_tot_table TYPE TABLE OF (gs_fagl_tabnames-tot_table).
  ASSIGN gr_tot_table->* TO <tot_table>.

  LOG-POINT ID fins_udoc  SUBKEY 'FGL_FAGLFLEXT'.

*     FAGLFLEXT begin ----------------------------------------------

*if 1 = 1.
*
*  CALL FUNCTION 'G_TABLE_SELECT_WITH_CURSOR'
*    EXPORTING
*      i_tabname         = gs_fagl_tabnames-tot_table
*      i_selection       = gt_selection_full
*      i_fieldlist       = gt_fieldlist
*      i_aggregation     = 'X'
*      i_zero_records    = 'X'                               "865353
*    CHANGING
*      c_t_data          = <tot_table>
*    EXCEPTIONS
*      invalid_selection = 1
*      invalid_table     = 2
*      internal_error    = 3
*      foreign_lock      = 4
*      OTHERS            = 5.
*
*ELSE.

    DATA lo_ex TYPE REF TO cx_static_check.
    TRY.
        CALL METHOD cl_fins_acdoc_gusl_adapter=>select_with_cursor
          EXPORTING
            it_selection    = gt_selection_full
            it_fieldlist    = gt_fieldlist
            iv_aggregation  = abap_true
            iv_zero_records = abap_true
          CHANGING
            ct_data         = <tot_table>.

      CATCH cx_fins_acdoc_filter_error cx_fins_acdoc_sql_error cx_fins_acdoc_cust_error INTO lo_ex.
        sy-subrc = 1.

    ENDTRY.

*   FAGLFLEXT end ------------------------------------------------------------
* ENDIF.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF <tot_table> is initial.
   l_gl_balance_not_found = 'X'."set the flag when no records found
  ENDIF.

********************************************************************
*Data declarations to calculate the GL balance of all the months
*before the key date month.
********************************************************************
  DATA l_count TYPE i VALUE 0.
  DATA l_total_debit TYPE esbtr_eb VALUE 0.
  DATA l_total_credit TYPE esbtr_eb VALUE 0.

  DATA l_balance_amount TYPE esbtr_eb VALUE 0.
  DATA lt_result TYPE TABLE OF faglflext.
  DATA ls_result LIKE LINE OF lt_result.
  DATA lt_col_result type TABLE OF faglflext.


  FIELD-SYMBOLS  <l_result> TYPE faglflext.

  LOOP AT <tot_table> ASSIGNING <l_result>.
    e_trans_currency = <l_result>-rtcur.
    collect <l_result> into lt_col_result.
  ENDLOOP.

  LOOP AT lt_col_result into ls_result.
    e_trans_currency = ls_result-rtcur.

    IF ls_result-DRCRK EQ 'H'.
      PERFORM add_credit USING l_buper ls_result
                        CHANGING l_total_credit.
    elseif ls_result-DRCRK EQ 'S'.
      PERFORM add_credit USING l_buper ls_result
                        CHANGING l_total_debit.
    ENDIF.
  ENDLOOP.


  l_balance_amount = l_total_credit + l_total_debit.

  REFRESH gt_selection_full.

*Build data to fetch the GL account posting details that have
*happened from the start of the key date month and the key date

  l_gusl_s_selection_wa-fieldname = 'BUKRS'."company code
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_bukrs.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RBUKRS'."company code
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_bukrs.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'HKONT'."gl account
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_hkont.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RACCT'."account number
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = i_hkont.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  l_gusl_s_selection_wa-fieldname = 'RLDNR'."ledger account
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'EQ'.
  l_t_range_wa-low = gv_relevant_ledger.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.



  DATA l_start_date_of_month TYPE char08.

  l_start_date_of_month = i_key_date.
* replace last 2 digits with '01' to get start date of key date month
  REPLACE SECTION OFFSET 6 LENGTH 2 OF
  l_start_date_of_month WITH '01'.

  l_gusl_s_selection_wa-fieldname = 'BUDAT'."posting date
  l_t_range_wa-sign = 'I'.
  l_t_range_wa-option = 'BT'.
  l_t_range_wa-low =  l_start_date_of_month.
  l_t_range_wa-high = i_key_date.
  APPEND l_t_range_wa TO lt_range.
  l_gusl_s_selection_wa-t_range = lt_range.
  CLEAR lt_range.
  APPEND l_gusl_s_selection_wa TO gt_selection_full.

  CALL FUNCTION 'FAGL_GET_ITEMS_BSIS'
             EXPORTING
               i_selection            = gt_selection_full
             CHANGING
               c_t_faglpose           = it_sel.

* IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

* when no Gl records and no line items found
  if it_sel is initial and l_gl_balance_not_found eq 'X' .
    MESSAGE e006(FTE_BSM) raising no_balance_available.
  endif.

  DATA l_it_sel_wa TYPE faglpose.
  DATA l_final_balance TYPE esbtr_eb VALUE 0.
*add all posting that have happened between the start of the
*month and key date
  LOOP AT it_sel INTO l_it_sel_wa.
    l_final_balance = l_final_balance + l_it_sel_wa-dmshb.
  ENDLOOP.

*add the key date month balance with the balance of all
*the postings that have occurred in all months before the
*key date month to get the GL balance on a given key date

  e_gl_balance = l_final_balance + l_balance_amount.

*pass the flag which tells whether GL records were found
  e_no_gl_balance = l_gl_balance_not_found .


ENDFUNCTION.
