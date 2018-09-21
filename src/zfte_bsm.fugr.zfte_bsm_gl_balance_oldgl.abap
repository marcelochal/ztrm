FUNCTION ZFTE_BSM_GL_BALANCE_OLDGL .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_HKONT) TYPE  HKONT
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_KEY_DATE) TYPE  BUDAT
*"  EXPORTING
*"     VALUE(E_GL_BALANCE) TYPE  ESBTR_EB
*"     VALUE(E_TRANS_CURRENCY) TYPE  RTCUR
*"     VALUE(E_NO_GL_BALANCE) TYPE  BOOLE_D
*"  EXCEPTIONS
*"      NO_BALANCE_AVAILABLE
*"--------------------------------------------------------------------
  TABLES: faglflexa.                         "n1832546

  CONSTANTS:c_credit TYPE char01 VALUE 'H',
            c_debit TYPE  char01 VALUE 'S'.

  TYPES: BEGIN OF gusl_s_selection,
         fieldname LIKE dfies-fieldname,
         t_range   TYPE rseloption," gusl_t_range,
         END   OF gusl_s_selection,
         BEGIN OF gusl_s_bseg_ext.
           INCLUDE STRUCTURE bseg.
  TYPES:   waers    TYPE waers,
         END OF gusl_s_bseg_ext,
         gusl_t_selection TYPE TABLE OF gusl_s_selection." OCCURS 20.

  DATA: lt_result TYPE fagl_t_glt0,
        ls_result TYPE glt0,
        l_gl_active TYPE boole_d VALUE '',
        ls_org_data TYPE glx_org_info,
        l_buper LIKE  t009b-poper,"period to which the date belongs
        l_gjahr LIKE  t009b-bdatj."fiscal year

  DATA: gt_selection_full     TYPE  gusl_t_selection,
        l_gusl_s_selection_wa TYPE gusl_s_selection,
        lt_range TYPE TABLE OF rsdsselopt,"gusl_t_range,
        l_t_range_wa TYPE rsdsselopt,"gusl_s_range,
        l_total_debit TYPE esbtr_eb VALUE 0,
        l_total_credit TYPE esbtr_eb VALUE 0,
        l_balance_before_keymonth TYPE esbtr_eb VALUE 0.
*______________________________________________________________________
*data declarations to calculate the balance from the start of
*the key date month until the keydate
*_____________________________________________________________________
  DATA: ls_t001 type t001,
        l_account_number TYPE belnr_d,
        lt_bseg_credit TYPE TABLE OF gusl_s_bseg_ext,
        lt_bseg_debit TYPE TABLE OF gusl_s_bseg_ext,
        l_bseg_wa  TYPE gusl_s_bseg_ext,
        l_xkres TYPE xkres,
        l_xopvw TYPE xopvw,
        l_start_date_of_month TYPE sydatum,
        cur_bseg TYPE cursor,
        l_credit_sum TYPE esbtr_eb VALUE 0,
        l_debit_sum TYPE esbtr_eb VALUE 0,
        l_balance_sum TYPE esbtr_eb VALUE 0.

  DATA: ld_leading_ledger  TYPE rldnr,            "n1832546
        ls_tablenames      TYPE fagl_tabnames,
        ld_glflex_active   TYPE boole_d,
        lr_table           TYPE REF TO data.

  FIELD-SYMBOLS: <table>      TYPE table,         "n1832546
                 <wa>         TYPE any.

  PERFORM get_current_fiscal_gl_balance USING i_hkont
                                              i_bukrs
                                              i_key_date
                                        CHANGING lt_result
                                                 l_buper
                                                 l_gjahr
                                                 l_start_date_of_month.

  PERFORM remove_zero_balances CHANGING lt_result.        "n1732653

  CALL FUNCTION 'COMPANY_CODE_READ'
    EXPORTING
      i_bukrs = i_bukrs
    IMPORTING
      e_t001  = ls_t001.

*set flag if no balance data found
  IF lt_result IS INITIAL.
    e_no_gl_balance = 'X'."set the flag when no records found
    MESSAGE e006(fte_bsm) RAISING no_balance_available.
  ENDIF.

  READ TABLE lt_result INDEX 1 INTO ls_result.
  IF sy-subrc <> 0.
    MESSAGE e006(fte_bsm) RAISING no_balance_available.
  ENDIF.
* assign currency to the exporting parameter
  e_trans_currency = ls_result-rtcur.
  CLEAR ls_result.
* get the total credit and total debit until the period
* to which the input keydate belongs
  LOOP AT lt_result INTO ls_result.
    IF ls_result-drcrk EQ c_credit."when credit
      PERFORM sum_credit USING l_buper ls_result
                        CHANGING l_total_credit.
    ELSEIF ls_result-drcrk EQ c_debit."when debit
      PERFORM sum_credit USING l_buper ls_result
                        CHANGING l_total_debit.
    ENDIF.
  ENDLOOP.

* calculate balance before the period to which keydate belongs
  l_balance_before_keymonth = l_total_credit + l_total_debit.

* check if GL account has line item and open item management
  SELECT SINGLE xkres xopvw FROM skb1 INTO (l_xkres, l_xopvw)
         WHERE bukrs = i_bukrs
         AND   saknr = i_hkont.

* bank accounts are usually line item managed --> XKRES = 'X'
  IF NOT l_xkres IS INITIAL.

    SELECT bukrs belnr gjahr buzei waers shkzg wrbtr dmbtr INTO CORRESPONDING FIELDS OF l_bseg_wa
           FROM  bsis
           WHERE bukrs EQ i_bukrs
           AND   hkont EQ i_hkont
           AND   budat BETWEEN l_start_date_of_month AND i_key_date
           AND   bstat EQ space.
      IF l_bseg_wa-shkzg EQ c_credit.
        APPEND l_bseg_wa TO lt_bseg_credit.
      ELSEIF l_bseg_wa-shkzg EQ c_debit.
        APPEND l_bseg_wa TO lt_bseg_debit.
      ENDIF.
    ENDSELECT.
    IF NOT l_xopvw IS INITIAL.
      SELECT bukrs belnr gjahr buzei waers shkzg wrbtr dmbtr INTO CORRESPONDING FIELDS OF l_bseg_wa
             FROM  bsas
             WHERE bukrs EQ i_bukrs
             AND   hkont EQ i_hkont
             AND   budat BETWEEN l_start_date_of_month AND i_key_date
             AND   bstat EQ space.
        IF l_bseg_wa-shkzg EQ c_credit.
          APPEND l_bseg_wa TO lt_bseg_credit.
        ELSEIF l_bseg_wa-shkzg EQ c_debit.
          APPEND l_bseg_wa TO lt_bseg_debit.
        ENDIF.
      ENDSELECT.
    ENDIF.

  ELSE.
*   check if NewGL is active                                  "n1832546
    CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
      IMPORTING
         e_glflex_active           = ld_glflex_active
       EXCEPTIONS
         error_in_setup            = 1
         OTHERS                    = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF NOT ld_glflex_active IS INITIAL.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr = ld_leading_ledger.

*     find database table
      CALL FUNCTION 'FAGL_GET_TABLENAMES'
        EXPORTING
          i_ledger    = ld_leading_ledger
        IMPORTING
          es_tabnames = ls_tablenames.

      CREATE DATA lr_table TYPE TABLE OF (ls_tablenames-si_table).
      ASSIGN lr_table->* TO <table>.

      SELECT * FROM (ls_tablenames-si_table) INTO TABLE <table>
               WHERE rldnr  = ld_leading_ledger
               AND   rbukrs = i_bukrs
               AND   racct  = i_hkont
               AND budat GE l_start_date_of_month
               AND budat LE i_key_date.
      LOOP AT <table> ASSIGNING <wa>.

        MOVE-CORRESPONDING <wa> TO faglflexa.

        l_bseg_wa-bukrs = faglflexa-rbukrs.
        l_bseg_wa-belnr = faglflexa-docnr.
        l_bseg_wa-gjahr = faglflexa-ryear.
        l_bseg_wa-buzei = faglflexa-docln.
        l_bseg_wa-dmbtr = faglflexa-hsl.
        l_bseg_wa-wrbtr = faglflexa-tsl.
        l_bseg_wa-shkzg = faglflexa-drcrk.
        l_bseg_wa-waers = faglflexa-rtcur.            "n2463149

        IF l_bseg_wa-shkzg EQ c_credit.
          l_bseg_wa-dmbtr = l_bseg_wa-dmbtr * -1.
          l_bseg_wa-wrbtr = l_bseg_wa-wrbtr * -1.
          COLLECT l_bseg_wa INTO lt_bseg_credit.
        ELSEIF l_bseg_wa-shkzg EQ c_debit.
          COLLECT l_bseg_wa INTO lt_bseg_debit.
        ENDIF.
      ENDLOOP.
    ELSE.                                                     "n1832546

*   collect all debit and credit postings that have happened
*   from the start of the key date month till the key date
    SELECT belnr waers FROM bkpf INTO (l_account_number, l_bseg_wa-waers)
             WHERE bukrs =  i_bukrs AND gjahr = l_gjahr
             AND budat GE l_start_date_of_month
             AND budat LE i_key_date
             AND bstat EQ space.
      OPEN CURSOR cur_bseg FOR
         SELECT * FROM bseg WHERE
                  bukrs = i_bukrs AND gjahr = l_gjahr
                  AND belnr = l_account_number
                  AND hkont = i_hkont.
      DO.
        FETCH NEXT CURSOR cur_bseg INTO CORRESPONDING FIELDS OF l_bseg_wa .
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF l_bseg_wa-shkzg EQ c_credit.
          COLLECT l_bseg_wa INTO lt_bseg_credit.
        ELSEIF l_bseg_wa-shkzg EQ c_debit.
          COLLECT l_bseg_wa INTO lt_bseg_debit.
        ENDIF.
      ENDDO.
      CLOSE CURSOR cur_bseg.
    ENDSELECT.
    ENDIF.
  ENDIF.

* when no GL postings within a date range
  IF lt_bseg_credit IS INITIAL
    AND lt_bseg_debit IS INITIAL
    AND e_no_gl_balance EQ 'X'.
    MESSAGE e006(fte_bsm) RAISING no_balance_available.
  ENDIF.
*calculate the sum of all credit postings , note that the credit
*postings here are positive values
  LOOP AT lt_bseg_credit INTO l_bseg_wa.
    IF e_trans_currency EQ l_bseg_wa-waers.
      l_credit_sum = l_credit_sum + l_bseg_wa-wrbtr.
    ELSEIF e_trans_currency EQ ls_t001-waers.
      l_credit_sum = l_credit_sum + l_bseg_wa-dmbtr.
    ELSE.
      RAISE no_balance_available. "never occurs (account in foreign curr --> items in same curr)
    ENDIF.
  ENDLOOP.
*calculate the sum of all debit postings
  LOOP AT lt_bseg_debit INTO l_bseg_wa.
    IF e_trans_currency EQ l_bseg_wa-waers.
      l_debit_sum = l_debit_sum + l_bseg_wa-wrbtr.
    ELSEIF e_trans_currency EQ ls_t001-waers.
      l_debit_sum = l_debit_sum + l_bseg_wa-dmbtr.
    ELSE.
      RAISE no_balance_available. "never occurs (see above)
    ENDIF.
  ENDLOOP.

* balance of postings from start of key date month till keydate
  l_balance_sum = l_debit_sum - l_credit_sum.
*__________________________________________________________________
* final balance is sum of keydate month balance and the balance
*from the start of the year till start of the key date month
*___________________________________________________________________
  e_gl_balance = l_balance_sum + l_balance_before_keymonth.

ENDFUNCTION.
