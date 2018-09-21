FUNCTION ZFTE_BSM_EXT_STATEMENTDATA_GET.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_BUKRS) TYPE  FTE_T_BUKRS OPTIONAL
*"     REFERENCE(I_STMNT_DATE) TYPE  AZDAT_EB
*"     REFERENCE(IT_HOUSEBANK) TYPE  FTE_T_HBKID OPTIONAL
*"     REFERENCE(IT_HOUSEBANKACCT) TYPE  FTE_T_HKTID OPTIONAL
*"     REFERENCE(I_STMNT_IMPORT_DATE) TYPE  EDATE_EB OPTIONAL
*"     REFERENCE(I_STMNT_IMPORT_TIME) TYPE  ETIME_EB OPTIONAL
*"     REFERENCE(IT_COUNTRYCODE) TYPE  FTE_T_BANKS OPTIONAL
*"     REFERENCE(IT_CURRENCY) TYPE  FTE_T_CURRENCY OPTIONAL
*"     REFERENCE(I_USER) TYPE  USER DEFAULT SY-UNAME
*"  EXPORTING
*"     REFERENCE(ET_STATEMENT) TYPE  FTE_BSM_T_STATEMENT
*"  EXCEPTIONS
*"      NO_BANK_FOUND
*"      NO_STATEMENT_FOUND
*"      NO_AUTHORITY
*"      NOTHING_FOUND
*"--------------------------------------------------------------------

  DATA:

  lr_housebank TYPE RANGE OF t012k-hbkid,
  lr_housebankacct TYPE RANGE OF t012k-hktid,
  lr_currency TYPE RANGE OF waers,
  lr_hbcountry TYPE RANGE OF t012-hbkid,
  lr_companycode TYPE RANGE OF bukrs,
  l_line_housebank LIKE LINE OF lr_housebank,
  l_line_housebankacct LIKE LINE OF lr_housebankacct,
  l_line_hbcountry LIKE LINE OF lr_hbcountry,
  l_line_companycode LIKE LINE OF lr_companycode,
  l_line_currency LIKE LINE OF lr_currency,
  l_bukrs TYPE bukrs,
  l_hbkid TYPE hbkid,
  l_hktid TYPE hktid,
  l_currency TYPE waers,
  l_countrycode TYPE t012-banks,
  lt_t012 TYPE TABLE OF t012,
  l_t012 TYPE t012,
  lt_t012k TYPE TABLE OF t012k,
  l_t012k TYPE t012k,
  l_febko_absnd TYPE absnd_eb,
  lt_febep TYPE TABLE OF febep,
  l_febep LIKE LINE OF lt_febep,
  l_absnd       TYPE febko_absnd_struct,
  l_idx TYPE sy-tabix,
  l_month TYPE tfmatage,
  l_days  TYPE tfmatage,
  l_date TYPE sy-datum,
  lt_calid TYPE trff_tab_calid,
  l_calid  TYPE skalid,
  l_gl_balance TYPE esbtr_eb,
  l_gl_curr TYPE waers,
  l_diff_amt  TYPE esbtr_eb,
  l_aznum TYPE aznum_eb,
  l_green TYPE boolean,
  l_yellow TYPE boolean,
  l_red TYPE boolean,
  l_error TYPE boolean,
  lt_t035d  TYPE TABLE OF t035d,
  l_t035d LIKE LINE OF lt_t035d,
  l_nofebko TYPE boolean.
  DATA: BEGIN OF lt_bankaccounts OCCURS 0 .
          INCLUDE STRUCTURE tfte_bsm_cust.
  DATA: countrycode TYPE t012-banks,
        bankl TYPE  bankl,
        bankn TYPE  bankn,
        bnkn2 TYPE bnkn2,
        bank_name type banka,
        alt_no TYPE boolean, " this is the alternative acct
        bkont TYPE bkont,                   "n2224241
        iban TYPE uiban.                    "n2553916
  DATA: END OF lt_bankaccounts.
  DATA: l_bankaccounts LIKE LINE OF lt_bankaccounts.
  DATA: l_save_bankdata LIKE LINE OF lt_bankaccounts.
  DATA: lt_statement TYPE fte_bsm_t_statement.
  DATA: l_statement LIKE LINE OF lt_statement.
  DATA: l_bnka TYPE bnka.
  DATA: l_addr_sel TYPE addr1_sel.
  DATA: l_addr_val TYPE addr1_val,
        l_actvt_anz TYPE activ_auth VALUE '03',
        l_number TYPE aznum_eb.
  DATA: BEGIN OF lt_febko OCCURS 0.
          INCLUDE STRUCTURE febko.
*  DATA: number TYPE aznum_eb.                        "n2391403
  DATA: END OF lt_febko.
  DATA: BEGIN OF lt_kukeys OCCURS 0.                  "n2391403
  DATA:    kukey TYPE KUKEY_EB.                       "n2391403
  DATA: END OF lt_kukeys.                             "n2391403
  DATA: l_tabix TYPE sy-tabix.                        "n2391403
  DATA: l_count_aznum TYPE febko-aznum.               "n2391403
  DATA: l_febko LIKE LINE OF lt_febko.
  DATA: l_febko2 LIKE LINE OF lt_febko.               "n2391403
  DATA: l_azpgno TYPE febko-azpgno.                   "n2391403
  DATA: l_old_aznum TYPE febko-AZNUM.                 "n2391403
  DATA: l_bankdata type BAPI1011_ADDRESS.
  DATA: l_absnd_br type FEBKO-ABSND.                  "n1671504
  DATA: l_bankn type t012k-bankn.                     "n2386672
  DATA: lt_t001 TYPE TABLE OF t001.                   "n1872424
  DATA: l_t001 LIKE LINE OF lt_t001.                  "n1872424
  DATA: lt_bukrs type FTE_T_BUKRS.                    "n1872424
  CONSTANTS: lc_status_red TYPE text VALUE 'RED'.
  CONSTANTS: lc_status_yellow TYPE text VALUE 'YELLOW'.
  CONSTANTS: lc_status_green TYPE text VALUE 'GREEN'.

  FIELD-SYMBOLS: <l_febko> TYPE febko.                "n2391403

  lt_bukrs[] = it_bukrs[].                       "n1872424

  IF lt_bukrs IS INITIAL.                        "n1872424
     SELECT * FROM t001 INTO TABLE lt_t001.      "n1872424
     LOOP AT lt_t001 INTO l_t001.                "n1872424
       MOVE l_t001-bukrs TO l_bukrs.             "n1872424
       APPEND l_bukrs TO lt_bukrs.               "n1872424
     ENDLOOP.                                    "n1872424
  ENDIF.                                         "n1872424

  LOOP AT lt_bukrs INTO l_bukrs.
    AUTHORITY-CHECK OBJECT 'F_FEBB_BUK'
         FOR USER i_user
         ID 'BUKRS' FIELD l_bukrs
         ID 'ACTVT' FIELD l_actvt_anz.
    IF sy-subrc NE 0.
      IF it_bukrs IS NOT INITIAL.                  "n1872424
        MESSAGE i204(fv) WITH 'F_FEBC_BUK' l_bukrs.
      ENDIF.                                       "n1872424
      CLEAR l_bukrs . CONTINUE.
    ENDIF.
    l_line_companycode-sign = 'I'.
    l_line_companycode-option = 'EQ'.
    l_line_companycode-low = l_bukrs.
    APPEND l_line_companycode TO lr_companycode.
  ENDLOOP.

  DESCRIBE TABLE lr_companycode.                  "n1510568
  If sy-tfill eq 0.
    MESSAGE e005(fte_bsm) RAISING NO_STATEMENT_FOUND.
  ENDIF.

* check parameters
  IF NOT it_housebank IS INITIAL.
    LOOP AT it_housebank INTO l_hbkid.
      l_line_housebank-sign = 'I'.
      l_line_housebank-option = 'EQ'.
      l_line_housebank-low = l_hbkid.
      APPEND l_line_housebank TO lr_housebank.
    ENDLOOP.
  ENDIF.

  IF NOT it_housebankacct IS INITIAL.
    IF it_housebank IS INITIAL.
      MESSAGE e004(fte_bsm) RAISING nothing_found.
    ENDIF.
    LOOP AT it_housebankacct INTO l_hktid.
      l_line_housebankacct-sign = 'I'.
      l_line_housebankacct-option = 'EQ'.
      l_line_housebankacct-low = l_hktid.
      APPEND l_line_housebankacct TO lr_housebankacct.
    ENDLOOP.
  ENDIF.

  IF NOT it_currency IS INITIAL.
    LOOP AT it_currency INTO l_currency.
      l_line_currency-sign = 'I'.
      l_line_currency-option = 'EQ'.
      l_line_currency-low = l_currency.
      APPEND l_line_currency TO lr_currency.
    ENDLOOP.
  ENDIF.

  IF NOT it_countrycode IS INITIAL.
    LOOP AT it_countrycode INTO l_countrycode.
      SELECT * FROM t012 INTO l_t012                    "#EC CI_GENBUFF
      WHERE bukrs IN lr_companycode
      AND hbkid IN lr_housebank
      AND banks = l_countrycode.
        l_line_hbcountry-sign = 'I'.
        l_line_hbcountry-option = 'EQ'.
        l_line_hbcountry-low = l_t012-hbkid.
        APPEND l_line_hbcountry TO lr_hbcountry.
        APPEND l_t012 TO lt_t012.
      ENDSELECT.
    ENDLOOP.

  ELSE.

    SELECT * FROM t012 INTO TABLE lt_t012               "#EC CI_GENBUFF
         WHERE bukrs IN  lr_companycode
         AND hbkid IN lr_housebank.

  ENDIF.

* Select all accounts
  SELECT * FROM t012k INTO TABLE lt_t012k               "#EC CI_GENBUFF
      FOR ALL ENTRIES IN lt_t012
      WHERE
      bukrs = lt_t012-bukrs
      AND
      hbkid = lt_t012-hbkid.

* get all relevant statements FDM_BSM_CUST
*  SELECT * FROM tfte_bsm_cust
  SELECT * FROM FCLMBAMBSMCUST
  INTO CORRESPONDING FIELDS OF TABLE lt_bankaccounts
  WHERE bukrs IN lr_companycode
  AND ( hbkid IN lr_housebank AND hbkid IN lr_hbcountry )
  AND hktid IN lr_housebankacct
  AND curr_diff IN lr_currency.

  IF sy-subrc NE 0.
    MESSAGE e002(fte_bsm) RAISING no_bank_found.
  ENDIF.


* get relevant FEBKO entries (last 5 each)

  LOOP AT lt_bankaccounts INTO l_bankaccounts.

    l_idx = sy-tabix.
* get housebank data
    READ TABLE lt_t012 INTO l_t012
    WITH KEY
    bukrs = l_bankaccounts-bukrs
    hbkid = l_bankaccounts-hbkid.

    l_bankaccounts-countrycode = l_t012-banks.
* get account data
    READ TABLE lt_t012k INTO l_t012k
    WITH KEY
    bukrs = l_bankaccounts-bukrs
    hbkid = l_bankaccounts-hbkid
    hktid = l_bankaccounts-hktid.

* read Bank name
    CALL FUNCTION 'BAPI_BANK_GETDETAIL'
      EXPORTING
        bankcountry        = l_t012-banks
        bankkey            = l_t012-bankl
     IMPORTING
       BANK_ADDRESS       = l_bankdata
*   BANK_DETAIL        =
*   RETURN             =
              .

*   read house bank IBAN                             "n2553916
    SELECT SINGLE iban FROM  tiban                   "n2553916
                       INTO  l_bankaccounts-iban     "n2553916
                       WHERE banks = l_t012-banks    "n2553916
                       AND   bankl = l_t012-bankl    "n2553916
                       AND   bankn = l_t012k-bankn   "n2553916
                       AND   bkont = l_t012k-bkont.  "n2553916

    l_bankaccounts-bank_name = l_bankdata-bank_name.

    l_bankaccounts-bankl = l_t012-bankl.
    l_bankaccounts-bankn = l_t012k-bankn.
    l_bankaccounts-bnkn2 = l_t012k-bnkn2.
    l_bankaccounts-bkont = l_t012k-bkont.        "n2224241
    MODIFY lt_bankaccounts FROM l_bankaccounts INDEX l_idx.

    CLEAR l_absnd. "note 1441202

    IF l_t012k-bnkn2 IS INITIAL. "origin acct data
      l_absnd-bankl = l_t012-bankl.
      l_absnd-bankn = l_t012k-bankn.
      l_absnd-waers = l_t012k-waers.
      l_febko_absnd = l_absnd.
    ELSE.                             " alt. acct no
      l_absnd-bankl = l_t012-bankl.
      l_absnd-bankn = l_t012k-bnkn2.
      l_febko_absnd = l_absnd.
    ENDIF.


* get all FEBKO data for HKTID
    DATA:                                            "note 1292262
      r_absnd TYPE RANGE OF febko-absnd,
      r_absnd_line LIKE LINE OF r_absnd.             "n1593355

    PERFORM FILL_ABSND_RANGE(SAPMF40K) TABLES r_absnd
                                       USING  l_absnd-bankl
                                              l_absnd-bankn
                                              l_t012k-waers.

    IF l_t012k-bnkn2 IS NOT INITIAL.      "n1593355
      clear R_ABSND_LINE.
      R_ABSND_LINE-OPTION  = 'EQ'.
      R_ABSND_LINE-SIGN    = 'I'.
      R_ABSND_LINE-LOW(15) = l_t012-bankl.    "n1871586
      R_ABSND_LINE-LOW+15  = l_t012k-bankn.
      APPEND R_ABSND_LINE to R_ABSND.
    ENDIF.

*   add entry for Japan                     "n2512616
    IF l_t012-banks = 'JP'.                 "n2512616
      l_absnd_br = l_t012-bankl.            "n2512616
      l_absnd_br+10 = l_t012k-bnkn2.        "n2512616
      l_absnd_br+25 = l_t012k-bkont+1(1).   "n2512616

      CLEAR R_ABSND_LINE.                   "n2512616
      R_ABSND_LINE-OPTION  = 'EQ'.          "n2512616
      R_ABSND_LINE-SIGN    = 'I'.           "n2512616
      R_ABSND_LINE-LOW     = l_absnd_br.    "n2512616
      APPEND R_ABSND_LINE to R_ABSND.       "n2512616
    ENDIF.                                  "n2512616

*   add entry for Brazil    "n1671504
    IF l_t012-banks = 'BR'.
     PERFORM FILL_BR_ABSND USING l_t012-bankl
                                 l_t012k-bankn
                           CHANGING l_absnd_br.
     IF l_absnd_br ne space.
       CLEAR R_ABSND_LINE.
       R_ABSND_LINE-OPTION  = 'EQ'.
       R_ABSND_LINE-SIGN    = 'I'.
       R_ABSND_LINE-LOW     = l_absnd_br.
       APPEND R_ABSND_LINE to R_ABSND.
     ENDIF.
*    begin note 2386672
     l_bankn = l_t012k-bankn.
     SHIFT l_bankn LEFT DELETING LEADING '0'.
     IF l_bankn ne l_t012k-bankn.
       PERFORM FILL_BR_ABSND USING l_t012-bankl
                                   l_bankn
                             CHANGING l_absnd_br.
       IF l_absnd_br ne space.
         CLEAR R_ABSND_LINE.
         R_ABSND_LINE-OPTION  = 'EQ'.
         R_ABSND_LINE-SIGN    = 'I'.
         R_ABSND_LINE-LOW     = l_absnd_br.
         APPEND R_ABSND_LINE to R_ABSND.
       ENDIF.
     ENDIF.
*    end note 2386672
    ENDIF.

    IF NOT i_stmnt_import_date IS INITIAL.

      CLEAR: l_number, l_old_aznum.                  "n2391403
      SELECT * FROM febko UP TO 100 ROWS INTO l_febko"n2391403
      WHERE
      anwnd = gc_statement
*      AND absnd = l_febko_absnd                     "note 1292262
      AND absnd in r_absnd
      AND bukrs = l_bankaccounts-bukrs
      AND    azdat LE i_stmnt_date
      AND    edate LE i_stmnt_import_date
      AND hbkid = l_bankaccounts-hbkid                          "n2442611
      AND hktid = l_bankaccounts-hktid                          "n2442611
      AND XBENR = gc_extrato                                    "S4H-FIN-EFD-M-050
      ORDER BY azdat DESCENDING kukey DESCENDING.               "n1623108
*       select 5 bank statements                     "n2391403
        if l_febko-aznum ne l_old_aznum.             "n2391403
          l_number = l_number + 1.                   "n2391403
        endif.                                       "n2391403
        if l_number > 5.                             "n2391403
          exit.   "select / endselect                "n2391403
        endif.                                       "n2391403
        l_old_aznum = l_febko-aznum.                 "n2391403
        l_febko-absnd = l_absnd.                     "note 1292262
        APPEND l_febko TO lt_febko.
      ENDSELECT.

    ELSE.

      CLEAR: l_number, l_old_aznum.                  "n2391403.
      SELECT * FROM febko UP TO 100 ROWS INTO l_febko"n2391403.
      WHERE
      anwnd = gc_statement
*      AND absnd = l_febko_absnd                     "note 1292262
      AND absnd in r_absnd
      AND bukrs = l_bankaccounts-bukrs
      AND    azdat LE i_stmnt_date
      AND hbkid = l_bankaccounts-hbkid                          "n2442611
      AND hktid = l_bankaccounts-hktid                          "n2442611
      ORDER BY azdat DESCENDING kukey DESCENDING.               "n1623108
*       select 5 bank statements                     "n2391403
        if l_febko-aznum ne l_old_aznum.             "n2391403
          l_number = l_number + 1.                   "n2391403
        endif.                                       "n2391403
        if l_number > 5.                             "n2391403
          exit.   "select / endselect                "n2391403
        endif.                                       "n2391403
        l_old_aznum = l_febko-aznum.                 "n2391403
        l_febko-absnd = l_absnd.                     "note 1292262
        APPEND l_febko TO lt_febko.
      ENDSELECT.

    ENDIF.                                           "n2391403

  ENDLOOP.

** get items data                                    "note 1291766
*  SELECT * FROM febep  INTO TABLE lt_febep
*  FOR ALL ENTRIES IN lt_febko
*  WHERE
*  kukey =  lt_febko-kukey.
* get Cash management account name
  SELECT * FROM t035d INTO TABLE lt_t035d
    FOR ALL ENTRIES IN lt_t012k                          "n1701896
    WHERE  bukrs = lt_t012k-bukrs                        "n1701896
    AND    bnkko = lt_t012k-hkont.                       "n1701896
*  SORT lt_febko BY absnd kukey DESCENDING.
  SORT lt_febko BY absnd DESCENDING
                   azdat DESCENDING
                   aznum DESCENDING
                   azpgno DESCENDING. "n2391403

* determine statement statuses
  LOOP AT lt_bankaccounts INTO l_bankaccounts.

    CLEAR l_statement.
    CLEAR l_t012.
    CLEAR l_t012k.

    READ TABLE lt_t012 INTO l_t012
    WITH KEY
    bukrs = l_bankaccounts-bukrs
    hbkid = l_bankaccounts-hbkid.

    READ TABLE lt_t012k INTO l_t012k
    WITH KEY
    bukrs = l_t012-bukrs
    hbkid = l_t012-hbkid
    hktid = l_bankaccounts-hktid.

    CLEAR l_absnd. "note 1441202

    IF l_t012k-bnkn2 IS INITIAL. "origin acct data
      l_absnd-bankl = l_t012-bankl.
      l_absnd-bankn = l_t012k-bankn.
      l_absnd-waers = l_t012k-waers.
      l_febko_absnd = l_absnd.
    ELSE.                             " alt. acct no
      l_absnd-bankl = l_t012-bankl.
      l_absnd-bankn = l_t012k-bnkn2.
      l_febko_absnd = l_absnd.
    ENDIF.

    READ TABLE lt_febko INTO l_febko
    WITH KEY
    anwnd = gc_statement
    absnd = l_febko_absnd.

    CLEAR l_nofebko.
    IF sy-subrc NE 0.
      CLEAR l_febko.
      l_nofebko = gc_xon.
    ENDIF.
** Copy data from bankacocunts to statement as errors may occurs and
** we need to show red light for proc status

    l_statement-company_code = l_bankaccounts-bukrs.
    l_statement-housebank_id = l_bankaccounts-hbkid.
    l_statement-bank_name = l_bankaccounts-bank_name.
    l_statement-account_id = l_bankaccounts-hktid.
    l_statement-bank_key = l_bankaccounts-bankl.
    l_statement-ext_account_no = l_bankaccounts-bankn.
    l_statement-alt_account_no = l_bankaccounts-bnkn2.
    l_statement-country = l_bankaccounts-countrycode.
    l_statement-contact_name = l_t012-name1.
    l_statement-contact_tel = l_t012-telf1.
    l_statement-tabix  = l_bankaccounts-sort_idx.
    l_statement-bkont = l_bankaccounts-bkont.      "n2224241
    l_statement-iban = l_bankaccounts-iban.        "n2553916

* fill the rest from FEBKO
    READ TABLE lt_t035d INTO l_t035d
    WITH KEY   bukrs = l_bankaccounts-bukrs
               bnkko = l_t012k-hkont.              "n1701896
    IF sy-subrc eq 0.                              "n1701896
      l_statement-cm_account_name = l_t035d-diskb. "n1701896
    ELSE.                                          "n1701896
      CLEAR l_statement-cm_account_name.           "n1701896
    ENDIF.                                         "n1701896

    l_statement-currency = l_febko-waers.
    l_statement-country = l_bankaccounts-countrycode.
    l_statement-gl_account = l_febko-hkont.
    l_statement-statement_date = l_febko-azdat.
    l_statement-statement_no = l_febko-aznum.
    l_statement-no_items = l_febko-anzes.
    l_statement-azpgno = l_febko-azpgno.           "n2391403
    l_statement-import_date = l_febko-edate.
    l_statement-import_time = l_febko-etime.
    l_statement-kukey = l_febko-kukey.
    l_statement-euser = l_febko-euser. "note 1486308
    IF l_febko-ssvoz = gc_soll.
      l_statement-open_bal_statemt = l_febko-ssbtr * -1.
    ELSE.
      l_statement-open_bal_statemt = l_febko-ssbtr.
    ENDIF.
    l_statement-total_debits = l_febko-sumso.
    l_statement-total_credits = l_febko-sumha.
    IF l_febko-esvoz = gc_soll.
      l_statement-clos_bal_statemt = l_febko-esbtr * -1.
    ELSE.
      l_statement-clos_bal_statemt = l_febko-esbtr.
    ENDIF.

* get SWIFT code
    IF NOT
    ( l_bnka-banks = l_bankaccounts-countrycode
    AND
      l_bnka-bankl = l_bankaccounts-bankl ).

      CALL FUNCTION 'READ_BANK_ADDRESS'
        EXPORTING
          bank_country = l_bankaccounts-countrycode
          bank_number  = l_bankaccounts-bankl
        IMPORTING
          bnka_wa      = l_bnka
        EXCEPTIONS
          not_found    = 1
          OTHERS       = 2.
    ENDIF.

    l_statement-swift_code = l_bnka-swift.


***** DETERMINE PROCESS STATUS
    IF l_bankaccounts-procstat_act = gc_xon.

      IF l_nofebko = gc_xon.
        PERFORM create_icon USING text-003
                              lc_status_red
                        CHANGING l_statement-proc_stat.
*        l_statement-proc_stat = icon_red_light.
      ELSE.

        CLEAR l_days. CLEAR l_month.

        CASE l_bankaccounts-exp_rhyth.
          WHEN 0. "month
            l_month = l_bankaccounts-exp_number.
          WHEN 1. " days
            l_days = l_bankaccounts-exp_number.
          WHEN 2.   " weeks (here 7 days)
            l_days = l_bankaccounts-exp_number * 7.
          WHEN 3.  " 1/2 month (here 15 days)
            l_days = l_bankaccounts-exp_number * 15.
          WHEN 4.   " years  (here 12 month)
            l_month = l_bankaccounts-exp_number * 12.
        ENDCASE.

** calendar for working day check
        REFRESH lt_calid.
        MOVE l_bankaccounts-calendar TO l_calid.
        APPEND l_calid TO lt_calid.

** Calculate   expected  statement date using calendar
        CALL FUNCTION 'FIMA_DATE_CREATE_WITH_CALENDAR'
          EXPORTING
            i_date                    = l_febko-azdat
            i_flg_calendar_year       = ' '
            i_months                  = l_month
            i_calendar_days           = l_days
            i_swerk                   = 1        " folgender Arbeitstag
          IMPORTING
            e_date                    = l_date
*   E_SUBRC                   =
          TABLES
            icalid                    = lt_calid
                  .
** check if statement arrived in time
        IF l_date GE i_stmnt_date.       "OK
** Check vb1OK
          IF l_febko-azpgno IS INITIAL OR        "n2391403
             l_febko-azpgno = '00000'.           "n2391403
            IF l_febko-astat = gc_posting_ok.
              PERFORM create_icon USING text-001
                                        lc_status_green
                                 CHANGING l_statement-proc_stat.
*            l_statement-proc_stat = icon_green_light.
            ELSE.
              PERFORM create_icon USING text-002
                                        lc_status_yellow
                                  CHANGING l_statement-proc_stat.
*            l_statement-proc_stat = icon_yellow_light.
            ENDIF.
* begin note 2391403
          ELSE.
*           check all pages of last bank statement
            CLEAR: l_yellow, l_red.
            l_azpgno = l_febko-azpgno.
            LOOP AT lt_febko ASSIGNING <l_febko>
                             WHERE absnd = l_febko-absnd
                             AND aznum = l_febko-aznum
                             AND bukrs = l_bankaccounts-bukrs.
*              check page gaps.
               AT FIRST.
                 IF <l_febko>-estyp = 'M'.
                   "error red: last page of bank statement missing
                   l_red = 'X'.
                   EXIT.
                 ENDIF.
               ENDAT.
               IF l_azpgno = <l_febko>-azpgno.
                 "no page gap
                 IF <l_febko>-astat ne 8.
                   "error yellow.
                   l_yellow = 'X'.
                 ENDIF.
               ELSE.
                 "error red: page gap
                 l_red = 'X'.
                 EXIT.
               ENDIF.
               l_azpgno = l_azpgno - 1.
*              check astat.
            ENDLOOP.
            IF l_azpgno > 0.
              l_red = 'X'.
            ENDIF.

            IF l_red = 'X'.
*             red
              PERFORM create_icon USING text-003
                      lc_status_red
                      CHANGING l_statement-proc_stat.
*             l_statement-proc_stat = icon_red_light.
            ELSEIF l_yellow = 'X'.
*             yellow
            PERFORM create_icon USING text-002
                                      lc_status_yellow
                                CHANGING l_statement-proc_stat.
*             l_statement-proc_stat = icon_yellow_light.
            ELSE.
*             green
              PERFORM create_icon USING text-001
                      lc_status_green
                      CHANGING l_statement-proc_stat.
*             l_statement-proc_stat = icon_green_light.
            ENDIF.
          ENDIF.
* end note 2391403
        ELSE.                            " not available in time!!!
          PERFORM create_icon USING text-003
                                    lc_status_red
                              CHANGING l_statement-proc_stat.
*        l_statement-proc_stat = icon_red_light.
        ENDIF.
      ENDIF.
    ELSE.
      l_bankaccounts-procstat_act = icon_light_out.
    ENDIF.

**** DETERMINE G/L ACCT BALANCE STATUS
    IF l_bankaccounts-balstat_act = gc_xon.
      IF l_nofebko = gc_xon.
        PERFORM create_icon USING text-003
                           lc_status_red
                     CHANGING l_statement-bal_stat.
*         l_statement-bal_stat = icon_red_light.
      ELSE.

        CALL FUNCTION 'ZFTE_BSM_GL_BALANCE_OLDGL'
          EXPORTING
            i_hkont              = l_febko-hkont
            i_bukrs              = l_bankaccounts-bukrs
            i_key_date           = l_febko-azdat
          IMPORTING
            e_gl_balance         = l_gl_balance
            e_trans_currency     = l_gl_curr
            e_no_gl_balance      = l_error
          EXCEPTIONS
            no_balance_available = 1.

        IF sy-subrc NE 0.
          PERFORM create_icon USING text-004
                                    lc_status_red
                              CHANGING l_statement-bal_stat.
*        l_statement-bal_stat = icon_red_light.
        ELSEIF l_error = 'X'.
          CLEAR l_error.
          PERFORM create_icon USING text-004
                                    lc_status_red
                              CHANGING l_statement-bal_stat.
*       l_statement-bal_stat = icon_red_light.
        ELSE.
*** check same currency                            Due to alt Acct Curr we cannot check
          IF l_gl_curr = l_febko-waers.

*           note 1292262:
            IF ( L_FEBKO-ESVOZ = 'D' OR L_FEBKO-ESVOZ = 'S' ) AND L_FEBKO-ESBTR > 0.
              L_FEBKO-ESBTR = L_FEBKO-ESBTR * -1.
            ENDIF.

            l_statement-clos_bal_gl   = l_gl_balance.
            l_diff_amt = l_gl_balance - l_febko-esbtr.
          ELSE.
*** convert gl_amt into febko curr
            CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
              EXPORTING
*            CLIENT                  = SY-MANDT
                date                    = i_stmnt_date
                foreign_amount          = l_gl_balance
                foreign_currency        = l_gl_curr
                local_currency          = l_febko-waers
*            RATE                    = 0
*            TYPE_OF_RATE            = 'M'
*            READ_TCURR              = 'X'
              IMPORTING
                local_amount            = l_gl_balance
             EXCEPTIONS
               no_rate_found           = 1
               overflow                = 2
               no_factors_found        = 3
               no_spread_found         = 4
               derived_2_times         = 5
               OTHERS                  = 6.

             l_statement-clos_bal_gl   = l_gl_balance.           "n1924523
             l_diff_amt = l_gl_balance - l_febko-esbtr.          "n1924523

          ENDIF.

          l_statement-diff_stat_gl = l_diff_amt.
** set icon status
          IF l_diff_amt = 0.
            PERFORM create_icon USING text-005
                                    lc_status_green
                              CHANGING l_statement-bal_stat.
*          l_statement-bal_stat = icon_green_light.
          ELSEIF abs( l_diff_amt ) LE abs( l_bankaccounts-amt_diff ).
            PERFORM create_icon USING text-006
                                     lc_status_yellow
                               CHANGING l_statement-bal_stat.
*          l_statement-bal_stat = icon_yellow_light.
          ELSE.
            PERFORM create_icon USING text-007
                                    lc_status_red
                              CHANGING l_statement-bal_stat.
*         l_statement-bal_stat = icon_red_light.
          ENDIF.
*        ENDIF.     " if l_gl_curr
        ENDIF.
      ENDIF.
    ELSEIF l_bankaccounts-balstat_act IS INITIAL.
      l_bankaccounts-balstat_act = icon_light_out.
    ENDIF.

******  DETERMINE SERIAL STATUS
    data l_serial_status type BOOLE_D VALUE ''.
    clear l_serial_status.                                 "note 1320491
    IF l_bankaccounts-serstat_act = gc_xon.
      IF l_nofebko = gc_xon.
        PERFORM create_icon USING text-003
                           lc_status_red
                     CHANGING l_statement-serial_stat.
      ELSE.
** read all up to 5 statements
* begin of note 2391403
        CLEAR: l_yellow, l_red, l_count_aznum.
        l_azpgno = l_febko-azpgno.
        l_aznum = l_febko-aznum.
        LOOP AT lt_febko ASSIGNING <l_febko>
                         WHERE anwnd = l_febko-anwnd
                         AND   absnd = l_febko-absnd
                         AND   bukrs = l_bankaccounts-bukrs.

*         check AZNUM gaps
          IF l_aznum ne <l_febko>-aznum.
*             AZNUM gap -> exit
              l_red = 'X'.
              EXIT.
          ENDIF.
*         check AZPGNO gaps.
          IF l_azpgno = 99999 AND <l_febko>-estyp ne 'M'. "last or normal statement
*           set last page of new aznum
            l_azpgno = <l_febko>-azpgno.
          ELSEIF l_azpgno = 99999.               "intermediate page
*           AZPGNO gap: last page missing
            l_red = 'X'.
          ENDIF.

          IF l_azpgno ne <l_febko>-azpgno and l_azpgno ne '00000'.
*           AZPGNO gap
            l_red = 'X'.
            EXIT.
          ENDIF.

*         determine next AZNUM and AZPGNO
          IF l_azpgno > 1.
            l_azpgno = l_azpgno - 1.
          ELSE.
            IF l_aznum > 1.
              l_count_aznum = l_count_aznum + 1.
              l_aznum = l_aznum - 1.
              l_azpgno = 99999.
            ELSE.
*             AZNUM is 0 -> stop serial check
              EXIT.
            ENDIF.
          ENDIF.
          IF l_count_aznum > 4.
*           check only 5 bank statements.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_red = 'X'.
            PERFORM create_icon USING text-008
                                    lc_status_red
                              CHANGING l_statement-serial_stat.
        ELSE.
          PERFORM create_icon USING text-009
                                    lc_status_green
                              CHANGING l_statement-serial_stat.
        ENDIF.
* end of note 2391403
      ENDIF.
    ELSE.
      l_bankaccounts-serstat_act = icon_light_out.
    ENDIF.

***** DETERMINE RECONCILIATION STATUS
    IF l_bankaccounts-reconstat_act = gc_xon.
      IF l_nofebko = gc_xon.
        PERFORM create_icon USING text-003
                            lc_status_red
                      CHANGING l_statement-recon_stat.
      ELSE.
** do what to do
** we have to find out if posting area 1 and 2 are relevant!!!!

*       begin note 1291766
        REFRESH: lt_febep, lt_kukeys.                           "n2391403
        CLEAR:   l_red, l_yellow, l_green.                      "note 1320491

        IF l_febko-azpgno > '00000'.                            "n2391403
          LOOP AT lt_febko ASSIGNING <l_febko>                  "n2391403
                           WHERE anwnd = l_febko-anwnd          "n2391403
                           AND   absnd = l_febko-absnd          "n2391403
                           AND   bukrs = l_bankaccounts-bukrs   "n2391403
                           AND   aznum = l_febko-aznum.         "n2391403
            APPEND <l_febko>-kukey TO lt_kukeys.                "n2391403
          ENDLOOP.                                              "n2391403

          SELECT * FROM febep  INTO TABLE lt_febep              "n2391403
                   FOR ALL ENTRIES IN lt_kukeys                 "n2391403
                   WHERE kukey = lt_kukeys-kukey.               "n2391403
        ELSE.                                                   "n2391403
          SELECT * FROM febep  INTO TABLE lt_febep
          WHERE kukey =  l_febko-kukey.
        ENDIF.                                                  "n2391403

        LOOP AT lt_febep INTO l_febep.
*       end note 1291766
          IF
          l_febep-vb1ok = gc_xon AND l_febep-vb2ok = gc_xon
          AND NOT ( l_febep-belnr IS INITIAL OR l_febep-nbbln IS INITIAL ).
            l_green = gc_xon.
          ELSEIF
          l_febep-vb1ok = gc_xon
          AND NOT ( l_febep-belnr IS INITIAL OR l_febep-akbln IS INITIAL ).
            l_yellow = gc_xon.
          ELSEIF
          l_febep-vb2ok = gc_xon
          AND NOT ( l_febep-nbbln IS INITIAL OR l_febep-ak1bl IS INITIAL ).
            l_yellow = gc_xon.
          ELSE.
            l_red = gc_xon.
            EXIT.
          ENDIF.
        ENDLOOP.

** overall statement status
        IF l_red = gc_xon.
          PERFORM create_icon USING text-010
                                    lc_status_red
                              CHANGING l_statement-recon_stat.
*       l_statement-recon_stat = icon_red_light.
        ELSEIF l_yellow = gc_xon.
          PERFORM create_icon USING text-011
                                lc_status_yellow
                          CHANGING l_statement-recon_stat.
*        l_statement-recon_stat = icon_yellow_light.
        ELSEIF l_green = gc_xon.
          PERFORM create_icon USING text-012
                                      lc_status_green
                                CHANGING l_statement-recon_stat.
*        l_statement-recon_stat = icon_green_light.
        ENDIF.
      ENDIF.
    ELSE.
      l_bankaccounts-reconstat_act = icon_light_out.
    ENDIF.

    APPEND l_statement TO lt_statement.

  ENDLOOP.

  SORT lt_statement BY tabix.

* Sort according to sort_IDX
  et_statement[] = lt_statement[].

ENDFUNCTION.
