*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
TYPE-POOLS: tpmco, valco.

*----------------------------------------------------------------------*
*       CLASS lcl_swap_fvc DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_swap_fcv DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
*     profit/loss calculation
      swap_fx_profit_loss
        IMPORTING
          im_position     TYPE REF TO cl_position_trl
          im_bustranscat  TYPE tpm_bustranscat
          im_bustransid   TYPE tpm_bustransid
          im_tab_flow_org TYPE trly_badi_manipulate_flow
        CHANGING
          ch_tab_flow     TYPE trly_badi_manipulate_flow
        EXCEPTIONS
          failed,

*     keydate valuation
      swap_fx_valuation
        IMPORTING
          im_position       TYPE REF TO cl_position_trl
          im_keydate        TYPE tpm_trldate
          im_valuation_cat  TYPE tpm_val_cat
        CHANGING
          ch_tab_flow       TYPE trly_badi_manipulate_flow
          ch_tab_flow_reset TYPE trly_badi_manipulate_flow
        EXCEPTIONS
          failed.

  PRIVATE SECTION.

    CLASS-DATA:

      gs_customizing   TYPE vals_cust,
      gv_flg_valuation TYPE char1,
      gv_curr_out      TYPE tpm_position_curr,
      gv_curr_in       TYPE tpm_position_curr.

    CLASS-METHODS:

      load_customizing
        IMPORTING
          im_position TYPE REF TO cl_position_trl
        EXCEPTIONS
          not_defined,

*     p/l calculation for one leg (currency)
      calc_profit_loss
        IMPORTING
          im_position           TYPE REF TO cl_position_trl
          im_currency           TYPE tpm_position_curr
          im_tab_flow           TYPE trly_flow_and_trans
        EXPORTING
          ex_translation_factor TYPE f
        CHANGING
          ch_tab_flow           TYPE trly_badi_manipulate_flow
        EXCEPTIONS
          failed,

*     keydate valuation for one leg (currency)
      calc_valuation
        IMPORTING
          im_position       TYPE REF TO cl_position_trl
          im_keydate        TYPE tpm_trldate
          im_currency       TYPE tpm_position_curr
          im_tab_flow       TYPE trly_flow_and_trans
          im_valuation_cat  TYPE tpm_val_cat
        CHANGING
          ch_tab_flow       TYPE trly_badi_manipulate_flow
          ch_tab_flow_reset TYPE trly_badi_manipulate_flow
        EXCEPTIONS
          failed,

*     determine all relevant values for a set of flows
      get_values
        IMPORTING
          im_position           TYPE REF TO cl_position_trl
          im_tab_flow           TYPE trly_flow_and_trans
        EXPORTING
          ex_nominal_amt_pc     TYPE tpm_amount
          ex_purchase_amt_vc    TYPE tpm_amount
          ex_translation_factor TYPE f
          ex_fx_valuation_vc    TYPE tpm_amount
          ex_fx_val_trans_vc    TYPE tpm_amount
          ex_profit_loss_vc     TYPE tpm_amount,

*     verify the deal relevance
      check_swap
        IMPORTING
          im_position        TYPE REF TO cl_position_trl
        RETURNING
          VALUE(re_flg_swap) TYPE char1
        EXCEPTIONS
          failed,

*     short check for transaction relevance
      check_for_pl_generation
        IMPORTING
          im_bustranscat       TYPE tpm_bustranscat
          im_tab_flow_org      TYPE trly_badi_manipulate_flow
        RETURNING
          VALUE(re_flg_pl_rel) TYPE char1,

*     position information access
      get_pos_attributes
        IMPORTING
          im_position       TYPE REF TO cl_position_trl
        EXPORTING
          ex_pos_man_proc   TYPE tpm_pos_man_proc
          ex_valuation_area TYPE tpm_val_area,

*     flow category
      get_flow_cat
        IMPORTING
          im_flowtype        TYPE trlt_flow-flowtype
        RETURNING
          VALUE(re_flow_cat) TYPE at19-sbktyp,

*     selection of flows
      get_flows_for_trans
        IMPORTING
          im_position   TYPE REF TO cl_position_trl
          im_for_trans  TYPE REF TO cl_transaction_trl OPTIONAL
          im_keydate    TYPE tpm_trldate OPTIONAL
        EXPORTING
          ex_currency_1 TYPE tpm_position_curr
          ex_currency_2 TYPE tpm_position_curr
          ex_tab_flow_1 TYPE trly_flow_and_trans
          ex_tab_flow_2 TYPE trly_flow_and_trans,

*     add valuation reset
      add_valuation_reset
        IMPORTING
          im_val_flow     TYPE trls_badi_manipulate_flow
          im_val_flow_cat TYPE tpm_val_flow_cat
        EXPORTING
          ex_reset_flow   TYPE trls_badi_manipulate_flow
        EXCEPTIONS
          failed.

ENDCLASS.                    "lcl_swap_fcv DEFINITION

*************************************************************************
*----------------------------------------------------------------------*
*       CLASS lcl_swap_fcv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_swap_fcv IMPLEMENTATION.

  METHOD swap_fx_profit_loss.

    DATA:
      lo_query_serv TYPE REF TO cl_query_service_trl,
      ls_flow       TYPE trls_badi_manipulate_flow,
      lo_trans      TYPE REF TO cl_transaction_trl,
      lv_val_area   TYPE tpm_val_area,
      lv_curr1      TYPE tpm_position_curr,
      lv_curr2      TYPE tpm_position_curr,
      lt_flow1      TYPE trly_flow_and_trans,
      lt_flow2      TYPE trly_flow_and_trans.

*   quick check (performance): do we need to generate PL at all?
    CHECK check_for_pl_generation( im_bustranscat  = im_bustranscat
                                   im_tab_flow_org = im_tab_flow_org )
                                   IS NOT INITIAL.
*   do we have a CCIRS?
    CHECK check_swap( im_position ) IS NOT INITIAL.

    load_customizing(
      EXPORTING
        im_position = im_position
      EXCEPTIONS
        not_defined = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*   get the transaction
    lo_query_serv = cl_query_service_trl=>cls_get_trl_query_service( ).

    get_pos_attributes(
      EXPORTING
        im_position       = im_position
      IMPORTING
        ex_valuation_area = lv_val_area ).

    lo_query_serv->get_transaction_by_bustransid(
      EXPORTING
        im_valuation_area = lv_val_area
        im_bustransid     = im_bustransid
      RECEIVING
        re_transaction    = lo_trans ).

*   get all flows up to this transaction
    get_flows_for_trans(
      EXPORTING
        im_position   = im_position
        im_for_trans  = lo_trans
      IMPORTING
        ex_currency_1 = lv_curr1
        ex_currency_2 = lv_curr2
        ex_tab_flow_1 = lt_flow1
        ex_tab_flow_2 = lt_flow2 ).

*   for nominal changes we just consider one leg (for each transaction)
    IF im_bustranscat = tpmco_otc_nominal_amt_change.
      LOOP AT im_tab_flow_org INTO ls_flow
          WHERE amount_cat = tpmco_con_pmcat_ind_change. "1006 - indrect position change
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0 OR ls_flow-position_curr IS INITIAL.
        RETURN.
      ENDIF.
*     keep only the flow-set for this transaction to be processed for P/L
      IF ls_flow-position_curr = lv_curr1.
        CLEAR lt_flow2.
      ELSE.
        CLEAR lt_flow1.
      ENDIF.
    ELSE.
*     this is the close transaction (details -> check_for_pl_generation() )
    ENDIF.

*   calc pl for 1st leg
    calc_profit_loss(
      EXPORTING
        im_position = im_position
        im_currency = lv_curr1
        im_tab_flow = lt_flow1
      CHANGING
        ch_tab_flow = ch_tab_flow
      EXCEPTIONS
        failed      = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 RAISING failed.
    ENDIF.

*   calc pl for 2nd leg
    calc_profit_loss(
      EXPORTING
        im_position = im_position
        im_currency = lv_curr2
        im_tab_flow = lt_flow2
      CHANGING
        ch_tab_flow = ch_tab_flow
      EXCEPTIONS
        failed      = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 RAISING failed.
    ENDIF.

  ENDMETHOD.                    "swap_fx_profit_loss

  METHOD swap_fx_valuation.

    DATA:
      lv_curr1 TYPE tpm_position_curr,
      lv_curr2 TYPE tpm_position_curr,
      lt_flow1 TYPE trly_flow_and_trans,
      lt_flow2 TYPE trly_flow_and_trans.

*   do we have a CCIRS?
    CHECK check_swap( im_position ) IS NOT INITIAL.

    load_customizing(
      EXPORTING
        im_position = im_position
      EXCEPTIONS
        not_defined = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*   do we need to process a valuation?
    CHECK ( gv_flg_valuation IS NOT INITIAL ).

*   get all flows up to keydate (for both currencies)
    get_flows_for_trans(
      EXPORTING
        im_position   = im_position
        im_keydate    = im_keydate
      IMPORTING
        ex_currency_1 = lv_curr1
        ex_currency_2 = lv_curr2
        ex_tab_flow_1 = lt_flow1
        ex_tab_flow_2 = lt_flow2 ).

*   calculate valuation for 1st currency
    calc_valuation(
      EXPORTING
        im_position       = im_position
        im_keydate        = im_keydate
        im_currency       = lv_curr1
        im_tab_flow       = lt_flow1
        im_valuation_cat  = im_valuation_cat
      CHANGING
        ch_tab_flow       = ch_tab_flow
        ch_tab_flow_reset = ch_tab_flow_reset
      EXCEPTIONS
        failed            = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 RAISING failed.
    ENDIF.

*   calculate valuation for 2nd currency
    calc_valuation(
      EXPORTING
        im_position       = im_position
        im_keydate        = im_keydate
        im_currency       = lv_curr2
        im_tab_flow       = lt_flow2
        im_valuation_cat  = im_valuation_cat
      CHANGING
        ch_tab_flow       = ch_tab_flow
        ch_tab_flow_reset = ch_tab_flow_reset
      EXCEPTIONS
        failed            = 1 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 RAISING failed.
    ENDIF.

  ENDMETHOD.                    "swap_fx_valuation


  METHOD calc_valuation.

    DATA:
      lv_val_flow_cat    TYPE tpm_val_flow_cat,
      ls_val_flow        TYPE trls_badi_manipulate_flow,
      ls_reset_flow      TYPE trls_badi_manipulate_flow,
      lv_val_curr        TYPE tpm_valuation_curr,
      lv_nominal_amt_pc  TYPE tpm_amount,
      lv_market_value_vc TYPE tpm_amount,
      lv_purchase_amt_vc TYPE tpm_amount,
      lv_fx_valuation_vc TYPE tpm_amount,
      lv_book_value_vc   TYPE tpm_amount,
      ls_tab_flow        TYPE trls_badi_manipulate_flow.

    CHECK ( im_tab_flow IS NOT INITIAL ).

    im_position->getcurrencies(
      IMPORTING
        ex_valuation_curr = lv_val_curr ).

    CHECK ( im_currency <> lv_val_curr ).

*   get values for flow-set
    get_values(
      EXPORTING
        im_position        = im_position
        im_tab_flow        = im_tab_flow
      IMPORTING
        ex_nominal_amt_pc  = lv_nominal_amt_pc     "current nominal pc
        ex_purchase_amt_vc = lv_purchase_amt_vc    "current purchase value vc
        ex_fx_valuation_vc = lv_fx_valuation_vc ). "valuations so far

*   current book value in local currency (purchase value + valuations)
    lv_book_value_vc = lv_purchase_amt_vc + lv_fx_valuation_vc.

*   market value for current nominal
    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = im_keydate
        foreign_amount   = lv_nominal_amt_pc
        foreign_currency = im_currency
        local_currency   = lv_val_curr
        type_of_rate     = gs_customizing-rate_category
      IMPORTING
        local_amount     = lv_market_value_vc
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING failed.
    ENDIF.

**** Consider the amount interest calculated before
    READ TABLE ch_tab_flow INTO ls_tab_flow INDEX 1.

    IF sy-subrc IS INITIAL.
      lv_book_value_vc = lv_book_value_vc + ls_tab_flow-position_amt.
    ENDIF.

*   NOT IS SCOPE: rules for write-up/down (here always to market value!)
    lv_fx_valuation_vc = lv_market_value_vc - lv_book_value_vc.

*   create valuation flow
    CLEAR: ls_val_flow,
           lv_val_flow_cat.
    ls_val_flow-position_curr = im_currency.
    ls_val_flow-valuation_curr = lv_val_curr.


    IF lv_fx_valuation_vc > 0.
      ls_val_flow-valuation_amt = lv_fx_valuation_vc.
      IF im_currency = gv_curr_in.
        ls_val_flow-amount_cat = '1914'.  "increase purchase curr
        lv_val_flow_cat = 'V500'.
      ELSE.
*       opposite sign!
        ls_val_flow-amount_cat = '1917'.  "decrease sale curr
        lv_val_flow_cat = 'V505'.
      ENDIF.
    ELSEIF lv_fx_valuation_vc < 0.
      ls_val_flow-valuation_amt = - lv_fx_valuation_vc.
      IF im_currency = gv_curr_in.
        ls_val_flow-amount_cat = '1915'.  "decrease purchase curr
        lv_val_flow_cat = 'V501'.
      ELSE.
*       opposite sign!
        ls_val_flow-amount_cat = '1916'.  "increase sale curr
        lv_val_flow_cat = 'V504'.
      ENDIF.
    ENDIF.

    IF lv_val_flow_cat IS NOT INITIAL.
      cl_customizing_tlv=>cls_get_distribution_flowtype(
        EXPORTING
          im_pos_man_proc        = gs_customizing-pos_man_proc
          im_val_flow_cat        = lv_val_flow_cat
        IMPORTING
          ex_dis_flowtype        = ls_val_flow-dis_flowtype
        EXCEPTIONS
          dis_flowtype_not_found = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING failed.
      ENDIF.
      APPEND ls_val_flow TO ch_tab_flow.
    ENDIF.

    IF im_valuation_cat = valco_val_during_year_reset AND lv_val_flow_cat IS NOT INITIAL.
      add_valuation_reset(
        EXPORTING
          im_val_flow     = ls_val_flow
          im_val_flow_cat = lv_val_flow_cat
        IMPORTING
          ex_reset_flow   = ls_reset_flow
        EXCEPTIONS
          failed          = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING failed.
      ENDIF.
      APPEND ls_reset_flow TO ch_tab_flow_reset.
    ENDIF.

  ENDMETHOD.                    "calc_valuation

  METHOD calc_profit_loss.

    DATA:
      lv_val_curr        TYPE tpm_valuation_curr,
      lv_fx_val_trans_vc TYPE tpm_amount,
      lv_profit_loss_vc  TYPE tpm_amount,
      ls_derived_flow    TYPE trls_badi_manipulate_flow.

    CHECK ( im_tab_flow IS NOT INITIAL ).

    im_position->getcurrencies(
      IMPORTING
        ex_valuation_curr = lv_val_curr ).

*   only necessary for foreign currency
    CHECK ( im_currency <> lv_val_curr ).

*   get values for flow-set
    get_values(
      EXPORTING
        im_position           = im_position
        im_tab_flow           = im_tab_flow
      IMPORTING
        ex_translation_factor = ex_translation_factor
        ex_fx_val_trans_vc    = lv_fx_val_trans_vc
        ex_profit_loss_vc     = lv_profit_loss_vc ).

*   build valuation translation flow
    CLEAR ls_derived_flow.
    ls_derived_flow-position_curr = im_currency.
    ls_derived_flow-valuation_curr = lv_val_curr.

    IF lv_fx_val_trans_vc > 0.
      ls_derived_flow-valuation_amt = lv_fx_val_trans_vc.
      IF im_currency = gv_curr_in.
        ls_derived_flow-amount_cat = 'E035'.  "translation positive valuation (purchase)
      ELSE.
*       opposite sign!
        ls_derived_flow-amount_cat = 'E038'.  "translation negative valuation (sale)
      ENDIF.
    ELSEIF lv_fx_val_trans_vc < 0.
      ls_derived_flow-valuation_amt = - lv_fx_val_trans_vc.
      IF im_currency = gv_curr_in.
        ls_derived_flow-amount_cat = 'E036'.  "translation negative valuation (purchase)
      ELSE.
*       opposite sign!
        ls_derived_flow-amount_cat = 'E037'.  "translation positive valuation (sale)
      ENDIF.
    ENDIF.

    IF ls_derived_flow-amount_cat IS NOT INITIAL.
*     get flowtype to be used
      cl_dft_assgn_trl=>cls_get_dist_flowtype(
        EXPORTING
          im_pos_man_proc = gs_customizing-pos_man_proc
          im_amt_pm_cat   = ls_derived_flow-amount_cat
        RECEIVING
          re_dis_flowtype = ls_derived_flow-dis_flowtype
        EXCEPTIONS
          OTHERS          = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING failed.
      ENDIF.
      APPEND ls_derived_flow TO ch_tab_flow.
    ENDIF.

*   build pl flow
    CLEAR ls_derived_flow.
    ls_derived_flow-position_curr = im_currency.
    ls_derived_flow-valuation_curr = lv_val_curr.

    IF lv_profit_loss_vc > 0.
      ls_derived_flow-valuation_amt = lv_profit_loss_vc.
      IF im_currency = gv_curr_in.
        ls_derived_flow-amount_cat = tpmco_pmcat_prof_fx_deal_purch. "profit purchase curr
      ELSE.
*       opposite sign!
        ls_derived_flow-amount_cat = tpmco_pmcat_loss_fx_deal_sale.  "loss sale curr
      ENDIF.
    ELSEIF lv_profit_loss_vc < 0.
      ls_derived_flow-valuation_amt = - lv_profit_loss_vc.
      IF im_currency = gv_curr_in.
        ls_derived_flow-amount_cat = tpmco_pmcat_loss_fx_deal_purch. "loss purchase curr
      ELSE.
*       opposite sign!
        ls_derived_flow-amount_cat = tpmco_pmcat_prof_fx_deal_sale.  "profit sale curr
      ENDIF.
    ENDIF.

    IF ls_derived_flow-amount_cat IS NOT INITIAL.
      cl_dft_assgn_trl=>cls_get_dist_flowtype(
        EXPORTING
          im_pos_man_proc = gs_customizing-pos_man_proc
          im_amt_pm_cat   = ls_derived_flow-amount_cat
        RECEIVING
          re_dis_flowtype = ls_derived_flow-dis_flowtype
        EXCEPTIONS
          OTHERS          = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING failed.
      ENDIF.
      APPEND ls_derived_flow TO ch_tab_flow.
    ENDIF.

  ENDMETHOD.                    "calc_profit_loss

  METHOD load_customizing.

    DATA:
      lv_pos_man_proc  TYPE tpm_pos_man_proc,
      ls_wrk_pm_proc   TYPE trgv_pm_proc,
      ls_wrk_step_proc TYPE trgv_step_proc.

    FIELD-SYMBOLS:
      <step_cat>  TYPE trgv_pm_proc-step_cat_1,
      <procedure> TYPE trgv_pm_proc-procedure_1,
      <flg_val>   TYPE trgv_pm_proc-flg_valuation_1.

    get_pos_attributes(
      EXPORTING
        im_position     = im_position
      IMPORTING
        ex_pos_man_proc = lv_pos_man_proc ).
    IF lv_pos_man_proc = gs_customizing-pos_man_proc.
      RETURN.
    ENDIF.

*   initialize
    CLEAR: gs_customizing,
           gv_flg_valuation.

    cl_pos_man_proc_rule_trg=>cls_get_attributes(
      EXPORTING
        im_pos_man_proc     = lv_pos_man_proc
      IMPORTING
        ex_wrk_pos_man_proc = ls_wrk_pm_proc
      EXCEPTIONS
        pos_man_proc_not_defined = 1 ).
    IF sy-subrc <> 0.
      RAISE not_defined.
    ENDIF.

    DO 5 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN: ls_wrk_pm_proc-step_cat_1      TO <step_cat>,
                  ls_wrk_pm_proc-procedure_1     TO <procedure>,
                  ls_wrk_pm_proc-flg_valuation_1 TO <flg_val>.
        WHEN 2.
          ASSIGN: ls_wrk_pm_proc-step_cat_2      TO <step_cat>,
                  ls_wrk_pm_proc-procedure_2     TO <procedure>,
                  ls_wrk_pm_proc-flg_valuation_2 TO <flg_val>.
        WHEN 3.
          ASSIGN: ls_wrk_pm_proc-step_cat_3      TO <step_cat>,
                  ls_wrk_pm_proc-procedure_3     TO <procedure>,
                  ls_wrk_pm_proc-flg_valuation_3 TO <flg_val>.
        WHEN 4.
          ASSIGN: ls_wrk_pm_proc-step_cat_4      TO <step_cat>,
                  ls_wrk_pm_proc-procedure_4     TO <procedure>,
                  ls_wrk_pm_proc-flg_valuation_4 TO <flg_val>.
        WHEN 5.
          ASSIGN: ls_wrk_pm_proc-step_cat_5      TO <step_cat>,
                  ls_wrk_pm_proc-procedure_5     TO <procedure>,
                  ls_wrk_pm_proc-flg_valuation_5 TO <flg_val>.
      ENDCASE.
      IF <step_cat> = '005'. "fx valuation
        EXIT.
      ENDIF.
    ENDDO.

    IF <step_cat> <> '005'.
      RAISE not_defined.
    ENDIF.

    cl_pos_man_proc_rule_trg=>cls_get_fe_proc(
      EXPORTING
        im_step_proc     = <procedure>
      IMPORTING
        ex_wrk_step_proc = ls_wrk_step_proc
      EXCEPTIONS
        text_not_found   = 1
        not_found        = 2 ).
    IF sy-subrc <> 0.
      RAISE not_defined.
    ENDIF.

    MOVE-CORRESPONDING ls_wrk_step_proc TO gs_customizing.
    gv_flg_valuation = <flg_val>.
    gs_customizing-pos_man_proc = lv_pos_man_proc.
    gs_customizing-pos_man_cat  = ls_wrk_pm_proc-deriv_cat.
    gs_customizing-pos_passiv   = ls_wrk_pm_proc-pos_passiv.

    IF gs_customizing-rate_category IS INITIAL.
      gs_customizing-rate_category = 'M'.
    ENDIF.

  ENDMETHOD.                    "read_customizing

  METHOD get_values.

*  Precondition: Flows are sorted

    DATA:
      ls_flow           TYPE trls_flow_and_trans,
      lv_bustranscat    TYPE tpm_bustranscat,
      lv_purchase_amt   TYPE tpm_valuation_amt,
      lv_nominal_amt    TYPE tpm_nominal_amt,
      lv_fx_val_amt     TYPE tpm_valuation_amt,
      lv_factor         TYPE f,
      lv_transl_amt     TYPE tpm_valuation_amt,
      lv_transl_amt_val TYPE tpm_valuation_amt,
      lv_pl_amt         TYPE tpm_valuation_amt.

    LOOP AT im_tab_flow INTO ls_flow.
      ls_flow-transaction->getattributes(
        IMPORTING
          ex_bustranscat = lv_bustranscat ).

      CASE lv_bustranscat.
        WHEN tpmco_otc_deal_close.
          EXIT. "last trans, keep pl from last nominal decrease

        WHEN tpmco_otc_nominal_amt_change.
          CLEAR: lv_pl_amt,      "reset (we only keep the last flows pl)
                 lv_transl_amt,  "reset
                 lv_transl_amt_val,
                 lv_factor.

          CASE get_flow_cat( ls_flow-flowtype ).
            WHEN 10.       "increase
              lv_purchase_amt = lv_purchase_amt + ls_flow-valuation_amt.
              lv_nominal_amt  = lv_nominal_amt  + ls_flow-nominal_amt.

            WHEN 11 OR 12. "decrease
              lv_factor = ls_flow-nominal_amt / lv_nominal_amt.

*             translation (purchase value to pl)
              lv_transl_amt = lv_purchase_amt * lv_factor.
              lv_purchase_amt = lv_purchase_amt - lv_transl_amt.

*             translation (valuation value to pl)
              lv_transl_amt_val = lv_fx_val_amt * lv_factor.
              lv_fx_val_amt = lv_fx_val_amt - lv_transl_amt_val.

*             profit/loss
              lv_pl_amt = ls_flow-valuation_amt - lv_transl_amt - lv_transl_amt_val.

*             adjust nominal
              lv_nominal_amt = lv_nominal_amt - ls_flow-nominal_amt.

            WHEN OTHERS.
          ENDCASE.

        WHEN tpmco_valuation OR tpmco_valuation_reset.

          CASE ls_flow-amount_cat.
            WHEN '1914'   "increase purchase curr
              OR '1917'.  "decrease sale curr (inv.sign)

              lv_fx_val_amt = lv_fx_val_amt + ls_flow-valuation_amt.

            WHEN '1915'   "decrease purchase curr
              OR '1916'.  "increase sale curr (inv.sign)

              lv_fx_val_amt = lv_fx_val_amt - ls_flow-valuation_amt.

          ENDCASE.

        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    ex_nominal_amt_pc = lv_nominal_amt.
    ex_purchase_amt_vc = lv_purchase_amt.
    ex_translation_factor = lv_factor.
    ex_profit_loss_vc = lv_pl_amt.
    ex_fx_valuation_vc = lv_fx_val_amt.
    ex_fx_val_trans_vc = lv_transl_amt_val.

  ENDMETHOD.                    "get_VALUES

  METHOD check_swap.

    DATA:
      ls_tzpa        TYPE tzpa,
      ls_atpa        TYPE atpa,
      ls_diff_values TYPE difs_diff_values,
      ls_deal        TYPE vtbfha,
      lo_identifier  TYPE REF TO cl_pos_identifier_dif.

* Determine product category ----------------------------------------
    im_position->getattributes(
      IMPORTING
        ex_identifier = lo_identifier ).

    ls_diff_values = lo_identifier->get_diff_values( ).

* get the product category
    CALL FUNCTION 'PRODUCT_TYPE_READ_SEC'
      EXPORTING
        i_gsart = ls_diff_values-product_type
      IMPORTING
        e_tzpa  = ls_tzpa
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc <> 0.
      RAISE failed.
    ENDIF.

    IF ls_tzpa-sanlf = tpmco_pc_swap.
      CALL FUNCTION 'PRODUCT_TYPE_DATA_READ_ADD'
        EXPORTING
          im_product_type = ls_diff_values-product_type
        IMPORTING
          ex_atpa         = ls_atpa
        EXCEPTIONS
          OTHERS          = 1.
      IF sy-subrc <> 0.
        RAISE failed.
      ENDIF.

      IF ls_atpa-jwswap = abap_true.  "CCIRS
*       an fx-pl makes only sense for CCIRS, otherwise we have the same value
*       on in-/outgoing side.
        re_flg_swap = abap_true.
      ENDIF.
    ENDIF.

    CHECK re_flg_swap IS NOT INITIAL.

*   get currencies
    CALL FUNCTION 'DEALDATA_READ'
      EXPORTING
        companycode      = ls_diff_values-company_code
        dealno           = ls_diff_values-deal_number
      IMPORTING
        deal             = ls_deal
      EXCEPTIONS
        deal_not_found   = 1
        status_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      CLEAR re_flg_swap.
      RETURN.
    ENDIF.

    gv_curr_out = ls_deal-wgschft1. "out
    gv_curr_in  = ls_deal-wgschft2. "in

  ENDMETHOD.                    "check_swap

  METHOD check_for_pl_generation.

    DATA
      ls_flow TYPE trls_badi_manipulate_flow.

    CLEAR re_flg_pl_rel.
    CASE im_bustranscat.
      WHEN tpmco_otc_deal_close.
        re_flg_pl_rel = abap_true.
      WHEN tpmco_otc_nominal_amt_change.
        LOOP AT im_tab_flow_org INTO ls_flow
          WHERE amount_cat = tpmco_con_pmcat_ind_change. "1006 - indrect position change
          IF get_flow_cat( ls_flow-dis_flowtype ) = '11'. "nominal reduction
            re_flg_pl_rel = abap_true.
          ENDIF.
          EXIT.
        ENDLOOP.
      WHEN OTHERS.
        CLEAR re_flg_pl_rel.
    ENDCASE.

  ENDMETHOD.                    "check_for_pl_generation

  METHOD get_pos_attributes.

    DATA:
      ls_diff_values TYPE difs_diff_values,
      lo_identifier  TYPE REF TO cl_pos_identifier_dif.

    im_position->getattributes(
      IMPORTING
        ex_identifier = lo_identifier ).

    ls_diff_values = lo_identifier->get_diff_values( ).
    ex_valuation_area = ls_diff_values-valuation_area.

    im_position->if_assessable_position_val~get_pos_man_proc(
      RECEIVING
        re_pos_man_proc = ex_pos_man_proc ).

  ENDMETHOD.                    "get_pos_attributes

  METHOD get_flows_for_trans.

    DATA:
      lo_trg_serv       TYPE REF TO cl_service_trg,
      lo_query_serv     TYPE REF TO cl_query_service_trl,
      lt_trans          TYPE trly_transaction,
      lt_trans_sort     TYPE trgy_sortable_transaction,
      lo_trans_sort     TYPE REF TO if_sortable_transaction_trg,
      lo_trans          TYPE REF TO cl_transaction_trl,
      lt_flow           TYPE trly_flow,
      ls_flow           TYPE trls_flow,
      ls_flow_and_trans TYPE trls_flow_and_trans.

    lo_query_serv = cl_query_service_trl=>cls_get_trl_query_service( ).
    lo_trg_serv   = cl_service_trg=>cls_get_general_service( ).

    lo_query_serv->get_transactions_in_interval(
      EXPORTING
        im_search_strategy = tpmco_search_db_ro
        im_position        = im_position
        im_incl1           = 0
        im_transaction2    = im_for_trans
        im_date2           = im_keydate
        im_incl2           = 1
        im_incl_deletable  = tpmco_con_trl_incl
        im_incl_reversable = tpmco_con_trl_incl
        im_incl_reversed   = tpmco_con_trl_excl
      RECEIVING
        re_tab_transaction = lt_trans
      EXCEPTIONS
        OTHERS             = 1 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_trans INTO lo_trans.
      APPEND lo_trans TO lt_trans_sort.
    ENDLOOP.

    lo_trg_serv->sort_transactions(
      CHANGING
        c_tab_transaction = lt_trans_sort ).

    LOOP AT lt_trans_sort INTO lo_trans_sort.

      lo_trans ?= lo_trans_sort.

      lo_trans->getattributes(
        IMPORTING
          ex_tab_flow = lt_flow ).

*     separate currencies
      LOOP AT lt_flow INTO ls_flow
        WHERE position = im_position
          AND ( amount_cat = tpmco_con_pmcat_ind_change "1006 - indrect position change (nominals)
             OR amount_cat = '1914'  " valuation increase purchase side
             OR amount_cat = '1915'  " valuation decrease purchase side
             OR amount_cat = '1916'  " valaution increase sale side
             OR amount_cat = '1917' )" valuation decrease sale side
          AND position_curr IS NOT INITIAL.
        MOVE-CORRESPONDING ls_flow TO ls_flow_and_trans.
        ls_flow_and_trans-transaction = lo_trans.

        IF ex_currency_1 IS INITIAL OR
           ex_currency_1 = ls_flow_and_trans-position_curr.

          ex_currency_1 = ls_flow_and_trans-position_curr.

        ELSEIF ex_currency_1 <> ls_flow_and_trans-position_curr AND
               ex_currency_2 IS INITIAL OR
               ex_currency_2 = ls_flow_and_trans-position_curr.

          ex_currency_2 = ls_flow_and_trans-position_curr.
        ELSE.
          MESSAGE x010(0t) WITH 'Currency Error'.
        ENDIF.
        IF ex_currency_1 = ls_flow_and_trans-position_curr.
          APPEND ls_flow_and_trans TO ex_tab_flow_1.
        ELSE.
          APPEND ls_flow_and_trans TO ex_tab_flow_2.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "get_flows_for_trans

  METHOD get_flow_cat.

    DATA:
      lv_sbktyp TYPE tb_sbktyp,
      ls_atfta  TYPE atfta,
      lt_atfta  TYPE trgy_atfta.

    CLEAR re_flow_cat.

    SELECT * FROM atfta INTO TABLE lt_atfta
      WHERE rantyp       = '6'
        AND dis_flowtype = im_flowtype.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_atfta INTO ls_atfta.
      SELECT SINGLE sbktyp
        FROM at19 INTO lv_sbktyp
        WHERE rantyp  = ls_atfta-rantyp
          AND sbewart = ls_atfta-sfhazba.
      CHECK sy-subrc = 0.
      re_flow_cat = lv_sbktyp.
      RETURN.
    ENDLOOP.

  ENDMETHOD.                    "get_flow_cat

  METHOD add_valuation_reset.

    DATA:
      ls_reset_flow   TYPE trls_badi_manipulate_flow,
      lv_val_flow_cat TYPE tpm_val_flow_cat.

    IF im_val_flow_cat IS NOT INITIAL AND im_val_flow IS NOT INITIAL.
      ls_reset_flow = im_val_flow.
      lv_val_flow_cat = im_val_flow_cat.
      lv_val_flow_cat+0(1) = 'R'.

      cl_valuation_rules_val=>cls_get_amount_cat(
        EXPORTING
          im_val_flow_cat = lv_val_flow_cat
          im_pos_man_proc = gs_customizing-pos_man_proc
        RECEIVING
          re_amount_cat   = ls_reset_flow-amount_cat ).
      IF NOT sy-subrc IS INITIAL.
        MESSAGE x899(tpm_tlv1).
      ENDIF.

      cl_customizing_tlv=>cls_get_distribution_flowtype(
        EXPORTING
          im_pos_man_proc        = gs_customizing-pos_man_proc
          im_val_flow_cat        = lv_val_flow_cat
        IMPORTING
          ex_dis_flowtype        = ls_reset_flow-dis_flowtype
        EXCEPTIONS
          dis_flowtype_not_found = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   RAISING failed.
      ENDIF.

      ex_reset_flow = ls_reset_flow.

    ENDIF.
  ENDMETHOD .                   "add_valuation_reset

ENDCLASS.                    "lcl_swap_fcv IMPLEMENTATION
