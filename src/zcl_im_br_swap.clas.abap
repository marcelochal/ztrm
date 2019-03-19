class ZCL_IM_BR_SWAP definition
  public
  final
  create public .

public section.

  interfaces IF_EX_TPM_TRL_MANIPULATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BR_SWAP IMPLEMENTATION.


  method IF_EX_TPM_TRL_MANIPULATE~CHANGE_TITLE_VALUATION.
  endmethod.


  method IF_EX_TPM_TRL_MANIPULATE~EXCL_OTC_FROM_VALUATION.
  endmethod.


  method IF_EX_TPM_TRL_MANIPULATE~MANIPULATE_CALC_BASIS.
  endmethod.


  method IF_EX_TPM_TRL_MANIPULATE~MANIPULATE_DERIVED_FLOWS.
*lcl_swap_fcv=>swap_fx_profit_loss(
*    EXPORTING
*      im_position     = im_position
*      im_bustranscat  = im_bustranscat
*      im_bustransid   = im_bustransid
*      im_tab_flow_org = im_tab_flow_org
*    CHANGING
*      ch_tab_flow     = ch_tab_flow
*    EXCEPTIONS
*      failed          = 1 ).
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*               RAISING failed.
*  ENDIF.

ENDMETHOD.              "if_ex_tpm_trl_manipulate~manipulate_derived_flows


  method IF_EX_TPM_TRL_MANIPULATE~MANIPULATE_VALUATION_FLOWS.
*lcl_swap_fcv=>swap_fx_valuation(
*      EXPORTING
*        im_position       = im_position
*        im_keydate        = im_position_date
*        im_valuation_cat  = im_valuation_cat
*      CHANGING
*        ch_tab_flow       = ch_tab_flow
*        ch_tab_flow_reset = ch_tab_flow_reset
*      EXCEPTIONS
*        failed            = 1 ).
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                 RAISING failed.
*    ENDIF.

ENDMETHOD.           "if_ex_tpm_trl_manipulate~manipulate_valuation_flows
ENDCLASS.
