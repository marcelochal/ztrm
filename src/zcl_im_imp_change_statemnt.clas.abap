class ZCL_IM_IMP_CHANGE_STATEMNT definition
  public
  final
  create public .

public section.

  interfaces IF_EX_FIEB_CHANGE_STATEMNT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IMP_CHANGE_STATEMNT IMPLEMENTATION.


  METHOD if_ex_fieb_change_statemnt~change_data.

    DATA lc_retorno  TYPE char7 VALUE 'RETORNO'.
    DATA lc_extrato  TYPE char7 VALUE 'EXTRATO'.
    DATA ls_febep    TYPE febep.

    IF it_febep IS NOT INITIAL.
      READ TABLE it_febep INTO ls_febep INDEX 1.

      IF sy-subrc IS INITIAL.
        IF ls_febep-belnr IS INITIAL.
          cs_febko-xbenr = lc_extrato.
        ELSE.
          cs_febko-xbenr = lc_retorno.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
