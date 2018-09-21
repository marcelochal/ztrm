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

    DATA lc_cobranca TYPE char8 VALUE 'COBRANCA'.
    DATA ls_febep    TYPE febep.

    IF sy-uname = 'FS002418' OR sy-uname = 'BC001118'.
      IF it_febep IS NOT INITIAL.
        READ TABLE it_febep INTO ls_febep INDEX 1.

        IF sy-subrc IS INITIAL.
          IF ls_febep-belnr IS NOT INITIAL.
            cs_febko-xbenr = lc_cobranca.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
