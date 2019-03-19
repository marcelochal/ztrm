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

    DATA: ln_vgext_ini(03) TYPE n VALUE '100',
          ln_vgext_fim(03) TYPE n VALUE '300',
          ln_vgext(03)     TYPE n.

    IF it_febep IS NOT INITIAL.

      READ TABLE it_febep INTO ls_febep INDEX 1.

      IF sy-subrc IS INITIAL.

*        IF ls_febep-belnr IS INITIAL.
*          cs_febko-xbenr = lc_extrato.
*        ELSE.
*          cs_febko-xbenr = lc_retorno.
*        ENDIF.

        ln_vgext = ls_febep-vgext.

        IF ( ln_vgext BETWEEN ln_vgext_ini AND ln_vgext_fim ) OR ( ls_febep-vgext IS INITIAL ). "Chamado 1000000878.
          cs_febko-xbenr = lc_extrato.
        ELSE.
          cs_febko-xbenr = lc_retorno.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
