class ZES_FQM_FLOW_ADJUST_CORE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_FQM_FLOW_ENHANCEMENTS_CORE .
protected section.
private section.
ENDCLASS.



CLASS ZES_FQM_FLOW_ADJUST_CORE IMPLEMENTATION.


  METHOD if_fqm_flow_enhancements_core~adjust_flows.


*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
    DATA: lv_tabix TYPE sytabix.

*----------------------------------------------------------------------*
* Processamento
*----------------------------------------------------------------------*

    LOOP AT ct_flows ASSIGNING FIELD-SYMBOL(<lfs_ctflows>).
      lv_tabix = sy-tabix.
      IF <lfs_ctflows>-flow_type EQ '999999'.
        DELETE ct_flows INDEX lv_tabix.
      ENDIF.
    ENDLOOP.





  ENDMETHOD.
ENDCLASS.
