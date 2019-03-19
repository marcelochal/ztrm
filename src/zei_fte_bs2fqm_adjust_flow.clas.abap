class ZEI_FTE_BS2FQM_ADJUST_FLOW definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_FTE_BS2FQM_ADJUST_FLOW .
protected section.
private section.
ENDCLASS.



CLASS ZEI_FTE_BS2FQM_ADJUST_FLOW IMPLEMENTATION.


  method IF_FTE_BS2FQM_ADJUST_FLOW~ADJUST_FLOW.

* dados em memória
*   IS_FEBKO_EXT
*   IS_FEBEP_EXT
*   IS_FEBKO_INT
*   IS_FEBEP_INT
*   CT_FLOW


    DATA gti_vgint         TYPE RANGE OF char11.
    DATA wa_flow TYPE fqms_flow_core_line_idx.

    REFRESH gti_vgint.

    SELECT sign opti low high
    FROM tvarvc
    INTO TABLE gti_vgint
    WHERE name = 'ZFI_FEBEP_VGINT'
    AND type   = 'S'.

    IF NOT gti_vgint[] IS INITIAL.

      IF is_febep_int-vgint IN gti_vgint.
        LOOP AT ct_flow INTO wa_flow.
          wa_flow-flow_type = '999999'.
          MODIFY ct_flow FROM wa_flow INDEX sy-tabix.
        ENDLOOP.
      ENDIF.

    ENDIF.

  endmethod.
ENDCLASS.
