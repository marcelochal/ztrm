FUNCTION z_trm_get_work_day.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DATA_REQUERIDA) TYPE  STICHTAG
*"  EXPORTING
*"     REFERENCE(DATA_ESPERADA) TYPE  STICHTAG
*"  EXCEPTIONS
*"      MOT_FOUND
*"----------------------------------------------------------------------

  DATA: t_days  TYPE STANDARD TABLE OF casdayattr,
        w_days  TYPE casdayattr,
        lv_data TYPE stichtag.

  MOVE data_requerida TO lv_data.
  DO.

    CALL FUNCTION 'DAY_ATTRIBUTES_GET'
      EXPORTING
        factory_calendar           = 'B1'
        holiday_calendar           = 'B1'
        date_from                  = lv_data
        date_to                    = lv_data
        language                   = sy-langu
      TABLES
        day_attributes             = t_days
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.

    IF sy-subrc EQ 0.

      READ TABLE t_days INTO w_days INDEX 1.

      IF w_days-freeday NE abap_true.

        data_esperada = lv_data.

        EXIT.

      ELSE.

        lv_data = lv_data - 1.

      ENDIF.

    ELSE.

      RAISE not_found.

    ENDIF.

  ENDDO.

ENDFUNCTION.
