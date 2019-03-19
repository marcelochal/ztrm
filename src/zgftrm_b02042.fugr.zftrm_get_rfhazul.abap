FUNCTION zftrm_get_rfhazul.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_COMPANY_CODE) TYPE  BAPI2042-COMPANY_CODE
*"     REFERENCE(P_TRANSACTION) TYPE  BAPI2042-TRANSACTION
*"  EXPORTING
*"     REFERENCE(E_RFHAZUL) TYPE  TB_RFHAZUL
*"----------------------------------------------------------------------

  SELECT SINGLE rfhazul INTO e_rfhazul
    FROM vtbfha
   WHERE bukrs    = p_company_code
     AND rfha     = p_transaction.

ENDFUNCTION.
